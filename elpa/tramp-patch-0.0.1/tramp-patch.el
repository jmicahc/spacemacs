;;; tramp-patch.el --- a simple package                     -*- lexical-binding: t; -*-

;; Author: John Collins <collins@braincorp.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This patches tamp to support our idiosyncratic prompt.

;;; Code:

(provide 'tramp-patch)

(eval-after-load "tramp"
  (defun tramp-maybe-open-connection (vec)
    "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
    (tramp-check-proper-method-and-host vec)

    (let ((p (tramp-get-connection-process vec))
          (process-name (tramp-get-connection-property vec "process-name" nil))
          (process-environment (copy-sequence process-environment))
          (pos (with-current-buffer (tramp-get-connection-buffer vec) (point))))

      ;; If Tramp opens the same connection within a short time frame,
      ;; there is a problem.  We shall signal this.
      (unless (or (and p (processp p) (memq (process-status p) '(run open)))
                  (not (equal (butlast (append vec nil) 2)
                              (car tramp-current-connection)))
                  (> (tramp-time-diff
                      (current-time) (cdr tramp-current-connection))
                     (or tramp-connection-min-time-diff 0)))
        (throw 'suppress 'suppress))

      ;; If too much time has passed since last command was sent, look
      ;; whether process is still alive.  If it isn't, kill it.  When
      ;; using ssh, it can sometimes happen that the remote end has hung
      ;; up but the local ssh client doesn't recognize this until it
      ;; tries to send some data to the remote end.  So that's why we
      ;; try to send a command from time to time, then look again
      ;; whether the process is really alive.
      (condition-case nil
          (when (and (> (tramp-time-diff
                         (current-time)
                         (tramp-get-connection-property
                          p "last-cmd-time" '(0 0 0)))
                        60)
                     p (processp p) (memq (process-status p) '(run open)))
            (tramp-send-command vec "echo are you awake" t t)
            (unless (and (memq (process-status p) '(run open))
                         (tramp-wait-for-output p 10))
              ;; The error will be caught locally.
              (tramp-error vec 'file-error "Awake did fail")))
        (file-error
         (tramp-cleanup-connection vec t)
         (setq p nil)))

      ;; New connection must be opened.
      (condition-case err
          (unless (and p (processp p) (memq (process-status p) '(run open)))

            ;; If `non-essential' is non-nil, don't reopen a new connection.
            (when (and (boundp 'non-essential) (symbol-value 'non-essential))
              (throw 'non-essential 'non-essential))

            (with-tramp-progress-reporter
                vec 3
                (if (zerop (length (tramp-file-name-user vec)))
                    (format "Opening connection for %s using %s"
                            (tramp-file-name-host vec)
                            (tramp-file-name-method vec))
                  (format "Opening connection for %s@%s using %s"
                          (tramp-file-name-user vec)
                          (tramp-file-name-host vec)
                          (tramp-file-name-method vec)))

              (catch 'uname-changed
                ;; Start new process.
                (when (and p (processp p))
                  (delete-process p))
                (setenv "TERM" tramp-terminal-type)
                (setenv "LC_ALL" (tramp-get-local-locale vec))
                (if (stringp tramp-histfile-override)
                    (setenv "HISTFILE" tramp-histfile-override)
                  (if tramp-histfile-override
                      (progn
                        (setenv "HISTFILE")
                        (setenv "HISTFILESIZE" "0")
                        (setenv "HISTSIZE" "0"))))
                (setenv "PROMPT_COMMAND")
                (setenv "PS1" tramp-initial-end-of-output)
                (unless (stringp tramp-encoding-shell)
                  (tramp-error vec 'file-error "`tramp-encoding-shell' not set"))
                (let* ((target-alist (tramp-compute-multi-hops vec))
                       ;; We will apply `tramp-ssh-controlmaster-options'
                       ;; only for the first hop.
                       (options (tramp-ssh-controlmaster-options vec))
                       (process-connection-type tramp-process-connection-type)
                       (process-adaptive-read-buffering nil)
                       (coding-system-for-read nil)
                       ;; This must be done in order to avoid our file
                       ;; name handler.
                       (p (let ((default-directory
                                  (tramp-compat-temporary-file-directory)))
                            (apply
                             'start-process
                             (tramp-get-connection-name vec)
                             (tramp-get-connection-buffer vec)
                             (if tramp-encoding-command-interactive
                                 (list tramp-encoding-shell
                                       tramp-encoding-command-interactive)
                               (list tramp-encoding-shell))))))

                  ;; Set sentinel and query flag.
                  (tramp-set-connection-property p "vector" vec)
                  (set-process-sentinel p 'tramp-process-sentinel)
                  (tramp-compat-set-process-query-on-exit-flag p nil)
                  (setq tramp-current-connection
                        (cons (butlast (append vec nil) 2) (current-time))
                        tramp-current-host (system-name))

                  (tramp-message
                   vec 6 "%s" (mapconcat 'identity (process-command p) " "))

                  ;; Check whether process is alive.
                  (tramp-barf-if-no-shell-prompt
                   p 10
                   "Couldn't find local shell prompt for %s" tramp-encoding-shell)

                  ;; Now do all the connections as specified.
                  (while target-alist
                    (let* ((hop (car target-alist))
                           (l-method (tramp-file-name-method hop))
                           (l-user (tramp-file-name-user hop))
                           (l-host (tramp-file-name-host hop))
                           (l-port nil)
                           (login-program
                            (tramp-get-method-parameter hop 'tramp-login-program))
                           (login-args
                            (tramp-get-method-parameter hop 'tramp-login-args))
                           (login-env
                            (tramp-get-method-parameter hop 'tramp-login-env))
                           (async-args
                            (tramp-get-method-parameter hop 'tramp-async-args))
                           (connection-timeout
                            (tramp-get-method-parameter
                             hop 'tramp-connection-timeout))
                           (gw-args
                            (tramp-get-method-parameter hop 'tramp-gw-args))
                           (gw (let ((tramp-verbose 0))
                                 (tramp-get-file-property hop "" "gateway" nil)))
                           (g-method (and gw (tramp-file-name-method gw)))
                           (g-user (and gw (tramp-file-name-user gw)))
                           (g-host (and gw (tramp-file-name-real-host gw)))
                           (command login-program)
                           ;; We don't create the temporary file.  In
                           ;; fact, it is just a prefix for the
                           ;; ControlPath option of ssh; the real
                           ;; temporary file has another name, and it is
                           ;; created and protected by ssh.  It is also
                           ;; removed by ssh when the connection is
                           ;; closed.  The temporary file name is cached
                           ;; in the main connection process, therefore
                           ;; we cannot use `tramp-get-connection-process'.
                           (tmpfile
                            (with-tramp-connection-property
                                (get-process (tramp-buffer-name vec)) "temp-file"
                              (make-temp-name
                               (expand-file-name
                                tramp-temp-name-prefix
                                (tramp-compat-temporary-file-directory)))))
                           spec r-shell)

                      ;; Add arguments for asynchronous processes.
                      (when (and process-name async-args)
                        (setq login-args (append async-args login-args)))

                      ;; Add gateway arguments if necessary.
                      (when gw
                        (tramp-set-connection-property p "gateway" t)
                        (when gw-args
                          (setq login-args (append gw-args login-args))))

                      ;; Check for port number.  Until now, there's no
                      ;; need for handling like method, user, host.
                      (when (string-match tramp-host-with-port-regexp l-host)
                        (setq l-port (match-string 2 l-host)
                              l-host (match-string 1 l-host)))

                      ;; Check, whether there is a restricted shell.
                      (dolist (elt tramp-restricted-shell-hosts-alist)
                        (when (string-match elt tramp-current-host)
                          (setq r-shell t)))

                      ;; Set variables for computing the prompt for
                      ;; reading password.  They can also be derived
                      ;; from a gateway.
                      (setq tramp-current-method (or g-method l-method)
                            tramp-current-user   (or g-user   l-user)
                            tramp-current-host   (or g-host   l-host))

                      ;; Add login environment.
                      (when login-env
                        (setq
                         login-env
                         (mapcar
                          (lambda (x)
                            (setq x (mapcar (lambda (y) (format-spec y spec)) x))
                            (unless (member "" x) (mapconcat 'identity x " ")))
                          login-env))
                        (while login-env
                          (setq command
                                (format
                                 "%s=%s %s"
                                 (pop login-env)
                                 (tramp-shell-quote-argument (pop login-env))
                                 command)))
                        (setq command (concat "env " command)))

                      ;; Replace `login-args' place holders.
                      (setq
                       l-host (or l-host "")
                       l-user (or l-user "")
                       l-port (or l-port "")
                       spec (format-spec-make ?t tmpfile)
                       options (format-spec options spec)
                       spec (format-spec-make
                             ?h l-host ?u l-user ?p l-port ?c options)
                       command
                       (concat
                        ;; We do not want to see the trailing local
                        ;; prompt in `start-file-process'.
                        (unless r-shell "exec ")
                        command " "
                        (mapconcat
                         (lambda (x)
                           (setq x (mapcar (lambda (y) (format-spec y spec)) x))
                           (unless (member "" x) (mapconcat 'identity x " ")))
                         login-args " ")
                        ;; Local shell could be a Windows COMSPEC.  It
                        ;; doesn't know the ";" syntax, but we must exit
                        ;; always for `start-file-process'.  It could
                        ;; also be a restricted shell, which does not
                        ;; allow "exec".
                        (when r-shell " && exit || exit")))

                      ;; Send the command.
                      (tramp-message vec 3 "Sending command `%s'" command)
                      (tramp-send-command vec (concat command " -t /bin/bash") t t)
                      (tramp-process-actions
                       p vec pos tramp-actions-before-shell
                       (or connection-timeout tramp-connection-timeout))
                      (tramp-message
                       vec 3 "Found remote shell prompt on `%s'" l-host))
                    ;; Next hop.
                    (setq options ""
                          target-alist (cdr target-alist)))

                  ;; Make initial shell settings.
                  (tramp-open-connection-setup-interactive-shell p vec)

                  ;; Mark it as connected.
                  (tramp-set-connection-property p "connected" t)))))

        ;; Cleanup, and propagate the signal.
        ((error quit)
         (tramp-cleanup-connection vec t)
         (signal (car err) (cdr err))))))

  )

;;; tramp-patch.el ends here
