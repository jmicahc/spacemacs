;;; brainos.el --- a simple package                     -*- lexical-binding: t; -*-

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

;; Adds minimal support for programming brainos sandbox from host.

;;; Code:

(provide 'brainos)
(require 'evil)
(require 'tramp)
(require 'projectile)

(defconst brain-method "ssh")

(defconst brain-user "brain")

(defconst brain-host "sandbox")

(defconst brain-repo-dir "/opt")

(defcustom shining-repo-dir "/home/parallels/Workspace/shining_software"
  "Shining Software repository location")

(defvar brainos-enable-sandbox-support nil)

(defconst brain-create-roc-client "
import os
import json
from shining_software.roc.roc_client import RocClient

def create_roc_client(username):
    home = os.path.expanduser(\"~\")
    credentials = os.path.join(home, \"rocc.json\")
    with open(credentials) as f:
        roc_cred = json.load(f)
    return RocClient(url=roc_cred[\"RocUrl\"], username=username, token=roc_cred[\"Token\"])

roc_client = create_roc_client('collins')
")


(defmacro py (&rest body)
  (apply 'string body))


(defun tramp-file-namep (filename)
  (condition-case nil
      (when (tramp-dissect-file-name filename)
        t)
    (error nil)))

;;;###autoload
(defun brain-patch (args)
  (interactive
   (cond ((equal current-prefix-arg nil) (list nil))
         (t (list (read-string "args: ")))))
  (save-excursion
    (let ((rootdir (projectile-project-root)))
      ;; Save current python buffers.
      (mapc (lambda (buff)
              (when (string-match-p ".*.py" (buffer-name buff))
                (with-current-buffer buff
                  (save-buffer))))
            (buffer-list))
      (apply 'start-process
             "brain-patch"
             "*brain-patch*"
             "brain-patch"
             (concat "--shining-repo=" shining-repo-dir)
             (when args (split-string args " ")))
      (message "Started brain-patch Process"))))

(define-key evil-normal-state-map (kbd "P") 'brain-patch);;;###autoload

;; ## Git

;;;###autoload
(defun brain-update-submodules ()
  (interactive)
  (async-shell-command "git submodule update --recursive --init"))

(define-key evil-normal-state-map (kbd ",bgu") 'brain-update-submodules)

;; ## Replays

(setq brain-play-replay-history '())
;;;###autoload
(defun brain-play-replay ()
  (interactive)
  (let ((telemetry-id (read-string "telemetry-id: " "" '(brain-play-replay-history . 0))))
    (setq brain-play-replay-history (cons telemetry-id brain-play-replay-history))
    (start-process "brain-player" "*brain-player*" "brain-player" telemetry-id)))

brain-play-replay-history

(define-key evil-normal-state-map (kbd ",br") 'brain-play-replay)

;; ## Roc

;;;###autoload
(defun brain-rocc-dev-connect ()
  (interactive)
  (let ((password (secret-lookup "roc")))
    (shell-command (concat (format "echo %s" password)
                           " | rocc -u https://api.dev.roc.braincorp.com login -u collins -d 1000000"
                           "; scp ~/rocc.json brain@sandbox:~"))))


;;;###autoload
(defun brain-rocc-prod-connect ()
  (interactive)
  (let ((password (secret-lookup "roc")))
    (shell-command (concat (format "echo %s" password)
                           "| rocc -u https://api.prod.roc.braincorp.com login -u collins -d 1000000"
                           "; scp ~/rocc.json brain@sandbox:~"))))


;;;###autoload
(defun brain-sandbox-send (cmd)
  (save-excursion
    (let ((current-directory (file-name-directory (buffer-file-name))))
      (cd (format "/ssh:%s@sandbox:~" brain-user))
      (async-shell-command cmd)
      (cd current-directory))))


;;;###autoload
(defun brain-sandbox-dev-roc-client ()
  (interactive)
  (brain-rocc-dev-connect)
  (python-shell-send-string brain-create-roc-client))


;;;###autoload
(defun brain-sandbox-prod-roc-client ()
  (interactive)
  (brain-rocc-prod-connect)
  (python-shell-send-string brain-create-roc-client))


;;;###autoload
(defun brain-sandbox-setup ()
  (interactive)
  (brain-sandbox-send (concat "sudo pip install --upgrade pip; "
                              "sudo pip install ipython pdbpp; "
                              "echo 'source /opt/shining_software/use_repo.sh' >> ~/.bashrc; "
                              "cd ~; git clone https://github.com/jparise/python-reloader; cd python-reloader; sudo pip install .")))

(define-key evil-normal-state-map (kbd ",bss") 'brain-sandbox-setup)

(setq s "from __future__ import division

import time
import logging

from enum import Enum
from collections import namedtuple
from brainos.utils.logger import logging_attempt_set_activity_name

from brainos.utils.result import Result
from brainos.utils.logger import get_configured_logger




TickResult = namedtuple('TickResult', ['payload', 'cycledata'])")

(message (replace-regexp-in-string "\n+" "\n" s))

;;;###autoload
(defun brain-python-send-region (start end &optional send-main msg)
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg t))
  (let ((string (string-trim
                 (replace-regexp-in-string
                  "\"\"\".*\"\"\"\"" ""
                  (replace-regexp-in-string
                   "\n+" "\n" (python-shell-buffer-substring start end (not send-main)))))))
    (setq brain-python-last-region string)
    ;;(python-shell-send-region start end send-main msg)
    (let ((process (python-shell-get-process-or-error msg)))
      (comint-send-string process "\\\n")
      (comint-send-string process string)
      (sleep-for 1)
      (comint-send-string process "\n\n"))))

(string-trim "\n\nabc\n\n")
(python-shell-internal-send-string "")

;;;###autoload
(defun brain-python-send-last-region ()
  (interactive)
  (python-shell-send-string brain-python-last-region))


(define-key evil-normal-state-map (kbd ",sL") 'brain-python-send-region)
(define-key evil-normal-state-map (kbd ",sl") 'brain-python-send-last-region)


;;;###autoload
(defun python-reload-symbol-at-point ()
  (interactive)
  (let ((sym (python-info-current-symbol)))
    (python-shell-send-string (format "
import inspect
import reloader
import types
module = inspect.getmodule(%s)
reloader.reload(module)
if isinstance(%s, (types.FunctionType, types.MethodType)):
    %s = getattr(module, '%s')
" sym sym sym sym))
    (python-shell-send-buffer)
    (message (concat "reloaded " sym))))


;;;###autoload
(define-key evil-normal-state-map (kbd ",rl") 'python-reload-symbol-at-point)


(defun wrap-python-shell-send-string (orig-fun &rest args)
  (if brainos-enable-sandbox-support
      (if-let ((f (buffer-file-name)))
          (condition-case nil
              (progn
                (tramp-dissect-file-name f)
                (apply orig-fun args))
            (error (let ((d (file-name-directory f)))
                     (cd (format "/%s:%s@%s:/" brain-method brain-user
                                 brain-host))
                     (apply orig-fun args)
                     (cd d))))
        (progn
          (cd "/ssh:brain@sandbox:/")
          (apply orig-fun args)))
    (apply orig-fun args)))


(defun wrap-python-shell-run (orig-fun &rest args)
  "Send strings to remote interpreter."
  (if brainos-enable-sandbox-support
      (let ((d (file-name-directory (buffer-file-name))))
        (progn
          (setq python-shell-interpreter-args "--simple-prompt -i")
          (setq python-shell-interpreter "ipython")
          (condition-case nil
              (progn
                (tramp-dissect-file-name d)
                (apply orig-fun args))
            (error (progn
                     (cd (format "/%s:%s@%s:/" brain-method brain-user brain-host))
                     (apply orig-fun args)
                     (cd d))))))
    (apply orig-fun args)))


(defun brain-sandbox-path ()
  (let ((project-rel-path (spacemacs/projectile-copy-file-path))
        (sandbox-abs-path (concat "/opt/shining_software/"
                                  (replace-regexp-in-string "src/" "" project-rel-path))))
    (if (tramp-file-namep filename)
        (let ((tramp-stuct (tramp-dissect-file-name filename)))
          (foramt "/%s:%s@%s|%s@%s#%s:%s"
                  (tramp-file-name-method tramp-struct)
                  (tramp-file-name-user tramp-struct)
                  (tramp-file-name-host tramp-struct)
                  (or (tramp-file-name-port tramp-struct) "22")
                  brain-user
                  brain-host
                  sandbox-abs-path))
      (format "/%s:%s@%s:%s" brain-method brain-user brain-host))))


(defun brain-shining-project-p ()
  "Robust way to tell if we're in a shining_software project"
  (let ((project-vcs (projectile-project-vcs))
        (project-root (projectile-project-root)))
    (when (equal "git" project-vcs)
      (let ((git-url (shell-command-to-string
                      (format "git -C %s remote.origin.url" project-root))))
        (string-match-p "braincorp/shining_software" git-url)))))


(defun pytest-parse-tramp-file-name-structure (tests)
  (mapcar (lambda (test)
            (condition-case nil
                (tramp-dissect-file-name test)
              (error (format "%s/%s"
                             brain-repo-dir
                             (concat "shining_software/"
                                     (substring test
                                                (+ (string-match "shining_software.*" test)
                                                   (length "shining_software/src/"))))))))
          tests))


(defun pytest-run-patched (&optional tests flags)
  "Run pytest.
     Optional argument TESTS Tests to run.
     Optional argument FLAGS py.test command line flags."
  (interactive "fTest directory or file: \nspy.test flags: ")
  (let* ((pytest (pytest-find-test-runner))
         (tests (cond
                 ((not tests)
                  (list "."))
                 ((listp tests) tests)
                 ((stringp tests)
                  (split-string tests))))
         (tnames (mapconcat (apply-partially 'format "'%s'")
                            tests
                            " "))
         (cmd-flags (if flags flags pytest-cmd-flags))
         (use-comint (s-contains? "pdb" cmd-flags)))
    (funcall #'(lambda (command)
                 (compilation-start command
                                    use-comint
                                    (lambda (mode)
                                      (concat (pytest-get-temp-buffer-name)))))
             (pytest-cmd-format pytest-cmd-format-string
                                "." pytest cmd-flags tnames))
    (if use-comint
        (with-current-buffer (get-buffer (pytest-get-temp-buffer-name))
          (inferior-python-mode)))))


(defun wrap-pytest-run (orig-fun &optional tramp-names &rest args)
  "Run pytests in remote environment."
  (if brainos-enable-sandbox-support
      (let* ((tramp-structs (pytest-parse-tramp-file-name-structure (if (listp tramp-names)
                                                                        tramp-names
                                                                      (list tramp-names))))
             (tests (mapcar (lambda (s)
                              (if (stringp s)
                                  s
                                (tramp-file-name-localname s)))
                            tramp-structs))
             (d (file-name-directory (buffer-file-name))))
        (if (stringp (car tramp-structs))
            (progn
              (cd (format "/%s:%s@%s:~" brain-method brain-user
                          brain-host))
              (apply 'pytest-run-patched tests args)
              (cd d))
          (progn
            (cd d)
            (apply 'pytest-run-patched tests args))))
    (apply orig-fun tramp-names args)))


(defun wrap-spacemaces/python-execute-file (orig-fun &optional args)
  "Execute a python script in a shell."
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (if brainos-enable-sandbox-support
      (let* ((universal-argument t)
             (filename (buffer-file-name))
             (d (file-name-directory filename))
             (buff (current-buffer))
             (repo-path (substring filename (+ (string-match "shining_software/src.*" filename)
                                               (length "shining_software/src/"))))
             (compile-command (format "python %s %s" repo-path (concat args))))
        (cd (format "/%s:%s@%s:/opt/shining_software/"
                    brain-method brain-user brain-host))
        (compile compile-command t)
        (cd d))
    (apply oirg-fun args)))


(defun wrap-spacemacs//python-setup-shell (orig-fun &rest args)
  (if brainos-enable-sandbox-support
      (progn (setq python-shell-interpreter-args "-i") 
             (setq python-shell-interpreter "python")
             )
    (apply orig-fun args)))


(defun brainos-setup-wrappers (symbol new-val op where)
  (if new-val
      (progn
        (message "brainos: adding python wrappers")
        (advice-add 'run-python :around #'wrap-python-shell-run)
        (advice-add 'python-shell-send-string :around #'wrap-python-shell-send-string)
        (advice-add 'pytest-run :around #'wrap-pytest-run))
    (progn
      (message "brainos: removing python wrappers")
      (advice-remove 'run-python #'wrap-python-shell-run)
      (advice-remove 'python-shell-send-string #'wrap-python-shell-send-string)
      (advice-remove 'pytest-run #'wrap-pytest-run))))


(defun wrap-pytest-find-test-runner-in-dir-named (_ dn runner)
  (let ((fn (expand-file-name runner dn))
        (nxt-dn (file-name-directory (directory-file-name dn))))
    (cond
     ((file-regular-p fn) fn)
     ((equal dn nxt-dn) nil)
     (t (pytest-find-test-runner-in-dir-named nxt-dn
                                              runner)))))


(defun brainos-setup ()
  (global-set-key [?\C-\'] #'python-execute-file-in-remote)
  (advice-add 'spacemacs/python-execute-file :around #'wrap-spacemaces/python-execute-file)
  (advice-add 'spacemacs//python-setup-shell :around #'wrap-spacemacs//python-setup-shell)
  (advice-add 'pytest-find-test-runner-in-dir-named :around #'wrap-pytest-find-test-runner-in-dir-named)
  (add-variable-watcher 'brainos-enable-sandbox-support #'brainos-setup-wrappers)
  )


;;;###autoload
(defun brain-open-project ()
  (let ((filepath (spacemacs/projectile-copy-file-path)))
    (projectile-switch-project-by-name (projectile-get-project-directories))
    (find-file filepath)))


;;;###autoload
(defun brain-open-in-project (&optional arg)
  "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (let ((filepath (spacemacs/projectile-copy-file-path))
        (projects (projectile-relevant-known-projects))
        (projectile-require-project-root nil))
    (if projects
        (projectile-completing-read
         "Switch to project: " projects
         :action (lambda (project)
                   (message (concat project filepath))
                   (find-file (concat project filepath))))
      (user-error "There are no known projects"))))

(define-key evil-normal-state-map (kbd ",bpo") 'brain-open-in-project)


;;;###autoload
(defun brain-launch-python ()
  (interactive)
  (start-process "Python" "*Python*" "ipython"))


;;;###autoload
(defun brain-build-sandbox ()
  (interactive)
  (async-shell-command "ssh brain@sandbox /opt/shining_software/build.sh --big"
                       "Build Sandbox"))

(define-key evil-normal-state-map (kbd ",bbs") 'brain-build-sandbox)

(define-key evil-normal-state-map (kbd "'") 'async-shell-command)
;;; brainos.el ends here
