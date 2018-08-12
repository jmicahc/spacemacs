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
(setq brain-method "ssh")
(setq brain-user "brain")
(setq brain-host "sandbox")
(setq brain-repo-dir "/opt")


(defun tramp-file-namep (filename)
  (condition-case nil
      (when (tramp-dissect-file-name filename)
        t)
    (error nil)))

(require 'tramp)


(defun wrap-python-shell-send-string (orig-fun &rest args)
  (if-let ((f (buffer-file-name)))
      (condition-case nil
          (progn (tramp-dissect-file-name f)
                 (apply orig-fun args))
        (error
         (let ((d (file-name-directory f)))
           (cd (format "/%s:%s@%s:/" brain-method brain-user brain-host))
           (apply orig-fun args)
 curl 'https://braincorporation.atlassian.net/s/-nk60j8/b/0/1.0/_/download/resources/jira.webresources:global-static/wiki-renderer.css?streamsSourceProduct=jira' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.13; rv:61.0) Gecko/20100101 Firefox/61.0' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Cookie: cloud.session.token=eyJraWQiOiJzZXNzaW9uLXNlcnZpY2VcL3Nlc3Npb24tc2VydmljZSIsImFsZyI6IlJTMjU2In0.eyJhc3NvY2lhdGlvbnMiOltdLCJzdWIiOiI1NTcwNTg6ODZhMmIxMjctYTlkZC00MTFkLWFlZDctMGM5NjAxYmQ3NWRkIiwiYXVkIjoiYXRsYXNzaWFuIiwiaW1wZXJzb25hdGlvbiI6W10sIm5iZiI6MTUzMzg5MTUzOCwicmVmcmVzaFRpbWVvdXQiOjE1MzM4OTIxMzgsImlzcyI6InNlc3Npb24tc2VydmljZSIsInNlc3Npb25JZCI6IjFiYjViZjE0LTk5ZTAtNDBmNS1hNDA5LTdkM2YwMTBhODU1ZCIsImV4cCI6MTUzNjQ4MzUzOCwiaWF0IjoxNTMzODkxNTM4LCJlbWFpbCI6ImNvbGxpbnNAYnJhaW5jb3Jwb3JhdGlvbi5jb20iLCJqdGkiOiIxYmI1YmYxNC05OWUwLTQwZjUtYTQwOS03ZDNmMDEwYTg1NWQifQ.LwKC03LqHtyct_yGrUFB_z5ewTidS-lLmAYeWnYaJ57274Bck-7J2A3FTIrKbmB23fqDAMToaGoDdcT72muB39qdM_LLg13YZMoE_oJT59iG6L3m7CFgYvkLs_93vGF5fRa78iAnrWw8-ok2OvcvxLPn9e4nZH4K4-ZnIjDh1933Ee1qeyZb7kNKYsfuU4fpqL2qJb4uYGYfJ8ctPgmXD7CYNlq15xQ8P9uuhv2O0J02ZdxWV4gHdbcJbJHSAbz9CU8uTQkgekVkuw8yE-m42hGBRkVV9z8KcNR4M4fws6g7HYuAljJJjrxUhZxhB_pNP89f85EHObKYFjtbEwfhEQ; ajs_group_id=null; ajs_anonymous_id=%22f4484c35-dcd5-4462-83c1-8d45a5573f3c%22; atlassian.xsrf.token=BN4G-6MNW-Z3B0-BR4S_93200a68af113fc61ba86bdc9028ea49263fe7b0_lin; AJS.conglomerate.cookie="|hipchat.inapp.links.first.clicked.collins=false"' -H 'Connection: keep-alive'          (cd d))))
    (cd "/ssh:brain@sandbox:/");;(format "/%s:%s@%s:/" brain-method brain-user brain-host))
    (apply orig-fun args)))


(defun wrap-python-shell-run (orig-fun &rest args)
  "Send strings to remote interpreter."
  (let ((d (file-name-directory (buffer-file-name))))
    (condition-case nil
        (progn
          (tramp-dissect-file-name d)
          (apply orig-fun args))
      (error (progn
               (cd (format "/%s:%s@%s:/" brain-method brain-user brain-host))
               (apply orig-fun args)
               (cd d))))))


(defun pytest-parse-tramp-file-name-structure (tests)
  (mapcar (lambda (test)
            (condition-case nil
                (tramp-dissect-file-name test)
              (error (format "%s/%s"
                             brain-repo-dir
                             (substring test (string-match "shining_software.*" test))))))
          tests))


(defun pytest-run-patched (&optional tests flags)
  "Run pytest.
     Optional argument TESTS Tests to run.
     Optional argument FLAGS py.test command line flags."
  (interactive "fTest directory or file: \nspy.test flags: ")
  (let* ((pytest (pytest-find-test-runner))
         (tests (cond ((not tests) (list "."))
                      ((listp tests) tests)
                      ((stringp tests) (split-string tests))))
         (tnames (mapconcat (apply-partially 'format "'%s'") tests " "))
         (cmd-flags (if flags flags pytest-cmd-flags))
         (use-comint (s-contains? "pdb" cmd-flags)))
    (funcall #'(lambda (command)
                 (compilation-start command use-comint
                                    (lambda (mode) (concat (pytest-get-temp-buffer-name)))))
             (pytest-cmd-format pytest-cmd-format-string "." pytest cmd-flags tnames))
    (if use-comint
        (with-current-buffer (get-buffer (pytest-get-temp-buffer-name))
          (inferior-python-mode)))))


(defun wrap-pytest-run (orig-fun &optional tramp-names &rest args)
  "Run pytests in remote environment."
  (let* ((tramp-structs (pytest-parse-tramp-file-name-structure
                         (if (listp tramp-names) tramp-names (list tramp-names))))
         (tests (mapcar (lambda (s) (if (stringp s) s (tramp-file-name-localname s))) tramp-structs))
         (d (file-name-directory (buffer-file-name))))
    (if (stringp (car tramp-structs))
        (progn
          (cd (format "/%s:%s@%s:~" brain-method brain-user brain-host brain-repo-dir))
          (apply 'pytest-run-patched tests args)
          (cd d))
      (progn
        (cd d)
        (apply 'pytest-run-patched tests args)))))


(defun python-execute-file-in-remote (&optional args)
  "Execute a python script in a shell."
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let* ((universal-argument t)
         (filename (buffer-file-name))
         (d (file-name-directory filename))
         (buff (current-buffer))
         (repo-path (substring filename (string-match "shining_software*"
                                                      filename)))
         (compile-command (format "python %s %s" repo-path (concat args))))
    (cd (format "/%s:%s@%s:/opt"
                brain-method brain-user brain-host))
    (compile compile-command t)
    (cd d)))


(defun brainos-setup-wrappers (symbol new-val op where)
  (if new-val
      (do (advice-add 'run-python :around #'wrap-python-shell-run)
          (advice-add 'python-shell-send-string :around #'wrap-python-shell-send-string)
          (advice-add 'pytest-run :around #'wrap-pytest-run))
    (do (advice-remove 'run-python)
        (advice-remove 'python-shell-send-string)
        (advice-remove 'pytest-run))))


(defun brainos-setup ()
  (global-set-key [?\C-\'] #'python-execute-file-in-remote)
  (add-variable-watcher 'brainos-enable-sandbox-support #'brainos-setup-wrappers))


;; Patch pytest.el to work on remote host
(eval-after-load "pytest"
  ;; Patch to stop when a fixed-point is reached, to avoid
  ;; infinite loops when reading TRAMP filenames.
  (defun pytest-find-test-runner-in-dir-named (dn runner)
    (let ((fn (expand-file-name runner dn))
          (nxt-dn (file-name-directory (directory-file-name dn))))
      (cond ((file-regular-p fn) fn)
            ((equal dn nxt-dn) nil)
            (t (pytest-find-test-runner-in-dir-named nxt-dn runner)))))

  )


;;; brainos.el ends here
