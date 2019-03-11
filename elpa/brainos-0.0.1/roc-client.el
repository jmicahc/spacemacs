(require 'request)
(require 'json)
(require 'time)
(require 'cl)
(require 'dired)

(defcustom roc-username "collins" "Username used to authenticate with ROC")
(defcustom roc-version "v0" "Version of roc API to use.")


(defun rocc-request (arg params)
  (let* ((auth-data (json-read-file "~/rocc.json"))
         (roc-url (assoc-default 'RocUrl auth-data))
         (token (assoc-default 'Token auth-data))
         (header (format "Bearer %s" token)))
    (request (format "%s/%s" roc-url arg)
             :parser 'json-read
             :params params
             :sync t
             :headers (list (list* "Authorization" header)))))


;; ### Login


(defun rocc-login ())


;; ### Sessions


(defun rocc-session-to-tramp-filename (username session)
  (format "/ssh:%s,%s@%s:"
          username
          (assoc-default 'accessPort session)
          ;; Transport sytax for port.
          (replace-regexp-in-string ":" "#" (assoc-default 'accessService session))))


(defun rocc-active-session-p (session)
  (< (time-to-seconds
      (time-subtract (current-time)
                     (parse-iso8601-time-string (assoc-default 'updatedAt session))))
     60))


(defun rocc-sessions-list ()
  (seq-filter 'rocc-active-session-p
              (request-response-data (rocc-request "v0/sessions" nil))))


;; ## Devices


(defun rocc-devices-list ()
  (request-response-data (rocc-request "v0/devices" nil)))


(defun rocc-devices-get (device-id)
  (request-response-data (rocc-request (format "v0/device/%s" device-id))))


;; ## Routes


(defun rocc-routes-list ())


(defun rocc-routes-get ())


(defun rocc-routes-download)


;; ## Telemetry


(defun rocc-telmetry-list ())


(defun rocc-telemtry-get ())


(defun rocc-telemetry-download ())


;; ## Assists


(defun rocc-assists-list ())


(defun rocc-assists-get ())


;; Helm

(defun helm-rocc-connections-fetch ()
  (cl-loop for ses in (rocc-sessions-list)
           collect (let* ((device (assoc-default 'device ses))
                          (candidate-display-string
                           (concat (assoc-default 'serialNumber device) ":"
                                  (assoc-default 'id device)))
                          (candidate-action-data ses))
                     (list* candidate-display-string candidate-action-data))))


(defun helm-rocc-connect-action (session)
  (with-helm-current-buffer
    (dired (rocc-session-to-tramp-filename roc-username session))))


(defvar helm-rocc-connections-suggest
      (helm-build-sync-source "ROC Connections"
        :candidates #'helm-rocc-connections-fetch
        :action '(("Connect" . (lambda (action)
                                 (helm-rocc-connect-action action))))))



;;;###autoload
(defun helm-rocc-connections ()
  "Preconfigured `helm' for Wikipedia lookup with Wikipedia suggest."
  (interactive)
  (helm :sources 'helm-rocc-connections-suggest
        :buffer "*helm rocc*"))


(defun helm-python-run-action (session)
  (with-helm-current-buffer
    (let ((d (file-name-directory (buffer-file-name)))
          (tramp-filename (rocc-session-to-tramp-filename roc-username session)))
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python")
      (cd tramp-filename)
      (shell-command "echo 'source /opt/shining_software/use_repo.sh' >> ~/.bashrc;")
      (run-python)
      (cd d))))


(setq helm-python-connections-suggest
  (helm-build-sync-source "ROC To Python Repl"
    :candidates #'helm-rocc-connections-fetch
    :action '(("Connect" . (lambda (session)
                             (helm-python-run-action session))))))


;;;###autoload
  (defun rocc-python-run ()
    (interactive)
    (helm :sources 'helm-python-connections-suggest
          :buffer "*helm python*"))
