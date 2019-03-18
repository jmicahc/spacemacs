;;; roc-client.el --- a simple package                     -*- lexical-binding: t; -*-

;; Author: John Collins <collins@braincorporation.com>
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

(require 'request)
(require 'json)
(require 'time)
(require 'cl)
(require 'dired)
(require 'f)


(defcustom roc-username "collins" "Username used to authenticate with ROC")


(defun rocc-request (arg params)
  (let* ((auth-data (json-read-file "~/rocc.json"))
         (roc-url (assoc-default 'RocUrl auth-data))
         (token (assoc-default 'Token auth-data))
         (header (format "Bearer %s" token)))
    (request (format "%s%s" roc-url arg)
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


(defun rocc-session-to-login (username session)
  (let* ((access-pieces (split-string (assoc-default 'accessService session) ":"))
         (ssh-port (cadr access-pieces))
         (access-port (assoc-default 'accessPort session))
         (access-host (car access-pieces)))
    (format "ssh -p %s '%s,%s@%s'"
            ssh-port
            username
            access-port
            access-host)))

;; (setq session (car (rocc-sessions-list)))
;; (async-shell-command (rocc-session-to-login "collins" session))

(defun rocc-session-active-p (session)
  (< (time-to-seconds
      (time-subtract (current-time)
                     (parse-iso8601-time-string (assoc-default 'updatedAt session))))
     60))


(defun rocc-sessions-list (&rest params)
  (seq-filter 'rocc-session-active-p
              (request-response-data (rocc-request "/v0/sessions" params))))

;; ## Events

(defun rocc-events-list (&rest params)
  (request-response-data (rocc-request "/v1/events" params)))


(defun rocc-events-get (event-id &rest params)
  (requests-response-data (rocc-request (format "/v1/events/%s" event-id) params)))


;; ## Devices

(defun rocc-devices-list (&rest params)
  (request-response-data (rocc-request "/v1/devices" params)))


(defun rocc-devices-get (device-id &rest params)
  (request-response-data (rocc-request (format "/v1/devices/%s" device-id) params)))


;; ## Blobs

(defun rocc-blobs-get (blob-id &rest params)
  (request-response-data (rocc-request (format "/v0/blobs/%s" blob-id) params)))


(defun rocc-blobs-download (blob-id filename)
  (let* ((blob (rocc-blobs-get blob-id))
         (download-url (assoc-default 'downloadUrl blob)))
    (request download-url
             :parser 'buffer-string
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (f-write-text data 'utf-8 filename))))))


;; ## Routes


(defun rocc-routes-list (&rest params)
  (request-response-data (rocc-request "/v0/routes" params)))


(defun rocc-routes-get (route-id &rest params)
  (request-response-data (rocc-request (format "/v0/routes/%s" route-id) params)))


(defun rocc-routes-download (route-id filename)
  (let* ((route (rocc-routes-get route-id))
         (blob-id (assoc-default 'blobId route)))
    (rocc-blobs-download blob-id filename)))


;; ## Telemetry


(defun rocc-telmetry-list (&rest params)
  (request-response-data (rocc-request "/v0/telemetry" params)))


(defun rocc-telemtry-get (telemetry-id &rest params)
  (request-response-data (rocc-request "/v0/telemetry/%s" telemetry-id)))


(defun rocc-telemetry-download (telemetry-id filename)
  (let* ((telemetry (rocc-telemetry-get telemetry-id))
         (blob-id (assoc-default 'blobId telemetry)))
    (rocc-blobs-download blob-id filename)))


;; ## Assists


(defun rocc-assists-list (&rest params)
  (request-response-data (rocc-request "/v1/assists" params)))


(defun rocc-assists-get (assist-id &rest params)
  (request-response-data (rocc-request (format "/v1/assists/%s" assist-id) params)))


;; ## Releases


(defun rocc-releases-list (&rest params)
  (request-response-data (rocc-request "/v1/releases" params)))


(defun rocc-releases-get (release-id &rest params)
  (request-response-data (rocc-request (format "/v1/releases/%s" release-id) params)))

;; (rocc-releases-get (assoc-default 'id (aref (rocc-releases-list '(limit . 1)) 0)))

;; ## Provision (Unsupported)


(defun rocc-provisions-list ()
  (rocc-request "v1/provisions" nil))


(defun rocc-provisions-get ())


(defun rocc-provisions-get ())


;; Helm

(defun rocc-connections-fetch ()
  (cl-loop for ses in (rocc-sessions-list)
           collect (let* ((device (assoc-default 'device ses))
                          (candidate-display-string
                           (concat (assoc-default 'serialNumber device) "\t"
                                   (assoc-default 'id device)))
                          (candidate-action-data ses))
                     (list* candidate-display-string candidate-action-data))))



(defun rocc-connect-action (session)
  (with-helm-current-buffer
    (dired (rocc-session-to-tramp-filename roc-username session))))


(setq rocc-connections-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c t") 'rocc-connections-run-login)
    map))


(defun rocc-connect-ssh (session)
  (let* ((device (assoc-default 'device session))
         (serial-number (assoc-default 'serialNumber device))
         (device-type (assoc-default 'device_type device)))
    (start-process (format "*%s:%s*" device-type serial-number)
                   (format "*%s:%s*" device-type serial-number)
                   "gnome-terminal"
                   "-e"
                   (rocc-session-to-login roc-username session))))


(defun rocc-connections-run-login ()
  (interactive)
  (with-helm-alive-p (helm-exit-and-execute-action 'rocc-connect-ssh)))

(put 'rocc-connections-run-login 'helm-only t)

(defvar rocc-connections-suggest
      (helm-build-sync-source "ROC Connections"
        :candidates #'rocc-connections-fetch
        :keymap rocc-connections-map
        :action '(("Connect" . (lambda (action)
                                 (rocc-connect-action action)))
                  ("Launch Terminal (C-c t)" . rocc-connect-ssh))))

;;;###autoload
(defun rocc-connections-list ()
  "Preconfigured `helm' for Wikipedia lookup with Wikipedia suggest."
  (interactive)
  (helm :sources 'rocc-connections-suggest
        :buffer "*helm rocc*"))


(defun rocc-remote ()
  (helm :sources 'rocc-connections-suggest
        :buffer "*Remotes*"))

(define-key evil-normal-state-map (kbd ",bc") 'rocc-connections-list)


(defun rocc-python-run-action (session)
  (with-helm-current-buffer
    (let ((d (file-name-directory (buffer-file-name)))
          (tramp-filename (rocc-session-to-tramp-filename roc-username session)))
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python")
      (cd tramp-filename)
      (shell-command "echo 'source /opt/shining_software/use_repo.sh' >> ~/.bashrc;")
      (run-python)
      (cd d))))


(defvar helm-python-connections-suggest
  (helm-build-sync-source "ROC To Python Repl"
    :candidates #'rocc-connections-fetch
    :action '(("Connect" . (lambda (session)
                             (rocc-python-run-action session))))))


;;;###autoload
(defun rocc-python-run ()
  (interactive)
  (helm :sources 'helm-python-connections-suggest
        :buffer "*helm python*"))

;; ## Completion

(defvar swagger-data (json-read-file "./swagger.json"))

(defun rocc-helm-complete-parameters (swagger-spec path operation)
  (let* ((swagger-paths (assoc-default 'paths swagger-spec))
         (spec (assoc-default path swagger-paths))
         (operation (assoc-default operation spec))
         (parameters (assoc-default 'parameters operation))
         (helm-candidates (mapcar (lambda (item)
                                    (list* (concat (assoc-default 'name item) "\t"
                                                   (assoc-default 'type item) "\t"
                                                   (assoc-default 'description item))
                                           (assoc-default 'name item)))
                                  parameters))
         (helm-source (helm-build-sync-source "Paramerters"
                        :candidates helm-candidates))
         (params nil)
         (parameters-string ""))
    (helm :sources (list helm-source)
          :history 'my-history)
    ;; (while (< (length params) (length helm-candidates))
    ;;   (let* ((param (helm :sources (list helm-source)
    ;;                       :history 'my-history))
    ;;          (value (json-read-from-string
    ;;                  (read-string (format "%s%s=:" parameters-string param)))))
    ;;     (setq parameters-string (concat parameters-string (format "%s=%s, " param value)))
    ;;     (setq params (cons (list* param value) params))))
    params))


;;;###autoload
(defun rocc-reflect ()
  (interactive)
  (let ((path (intern (read-string "API endpoint: ")))
        (operation (intern (read-string "Operation (default: get): " "" t "get"))))
    (rocc-helm-complete-parameters swagger-data path operation)))

(define-key evil-normal-state-map (kbd ",bf") 'rocc-reflect)

;;; roc-client.el ends here
