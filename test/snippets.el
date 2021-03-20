(defun abap-submit-source ()
  "Submit source back to server."
  (interactive)
  (let* ((curr-buffer (current-buffer))
         (object-path (file-name-directory (buffer-file-name)))
         (source-name (file-name-nondirectory (buffer-file-name)))
         (object-uri (abaplib-get-property 'uri))
         (source-uri (abaplib-get-property 'source-uri source-name))
         (full-source-uri (concat object-uri "/" source-uri))
         (source-code (abaplib-buffer-whole-string curr-buffer))
         (package     (abaplib-get-property 'package))
         (tr-number)
         ;; need object-path, source-name, full-source-uri
         (object-info (list (path . ,object-path)
                            (file . ,source-name)
                            (full-source-uri . ,full-source-uri))))
    (unless (string= package "$TMP")
      (let* ((requests (abaplib-retrieve-trans-request full-source-uri))
             (selected-req (completing-read "Change Request: " requests)))
        (setq tr-number (string-trim (car (split-string selected-req "|"))))
        (message (abaplib-post-cm-checkrun tr-number full-source-uri))))
    (abaplib-do-submit source-code object-info tr-number)))

(defun abaplib-do-submit (source-code object-info &optional tr-number)
  "Submit source back to server."
  (let* ((full-source-uri (cdr (assoc 'full-source-uri object-info)))
         (csrf-token (abaplib-get-csrf-token))
         (lock-handle (abaplib--lock-sync full-source-uri csrf-token))
         (params `(("lockHandle" . ,lock-handle)))
         (headers `(("Content-Type" . "text/plain")
                    ("x-csrf-token" . ,csrf-token))))
    (when tr-number
      (push `(corrNr . ,tr-number) params))
    (abaplib--rest-api-call
     full-source-uri
     (lambda (&rest rest)
       (let* ((response (cl-getf rest :response))
              (response-string (format "%s" response))
              (etag (progn
                      (string-match "etag: \\([1-9][0-9]+\\)" response-string)
                      (match-string 1 response-string))))
         (if etag
             (abaplib--metadata-post-submit etag object-info))
         (message "Submitting source to server succeeded!")))
     :type "PUT"
     :data source-code
     :headers headers
     :params params)))

(defun abaplib--metadata-post-submit (etag object-info)
  (let* ((object-path (cdr (assoc 'path object-info)))
         (source-name (cdr (assoc 'file object-info)))
         (property-file (expand-file-name abaplib--property-file object-path))
         (metadata (json-read-file property-file)))
    (setf (alist-get 'version metadata) "inactive")
    (setf (alist-get 'version (assoc-string source-name (assoc 'sources metadata))) "inactive")
    (setf (alist-get 'etag    (assoc-string source-name (assoc 'sources metadata))) etag)
    (write-region (with-temp-buffer
                    (insert (json-encode metadata))
                    (json-pretty-print (point-min) (point-max))
                    (buffer-string))
                  nil property-file)))
