;;========================================================================
;; Refactoring of abap-navigate-code and underlying methods
;;========================================================================


(defun abap-navigate-code ()
  "Navigate to object under cursor."
  (interactive)
  (let* ((curr-buffer (current-buffer))
         (source-name (file-name-nondirectory (buffer-file-name)))
         (object-uri (abaplib-get-property 'uri))
         (source-uri (abaplib-get-property 'source-uri source-name))
         (full-source-uri (concat object-uri "/" source-uri))
         (source-code (abaplib-buffer-whole-string curr-buffer))
         (target-navi-uri
          (abaplib-get-navigation-target full-source-uri
                                         (line-number-at-pos)
                                         (current-column)
                                         source-code))
         ;; TODO Marian: rename abaplib--get-target-source-uri -> abaplib-get-target-source-uri
         (other-window (not (string= (abaplib--get-target-source-uri target-navi-uri)
                                     full-source-uri))))
    (abaplib-do-navigate target-navi-uri other-window)))


(defun abaplib-get-object-info (full-source-uri)
  "Get object info of ABAP development object from `full-source-uri'."
  (let* ((split-on-source (-split-when (lambda (elem) (string= elem "source"))
                                       (split-string full-source-uri "/")))
         (object-uri (mapconcat 'directory-file-name (car split-on-source) "/")) ;; everything before /source in uri
         (object-filename-base (if (cadr split-on-source) (caadr split-on-source) "main")) ;; gives main or implementations etc.
         (object-info (abaplib--rest-api-call object-uri nil :parser 'abaplib-util-xml-parser))
         (object-type (xml-get-attribute object-info 'type))
         (object-name (xml-get-attribute object-info 'name))
         (object-path (abaplib-get-path object-type object-name object-uri)))
    `((path . ,object-path)
      (filename-base . ,object-filename-base)
      (name . ,object-name)
      (type . ,object-type)
      (uri  . ,object-uri))))


(defun abaplib-do-navigate (target-navi-uri other-window)
  "Navigate to source and position encoded in `target-navi-uri'."
  (let* ((target-source-uri (abaplib--get-target-source-uri target-navi-uri))
         (target-object-info (abaplib-get-object-info target-source-uri))
         (object-path     (cdr (assoc 'path target-object-info)))
         (obj-fname-base  (cdr (assoc 'filename-base target-object-info)))
         (object-filename (file-name-completion obj-fname-base object-path))
         (object-filepath))
    ;; TODO add source retrieve
    (unless object-filename
      (error (format "Cannot navigate to target uri \"%s\"! Please fetch from server first." target-source-uri)))
    (setq object-filepath (concat object-path "/" object-filename))
    (if other-window
        (switch-to-buffer (find-file-other-window object-filepath))
      (switch-to-buffer (current-buffer)))
    (cond ((progn
             (string-match "#start=\\([0-9]+,[0-9]+\\)" target-navi-uri)
             (match-string 1 target-navi-uri))
           (let ((target-source-pos (split-string (progn
                                                    (string-match "#start=\\([0-9]+,[0-9]+\\)" target-navi-uri)
                                                    (match-string 1 target-navi-uri)) "," ))
                 )
             (goto-line (string-to-number (car target-source-pos)))
             (move-to-column (string-to-number (cadr target-source-pos)))))
          ((progn
             (string-match "#name=\\([A-Za-z0-9_-]+\\)" target-navi-uri)
             (match-string 1 target-navi-uri))
           (let ((target-source-pos (progn
                                      (string-match "#name=\\([A-Za-z0-9_-]+\\)" target-navi-uri)
                                      (match-string 1 target-navi-uri)))
                 )
             (goto-char 1)
             (search-forward target-source-pos)
             (skip-chars-backward "A-Za-z0-9_-")))
          (t (goto-char 1)))
    ))


;;========================================================================
;; Snippet - function abap-where-used
;;========================================================================


(defun abap-where-used ()
  "Get Where-Used List of object under cursor."
  (interactive)
  (let* ((source-name (file-name-nondirectory (buffer-file-name)))
         (object-uri (abaplib-get-property 'uri))
         (source-uri (abaplib-get-property 'source-uri source-name))
         (full-source-uri (concat object-uri "/" source-uri)))
    (abaplib-where-used full-source-uri (line-number-at-pos) (current-column))
    ))

(defun abaplib-where-used (full-source-uri row-pos col-pos)
  "Get Where-Used List of object in `full-source-uri' at linenumber `row-pos', column number `col-pos'."
  (message "Getting Where-Used list...")
  (let* ((request-uri "")      ;; TODO Marian: request-uri
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ))         ;; TODO Marian: headers
         (post-data (concat )) ;; TODO Marian: concat post-data
         (params `((uri . ,(format "%s#start=%d,%d"
                                   full-source-uri row-pos col-pos))
                               ;; TODO Marian: possibly more params
                   ))
         (where-used (abaplib--rest-api-call request-uri
                                             nil
                                             :parser 'abaplib-util-xml-parser
                                             :type "POST"
                                             :headers headers
                                             :data post-data
                                             :params params)))
    ))


;;========================================================================
;; Refactoring of abap-submit-source and underlying methods
;; to refresh properties after submit
;;========================================================================


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
         (object-info `((path . ,object-path)
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
