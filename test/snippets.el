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
  (let* ((request-uri "/sap/bc/adt/repository/informationsystem/usageReferences")
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ("x-sap-adt-sessiontype" . "stateful")
                    ("Content-Type" . "application/vnd.sap.adt.repository.usagereferences.request.v1+xml")
                    ("Accept"       . "application/vnd.sap.adt.repository.usagereferences.result.v1+xml")))
         (post-data (concat
                     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                     "<usagereferences:usageReferenceRequest xmlns:usagereferences=\"http://www.sap.com/adt/ris/usageReferences\">"
                     "<usagereferences:affectedObjects/>"
                     "</usagereferences:usageReferenceRequest>"))
         (params `((uri . ,(format "%s#start=%d,%d"
                                   full-source-uri row-pos col-pos))))
         (where-used (abaplib--rest-api-call request-uri
                                             nil
                                             :parser 'abaplib-util-xml-parser
                                             :type "POST"
                                             :headers headers
                                             :data post-data
                                             :params params)))
    ))

(defconst abaplib--where-used-buffer "*ABAP Where-Used*"
  "ABAP Where-Used buffer");

(defun abaplib-util-where-used-buf-write (log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--where-used-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "%s" log))
    (setq buffer-read-only t)));

(let* ((where-used (car (xml-parse-file "~/packages/ABAPInEmacs/test/where-used/template_where_used.xml"))))
  (abaplib--where-used-post where-used));

(defun abaplib--where-used-post (where-used)
  (let* ((objects-node (car (xml-get-children where-used 'usageReferences:referencedObjects)))
         (ref-objects (xml-get-children objects-node 'usageReferences:referencedObject))
         (main-items  (-filter (lambda (elem)
                                 (dolist (adt-object (xml-get-children elem 'usageReferences:adtObject))
                                   (when (xml-get-attribute-or-nil adt-object 'adtcore:description)
                                     (return t))))
                               ref-objects))
         (objects-wId (-filter (lambda (elem) (xml-get-children elem 'objectIdentifier)) ref-objects))
         (snippets (abaplib--get-usage-snippets objects-wId))
         (output-log (format "%s\n\n" (xml-get-attribute where-used 'resultDescription))))
    ;; (message "%s" ref-objects)
    ;; (message "%s" (xml-get-children (car ref-objects) 'usageReferences:adtObject))
    ;; (message "%s" main-items)
    ;; (message "%s" (length main-items))
    ;; (message "%s" (length objects-wId))
    (dolist (elem main-items)
      (cl-assert (= (length (xml-get-children elem 'usageReferences:adtObject)) 1))
      (let* ((adt-object (car (xml-get-children elem 'usageReferences:adtObject)))
             (object-uri (xml-get-attribute elem 'uri))
             (sub-elems (-filter (lambda (elem)
                                   (string= (xml-get-attribute elem 'parentUri) object-uri))
                                 objects-wId)))
        ;; print main item
        (setq output-log (concat output-log "\n"
                                 (format "%s %s"
                                         (xml-get-attribute adt-object 'adtcore:description)
                                         (xml-get-attribute adt-object 'adtcore:name))))
        (when sub-elems
          (dolist (sub-elem sub-elems)
            (let* ((sub-adt-object (car (xml-get-children sub-elem 'usageReferences:adtObject)))
                   (object-Id (car (xml-get-children sub-elem 'objectIdentifier)))
                   (snippet (-first (lambda (elem)
                                      (string= (nth 2 object-Id)
                                               (nth 2 (car (xml-get-children elem 'objectIdentifier)))))
                                    snippets))
                   (code-snippets-node (car (xml-get-children snippet 'codeSnippets)))
                   (code-snippets (xml-get-children code-snippets-node 'codeSnippet)))
              ;; print subitems
              (setq output-log (concat output-log "\n"
                                       (format "  %s" (xml-get-attribute sub-adt-object 'adtcore:name))))
              ;; TODO Marian: rework processing and printing of results
              ;; (dolist (code-snippet code-snippets)
              ;;   ;; print code of where-used result
              ;;   (setq output-log (concat output-log "\n"
              ;;                            (format "    %s" (abaplib--print-where-used-result code-snippet)))))
              )))
        (unless sub-elems
          (cl-assert (= (length (xml-get-children elem 'objectIdentifier)) 1))
          (let* ((object-Id (car (xml-get-children elem 'objectIdentifier)))
                 (snippet (-first (lambda (elem)
                                    (string= (nth 2 object-Id)
                                             (nth 2 (car (xml-get-children elem 'objectIdentifier)))))
                                  snippets))
                 (code-snippets-node (car (xml-get-children snippet 'codeSnippets)))
                 (code-snippets (xml-get-children code-snippets-node 'codeSnippet)))
            (dolist (code-snippet code-snippets)
              ;; print code of where-used result
              (setq output-log (concat output-log "\n"
                                       (format "  %s" (abaplib--print-where-used-result code-snippet)))))))
        ))
    (abaplib-util-where-used-buf-write output-log)
    (pop-to-buffer (get-buffer-create abaplib--where-used-buffer))
    )
  );

(defun abaplib--print-where-used-result (code-snippet)
  (cl-assert (= (length (xml-get-children code-snippet 'content)) 1))
  (let* ((target-uri         (xml-get-attribute code-snippet 'uri))
         (target-source-uri  (abaplib-get-target-source-uri target-uri))
         (target-object-info (abaplib-get-object-info target-source-uri))
         (target-source-pos  (split-string (progn
                                             (string-match "#start=\\([0-9]+,[0-9]+\\)" target-uri)
                                             (match-string 1 target-uri)) "," ))
         (object-path        (cdr (assoc 'path target-object-info)))
         (obj-fname-base     (cdr (assoc 'filename-base target-object-info)))
         (content            (car (xml-get-children code-snippet 'content)))
         (map (make-sparse-keymap))
         (fn-follow-pos `(lambda ()
                          (interactive)
                          (let* ((path        ,object-path)
                                 (fname-base  ,obj-fname-base)
                                 (filename    (file-name-completion fname-base path))
                                 (line   ,(string-to-number (car target-source-pos)))
                                 (column ,(string-to-number (cadr target-source-pos)))
                                 (filepath))
                            (unless filename
                              (error "File does not exist locally!"))
                            (setq filepath (concat path "/" filename))
                            (switch-to-buffer (find-file-other-window filepath))
                            (abaplib-util-goto-position line column))))
         )
    (define-key map (kbd "<down-mouse-1>") fn-follow-pos)
    (define-key map (kbd "<RET>") fn-follow-pos)
    (propertize (nth 2 content)
                'face 'underline
                'mouse-face 'highlight
                'keymap map)));

(defun abaplib--parse-usage-snippets (snippets)
  ;; (message "-- length: %s" (length (xml-get-children snippets 'codeSnippetObjects)))
  ;; (message "-- Children: %s" (length (xml-node-children snippets)))
  (let* ((snippets-node (car (xml-get-children snippets 'codeSnippetObjects)))
         (snippet-objects (xml-get-children snippets-node 'codeSnippetObject))
         (test-snippet  (car snippet-objects)))
    (message "-- Number of snippets: %s" (length snippet-objects)) ;; => 12
    ;; objectIdentifier of a snippet-object
    (message "-- objectIdentifier: %s" (nth 2 (car (xml-get-children test-snippet 'objectIdentifier))))
    ;; code-snippets node of a snippet-object
    (message "-- codeSnippets: %s" (length (xml-get-children test-snippet 'codeSnippets))) ;; => 1
    ;; code-snippets of a snippet-object
    (message "-- codeSnippet: %s" (length (xml-get-children (car (xml-get-children test-snippet 'codeSnippets)) 'codeSnippet)))
    (message "-- codeSnippet attrs: %s"
             (xml-node-attributes (car    (xml-get-children (car (xml-get-children test-snippet 'codeSnippets)) 'codeSnippet)))) ;; => attributes 'uri' and 'matches'
    (message "-- codeSnippet chlds: %s"
             (xml-node-children   (car    (xml-get-children (car (xml-get-children test-snippet 'codeSnippets)) 'codeSnippet)))) ;; => node children 'content' and 'description'
    )
  );

(defun abaplib--get-usage-snippets (objects-wId)
  (let* ((request-uri "/sap/bc/adt/repository/informationsystem/usageSnippets")
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ("x-sap-adt-sessiontype" . "stateful")
                    ("Content-Type" . "application/vnd.sap.adt.repository.usagesnippets.request.v1+xml")
                    ("Accept"       . "application/vnd.sap.adt.repository.usagesnippets.result.v1+xml")))
         (post-data (abaplib--get-usage-snippets-template objects-wId))
         (snippets (abaplib--rest-api-call request-uri
                                           nil
                                           :sync t
                                           :parser 'abaplib-util-xml-parser
                                           :type "POST"
                                           :headers headers
                                           :data post-data)))
    (cl-assert (= (length  (xml-get-children snippets 'codeSnippetObjects)) 1))
    (xml-get-children (car (xml-get-children snippets 'codeSnippetObjects)) 'codeSnippetObject)));

(defun abaplib--get-usage-snippets-template (objects-wId)
  (let ((post-body
         (concat
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          "<usagereferences:usageSnippetRequest xmlns:usagereferences=\"http://www.sap.com/adt/ris/usageReferences\">"
          "<usagereferences:objectIdentifiers>")))
    (dolist (ref-object objects-wId)
      (cl-assert (= (length (xml-get-children ref-object 'objectIdentifier)) 1))
      (let ((object-id (car (xml-get-children ref-object 'objectIdentifier))))
        (setq post-body (concat post-body
                                (format "<usagereferences:objectIdentifier optional=\"false\">%s</usagereferences:objectIdentifier>" (nth 2 object-id))))))
        ;; (message "-- object-id %s" (nth 2 object-id))))
    (setq post-body (concat post-body
                            "</usagereferences:objectIdentifiers>"
                            "<usagereferences:affectedObjects/>"
                            "</usagereferences:usageSnippetRequest>"))
    post-body));


;;========================================================================
;; Loading items of search result:
;;========================================================================

;; # SVER_NATIVE_GENERATE_VERI


;; REQUEST_LINE
;; METHOD:  POST
;; URI:     /sap/bc/adt/repository/informationsystem/usageSnippets
;; VERSION: HTTP/1.1

;; HEADER_FIELDS
;; Content-Type         application/vnd.sap.adt.repository.usagesnippets.request.v1+xml
;; Accept               application/vnd.sap.adt.repository.usagesnippets.result.v1+xml
;; X-sap-adt-profiling  server-time

;; MESSAGE_BODY (see where-used/template_request_item1.xml)

;; RESPONSE-MESSAGE_BODY (see where-used/template_response_item1.xml)


;; # CL_SVER_DBI_GEN_NATIVE_VERI :: CHECK_TARGET_REPORT_NAMES

;; REQUEST_LINE
;; METHOD:  POST
;; URI:     /sap/bc/adt/repository/informationsystem/usageSnippets
;; VERSION: HTTP/1.1

;; HEADER_FIELDS
;; Content-Type         application/vnd.sap.adt.repository.usagesnippets.request.v1+xml
;; Accept               application/vnd.sap.adt.repository.usagesnippets.result.v1+xml
;; X-sap-adt-profiling  server-time

;; MESSAGE_BODY (see where-used/template_request_item2.xml)

;; RESPONSE-MESSAGE_BODY (see where-used/template_response_item2.xml)
