;;========================================================================
;; ABAP outline
;;========================================================================

(defconst abaplib--outline-buffer "*ABAP Outline*"
  "ABAP Outline buffer");

(defun abaplib-util-outline-buf-write (log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--outline-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "%s" log))
    (setq buffer-read-only t)));

(defun abap-outline ()
  "Get object structure of ABAP development object."
  (interactive)
  (let ((object-uri     (abaplib-get-property 'uri))
        (object-version (abaplib-get-property 'version)))
    (abaplib-outline object-uri object-version)));

(defun abaplib-outline (object-uri object-version)
  (message "Getting outline...")
  (let* ((request-uri (concat object-uri "/objectstructure"))
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ("Accept"       . "application/xml;q=0.8, application/vnd.sap.adt.objectstructure+xml;q=0.9, application/vnd.sap.adt.objectstructure.v2+xml")))
         (params `((version . ,object-version)
                   (withShortDescriptions . "true")))
         (outline (abaplib--rest-api-call request-uri
                                          nil
                                          :parser 'abaplib-util-xml-parser
                                          :headers headers
                                          :params params)))
    (abaplib--outline-post outline)));

(defun abaplib--outline-post (outline)
  "Build outline buffer."
  (let ((object-name (xml-get-attribute outline 'name))
        (object-type (xml-get-attribute outline 'type))
        (main-links  (xml-get-children outline 'link))
        (object-structure (xml-get-children outline 'objectStructureElement))
        (output-log))
    (setq output-log (format "Outline for %s\n\n" object-name))
    (dolist (elem object-structure)
      (let ((sub-obj-structure (xml-get-children elem 'objectStructureElement))
            (links (xml-get-children elem 'link)))
        (setq output-log (concat output-log "\n"
                                 (format "  %s %s" (xml-get-attribute elem 'name) (xml-get-attribute elem 'type))))
        (dolist (link links)
          ;; do something here
          )
        (dolist (sub-elem sub-obj-structure)
          (let ((sub-links (xml-get-children sub-elem 'link)))
            (setq output-log (concat output-log "\n"
                                     (format "    %s %s" (xml-get-attribute sub-elem 'name) (xml-get-attribute sub-elem 'type))))
            (dolist (sub-link sub-links)
              ;; do something here
              )

        ))))
    (abaplib-util-outline-buf-write output-log)
    (pop-to-buffer (get-buffer-create abaplib--outline-buffer))));

;;========================================================================
;; Snippet - examining ABAP Where-Used list
;;========================================================================

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
