;;========================================================================
;; Snippet - ABAP outline
;;========================================================================

(defconst abaplib--outline-buffer "*ABAP Outline*"
  "ABAP Outline buffer")

(defun abaplib-util-outline-buf-write (log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--outline-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (format "%s" log))
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun abap-outline ()
  "Get object structure of ABAP development object."
  (interactive)
  (let ((object-uri     (abaplib-get-property 'uri))
        (object-version (abaplib-get-property 'version)))
    (abaplib-outline object-uri object-version)))

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
    (abaplib--outline-post outline)))

(defun abaplib--outline-post (outline)
  "Build outline buffer."
  (let* ((object-name (xml-get-attribute outline 'name))
         (object-type (xml-get-attribute outline 'type))
         (object-path (file-name-directory (buffer-file-name)))
         (main-links  (xml-get-children outline 'link))
         ;; TODO Marian: -drop-last very bad hardcode!!!
         (object-structure (-drop-last 1 (xml-get-children outline 'objectStructureElement)))
         (main-link (-first (lambda (elem)
                              (cl-search "definitionIdentifier" (xml-get-attribute elem 'rel)))
                            main-links))
         ;; TODO Marian: the following two functions are hardcodes for pattern with #start=linno,colno
         (source-pos (abaplib--outline-get-source-pos main-link))
         (fname-base (abaplib--outline-get-filename-base main-link))
         (source-filename (file-name-completion fname-base object-path))
         (target-buffer (find-file-noselect source-filename))
         (output-log (format "Outline for %s\n\n" object-name)))
    (setq output-log (concat output-log
                             (format "%s" (abaplib--outline-print-item source-pos target-buffer))))
    (dolist (elem object-structure)
      (let* ((sub-obj-structure (xml-get-children elem 'objectStructureElement))
             (links (-filter (lambda (link)
                               (or (cl-search "definitionIdentifier" (xml-get-attribute link 'rel))
                                   (cl-search "implementationIdentifier" (xml-get-attribute link 'rel))))
                             (xml-get-children elem 'link)))
             (link (if sub-obj-structure (car links) (-last-item links)))
             ;; TODO Marian: the following two functions are hardcodes for pattern with #start=linno,colno
             (source-pos (abaplib--outline-get-source-pos link))
             (fname-base (abaplib--outline-get-filename-base link))
             (source-filename (file-name-completion fname-base object-path))
             (target-buffer (find-file-noselect source-filename)))
        (setq output-log (concat output-log
                                 (format "  %s" (abaplib--outline-print-item source-pos target-buffer))))
        (dolist (sub-elem sub-obj-structure)
          (let* ((sub-links (xml-get-children sub-elem 'link))
                 (sub-link (-last (lambda (sub-link)
                                    (or (cl-search "definitionIdentifier" (xml-get-attribute sub-link 'rel))
                                        (cl-search "implementationIdentifier" (xml-get-attribute sub-link 'rel))))
                                  sub-links))
                 ;; TODO Marian: the following two functions are hardcodes for pattern with #start=linno,colno
                 (source-pos (abaplib--outline-get-source-pos sub-link))
                 (fname-base (abaplib--outline-get-filename-base sub-link))
                 (source-filename (file-name-completion fname-base object-path))
                 (target-buffer (find-file-noselect source-filename)))
            (setq output-log (concat output-log
                                     (format "    %s" (abaplib--outline-print-item source-pos target-buffer))))

        ))))
    (abaplib-util-outline-buf-write output-log)
    (pop-to-buffer (get-buffer-create abaplib--outline-buffer))))

(defun abaplib--outline-get-source-pos (link)
  (let* ((navi-uri (xml-get-attribute link 'href))
         (source-pos (progn
                       (string-match "#start=\\([0-9]+,[0-9]+\\)" navi-uri)
                       (match-string 1 navi-uri))))
    (unless source-pos
      (error (format "Could not determine line and column number from \"%s\"." navi-uri)))
    (split-string source-pos ",")))

(defun abaplib--outline-get-filename-base (link)
  (let* ((navi-uri (xml-get-attribute link 'href))
         (src-uri  (car (split-string navi-uri "#start=\\([0-9]+,[0-9]+\\)"))))
    (-last-item (split-string src-uri "/"))))


(defun abaplib--outline-print-item (position target-buffer)
  (set-buffer target-buffer)
  (let* ((line (string-to-number (car position)))
         (column (string-to-number (cadr position)))
         (map (make-sparse-keymap))
         (fn-follow-pos `(lambda ()
                           (interactive)
                           (pop-to-buffer ,target-buffer)
                           (abaplib-util-goto-position ,line ,column))))
    (define-key map (kbd "<down-mouse-1>") fn-follow-pos)
    (define-key map (kbd "<RET>") fn-follow-pos)
    (propertize (progn
                  (goto-line line)
                  (string-trim-left (thing-at-point 'line)))
                'face 'underline
                'mouse-face 'highlight
                'keymap map)))

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
