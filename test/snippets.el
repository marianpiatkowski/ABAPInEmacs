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
         (object-structure (xml-get-children outline 'objectStructureElement))
         ;; drop last item of object-structure == text elements
         (object-structure (-drop-last 1 object-structure))
         (main-link (-first (lambda (elem)
                              (cl-search "definitionIdentifier" (xml-get-attribute elem 'rel)))
                            main-links))
         (fname-base (abaplib--outline-get-filename-base main-link))
         (source-filename (file-name-completion fname-base object-path))
         (target-buffer (find-file-noselect source-filename))
         (source-pos (abaplib--outline-get-source-pos main-link target-buffer))
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
             (fname-base (abaplib--outline-get-filename-base link))
             (source-filename (file-name-completion fname-base object-path))
             (target-buffer (find-file-noselect source-filename))
             (source-pos (abaplib--outline-get-source-pos link target-buffer)))
        (setq output-log (concat output-log
                                 (format "  %s" (abaplib--outline-print-item source-pos target-buffer))))
        (dolist (sub-elem sub-obj-structure)
          (let* ((sub-links (xml-get-children sub-elem 'link))
                 (sub-link (-last (lambda (sub-link)
                                    (or (cl-search "definitionIdentifier" (xml-get-attribute sub-link 'rel))
                                        (cl-search "implementationIdentifier" (xml-get-attribute sub-link 'rel))))
                                  sub-links))
                 (fname-base (abaplib--outline-get-filename-base sub-link))
                 (source-filename (file-name-completion fname-base object-path))
                 (target-buffer (find-file-noselect source-filename))
                 (source-pos (abaplib--outline-get-source-pos sub-link target-buffer)))
            (setq output-log (concat output-log
                                     (format "    %s" (abaplib--outline-print-item source-pos target-buffer))))

        ))))
    (abaplib-util-outline-buf-write output-log)
    (pop-to-buffer (get-buffer-create abaplib--outline-buffer))))

(defun abaplib--outline-get-filename-base (link)
  (let* ((navi-uri (xml-get-attribute link 'href))
         (navi-uri (url-unhex-string navi-uri))
         ;; assume if exactly one of the two patterns in navi-uri occur that
         ;; sth. follows before and after first occurrence this pattern
         ;; => split1 or split2 has at least two elements
         ;; => check on (cdr split1) or (cdr split2) being nil, respectively, possible
         (split1 (split-string navi-uri "#start=\\([0-9]+,[0-9]+\\)"))
         (split2 (split-string navi-uri "#type=\\([A-Z]+/[A-Z]+\\)")))
    (cond ((cdr split1)
           (-last-item (split-string (car split1) "/")))
          ((cdr split2)
           (-last-item (split-string (car split2) "/")))
          ;; treatment for main link representing dev object
          ((string= navi-uri (abaplib-get-property 'uri))
           "main")
          (t (error (format "Cannot determine outline filename base from \"%s\"." navi-uri))))))

(defun abaplib--outline-get-source-pos (link target-buffer)
  (let* ((navi-uri (xml-get-attribute link 'href))
         (navi-uri (url-unhex-string navi-uri))
         (pattern1 (progn
                     (string-match "#start=\\([0-9]+,[0-9]+\\)" navi-uri)
                     (match-string 1 navi-uri)))
         (pattern2 (progn
                     (string-match ";name=\\([A-Za-z0-9_-]+\\(\s+[A-Za-z0-9_-]+\\)?\\)" navi-uri)
                     (match-string 1 navi-uri))))
    (cond (pattern1
           (let ((source-pos (split-string pattern1 ",")))
             (mapcar 'string-to-number source-pos)))
          (pattern2
           (let* ((search-patterns (split-string pattern2))
                  (type (progn
                          (string-match "#type=\\([A-Z]+/[A-Z]+\\)" navi-uri)
                          (match-string 1 navi-uri)))
                  (type-list  (split-string type "/"))
                  (major-type (car type-list))
                  (minor-type (cadr type-list))
                  (impl-func  (intern (concat "abaplib--outline-search-" (downcase minor-type)))))
             (funcall impl-func search-patterns target-buffer)))
          ;; treatment for main link representing dev object
          ((string= navi-uri (abaplib-get-property 'uri))
           (let* ((search-patterns (list (abaplib-get-property 'name)))
                  (type (abaplib-get-property 'type))
                  (type-list  (split-string type "/"))
                  (major-type (car type-list))
                  (minor-type (cadr type-list))
                  (impl-func  (intern (concat "abaplib--outline-search-" (downcase minor-type)))))
             (funcall impl-func search-patterns target-buffer)))
          (t (error (format "Cannot determine line and column number from \"%s\"." navi-uri))))))

(defun abaplib--outline-search-p (pattern target-buffer)
  "Search for beginning of program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "\\(REPORT\\|PROGRAM\\)" "\s+" (car pattern) "."))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pn (pattern target-buffer)
  "Search for local interface in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "INTERFACE" "\s+" (car pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pnm (pattern target-buffer)
  "Search for local interface method in program specified by `pattern'."
  (cl-assert (>= (length pattern) 2))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "INTERFACE" "\s+" (car pattern)))
    (re-search-forward (concat "METHODS" "\s+" (cadr pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pl (pattern target-buffer)
  "Search for local class in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "CLASS" "\s+" (car pattern) "\s+" "DEFINITION"))
    (backward-word)
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-plm (pattern target-buffer)
  "Search for local class method in program specified by `pattern'."
  (cl-assert (>= (length pattern) 2))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "CLASS" "\s+" (car pattern) "\s+" "IMPLEMENTATION"))
    (re-search-forward (concat "METHOD" "\s+" (cadr pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pla (pattern target-buffer)
  "Search for local class attribute in program specified by `pattern'."
  (cl-assert (>= (length pattern) 2))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "CLASS" "\s+" (car pattern) "\s+" "DEFINITION"))
    (re-search-forward (concat "DATA" "\s+" (cadr pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-py (pattern target-buffer)
  "Search for global type in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (car pattern))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pd (pattern target-buffer)
  "Search for global variable in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "DATA(" (car pattern) ")\\|DATA" "\s+" (car pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pe (pattern target-buffer)
  "Search for events in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (car pattern))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-print-item (position target-buffer)
  (set-buffer target-buffer)
  (let* ((line (car position))
         (column (cadr position))
         (map (make-sparse-keymap))
         (fn-follow-pos `(lambda ()
                           (interactive)
                           (pop-to-buffer ,target-buffer)
                           (abaplib-util-goto-position ,line ,column))))
    (define-key map (kbd "<down-mouse-1>") fn-follow-pos)
    (define-key map (kbd "<RET>") fn-follow-pos)
    (propertize (save-excursion
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
