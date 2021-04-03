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
