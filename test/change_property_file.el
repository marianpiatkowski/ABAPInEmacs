
(assoc 'version
(json-read-file "~/packages/ABAPInEmacs/test/properties_template.json");
);

(let* ((json-file (json-read-file "~/packages/ABAPInEmacs/test/properties_template.json")))
  (setf (alist-get 'version json-file) "inactive")
  (setf (alist-get 'version (assoc-string "main.prog.abap" (assoc 'sources json-file))) "inactive")
  ;; json-file);
  (write-region (with-temp-buffer
                  (insert (json-encode json-file))
                  (json-pretty-print (point-min) (point-max))
                  (message (buffer-string))
                  (buffer-string))
                nil "~/packages/ABAPInEmacs/test/properties_template_changed.json"));
