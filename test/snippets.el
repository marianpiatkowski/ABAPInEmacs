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
