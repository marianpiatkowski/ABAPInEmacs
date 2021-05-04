;;; abaplib.el --- sap abap server lib          -*- lexical-binding: t; -*-

;; Copyright (C) 2020-  Marian Piatkowski
;; Copyright (C) 2018  Marvin Qian

;; Author: Marian Piatkowski <marianpiatkowski@web.de>
;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: SAP, ABAP, CDS

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

;;

;;; Code:

(require 'cl)
(require 'cl-lib)
(require 'request)
(require 'xml)
(require 'dash)
(require 'dom)


;; (defgroup abap nil
;;   "ABAP development environment in Emacs.")

(defcustom abap-workspace-dir "~/ws-abap/"
  "ABAP workspace directory"
  :type 'string
  :group 'abap)

(defcustom abap-save-login-credential t
  "Save logon data"
  :type 'boolean
  :group 'abap)

(defcustom abap-login-timeout 1800
  "login timeout (seconds)"
  :type 'number
  :group 'abap)


(defcustom abap-search-list-max-results 51
  "Object query list maximum result"
  :type 'number
  :group 'abap)

;; NOT Used Yet
(defvar abap-log-level nil
  "Logging level for abap
   One of value `debug'")

(defvar abaplib--login-token-cache nil
  "ABAP token used for authentication.")

(defvar abaplib--sap-client-cache nil
  "ABAP system logon client")

(defvar abaplib--login-last-time nil
  "Last login timestamp")

(defvar abaplib--workspace-descriptor-cache nil
  "Cache for workspace descriptor")

(defvar abaplib--current-project nil
  "Current working project")

(defvar-local abaplib--abap-object-properties nil
  "ABAP Object Properties")

(defvar-local abaplib--abap-object-tr-number nil
  "")

(defconst abaplib--log-buffer "*ABAP Log*"
  "ABAP log buffer")

(defconst abaplib--where-used-buffer "*ABAP Where-Used*"
  "ABAP Where-Used buffer")

(defconst abaplib--outline-buffer "*ABAP Outline*"
  "ABAP Outline buffer")

(defconst abaplib--console-buffer "*ABAP Console*"
  "ABAP Console buffer")

(defconst abaplib--code-search-buffer "*ABAP Code Search*"
  "ABAP Code Search buffer")

(defvar abaplib--code-search-params nil
  "Search parameters for ABAP Code Search")

(defvar abaplib-base-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    map)
  "Base keymap for *ABAP ...* buffers.")

(defvar abaplib-code-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") '(lambda () (interactive)
                                 (let ((search-result (abaplib-code-search abaplib--code-search-params)))
                                   (abaplib-display-code-search search-result))))
    map)
  "Keymap for *ABAP Code Search* buffer")

(defconst abaplib--location-stack-buffer "*ABAP Location Stack*"
  "ABAP Location Stack")

(defconst abaplib--uri-login "/sap/bc/adt/core/discovery")

(defconst abaplib--property-file ".properties.json")

(defvar abaplib--etag-parse-months '(("01" . "January") ("02" . "February") ("03" . "March")
                                     ("04" . "April") ("05" . "May") ("06" . "June")
                                     ("07" . "July") ("08" . "August") ("09" . "September")
                                     ("10" . "October") ("11" . "November") ("12" . "December")))

(defvar abaplib--location-stack nil
  "Stack of locations obtained by e.g. code navigation")

(defvar abaplib--location-stack-index 0
  "Index pointing to current element in location stack.
Value 0 means top of stack.")

(defcustom abaplib--location-stack-max-count 100
  "How many elements to keep on the location stack"
  :type 'integer
  :safe 'integerp)


;;==============================================================================
;; Module - Tools & Utilities
;;==============================================================================
(defun abaplib-util-current-dir ()
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun abaplib-util-xml-parser ()
  (libxml-parse-xml-region (point-min) (point-max)))

(defun abaplib-util-sourcecode-parser ()
  (progn
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match ""))
    (buffer-string)))

(defun abaplib-util-log-buf-write (log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--log-buffer))
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert "\n\n")
    (insert (concat "Log at: "
                    (format-time-string "%Y-%m-%dT%T")
                    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
                     (format-time-string "%z"))))
    (insert (format "%s" log))
    (use-local-map abaplib-base-mode-map)
    (setq buffer-read-only t)
    ))

(defun abaplib-util-where-used-buf-write (log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--where-used-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (format "%s" log))
    (use-local-map abaplib-base-mode-map)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun abaplib-util-outline-buf-write (log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--outline-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (format "%s" log))
    (use-local-map abaplib-base-mode-map)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun abaplib-util-console-buf-write (log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--console-buffer))
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert "\n\n")
    (insert (concat "Log at: "
                    (format-time-string "%Y-%m-%dT%T")
                    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
                     (format-time-string "%z"))))
    (insert "\n\n")
    (insert (format "%s" log))
    (use-local-map abaplib-base-mode-map)
    (setq buffer-read-only t)))

(defun abaplib-util-code-search-buf-write (log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--code-search-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (concat "Code search finished at: "
                    (format-time-string "%Y-%m-%dT%T")
                    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
                     (format-time-string "%z"))))
    (insert "\n\n")
    (insert (format "%s" log))
    (use-local-map abaplib-code-search-map)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun abaplib-util-log-buf-pop ()
  (pop-to-buffer (get-buffer-create abaplib--log-buffer)))

(defun abaplib-util-jsonize-to-file (list file)
  (write-region (with-temp-buffer
                  (insert (json-encode list))
                  (json-pretty-print (point-min) (point-max))
                  (buffer-string))
                nil file))

(defun abaplib-util-upsert-alists (alists pair)
  "Append/Update alist with pair"
  (let* ((key (car pair))
         (existp (assoc-string key alists)))
    (if existp
        (setcdr (assoc-string key alists) (cdr pair))
      (setq alists (append alists (list pair)))))
  alists)

(defun abaplib-util-goto-position (line column)
  (goto-line line)
  (move-to-column column))

(defun abaplib-util-get-xml-value (node key)
  (car (last (car (xml-get-children node key)))))

(defun abaplib-util-etag-equal (etag1 etag2)
  "Compare two ETags based on the substring from 0 to 14."
  (string= (substring etag1 0 14) (substring etag2 0 14)))

(defun abaplib-util-get-filename-base (full-source-uri)
  "Get filename base from `full-source-uri', i.e. substring after /source/."
  (let* ((split-on-source (-split-when (lambda (elem) (string= elem "source"))
                                       (split-string full-source-uri "/")))
         ;; gives main or implementations etc.
         (object-filename-base (if (cadr split-on-source) (caadr split-on-source) "main")))
    object-filename-base))

(defun abaplib-util-compare-source-code (list1 list2)
  "Given line and column number encoded in `list1' and `list2', respectively,
check whether position described by `list1' is before position `list2' in source code."
  (catch 'res
    (dolist (elem (cl-mapcar '- list2 list1))
    (if (> elem 0)
        (throw 'res t))
    (if (< elem 0)
        (throw 'res nil)))
    (throw 'res t)))

(defun abaplib--get-local-properties ()
  "Load property file on current directory for current buffer."
  (let ((property-file (expand-file-name abaplib--property-file)))
    (unless (file-exists-p property-file)
      (error "Missing property file, please use `abap-search-object' to retrieve again!"))
    (setq abaplib--abap-object-properties (json-read-file property-file))))

(defun abaplib-get-property (name &optional source-name)
  (unless abaplib--abap-object-properties
    (abaplib--get-local-properties))
  (if source-name
      (let* ((sources (alist-get 'sources abaplib--abap-object-properties))
             (source-properties (assoc-string source-name sources)))
        (alist-get name source-properties))
    (alist-get name abaplib--abap-object-properties)))

(defun abaplib-get-path (type &optional extra-directory extra-uri)
  (let* ((--dir-source-code "Source Code Library")
         (type-list (split-string type "/"))
         (major-type (intern (car type-list)))
         (minor-type (intern (nth 1 type-list)))
         (parent-directory)
         (sub-directory)
         (final-directory))

    (cond ((eq major-type 'CLAS)
           (setq parent-directory --dir-source-code)
           (setq sub-directory "Classes" ))
          ((eq major-type 'PROG)
           (setq parent-directory --dir-source-code)
           (setq sub-directory "Programs" ))
          ((eq major-type 'INTF)
           (setq parent-directory --dir-source-code)
           (setq sub-directory "Interfaces"))
          ((eq major-type 'TABL)
           (setq parent-directory --dir-source-code)
           (setq sub-directory "Tables"))
          ((eq major-type 'DDLS)
           (setq parent-directory --dir-source-code)
           (setq sub-directory "DDLSources"))
          ((eq major-type 'BDEF)
           (setq parent-directory --dir-source-code)
           (setq sub-directory "BusinessObjects"))
          ((eq major-type 'FUGR)
           (setq parent-directory --dir-source-code)
           (setq sub-directory "Functions")
           (if (and extra-directory extra-uri)
               (let ((object-uri (member "functions" (split-string extra-uri "/"))) ;; object-uri as a list
                     )
                 (setq extra-directory (upcase (mapconcat 'directory-file-name (cdr object-uri) "/"))))))
          )
    (unless (and parent-directory sub-directory)
      (error (format "Unknown ABAP source type %s" major-type)))

    (let* ((parent-path (expand-file-name parent-directory
                                          (abaplib-get-project-path)))
           (sub-path (expand-file-name sub-directory parent-path)))
      (unless (file-exists-p parent-path)
        (make-directory parent-path))
      (unless (file-exists-p sub-path)
        (make-directory sub-path))
      (if extra-directory
          (let ((extra-path (expand-file-name extra-directory sub-path)))
            (unless (file-exists-p extra-path)
              (make-directory extra-path t))
            extra-path)
        sub-path))))

(defun abaplib-is-logged ()
  (let ((now (time-to-seconds (current-time))))
    (and abaplib--login-last-time
         (<= (- now abaplib--login-last-time) abap-login-timeout))))

(defun abaplib-buffer-whole-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun abaplib--rest-api-call (uri success-callback complete-callback &rest args)
  "Call service API."
  (let* ((url (abaplib-get-project-api-url uri))
         (login-token (cons "Authorization" (abaplib-get-login-token)))
         (headers (cl-getf args :headers))
         (type    (or (cl-getf args :type) "GET"))
         (sync    (or (cl-getf args :sync) (and (not success-callback) (not complete-callback))))
         (params  (cl-getf args :params)))
    ;; Verify whether need to login with token
    (unless (abaplib-is-logged)
      (abaplib-auth-login-with-token abaplib--current-project
                                     login-token
                                     (abaplib-get-sap-client)))
    ;; For method like POST, PUT, DELETE, required to get CSRF Token first
    ;; (message "headers:= %s" headers)
    (unless (string= type "GET")
      ;; (setq headers (append headers (list login-token)))
      (unless (assoc-string 'x-csrf-token headers)
        (let ((csrf-token (abaplib-get-csrf-token)))
          (setq headers
                (append headers
                        (list (cons "x-csrf-token" csrf-token)))))))
    ;; TODO Delete :headers from args as we have explicitly put headers here
    ;; (setq params (append params
    ;;                      (list (cons "sap-client" sap-client))))
    (append (request-response-data
             (apply #'request
                    url
                    :sync sync
                    :headers headers
                    :status-code '(
                                   (304 . (lambda (&rest _) (message "Got 304: Source Not Modified")))
                                   (400 . (lambda (&rest _) (message "Got 400: Bad request")))
                                   (401 . (lambda (&rest _) (message "Got 401: Not Authorized")))
                                   (403 . (lambda (&rest _) (message "Got 403: Forbidden")))
                                   (415 . (lambda (&rest _) (message "Got 415: Unsupported Media Type"))))
                    :params params
                    :success success-callback
                    :error  (lambda (&key error-thrown &allow-other-keys &rest _)
                              (let ((error-message)))
                              (if error-thrown
                                  (setq error-message ((lambda (exception-node)
                                                         (car (last
                                                               (car (xml-get-children exception-node 'localizedMessage)))))
                                                       error-thrown))
                                (setq error-message "Unknown error occurred."))
                              (message "%s" error-message))
                    :complete complete-callback
                    args)))))


(defun abaplib-get-etag (uri)
  "Get ETag from uri.
If `uri' is of the type object-uri it returns the ETag of the ABAP development object.
If `uri' is of the type full-source-uri it returns the ETag of the source
being part of this development object."
  (let ((etag))
    (abaplib--rest-api-call
      uri
      (lambda (&rest rest)
        (let ((response (cl-getf rest :response)))
          (setq etag (request-response-header response "ETag"))))
      nil
      :sync t)
    etag))

(defun abaplib-check-version (etag source-properties &optional source-name)
  "Compare local and server-side source/object using the quantity ETag.
If `source-name' is specified `etag' acts like a source-etag and only the ETag of the source is compared.
Otherwise `etag' acts like a object-etag and every ETag as part of this development object is compared."
  (let* ((properties (if source-name (list (assoc-string source-name source-properties)) source-properties))
         (etag-match (-filter (lambda (source)
                                (abaplib-util-etag-equal (cdr (assoc 'etag source)) etag))
                              properties)))
    (unless etag-match
      (error "Local source/object seems not up to date. Cancelling request."))))

(defun abaplib-util-location-stack-elem-equal (elem1 elem2)
  "Compare two elements of location stack if they describe the same position."
  (let* ((buffer1 (cdr (assoc 'target-buffer elem1)))
         (pos1    (cdr (assoc 'position elem1)))
         (buffer2 (cdr (assoc 'target-buffer elem2)))
         (pos2    (cdr (assoc 'position elem2))))
    (and (eq buffer1 buffer2) (every '= pos1 pos2))))

(defun abaplib-location-stack-push (target-buffer row-pos col-pos)
  "Push location given by `target-buffer', `row-pos', and `col-pos' into location stack.
Format for the stack elements is the buffer and the position in the source file
given by line number and column number."
  (while (> abaplib--location-stack-index 0)
    (cl-decf abaplib--location-stack-index)
    (pop abaplib--location-stack))
  (let* ((source-pos (list row-pos col-pos))
         (stack-elem `((target-buffer . ,target-buffer)
                       (position      . ,source-pos))))
    (unless (abaplib-util-location-stack-elem-equal stack-elem (car abaplib--location-stack))
      (push stack-elem abaplib--location-stack)
      (when (> (length abaplib--location-stack) abaplib--location-stack-max-count)
        (nbutlast abaplib--location-stack
                  (- (length abaplib--location-stack) abaplib--location-stack-max-count))))))

(defun abaplib-location-stack-pop ()
  (if (> abaplib--location-stack-index 0)
      (cl-decf abaplib--location-stack-index))
  (pop abaplib--location-stack))

(defun abaplib-location-stack-forward ()
  (let ((stack-elem (nth abaplib--location-stack-index abaplib--location-stack))
        ;; bring position under cursor to format of location stack elements
        (curr-pos `((target-buffer . ,(current-buffer))
                    (position      . ,(list (line-number-at-pos) (current-column))))))
    (if (abaplib-util-location-stack-elem-equal curr-pos stack-elem)
        (abaplib-location-stack-jump -1)
      (abaplib-location-stack-jump 0))))

(defun abaplib-location-stack-back ()
  (let ((stack-elem (nth abaplib--location-stack-index abaplib--location-stack))
        ;; bring position under cursor to format of location stack elements
        (curr-pos `((target-buffer . ,(current-buffer))
                    (position      . ,(list (line-number-at-pos) (current-column))))))
    (if (abaplib-util-location-stack-elem-equal curr-pos stack-elem)
        (abaplib-location-stack-jump 1)
      (abaplib-location-stack-jump 0))))

(defun abaplib-location-stack-jump (by)
  "Move in location stack.
If `by' < 0 then move up in the location stack.
If `by' > 0 then move down in the location stack.
The value 0 for `abaplib--location-stack-index' points to the top of the stack."
  (let ((target-index (+ abaplib--location-stack-index by)))
    (when (and (>= target-index 0) (< target-index (length abaplib--location-stack)))
      (setq abaplib--location-stack-index target-index)
      (let* ((target-elem   (nth target-index abaplib--location-stack))
             (target-buffer (cdr (assoc 'target-buffer target-elem)))
             (source-pos    (cdr (assoc 'position target-elem)))
             (line          (car source-pos))
             (column        (cadr source-pos)))
        (pop-to-buffer target-buffer)
        (abaplib-util-goto-position line column)))))

(defvar abaplib-location-stack-visualize-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") '(lambda () (interactive) (abaplib-location-stack-visualize)))
    map)
  "Keymap for *ABAP Location Stack* buffer.")

(defun abaplib-location-stack-visualize ()
  "Print current location stack."
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--location-stack-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "\n")
    (let ((cur-index 0))
      (dolist (elem abaplib--location-stack)
        (if (= cur-index abaplib--location-stack-index)
            (insert (format "  %s  <---\n" (abaplib--print-location-stack-elem elem cur-index)))
          (insert (format "  %s\n" (abaplib--print-location-stack-elem elem cur-index))))
        (cl-incf cur-index)))
    (use-local-map abaplib-location-stack-visualize-mode-map)
    (goto-char (point-min))
    (setq buffer-read-only t))
  (pop-to-buffer (get-buffer-create abaplib--location-stack-buffer)))

(defun abaplib--print-location-stack-elem (stack-elem stack-index)
  (let* ((target-buffer  (cdr (assoc 'target-buffer stack-elem)))
         (source-pos     (cdr (assoc 'position stack-elem)))
         (line           (car source-pos))
         (column         (cadr source-pos))
         (map            (make-sparse-keymap))
         (fn-follow-pos `(lambda ()
                           (interactive)
                           (pop-to-buffer ,target-buffer)
                           (abaplib-util-goto-position ,line ,column)
                           (setq abaplib--location-stack-index ,stack-index))))
    (define-key map (kbd "<down-mouse-1>") fn-follow-pos)
    (define-key map (kbd "<RET>") fn-follow-pos)
    (propertize (format "%s" stack-elem)
                'face 'underline
                'mouse-face 'highlight
                'keymap map)))

;;==============================================================================
;; Module - Project
;;==============================================================================
(defun abaplib-get-ws-describe-file ()
  "Get workspace description file."
  (let* ((config-dir (expand-file-name ".abap" abap-workspace-dir))
         (describe-file (expand-file-name "projects.json" config-dir)))
    (unless (file-directory-p config-dir) ;; Initialize for first time
      (make-directory config-dir))
    (unless (file-exists-p describe-file)
      (abaplib-util-jsonize-to-file nil describe-file))
    describe-file))

(defun abaplib-save-workspace-descriptor (descriptor)
  "Save descriptor to workspace in `overwrite' way."
  (abaplib-util-jsonize-to-file
   descriptor
   (abaplib-get-ws-describe-file))
  (setq abaplib--workspace-descriptor-cache descriptor))

(defun abaplib-get-workspace-descriptor ()
  "Get workspace descriptor."
  (unless abaplib--workspace-descriptor-cache
    (setq abaplib--workspace-descriptor-cache
          (json-read-file (abaplib-get-ws-describe-file))))
  abaplib--workspace-descriptor-cache)

(defun abaplib-get-project-props (project)
  "Get project properties by project key.
   In current implemention should be symbol of project directory."
  (assoc-string project (abaplib-get-workspace-descriptor)))

(defun abaplib-project-set-property (project property)
  "Set property to project, property should be a key value pair."
  (let* ((project-props (abaplib-get-project-props project))
         (new-props (abaplib-util-upsert-alists project-props property)))
    (abaplib-upsert-project new-props)))

(defun abaplib-project-get-property (key &optional project)
  (let ((project (or project abaplib--current-project)))
    (unless project
      (error "Missing project"))
    (cdr (assoc-string key
                       (cdr (abaplib-get-project-props project))))))

(defun abaplib-upsert-project (project-props)
  "When creating or initializing a project, add project information into workspace descriptor."
  (let* ((descriptor (abaplib-get-workspace-descriptor))
         (new-descriptor (abaplib-util-upsert-alists descriptor project-props)))
    (abaplib-save-workspace-descriptor new-descriptor)))

(defun abaplib-create-project (project-dir)
  "Create project. If one already exists, do nothing"
  (let* ((project-dir (replace-regexp-in-string "/$" "" project-dir))
         (project-props-curr (abaplib-get-project-props project-dir))
         (project-props-new (or project-props-curr
                                (cons (intern project-dir)
                                      (list (cons 'path project-dir))))))
    ;; Warning if already exist
    (if project-props-curr
        (error "Project %s already exist!" project-dir)
      (progn
        ;; Create project directory
        (unless (file-directory-p project-dir)
          (make-directory project-dir))
        (abaplib-upsert-project project-props-new)
        (abaplib-switch-project project-dir)))))

(defun abaplib-project-init-propose (dir)
  "Give a project suggestion in prompt."
  (if dir (cond
           ((locate-dominating-file dir ".git"))
           ((locate-dominating-file dir ".abap"))
           (t abap-workspace-dir))
    abap-workspace-dir))

(defun abaplib-get-project-list ()
  "Get project list described in workspace descriptor file <workspace_dir>/.abap_workspace."
  (let ((descriptor (abaplib-get-workspace-descriptor)))
    (mapcar
     (lambda (project-props)
       (car project-props))
     descriptor)))

(defun abaplib-switch-project (project)
  "Set the variable `abaplib--current-project'."
  (setq abaplib--current-project project))

(defun abaplib-remove-project (project)
  "Remove project from workspace."
  (let* ((descriptor (abaplib-get-workspace-descriptor))
         (new-descriptor (seq-filter
                          (lambda (project-props)
                            (not (eq (car project-props) (intern project))))
                          descriptor)))
    (abaplib-save-workspace-descriptor new-descriptor)
    (setq abaplib--current-project nil)))

(defun abaplib-get-project-api-url (uri)
  "Build full API URL."
  (if (string-match "^http[s]*://" uri)
      uri
    (concat (replace-regexp-in-string "/*$" "/" (abaplib-project-get-property 'server))
            (replace-regexp-in-string "^/*" "" uri))))

(defun abaplib-get-project-cache-dir ()
  "Get project cache directory."
  (let ((cache-dir (expand-file-name
                    ".cache"
                    (abaplib-project-get-property 'path))))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir))
    cache-dir))


(defun abaplib-add-server-to-project (project server)
  (abaplib-project-set-property project (cons 'server server)))

(defun abaplib-get-project-path ()
  "Get path of current project."
  (abaplib-project-get-property 'path))

;;==============================================================================
;; Module - Authentication
;;==============================================================================

(defun abaplib-auth-login-with-token (project login-token sap-client &optional save?)
  "Login into ABAP Server with token."
  (let ((url ;; "http://ldcier9.wdf.sap.corp:50000/sap/bc/adt/core/discovery"
         (abaplib-get-project-api-url abaplib--uri-login))
        (server (abaplib-project-get-property 'server project))
        (login-status))
    (unless server
      (error "Project %s not bind to any server" project))
    (message "Logging in...")
    ;; First login with token to get cookie
    (request
     url
     :sync t
     :headers (list (cons "Authorization" login-token))
     :params (list (cons "sap-client" sap-client)))
    ;; Login with cookie
    (setq login-status
          (request-response-symbol-status
           (request
            url
            :sync t)))
    (unless (eq login-status 'success)
      (error "Connecting to server failed!"))
    ;; Login succeed
    (setq abaplib--login-last-time (time-to-seconds (current-time)))
    (when save?
      (let ((login-credential (cons 'login_token login-token))
            (sap-client       (cons 'sap_client sap-client)))
        (abaplib-project-set-property project login-credential)
        (abaplib-project-set-property project sap-client)))
    (message "Connected to server!")))

(defun abaplib-get-login-token ()
  "Get login token and validate `abaplib--login-token-cache'."
  (let ((login-token (or abaplib--login-token-cache
                         (abaplib-project-get-property 'login_token))))
    (unless login-token
      (error "Please login first"))
    (setq abaplib--login-token-cache login-token)))

(defun abaplib-get-sap-client ()
  "Get SAP client and validate `abaplib--sap-client-cache'."
  (let ((sap-client (or abaplib--sap-client-cache
                        (abaplib-project-get-property 'sap_client))))
    (unless sap-client
      (error "Please login first"))
    (setq abaplib--sap-client-cache sap-client)))

(defun abaplib-get-csrf-token ()
  (let* ((login-token (cons "Authorization" (abaplib-get-login-token)))
         (sap-client (abaplib-get-sap-client))
         (login-url (abaplib-get-project-api-url abaplib--uri-login))
         (response (request
                    login-url
                    :sync t
                    :headers (list (cons "X-CSRF-Token" "Fetch") login-token)
                    :params `((sap-client . ,sap-client)))))
    (request-response-header response "x-csrf-token")))



;;==============================================================================
;; Module - Core Services - Search
;;==============================================================================
(defun abaplib-do-search (query-string)
  "Search ABAP objects in server with synchronous URL call."
  (let* ((search-uri "/sap/bc/adt/repository/informationsystem/search")
         (params `((operation . "quickSearch")
                   (query . ,(concat query-string "*"))
                   (maxResults . ,abap-search-list-max-results)))
         (data (abaplib--rest-api-call search-uri
                                       nil
                                       nil
                                       :params params
                                       :parser 'abaplib-util-xml-parser))
         (object-list (xml-get-children data 'objectReference)))
    object-list))

(defun abaplib-complete-packages-from-string (s)
  (let* ((search-uri "/sap/bc/adt/repository/informationsystem/search")
         (params `((operation         . "quickSearch")
                   (query             . ,(concat s "*"))
                   (useSearchProvider . "X")
                   (maxResults        . ,abap-search-list-max-results)
                   (objectType       . "DEVC/K")))
         (data (abaplib--rest-api-call search-uri
                                       nil
                                       nil
                                       :params params
                                       :parser 'abaplib-util-xml-parser))
         (package-list))
    (dolist (object-ref (xml-get-children data 'objectReference))
      (setq package-list (-snoc package-list (xml-get-attribute object-ref 'name))))
    package-list))

(defun abaplib-code-search (search-params)
  (message "Searching in ABAP source code...")
  (let* ((search-uri "/sap/bc/adt/repository/informationsystem/textsearch")
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ("Accept" . "application/xml"))))
    (setq abaplib--code-search-params search-params)
    (abaplib--rest-api-call search-uri
                            nil
                            nil
                            :headers headers
                            :params search-params
                            :parser 'abaplib-util-xml-parser)))

(defun abaplib-display-code-search (search-result)
  (let* ((text-search-node    (car (xml-get-children search-result 'textSearchObjects)))
         (text-search-objects (xml-get-children text-search-node 'textSearchObject))
         ;; filter objects that actually contain search matches
         (text-search-objects (-filter (lambda (elem)
                                         (= (length (xml-get-children elem 'textLines)) 1))
                                       text-search-objects))
         (search-pattern      (cdr (assoc 'searchString abaplib--code-search-params)))
         (no-search-results   (xml-get-attribute search-result 'numberOfResults))
         (output-log (format "ABAP Source Search: '%s' in %s"
                             search-pattern
                             (assoc 'packageName abaplib--code-search-params))))
    (setq output-log
          (concat output-log "\n"
                  (format "Number of matches: %s\n\n" no-search-results)))
    ;; loop over objects that contain matches
    (dolist (text-search-object text-search-objects)
      (let* ((object-node (car (xml-get-children text-search-object 'adtMainObject)))
             (text-lines-node  (car (xml-get-children text-search-object 'textLines)))
             (object-type (xml-get-attribute object-node 'type))
             (object-name (xml-get-attribute object-node 'name)))
        (setq output-log
              (concat output-log "\n"
                      (format "%s:" (propertize (xml-get-attribute object-node 'name)
                                                'face '(:foreground "dark green")))))
        (dolist (text-line (xml-get-children text-lines-node 'textLine))
          (let* ((target-uri (url-unhex-string (xml-get-attribute text-line 'uri)))
                 (content    (car (xml-get-children text-line 'content)))
                 (content    (abaplib--code-search-format-match search-pattern (nth 2 content)))
                 (target-object-info `((name . ,object-name)
                                       (type . ,object-type)
                                       (uri  . ,target-uri))))
            (setq output-log
                  (concat output-log "\n"
                          (format "%s" (abaplib--code-search-print-item content target-object-info))))
            ))))
    (abaplib-util-code-search-buf-write output-log)
    (pop-to-buffer (get-buffer-create abaplib--code-search-buffer))))

(defun abaplib--code-search-format-match (pattern content)
  "Reformat `content' line obtained by HTTP request highlighting search `pattern'."
  (let* ((text-elems (string-remove-suffix "..." content))
         (text-elems (string-remove-prefix "..." text-elems))
         (text-elems (with-temp-buffer
                       (insert text-elems)
                       (dom-strings (libxml-parse-html-region (point-min) (point))))))
    (mapconcat (lambda (elem)
                 (if (string= elem pattern)
                     (format "%s" (propertize elem 'face '(bold (:foreground "red"))))
                   elem))
               text-elems "")))

(defun abaplib--code-search-print-item (content object-info)
  (let* ((object-name     (cdr (assoc 'name object-info)))
         (object-type     (cdr (assoc 'type object-info)))
         (target-uri      (cdr (assoc 'uri  object-info)))
         (object-path     (abaplib-get-path object-type object-name))
         (object-filename (file-name-completion "main" object-path)) ;; may be nil, handled below
         (object-filepath (concat object-path "/" object-filename))
         (source-pos      (progn
                            (string-match "#start=\\([0-9]+,[0-9]+\\)" target-uri)
                            (match-string 1 target-uri)))
         (source-pos      (split-string source-pos ","))
         (source-pos      (mapcar 'string-to-number source-pos))
         (map             (make-sparse-keymap))
         (fn-follow-pos  `(lambda ()
                            (interactive)
                            (let* ((path     ,object-path)
                                   (filepath ,object-filepath)
                                   (line     ,(car source-pos))
                                   (column   ,(cadr source-pos)))
                              (switch-to-buffer (find-file-other-window filepath))
                              (abaplib-util-goto-position line column)))))
    (define-key map (kbd "<down-mouse-1>") fn-follow-pos)
    (define-key map (kbd "<RET>") fn-follow-pos)
    (if object-filename ;; print item with link
        (propertize content
                    'mouse-face 'highlight
                    'keymap map)
      content)))


;;==============================================================================
;; Module - Core Services - Syntax Check
;;==============================================================================
(defun abaplib-do-check (object-info source-code dont-show-error?)
  "Check syntax of `source-code'."
  (message "Sending syntax check request...")
  (let* ((object-uri (cdr (assoc 'uri         object-info)))
         (source-uri (cdr (assoc 'src-uri     object-info)))
         (version    (cdr (assoc 'src-version object-info)))
         (chkrun-uri (concat object-uri "/" source-uri))
         (chkrun-etag    (abaplib-get-etag chkrun-uri))
         (chkrun-content (base64-encode-string source-code)))
    (abaplib-check-version chkrun-etag
                           (abaplib-get-property 'sources)
                           (cdr (assoc 'file object-info)))
    (if dont-show-error?
        (abaplib--check-post-sync version object-uri chkrun-uri chkrun-content)
      (abaplib--check-post-async version object-uri chkrun-uri chkrun-content))))

(defun abaplib--check-template (adtcore-uri chkrun-uri version chkrun-content)
  "Return XML of 'checkObjects' node."
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   "<chkrun:checkObjectList xmlns:adtcore=\"http://www.sap.com/adt/core\" xmlns:chkrun=\"http://www.sap.com/adt/checkrun\">"
   (format "<chkrun:checkObject adtcore:uri=\"%s\" chkrun:version=\"%s\">"
           adtcore-uri
           version)
   (when chkrun-content
     (concat
      "<chkrun:artifacts>"
      (format "<chkrun:artifact chkrun:contentType=\"text/plain; charset=utf-8\" chkrun:uri=\"%s\">"
              chkrun-uri)
      (format "<chkrun:content>%s</chkrun:content>"
              chkrun-content)
      "</chkrun:artifact>"
      "</chkrun:artifacts>" ))
   "</chkrun:checkObject>"
   "</chkrun:checkObjectList>"))

(defun abaplib--check-post-async (version adtcore-uri chkrun-uri chkrun-content)
  (let ((check-uri "/sap/bc/adt/checkruns")
        (post-data (abaplib--check-template adtcore-uri chkrun-uri version chkrun-content)))
    (abaplib--rest-api-call
     check-uri
     (lambda (&rest rest)
       (let* ((check-report (xml-get-children (cl-getf rest :data) 'checkReport))
              (message-list (xml-get-children (car check-report) 'checkMessageList))
              (messages (xml-get-children (car message-list) 'checkMessage)))
         (abaplib-check-show-message messages)))
     nil
     :parser 'abaplib-util-xml-parser
     :type "POST"
     :data post-data
     :params '((reporters . abapCheckRun))
     :headers `(("Content-Type" . "application/vnd.sap.adt.checkobjects+xml")))))

(defun abaplib--check-post-sync (version adtcore-uri chkrun-uri chkrun-content)
  (let* ((check-uri "/sap/bc/adt/checkruns")
         (post-data (abaplib--check-template adtcore-uri chkrun-uri version chkrun-content))
         (response-data (abaplib--rest-api-call
                         check-uri
                         nil
                         nil
                         :parser 'abaplib-util-xml-parser
                         :type "POST"
                         :data post-data
                         :params '((reporters . abapCheckRun))
                         :headers `(("Content-Type" . "application/vnd.sap.adt.checkobjects+xml"))))
         (check-report (xml-get-children response-data 'checkReport))
         (message-list (xml-get-children (car check-report) 'checkMessageList))
         (messages (xml-get-children (car message-list) 'checkMessage))
         (parsed-errors))
    (mapcar (lambda (message)
              (let* ((uri (xml-get-attribute message 'uri))
                     (level (case (intern (xml-get-attribute message 'type))
                             ('W "warning")
                             ('E "error")
                             (t "success")))
                     (text (xml-get-attribute message 'shortText))
                     (position (split-string (progn
                                 (string-match "#start=\\([0-9]+,[0-9]+\\)" uri)
                                 (match-string 1 uri)) ","))
                     (pos-line (string-to-number (car position)))
                     (pos-column (string-to-number (cadr position))))
                `((line . ,pos-line)
                  (column . ,pos-column)
                  (level . ,level)
                  (message . ,text))
                )) messages)))

(defun abaplib--check-render-type-text (type)
  (cond ((string= type "E") (propertize "Error"       'face '(bold (:foreground "red"))))
        ((string= type "W") (propertize "Warning"     'face '(bold (:foreground "orange"))))
        ((string= type "I") (propertize "Information" 'face '(bold (:foreground "green"))))
        (t "Other")))

(defun abaplib--check-render-pos (position &optional target-buffer)
  (let* ((target-buffer (or target-buffer (current-buffer)))
         (pos-list (split-string position ","))
         (line (string-to-number (car pos-list)))
         (column (string-to-number (cadr pos-list)))
         (map (make-sparse-keymap))
         (fn-follow-pos `(lambda ()
                           (interactive)
                           (pop-to-buffer ,target-buffer)
                           (abaplib-util-goto-position ,line ,column))
                        ))
    (define-key map (kbd "<down-mouse-1>") fn-follow-pos)
    (define-key map (kbd "<RET>") fn-follow-pos)
    (propertize position
                'face 'underline
                'mouse-face 'highlight
                'keymap map)))

(defun abaplib-check-show-message (messages)
  (let ((severity-level "I")
        (output-log))
    (dolist (message messages (cl-values severity-level output-log))
      (let* ((uri (xml-get-attribute message 'uri))
             (type (xml-get-attribute message 'type))
             (text (xml-get-attribute message 'shortText))
             (position (progn
                         (string-match "#start=\\([0-9]+,[0-9]+\\)" uri)
                         (match-string 1 uri))))
        (if (or (and (string= type "W") (string= severity-level "I"))
                (and (string= type "E") (or (string= severity-level "W")
                                            (string= severity-level "I"))))
            (setq severity-level type))
        (setq output-log
              (concat output-log "\n"
                      (concat (format "[%s] " (abaplib--check-render-type-text type))
                              (format "at position (%s): "
                                      (abaplib--check-render-pos position))
                              text)))))
    (if output-log
        (abaplib-util-log-buf-write output-log))
    (cond ((string= severity-level "I")
           (message "Syntax check completed with `success' result."))
          ((string= severity-level "W")
           (message "Syntax check completed with `warning' messages."))
          ((string= severity-level "E")
           (progn
             (message "Syntax check completed with `error' messages.")
             (abaplib-util-log-buf-pop))))))

;;==============================================================================
;; Module - Core Services - Lock
;;==============================================================================
(defun abaplib--lock-sync (uri csrf-token)
  (message "Trying to lock object...")
  (let* ((root-node (abaplib--rest-api-call
                     uri
                     nil
                     nil
                     :parser 'abaplib-util-xml-parser
                     :params '((_action . LOCK)
                               (accessMode . MODIFY))
                     :type "POST"
                     :headers `(("X-sap-adt-sessiontype" . "stateful")
                                ("x-csrf-token" . ,csrf-token))))
         (node-name (xml-node-name root-node)))
    (if (string= node-name "abap")
        ((lambda (abap-node) ;; Get lock handle
           (car (last
                 (car (xml-get-children
                       (car (xml-get-children
                             (car (xml-get-children abap-node 'values))
                             'DATA))
                       'LOCK_HANDLE))))) root-node)
      ;; Request lock failed
      (let ((error-message
             ((lambda (exception-node)
                (car (last
                      (car (xml-get-children exception-node 'localizedMessage)))))
              root-node)))
        (error  error-message)))))

;; Seems not required as next call of lock will automatically do the unlock
(defun abaplib-unlock-async (uri lock-handle)
  (let ((prog-name (abaplib-object-get-name)))
    (abaplib--rest-api-call
     uri
     (lambda (&rest response)
       (message "Unlocked."))
     nil
     :type "POST"
     :headers `(("X-sap-adt-sessiontype" . "stateless"))
     :params `(("lockHandle" . ,lock-handle)))))

;;========================================================================
;; Module - Core Services - Activate Server Side Source
;;========================================================================
(defun abaplib-do-activate (object-info)
  (message "Post activation request...")
  (let ((object-name (cdr (assoc 'name object-info)))
        (object-type (cdr (assoc 'type object-info)))
        (object-uri  (cdr (assoc 'uri  object-info))))
    (let ((object-etag (abaplib-get-etag object-uri)))
      (abaplib-check-version object-etag (abaplib-get-property 'sources)))
    (let ((activation-result (abaplib--activate-post object-name object-uri)))
      (when (or (cl-search "Activation successful" activation-result)
                (cl-search "Source activated"      activation-result))
        (abaplib-do-retrieve-metadata object-name object-type object-uri)
        ;; redisplay message
        (sit-for 0.1)
        (message activation-result)))))

(defun abaplib--activate-parse-result (result)
  (let ((result-type (car result)))
    (case result-type
      ('messages (abaplib--activate-show-message (xml-get-children result
                                                                   'msg)))
      ('inactiveObjects (abaplib--activate-postaudit (xml-get-children result
                                                                       'entry)))
      (t (message "Source activated in server!")))))

(defun abaplib--activate-post (adtcore-name adtcore-uri)
  (let ((preaudit-result (abaplib--activate-preaudit adtcore-name adtcore-uri)))
    (abaplib--activate-parse-result preaudit-result)))

(defun abaplib--activate-preaudit (adtcore-name adtcore-uri)
  (let ((activate-uri "/sap/bc/adt/activation")
        (post-body (abaplib--activate-preaudit-template adtcore-name adtcore-uri)))
    (abaplib--rest-api-call activate-uri
                            nil
                            nil
                            :parser 'abaplib-util-xml-parser
                            :type "POST"
                            :params '((method . activate)
                                      (preauditRequested . true))
                            :data post-body)))

(defun abaplib--activate-preaudit-template (adtcore-name adtcore-uri)
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   "<adtcore:objectReferences xmlns:adtcore=\"http://www.sap.com/adt/core\">"
   (format "<adtcore:objectReference adtcore:name=\"%s\" adtcore:uri=\"%s\"/>"
           adtcore-name
           adtcore-uri)
   "</adtcore:objectReferences>"))

(defun abaplib--activate-postaudit (inactive-objects)
  (let ((activate-uri "/sap/bc/adt/activation")
        (post-body (abaplib--activate-postaudit-template inactive-objects)))
    (abaplib--rest-api-call activate-uri
                            (lambda (&rest rest)
                              (let ((result (cl-getf rest :data)))
                                (abaplib--activate-parse-result result)))
                            nil
                            :parser 'abaplib-util-xml-parser
                            :type "POST"
                            :params '((method . activate)
                                      (preauditRequested . false))
                            :data post-body)))

(defun abaplib--activate-postaudit-template (inactive-objects)
  (let ((post-xml ""))
    (setq post-xml (concat post-xml
                           "<?xml version=\"1.0\" encoding=\"UTF-8\"?><adtcore:objectReferences xmlns:adtcore=\"http://www.sap.com/adt/core\">"))
    (dolist (entry inactive-objects)
      (let* ((object (car (xml-get-children entry 'object)))
             (object-ref (car (xml-get-children object 'ref))))
        (when object-ref
          (let ((name (xml-get-attribute object-ref 'name))
                (type (xml-get-attribute object-ref 'type))
                (uri (xml-get-attribute object-ref 'uri))
                (parent-uri (xml-get-attribute object-ref 'parentUri))
                (object-ref-str))
            (if (string= parent-uri "")
                (setq object-ref-str
                      (format "<adtcore:objectReference adtcore:uri=\"%s\" adtcore:type=\"%s\" adtcore:name=\"%s\"/>" uri type name ))
              (setq object-ref-str
                    (format "<adtcore:objectReference adtcore:uri=\"%s\" adtcore:type=\"%s\" adtcore:name=\"%s\" adtcore:parentUri=\"%s\"/>" uri type name parent-uri)))
            (setq post-xml (concat post-xml object-ref-str))))))
    (setq post-xml (concat post-xml
                           "</adtcore:objectReferences>"))
    post-xml))

(defun abaplib--activate-show-message (messages)
  (let ((severity-level "I")
        (output-log))
    (dolist (message messages (cl-values severity-level output-log))
      (let* ((uri (xml-get-attribute message 'href))
             (type (xml-get-attribute message 'type))
             (text (car (last (car (xml-get-children
                                    (car (xml-get-children message 'shortText))
                                    'txt)))))
             (position (progn
                         (string-match "#start=\\([0-9]+,[0-9]+\\)" uri)
                         (match-string 1 uri))))
        (if (or (and (string= type "W") (string= severity-level "I"))
                (and (string= type "E") (or (string= severity-level "W")
                                            (string= severity-level "I"))))
            (setq severity-level type))

        (setq output-log
              (concat output-log "\n"
                      (concat (format "[%s] " (abaplib--check-render-type-text type))
                              (format "at position (%s): "
                                      (abaplib--check-render-pos position))
                              text)))))
    (if output-log
        (abaplib-util-log-buf-write output-log))
    (cond ((string= severity-level "I")
           (message "Activation successful!"))
          ((string= severity-level "W")
           (message "Activation successful with `warnings'!"))
          ((string= severity-level "E")
           (progn
             (abaplib-util-log-buf-pop)
             (message "Activation failed with `errors'!"))))))

;;========================================================================
;; Module - Core Services - Handle Change Request
;;========================================================================
(defun abaplib-retrieve-trans-request (full-source-uri)
  "Check and retrieve transport request."
  (message "Checking transport request...")
  (let* ((transcheck-uri "/sap/bc/adt/cts/transportchecks")
         (post_data (abaplib--transport-check-template full-source-uri))
         (xml-root (abaplib--rest-api-call
                    transcheck-uri
                    nil
                    nil
                    :type "POST"
                    :data post_data
                    :headers `(("Content-Type" . "application/vnd.sap.as+xml"))
                    :parser 'abaplib-util-xml-parser))
         (value-node (car (xml-get-children xml-root 'values)))
         (data-node (car (xml-get-children value-node 'DATA)))
         (req-node (car (xml-get-children data-node 'REQUESTS)))
         (requests (xml-get-children req-node 'CTS_REQUEST))
         (locks-node (car (xml-get-children data-node 'LOCKS)))
         (object-lock (car (xml-get-children locks-node 'CTS_OBJECT_LOCK))))
    (if requests
        (mapcar (lambda (request)
                  (let* ((req-header (car (xml-get-children request 'REQ_HEADER)))
                         (tr-number (car (last (car (xml-get-children req-header 'TRKORR)))))
                         (text (car (last (car (xml-get-children req-header 'AS4TEXT))))))
                    (format "%s | %s" tr-number text)))
                requests)
      (if object-lock
          (let* ((lock-holder (car (xml-get-children object-lock 'LOCK_HOLDER)))
                 (req-header (car (xml-get-children lock-holder 'REQ_HEADER)))
                 (tr-number (car (last (car (xml-get-children req-header 'TRKORR)))))
                 (text (car (last (car (xml-get-children req-header 'AS4TEXT))))))
            (list (format "%s | %s" tr-number text)))
        (error "Can't find available transport request.")))))

(defun abaplib--transport-check-template (full-source-uri)
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?><asx:abap xmlns:asx=\"http://www.sap.com/abapxml\" version=\"1.0\">"
   "<asx:values>"
   "<DATA>"
   "<PGMID/>"
   "<OBJECT/>"
   "<OBJECTNAME/>"
   "<DEVCLASS/>"
   "<OPERATION/>"
   "<URI>" full-source-uri "</URI>"
   "</DATA>"
   "</asx:values>"
   "</asx:abap>"))

(defun abaplib-post-cm-checkrun (tr-number full-source-uri)
  (let* ((cm-checkrun-uri "/sap/bc/adt/solutionmanager/cm/checkruns")
         (post_data (abaplib--solutionmanager-check-template tr-number full-source-uri))
         (xml-root (abaplib--rest-api-call
                    cm-checkrun-uri
                    nil
                    nil
                    :type "POST"
                    :data post_data
                    :headers `(("Content-Type" . "application/vnd.sap.adt.cts.solman.checkobjects.v1+xml"))
                    :parser 'abaplib-util-xml-parser))
         (status-text (xml-get-attribute xml-root 'statusText)))))

(defun abaplib--solutionmanager-check-template (tr-number full-source-uri)
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?><csol:checkObjects xmlns:csol=\"http://www.sap.com/solman/check\">"
   "<csol:trRequest>" tr-number "</csol:trRequest>"
   "<csol:checkObject operation=\"Change\" uri=\"" full-source-uri "\"/>"
   "</csol:checkObjects>"))
;;========================================================================
;; Module - Core Services - Retrieve Metadata & Source Code
;;========================================================================
(defun abaplib-do-retrieve (name type uri &optional source-name)
  "Retrieve ABAP development object."
  ;; 1. Retrieve metadata from uri -- common
  ;; 2. Parse metadata -- specific
  ;; 3. Save properties & Old properties -- common
  ;; 4. Parse source part --> source type/uri/etag -- specific
  ;; 5. Retrieve source
  ;; TODO Preserver previous Etag Etag
  (let* ((object-path (abaplib-get-path type name uri))
         (property-file (expand-file-name abaplib--property-file object-path))
         (properties (abaplib--retrieve-metadata uri type property-file))
         (sources (alist-get 'sources properties)))
    (mapc (lambda (source-property)
            (let* ((local-source-name (car source-property))
                   (source-uri (alist-get 'source-uri source-property))
                   (full-source-uri (concat uri "/" source-uri))
                   (etag nil)
                   (file-path (expand-file-name local-source-name object-path)))
              (when (or (not source-name)
                        (string= local-source-name source-name))
                (abaplib--retrieve-source source-name
                                          full-source-uri
                                          etag
                                          file-path))))
          sources)
    object-path))

(defun abaplib--format-etag-timestamp (etag-string)
  (let ((year (substring etag-string 0 4))
        (month (substring etag-string 4 6))
        (day (substring etag-string 6 8))
        (hour (substring etag-string 8 10))
        (minute (substring etag-string 10 12))
        (second (substring etag-string 12 14))
        )
    (format "%s %s %s %s:%s:%s"
            (cdr (assoc-string month abaplib--etag-parse-months))
            day year hour minute second)))

(defun abaplib-do-compare-wserver (object-info)
  "Check whether local version of ABAP development object is up to date with server."
  (let* ((object-path (cdr (assoc 'path object-info)))
         (source-name (cdr (assoc 'file object-info)))
         (type        (cdr (assoc 'type object-info)))
         (uri         (cdr (assoc 'uri  object-info)))
         (property-file (expand-file-name abaplib--property-file object-path))
         (metadata-local (json-read-file property-file))
         (sources-local (assoc 'sources metadata-local))
         (source-property (assoc-string source-name sources-local))
         (etag-local (cdr (assoc 'etag source-property)))
         (major-type (substring type 0 4))
         (impl-func (intern (concat "abaplib-" (downcase major-type) "-get-changedby")))
         (last-change (funcall impl-func uri source-name))
         (etag-server (car last-change))
         ;; as timestamps we use here a substring of etag
         (timestamp-local (substring etag-local 0 14))
         (timestamp-server (substring etag-server 0 14))
         (last-author (cadr last-change)))
    (if (abaplib-util-etag-equal etag-server etag-local)
        (message "Local source up to date.")
      (message (format "Timestamps differ - Server: %s, last change by %s Local: %s."
                       (abaplib--format-etag-timestamp timestamp-server)
                       last-author
                       (abaplib--format-etag-timestamp timestamp-local))))
    ))

(defun abaplib-do-retrieve-metadata (name type uri)
  "Retrieve metadata of ABAP development object."
  (let* ((object-path (abaplib-get-path type name uri))
         (property-file (expand-file-name abaplib--property-file object-path)))
    (abaplib--retrieve-metadata uri type property-file)))

(defun abaplib--retrieve-metadata (uri type &optional file-name)
  (let* ((major-type (substring type 0 4))
         (metadata-raw (abaplib--rest-api-call uri
                                               nil
                                               nil
                                               :parser 'abaplib-util-xml-parser))
         (impl-func (intern (concat "abaplib-"
                                    (downcase major-type)
                                    "-metadata-parser")))
         (properties (apply impl-func (list metadata-raw))))
    (push (cons 'uri uri) properties)
    (setq abaplib--abap-object-properties properties)
    (when file-name
      (abaplib-util-jsonize-to-file properties file-name))
    properties))

(defun abaplib--retrieve-source (name uri etag file-path)
  (abaplib--rest-api-call
   uri
   (lambda (&rest rest)
     (let ((response-data (cl-getf rest :data))
           (status-code (request-response-status-code (cl-getf rest :response))))
       (if (eq status-code 304)
           (message "Source remains unchanged in server.")
         (write-region response-data nil file-path)
         (when (get-file-buffer file-path)
           (with-current-buffer (get-file-buffer file-path)
             (revert-buffer :ignore-auto :noconfirm))
           (message "Source retrieved from server and overwrite local.")))))
   nil
   :parser 'abaplib-util-sourcecode-parser
   :headers (list `("If-None-Match" . ,etag)
                  '("Content-Type" . "plain/text"))))

;;========================================================================
;; Module - Core Services - Submit Source to Server
;;========================================================================
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
              (etag (request-response-header response "ETag")))
         (if etag
             (abaplib--metadata-post-submit etag object-info))
         (message "Submitting source to server succeeded!")))
     nil
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
    (setq abaplib--abap-object-properties metadata)
    (write-region (with-temp-buffer
                    (insert (json-encode metadata))
                    (json-pretty-print (point-min) (point-max))
                    (buffer-string))
                  nil property-file)))

;;========================================================================
;; Module - Core Services - Format Source From Server
;;========================================================================
(defun abaplib-do-format (source-code)
  "Format source code - `pretty print'."
  (let* ((format-uri "/sap/bc/adt/abapsource/prettyprinter"))
    (abaplib--rest-api-call format-uri
                            nil
                            nil
                            :type "POST"
                            :data source-code
                            :parser 'abaplib-util-sourcecode-parser)))

;;========================================================================
;; Module - Core Services - Code Completion
;;========================================================================
(defun abaplib-do-codecompletion-proposal (full-source-uri row-pos col-pos source-code)
  "Request for code completion."
  (message "Requesting proposal completion from server...")
  (let* ((request-uri "/sap/bc/adt/abapsource/codecompletion/proposal")
         (params `((uri . ,(format "%s#start=%d,%d"
                                   full-source-uri row-pos col-pos))
                   (signalCompleteness . true)))
         (completion-result (abaplib--rest-api-call request-uri
                                                    nil
                                                    nil
                                                    :type "POST"
                                                    :data source-code
                                                    :params params
                                                    :parser 'abaplib-util-xml-parser)))

    (when completion-result
      (let* ((values-node (car (xml-get-children completion-result 'values)))
             (data-node   (car (xml-get-children values-node 'DATA))))
        (xml-get-children data-node 'SCC_COMPLETION)))))

(defun abaplib-do-codecompletion-insert (full-source-uri row-pos col-pos pattern-key source-code)
  "Insert code completion to source."
  (let* ((request-uri "/sap/bc/adt/abapsource/codecompletion/insertion")
         (params `((uri . ,(format "%s#start=%d,%d"
                                   full-source-uri row-pos col-pos))
                   (patternKey . ,pattern-key))))
    (abaplib--rest-api-call request-uri
                            nil
                            nil
                            :type "POST"
                            :data source-code
                            :params params
                            :parser 'abaplib-util-sourcecode-parser)))

;;========================================================================
;; Module - Core Services - Code Navigation
;;========================================================================
(defun abaplib-get-navigation-target (full-source-uri row-pos col-pos source-code)
  "Navigate to matching statement."
  (abaplib-location-stack-push (current-buffer) row-pos col-pos)
  (message "Navigating...")
  (let* ((request-uri "/sap/bc/adt/navigation/target")
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ("x-sap-adt-sessiontype" . "stateful")
                    ("Content-Type" . "text/plain")
                    ("Accept" . "application/xml")))
         (params `((uri . ,(format "%s#start=%d,%d"
                                   full-source-uri row-pos col-pos))
                   (filter . "implementation")
                   (filter . "matchingStatement")))
         (http-status)
         (navigation-result
          (abaplib--rest-api-call request-uri
                                  nil
                                  (lambda (&rest rest)
                                    (let ((response (cl-getf rest :response)))
                                      (setq http-status (request-response-status-code response))))
                                  :sync t
                                  :parser 'abaplib-util-xml-parser
                                  :type "POST"
                                  :headers headers
                                  :data source-code
                                  :params params)))
    (case http-status
      (200 (xml-get-attribute navigation-result 'uri))
      (422 (abaplib-select-navigation-target full-source-uri row-pos col-pos source-code)))))

(defun abaplib-select-navigation-target (full-source-uri row-pos col-pos source-code)
  "Create select option if navigation target is undecidable."
  (let* ((completing-list)
         (index 1)
         (type-hierarchy    (abaplib-get-typehierarchy full-source-uri row-pos col-pos source-code))
         (origin-node  (car (xml-get-children type-hierarchy 'origin)))
         (origin-method     (xml-get-attribute origin-node 'methodName))
         (origin-type       (xml-get-attribute origin-node 'typeName))
         (entries-node (car (xml-get-children type-hierarchy 'entries)))
         (entries           (xml-get-children entries-node 'entry)))
    (dolist (entry entries)
      (let* ((entry-name (xml-get-attribute entry 'name))
             (entry-type (xml-get-attribute entry 'type)))
        (if (xml-get-attribute-or-nil entry 'parentUri)
            ;; process subtypes, indent with depth 2 after index when formatting
            (setq completing-list
                  (append completing-list
                          `((,(format "%-3s   %-30s %s" index entry-name entry-type)))))
          ;; process super-types, no indent after index
          (setq completing-list
                (append completing-list
                        `((,(format "%-3s %-30s %s" index entry-name entry-type))))))
        (setq index (1+ index))))
    (let* ((select-prompt   (format "Matches for %s->%s: " origin-type origin-method))
           (selected-item   (completing-read select-prompt completing-list))
           (selected-index  (string-to-number (car (split-string selected-item " " t))))
           (selected-target (nth (- selected-index 1) entries)))
      (xml-get-attribute selected-target 'uri))))

(defun abaplib-get-typehierarchy (full-source-uri row-pos col-pos source-code)
  (let* ((request-uri "/sap/bc/adt/abapsource/typehierarchy")
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ("Content-Type" . "text/plain")
                    ("Accept" . "application/xml;q=0.9, application/vnd.sap.adt.typehierachy.result.v1+xml")))
         (params `((uri . ,(format "%s#start=%d,%d"
                                   full-source-uri row-pos col-pos))
                   (type . "subTypes"))))
    (abaplib--rest-api-call request-uri
                            nil
                            nil
                            :parser 'abaplib-util-xml-parser
                            :type "POST"
                            :headers headers
                            :data source-code
                            :params params)))

(defun abaplib-get-target-source-uri (navigation-uri)
  "Get the target source uri from the navigation uri.
A navigation uri usually ends with a pattern like '#start=# or '#name='. Then the target source uri is everything before this pattern.
Otherwise take the navigation uri as target source uri."
  (cond ((string-match "#start=\\([0-9]+,[0-9]+\\)" navigation-uri)
         (car (split-string navigation-uri "#start=\\([0-9]+,[0-9]+\\)")))
        ((string-match "#name=\\([A-Za-z0-9_-]+\\)" navigation-uri)
         (car (split-string navigation-uri "#name=\\([A-Za-z0-9_-]+\\)")))
        (t navigation-uri)))

(defun abaplib-get-object-info (full-source-uri)
  "Get object info of ABAP development object from `full-source-uri'."
  (let* ((split-on-source (-split-when (lambda (elem) (string= elem "source"))
                                       (split-string full-source-uri "/")))
         (object-uri (mapconcat 'directory-file-name (car split-on-source) "/")) ;; everything before /source in uri
         (object-filename-base (if (cadr split-on-source) (caadr split-on-source) "main")) ;; gives main or implementations etc.
         (object-info (abaplib--rest-api-call object-uri nil nil :parser 'abaplib-util-xml-parser))
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
  (let* ((target-source-uri (abaplib-get-target-source-uri target-navi-uri))
         (target-object-info (abaplib-get-object-info target-source-uri))
         (object-path     (cdr (assoc 'path target-object-info)))
         (obj-fname-base  (cdr (assoc 'filename-base target-object-info)))
         (object-filename (file-name-completion obj-fname-base object-path))
         (pattern1 (progn
                     (string-match "#start=\\([0-9]+,[0-9]+\\)" target-navi-uri)
                     (match-string 1 target-navi-uri)))
         (pattern2 (progn
                     (string-match "#name=\\([A-Za-z0-9_-]+\\)" target-navi-uri)
                     (match-string 1 target-navi-uri)))
         (object-filepath))
    (unless object-filename
      (message "Fetching source from server...")
      (abaplib-do-retrieve (cdr (assoc 'name target-object-info))
                           (cdr (assoc 'type target-object-info))
                           (cdr (assoc 'uri  target-object-info)))
      (setq abaplib--abap-object-properties nil)
      (while (not object-filename)
        (sit-for 1)
        (setq object-filename (file-name-completion obj-fname-base object-path))))
    (setq object-filepath (concat object-path "/" object-filename))
    (if other-window
        (switch-to-buffer (find-file-other-window object-filepath))
      (switch-to-buffer (current-buffer)))
    (cond (pattern1
           (let ((target-source-pos (split-string pattern1 ",")))
             (goto-line (string-to-number (car target-source-pos)))
             (move-to-column (string-to-number (cadr target-source-pos)))))
          (pattern2
           (let ((search-name pattern2))
             (goto-char 1)
             (search-forward search-name)
             (skip-chars-backward "A-Za-z0-9_-")))
          (t (goto-char 1)))
    (abaplib-location-stack-push (current-buffer) (line-number-at-pos) (current-column))))

;;========================================================================
;; Module - Core Services - Execute
;;========================================================================
(defun abaplib-do-execute (full-source-uri object-name)
  "Execute ABAP development object via WTS."
  (let ((url (abaplib-get-project-api-url "/sap/bc/gui/sap/its/webgui")))
    (cond ((string-match "/adt/programs/programs/" full-source-uri)
           (setq url (concat url (format "?~TRANSACTION=*se38%%20RS38M-PROGRAMM=%s;DYNP_OKCODE=STRT" object-name))))
          ((string-match "/adt/oo/classes/" full-source-uri)
           (setq url (concat url (format "?~TRANSACTION=*se24%%20SEOCLASS-CLSNAME=%s;DYNP_OKCODE=WB_EXEC" object-name))))
          )
    (browse-url url)))

(defun abaplib-console-run (object-name object-type)
  (cond ((string= object-type "PROG/P")
         (abaplib-console-run-program object-name))
        ((string= object-type "CLAS/OC")
         (abaplib-console-run-class object-name))
        (t (error (format "Cannot launch application %s in console." object-name)))))

(defun abaplib-console-run-program (object-name)
  (let* ((request-uri (concat "/sap/bc/adt/programs/programrun/" object-name))
         (headers    `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                       ("Accept"       . "text/plain")))
         (run-result  (abaplib--rest-api-call request-uri
                                              nil
                                              nil
                                              :parser 'abaplib-util-sourcecode-parser
                                              :type "POST"
                                              :headers headers)))
    (abaplib-util-console-buf-write run-result)
    (pop-to-buffer (get-buffer-create abaplib--console-buffer))))

(defun abaplib-console-run-class (object-name)
  (let* ((request-uri (concat "/sap/bc/adt/oo/classrun/" object-name))
         (headers    `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                       ("Accept"       . "text/plain")))
         (run-result  (abaplib--rest-api-call request-uri
                                              nil
                                              nil
                                              :parser 'abaplib-util-sourcecode-parser
                                              :type "POST"
                                              :headers headers)))
    (abaplib-util-console-buf-write run-result)
    (pop-to-buffer (get-buffer-create abaplib--console-buffer))))

;;========================================================================
;; Module - Core Services - Where-Used
;;========================================================================
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
                                             nil
                                             :parser 'abaplib-util-xml-parser
                                             :type "POST"
                                             :headers headers
                                             :data post-data
                                             :params params)))
    (abaplib--where-used-post where-used)))

(defun abaplib--where-used-post (where-used)
  (let* ((objects-node (car (xml-get-children where-used 'referencedObjects)))
         (ref-objects (xml-get-children objects-node 'referencedObject))
         (main-items  (-filter (lambda (elem)
                                 (dolist (adt-object (xml-get-children elem 'adtObject))
                                   (when (xml-get-attribute-or-nil adt-object 'description)
                                     (return t))))
                               ref-objects))
         (objects-wId (-filter (lambda (elem) (xml-get-children elem 'objectIdentifier)) ref-objects))
         (snippets (abaplib--get-usage-snippets objects-wId))
         (output-log (format "%s\n\n" (xml-get-attribute where-used 'resultDescription))))
    (dolist (elem main-items)
      (cl-assert (= (length (xml-get-children elem 'adtObject)) 1))
      (let* ((adt-object (car (xml-get-children elem 'adtObject)))
             (object-uri (xml-get-attribute elem 'uri))
             (sub-elems (-filter (lambda (elem)
                                   (string= (xml-get-attribute elem 'parentUri) object-uri))
                                 objects-wId)))
        ;; print main item
        (setq output-log (concat output-log "\n"
                                 (format "%s %s"
                                         (xml-get-attribute adt-object 'description)
                                         (xml-get-attribute adt-object 'name))))
        (when sub-elems
          (dolist (sub-elem sub-elems)
            (let* ((sub-adt-object (car (xml-get-children sub-elem 'adtObject)))
                   (object-Id (car (xml-get-children sub-elem 'objectIdentifier)))
                   (snippet (-first (lambda (elem)
                                      (string= (nth 2 object-Id)
                                               (nth 2 (car (xml-get-children elem 'objectIdentifier)))))
                                    snippets))
                   (code-snippets-node (car (xml-get-children snippet 'codeSnippets)))
                   (code-snippets (xml-get-children code-snippets-node 'codeSnippet)))
              ;; print subitems
              (setq output-log (concat output-log "\n"
                                       (format "  %s" (xml-get-attribute sub-adt-object 'name))))
              ;; TODO Marian: rework processing and printing of results
              ;; (dolist (code-snippet code-snippets)
              ;;   ;; print code of where-used result
              ;;   (setq output-log (concat output-log "\n"
              ;;                            (format "    %s" (abaplib--print-where-used elem code-snippet)))))
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
                                       (format "  %s" (abaplib--print-where-used elem code-snippet)))))))
        ))
    (abaplib-util-where-used-buf-write output-log)
    (pop-to-buffer (get-buffer-create abaplib--where-used-buffer))))

(defun abaplib--get-usage-snippets (objects-wId)
  (let* ((request-uri "/sap/bc/adt/repository/informationsystem/usageSnippets")
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ("x-sap-adt-sessiontype" . "stateful")
                    ("Content-Type" . "application/vnd.sap.adt.repository.usagesnippets.request.v1+xml")
                    ("Accept"       . "application/vnd.sap.adt.repository.usagesnippets.result.v1+xml")))
         (post-data (abaplib--get-usage-snippets-template objects-wId))
         (snippets (abaplib--rest-api-call request-uri
                                           nil
                                           nil
                                           :sync t
                                           :parser 'abaplib-util-xml-parser
                                           :type "POST"
                                           :headers headers
                                           :data post-data)))
    (cl-assert (= (length  (xml-get-children snippets 'codeSnippetObjects)) 1))
    (xml-get-children (car (xml-get-children snippets 'codeSnippetObjects)) 'codeSnippetObject)))

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
    (setq post-body (concat post-body
                            "</usagereferences:objectIdentifiers>"
                            "<usagereferences:affectedObjects/>"
                            "</usagereferences:usageSnippetRequest>"))
    post-body))

(defun abaplib--print-where-used (item code-snippet)
  (cl-assert (= (length (xml-get-children code-snippet 'content)) 1))
  (let* ((target-uri  (xml-get-attribute code-snippet 'uri))
         (source-uri  (abaplib-get-target-source-uri target-uri))
         (source-pos  (split-string (progn
                                      (string-match "#start=\\([0-9]+,[0-9]+\\)" target-uri)
                                      (match-string 1 target-uri)) "," ))
         (object-uri  (xml-get-attribute item 'uri))
         (adt-object  (car (xml-get-children item 'adtObject)))
         (object-type (xml-get-attribute adt-object 'type))
         (object-name (xml-get-attribute adt-object 'name))
         (object-path (abaplib-get-path object-type object-name object-uri))
         (obj-fname-base    (abaplib-util-get-filename-base source-uri))
         (content           (car (xml-get-children code-snippet 'content)))
         (map (make-sparse-keymap))
         (fn-follow-pos `(lambda ()
                          (interactive)
                          (let* ((path        ,object-path)
                                 (fname-base  ,obj-fname-base)
                                 (filename    (file-name-completion fname-base path))
                                 (line   ,(string-to-number (car source-pos)))
                                 (column ,(string-to-number (cadr source-pos)))
                                 (filepath))
                            (unless filename
                              (message "Fetching source from server...")
                              (abaplib-do-retrieve ,object-name ,object-type ,object-uri)
                              (while (not filename)
                                (sit-for 1)
                                (setq filename (file-name-completion fname-base path))))
                            (setq filepath (concat path "/" filename))
                            (switch-to-buffer (find-file-other-window filepath))
                            (abaplib-util-goto-position line column))))
         )
    (define-key map (kbd "<down-mouse-1>") fn-follow-pos)
    (define-key map (kbd "<RET>") fn-follow-pos)
    (propertize (string-trim-left (nth 2 content))
                'face 'underline
                'mouse-face 'highlight
                'keymap map)))

;;========================================================================
;; Module - Core Services - Outline
;;========================================================================
(defun abaplib-outline (object-uri object-version)
  (message "Getting outline...")
  (let* ((request-uri (concat object-uri "/objectstructure"))
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ("Accept"       . "application/xml;q=0.8, application/vnd.sap.adt.objectstructure+xml;q=0.9, application/vnd.sap.adt.objectstructure.v2+xml")))
         (params `((version . ,object-version)
                   (withShortDescriptions . "true")))
         (outline (abaplib--rest-api-call request-uri
                                          nil
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
         (object-structure (abaplib--outline-post-filter object-structure))
         (main-link (-first (lambda (elem)
                              (cl-search "definitionIdentifier" (xml-get-attribute elem 'rel)))
                            main-links))
         (fname-base (abaplib--outline-get-filename-base main-link))
         (source-filename (file-name-completion fname-base object-path))
         (target-buffer (find-file-noselect source-filename))
         (search-patterns (list object-name))
         (source-pos (abaplib--outline-get-source-pos search-patterns main-link target-buffer))
         (output-log (format "Outline for %s\n\n" object-name)))
    (setq output-log (concat output-log
                             (format "%s" (abaplib--outline-print-item source-pos target-buffer))))
    (dolist (elem object-structure)
      (let* ((elem-name (xml-get-attribute elem 'name))
             (sub-obj-structure (xml-get-children elem 'objectStructureElement))
             (links (-filter (lambda (link)
                               (or (cl-search "definitionIdentifier" (xml-get-attribute link 'rel))
                                   (cl-search "implementationIdentifier" (xml-get-attribute link 'rel))))
                             (xml-get-children elem 'link)))
             (link (if sub-obj-structure (car links) (-last-item links)))
             (fname-base (abaplib--outline-get-filename-base link))
             (source-filename (file-name-completion fname-base object-path))
             (target-buffer (find-file-noselect source-filename))
             (search-patterns (list elem-name))
             (source-pos (abaplib--outline-get-source-pos search-patterns link target-buffer)))
        (setq output-log (concat output-log
                                 (format "  %s" (abaplib--outline-print-item source-pos target-buffer))))
        (dolist (sub-elem sub-obj-structure)
          (let* ((sub-elem-name (xml-get-attribute sub-elem 'name))
                 (sub-links (xml-get-children sub-elem 'link))
                 (sub-link (-last (lambda (sub-link)
                                    (or (cl-search "definitionIdentifier" (xml-get-attribute sub-link 'rel))
                                        (cl-search "implementationIdentifier" (xml-get-attribute sub-link 'rel))))
                                  sub-links))
                 (fname-base (abaplib--outline-get-filename-base sub-link))
                 (source-filename (file-name-completion fname-base object-path))
                 (target-buffer (find-file-noselect source-filename))
                 (search-patterns (list elem-name sub-elem-name))
                 (source-pos (abaplib--outline-get-source-pos search-patterns sub-link target-buffer)))
            (setq output-log (concat output-log
                                     (format "    %s" (abaplib--outline-print-item source-pos target-buffer))))

        ))))
    (abaplib-util-outline-buf-write output-log)
    (pop-to-buffer (get-buffer-create abaplib--outline-buffer))))

(defun abaplib--outline-post-filter (object-structure)
  (let ((text-elem-types (list "CLAS/OCX" "PROG/PX" "PROG/PS")))
    (-remove (lambda (elem)
               (--any? (string= (xml-get-attribute-or-nil elem 'type) it) text-elem-types))
             object-structure)))

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

(defun abaplib--outline-get-source-pos (search-patterns link target-buffer)
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
           (let* ((type (progn
                          (string-match "#type=\\([A-Z]+/[A-Z]+\\)" navi-uri)
                          (match-string 1 navi-uri)))
                  (type-list  (split-string type "/"))
                  (major-type (car type-list))
                  (minor-type (cadr type-list))
                  (def-identifier (cl-search "definitionIdentifier" (xml-get-attribute link 'rel)))
                  (impl-func  (intern (concat "abaplib--outline-search-" (downcase minor-type)))))
             (funcall impl-func search-patterns target-buffer def-identifier)))
          ;; treatment for main link representing dev object
          ((string= navi-uri (abaplib-get-property 'uri))
           (let* ((type (abaplib-get-property 'type))
                  (type-list  (split-string type "/"))
                  (major-type (car type-list))
                  (minor-type (cadr type-list))
                  (def-identifier (cl-search "definitionIdentifier" (xml-get-attribute link 'rel)))
                  (impl-func  (intern (concat "abaplib--outline-search-" (downcase minor-type)))))
             (funcall impl-func search-patterns target-buffer def-identifier)))
          (t (error (format "Cannot determine line and column number from \"%s\"." navi-uri))))))

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

(defun abaplib--outline-search-p (pattern target-buffer def-identifier)
  "Search for beginning of program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "\\(REPORT\\|PROGRAM\\)" "\s+" (car pattern) "."))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pu (pattern target-buffer def-identifier)
  "Search for form routine in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "FORM" "\s+" (car pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pn (pattern target-buffer def-identifier)
  "Search for local interface in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "INTERFACE" "\s+" (car pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pnm (pattern target-buffer def-identifier)
  "Search for local interface method in program specified by `pattern'."
  (cl-assert (>= (length pattern) 2))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "INTERFACE" "\s+" (car pattern)))
    (re-search-forward (concat "METHODS" "\s+" (cadr pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pl (pattern target-buffer def-identifier)
  "Search for local class in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "CLASS" "\s+" (car pattern) "\s+" "DEFINITION"))
    (backward-word)
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-plm (pattern target-buffer def-identifier)
  "Search for local class method in program specified by `pattern'."
  (cl-assert (>= (length pattern) 2))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (if def-identifier
        (progn
          (re-search-forward (concat "CLASS" "\s+" (car pattern) "\s+" "DEFINITION"))
          (re-search-forward "METHODS")
          (re-search-forward (cadr pattern)))
      (re-search-forward (concat "CLASS" "\s+" (car pattern) "\s+" "IMPLEMENTATION"))
      (re-search-forward (concat "METHOD" "\s+" (cadr pattern))))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pla (pattern target-buffer def-identifier)
  "Search for local class attribute in program specified by `pattern'."
  (cl-assert (>= (length pattern) 2))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "CLASS" "\s+" (car pattern) "\s+" "DEFINITION"))
    (re-search-forward (concat "DATA" "\s+" (cadr pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-ply (pattern target-buffer def-identifier)
  "Search for local class type (typedef) in program specified by `pattern'."
  (cl-assert (>= (length pattern) 2))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "CLASS" "\s+" (car pattern) "\s+" "DEFINITION"))
    (re-search-forward "TYPES")
    (re-search-forward (cadr pattern))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-py (pattern target-buffer def-identifier)
  "Search for global type in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (car pattern))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pd (pattern target-buffer def-identifier)
  "Search for global variable in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "DATA(" (car pattern) ")\\|DATA" "\s+" (car pattern) "\\|"
                               "PARAMETERS" "\s+" (car pattern) "\\|"
                               "BEGIN\s+OF\s+BLOCK" "\s+" (car pattern)))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

(defun abaplib--outline-search-pe (pattern target-buffer def-identifier)
  "Search for events in program specified by `pattern'."
  (cl-assert (>= (length pattern) 1))
  (set-buffer target-buffer)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (car pattern))
    (backward-word)
    (list (line-number-at-pos) (current-column))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module - Object Type Specific - ABAP Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abaplib-clas-metadata-parser (metadata)
  (let* ((adtcore-type (xml-get-attribute metadata 'type))
         (name (xml-get-attribute metadata 'name))
         (description (xml-get-attribute metadata 'description))
         (version (xml-get-attribute metadata 'version))
         (package-node (car (xml-get-children metadata 'packageRef)))
         (package (xml-get-attribute package-node 'name))
         (includes-node (xml-get-children metadata 'include))
         (includes (mapcar
                    (lambda (include)
                      (let* ((version (xml-get-attribute include 'version))
                             (source-uri (xml-get-attribute include 'sourceUri))
                             (include-type (xml-get-attribute include 'includeType))
                             (type (xml-get-attribute include 'type))
                             (links (xml-get-children include 'link))
                             (file-name (concat include-type ".clas.abap"))
                             (etag))
                        (dolist (link links)
                          (when (string= (xml-get-attribute link 'type) "text/plain")
                            (setq etag (xml-get-attribute link 'etag))
                            (return)))
                        (cons file-name `((version . ,version)
                                          (source-uri . ,source-uri)
                                          (include-type . ,include-type)
                                          (type . ,type)
                                          (etag . ,etag)))))
                    includes-node)))
    `((name . ,name)
      (description . ,description)
      (type . ,adtcore-type)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

(defun abaplib-clas-get-changedby (uri source-name)
  (let* ((metadata-server (abaplib--rest-api-call uri
                                                  nil
                                                  nil
                                                  :parser 'abaplib-util-xml-parser))
         (includes (xml-get-children metadata-server 'include))
         (include (-first (lambda (include) (cl-search (xml-get-attribute include 'includeType) source-name)) includes))
         (last-author (xml-get-attribute include 'changedBy))
         (links (xml-get-children include 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         )
    (list (xml-get-attribute link 'etag) last-author)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module - Object Type Specific - ABAP Program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abaplib-prog-metadata-parser (metadata)
  (let* ((adtcore-type (xml-get-attribute metadata 'type))
         (name (xml-get-attribute metadata 'name))
         (description (xml-get-attribute metadata 'description))
         (version (xml-get-attribute metadata 'version))
         (package-node (car (xml-get-children metadata 'packageRef)))
         (package (xml-get-attribute package-node 'name))
         (links (xml-get-children metadata 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         (file-name "main.prog.abap")
         (includes (list (cons file-name `((version . ,version)
                                           (source-uri . ,(xml-get-attribute metadata 'sourceUri))
                                           (include-type . main)
                                           (type . ,adtcore-type)
                                           (etag . ,(xml-get-attribute link 'etag))
                                           )))))
    `((name . ,name)
      (description . ,description)
      (type . ,adtcore-type)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

(defun abaplib-prog-get-changedby (uri source-name)
  (let* ((metadata-server (abaplib--rest-api-call uri
                                                  nil
                                                  nil
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         )
    (list (xml-get-attribute link 'etag) (xml-get-attribute metadata-server 'changedBy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module - Object Type Specific - Function Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abaplib-fugr-metadata-parser (metadata)
  (let* ((adtcore-type (xml-get-attribute metadata 'type))
         (name (xml-get-attribute metadata 'name))
         (description (xml-get-attribute metadata 'description))
         (version (xml-get-attribute metadata 'version))
         (container-node (car (xml-get-children metadata 'containerRef)))
         (package (xml-get-attribute container-node 'packageName))
         (links (xml-get-children metadata 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         (file-name "main.func.abap")
         (includes (list (cons file-name `((version . ,version)
                                           (source-uri . ,(xml-get-attribute metadata 'sourceUri))
                                           (include-type . main)
                                           (type . ,adtcore-type)
                                           (etag . ,(xml-get-attribute link 'etag))
                                           )))))
    `((name  . ,name)
      (description . ,description)
      (type . ,adtcore-type)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

(defun abaplib-fugr-get-changedby (uri source-name)
  (let* ((metadata-server (abaplib--rest-api-call uri
                                                  nil
                                                  nil
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         )
    (list (xml-get-attribute link 'etag) (xml-get-attribute metadata-server 'changedBy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module - Object Type Specific - ABAP Interfaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abaplib-intf-metadata-parser (metadata)
  (let* ((adtcore-type (xml-get-attribute metadata 'type))
         (name (xml-get-attribute metadata 'name))
         (description (xml-get-attribute metadata 'description))
         (version (xml-get-attribute metadata 'version))
         (package-node (car (xml-get-children metadata 'packageRef)))
         (package (xml-get-attribute package-node 'name))
         (links (xml-get-children metadata 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         (file-name "main.intf.abap")
         (includes (list (cons file-name `((version . ,version)
                                           (source-uri . ,(xml-get-attribute metadata 'sourceUri))
                                           (include-type . main)
                                           (type . ,adtcore-type)
                                           (etag . ,(xml-get-attribute link 'etag))
                                           )))))
    `((name . ,name)
      (description . ,description)
      (type . ,adtcore-type)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

(defun abaplib-intf-get-changedby (uri source-name)
  (let* ((metadata-server (abaplib--rest-api-call uri
                                                  nil
                                                  nil
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         )
    (list (xml-get-attribute link 'etag) (xml-get-attribute metadata-server 'changedBy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module - Object Type Specific - Database Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abaplib-tabl-metadata-parser (metadata)
  (let* ((adtcore-type (xml-get-attribute metadata 'type))
         (source-uril (last (split-string (xml-get-attribute metadata 'sourceUri) "/") 2)) ;; source-uri as a list
         (source-uri (concat (car source-uril) "/" (cadr source-uril)))
         (name (xml-get-attribute metadata 'name))
         (description (xml-get-attribute metadata 'description))
         (version (xml-get-attribute metadata 'version))
         (package-node (car (xml-get-children metadata 'packageRef)))
         (package (xml-get-attribute package-node 'name))
         (links (xml-get-children metadata 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         (file-name "main.ddic.tabl")
         (includes (list (cons file-name `((version . ,version)
                                           (source-uri . ,source-uri)
                                           (include-type . main)
                                           (type . ,adtcore-type)
                                           (etag . ,(xml-get-attribute link 'etag))
                                           )))))
    `((name . ,name)
      (description . ,description)
      (type . ,adtcore-type)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

(defun abaplib-tabl-get-changedby (uri source-name)
  (let* ((metadata-server (abaplib--rest-api-call uri
                                                  nil
                                                  nil
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         )
    (list (xml-get-attribute link 'etag) (xml-get-attribute metadata-server 'changedBy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module - Object Type Specific - Data Definition Language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abaplib-ddls-metadata-parser (metadata)
  (let* ((adtcore-type (xml-get-attribute metadata 'type))
         (name (xml-get-attribute metadata 'name))
         (description (xml-get-attribute metadata 'description))
         (version (xml-get-attribute metadata 'version))
         (package-node (car (xml-get-children metadata 'packageRef)))
         (package (xml-get-attribute package-node 'name))
         (links (xml-get-children metadata 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/html")) links))
         (file-name "main.ddls.cds")
         (includes (list (cons file-name `((version . ,version)
                                           (source-uri . ,(xml-get-attribute metadata 'sourceUri))
                                           (include-type . main)
                                           (type . ,adtcore-type)
                                           (etag . ,(xml-get-attribute link 'etag))
                                           )))))
    `((name . ,name)
      (description . ,description)
      (type . ,adtcore-type)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

(defun abaplib-ddls-get-changedby (uri source-name)
  (let* ((metadata-server (abaplib--rest-api-call uri
                                                  nil
                                                  nil
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/html")) links))
         )
    (list (xml-get-attribute link 'etag) (xml-get-attribute metadata-server 'changedBy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module - Object Type Specific - Business Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abaplib-bdef-metadata-parser (metadata)
  (let* ((adtcore-type (xml-get-attribute metadata 'type))
         (source-uril (last (split-string (xml-get-attribute metadata 'sourceUri) "/") 2)) ;; source-uri as a list
         (source-uri (concat (car source-uril) "/" (cadr source-uril)))
         (name (xml-get-attribute metadata 'name))
         (description (xml-get-attribute metadata 'description))
         (version (xml-get-attribute metadata 'version))
         (package-node (car (xml-get-children metadata 'packageRef)))
         (package (xml-get-attribute package-node 'name))
         (links (xml-get-children metadata 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         (file-name "main.bdef.cds")
         (includes (list (cons file-name `((version . ,version)
                                           (source-uri . ,source-uri)
                                           (include-type . main)
                                           (type . ,adtcore-type)
                                           (etag . ,(xml-get-attribute link 'etag))
                                           )))))
    `((name . ,name)
      (description . ,description)
      (type . ,adtcore-type)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

(defun abaplib-bdef-get-changedby (uri source-name)
  (let* ((metadata-server (abaplib--rest-api-call uri
                                                  nil
                                                  nil
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (-first (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links))
         )
    (list (xml-get-attribute link 'etag) (xml-get-attribute metadata-server 'changedBy))))

(provide 'abaplib)
;;; abaplib.el ends here
