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

(defconst abaplib--uri-login "/sap/bc/adt/core/discovery")

(defconst abaplib--property-file ".properties.json")

(defvar etag-parse-months '(("01" . "January") ("02" . "February") ("03" . "March")
                            ("04" . "April") ("05" . "May") ("06" . "June")
                            ("07" . "July") ("08" . "August") ("09" . "September")
                            ("10" . "October") ("11" . "November") ("12" . "December")))


;;==============================================================================
;; Module - Tools & Utilities
;;==============================================================================
(defun abaplib-util-current-dir ()
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun abaplib-util-xml-parser()
  (libxml-parse-xml-region (point-min) (point-max)))

(defun abaplib-util-sourcecode-parser()
  (progn
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match ""))
    (buffer-string)))

(defun abaplib-util-log-buf-write(log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--log-buffer))
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert "\n\n")
    (insert (concat "Log at: "
                    (format-time-string "%Y-%m-%dT%T")
                    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
                     (format-time-string "%z"))))
    ;; (insert "\n")
    (insert (format "%s" log))
    (setq buffer-read-only t)
    ))

(defun abaplib-util-log-buf-pop()
  (pop-to-buffer (get-buffer-create abaplib--log-buffer)))

(defun abaplib-util-jsonize-to-file(list file)
  (write-region (with-temp-buffer
                  (insert (json-encode list))
                  (json-pretty-print (point-min) (point-max))
                  (buffer-string)) nil file))

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

(defun abaplib--get-local-properties ()
  "Load property file on current directory for current buffer."
  (let ((property-file (expand-file-name abaplib--property-file)))
    (unless (file-exists-p property-file)
      (error "Missing property file, please use `search' to retrieve again!"))
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

(defun abaplib-is-logged()
  (let ((now (time-to-seconds (current-time))))
    (and abaplib--login-last-time
         (<= (- now abaplib--login-last-time) abap-login-timeout))))

(defun abaplib-buffer-whole-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun abaplib--rest-api-call(uri success-callback &rest args)
  "Call service API."
  (let* ((url (abaplib-get-project-api-url uri))
         (login-token (cons "Authorization" (abaplib-get-login-token)))
         (headers (cl-getf args :headers))
         (type    (or (cl-getf args :type) "GET"))
         (params (cl-getf args :params)))

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
                    :sync (not success-callback)
                    :headers headers
                    :status-code '(
                                   ;; (304 . (lambda (&rest _) (message "304 Source Not Modified")))
                                   (401 . (lambda (&rest _) (message "Got 401: Not Authorized")))
                                   (403 . (lambda (&rest _) (message "Got 403: Forbidden"))))
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
                    ;; :complete (lambda (&rest -) (message "Complete" ))
                    args)))))


;;==============================================================================
;; Module - Project
;;==============================================================================
(defun abaplib-get-ws-describe-file()
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

(defun abaplib-get-workspace-descriptor()
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

(defun abaplib-upsert-project(project-props)
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

(defun abaplib-auth-login-with-token(project login-token sap-client &optional save?)
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
                                       :params params
                                       :parser 'abaplib-util-xml-parser))
         (object-list (xml-get-children data 'objectReference)))
    object-list))


;;==============================================================================
;; Module - Core Services - Syntax Check
;;==============================================================================
(defun abaplib-do-check(version uri source-uri source-code dont-show-error?)
  "Check syntax for program source.
  TODO check whether source changed since last retrieved from server.
       Not necessary to send the source code to server if no change."
  (message "Sending syntax check request...")
  (let ((chkrun-uri (concat uri "/" source-uri))
        (chkrun-content (base64-encode-string source-code)))
    (if dont-show-error?
        (abaplib--check-post-sync version uri chkrun-uri chkrun-content)
      (abaplib--check-post-async version uri chkrun-uri chkrun-content))))

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
                     (pos-column (string-to-number (car (cdr position)))))
                `((line . ,pos-line)
                  (column . ,pos-column)
                  (level . ,level)
                  (message . ,text))
                )) messages)))

(defun abaplib--check-render-type-text(type)
  (cond ((string= type "E") (propertize "Error"       'face '(bold (:foreground "red"))))
        ((string= type "W") (propertize "Warning"     'face '(bold (:foreground "orange"))))
        ((string= type "I") (propertize "Information" 'face '(bold (:foreground "green"))))
        (t "Other")))

(defun abaplib--check-render-pos(position &optional target-buffer)
  (let* ((target-buffer (or target-buffer (current-buffer)))
         (pos-list (split-string position ","))
         (line (string-to-number (car pos-list)))
         (column (string-to-number (car (cdr pos-list))))
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
(defun abaplib--lock-sync(uri csrf-token)
  (message "Trying to lock object...")
  (let* ((root-node (abaplib--rest-api-call
                     uri
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

(defun abaplib--activate-post(adtcore-name adtcore-uri)
  (let* ((preaudit-result (abaplib--activate-preaudit adtcore-name adtcore-uri))
         (result-type (car preaudit-result)))
    (abaplib--activate-parse-result preaudit-result)))

(defun abaplib--activate-preaudit (adtcore-name adtcore-uri)
  (let ((activate-uri "/sap/bc/adt/activation")
        (post-body (abaplib--activate-preaudit-template adtcore-name adtcore-uri)))
    (abaplib--rest-api-call activate-uri
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
            (cdr (assoc-string month etag-parse-months))
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
         ;; as timestamps we use here a substring of etag
         (timestamp-local (substring etag-local 0 14))
         (timestamp-server (substring (car last-change) 0 14))
         (last-author (cadr last-change)))
    (if (= (string-to-number timestamp-server) (string-to-number timestamp-local))
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
              (response-string (format "%s" response))
              (etag (request-response-header response "ETag")))
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

;;========================================================================
;; Module - Core Services - Format Source From Server
;;========================================================================
(defun abaplib-do-format (source-code)
  "Format source code - `pretty print'."
  (let* ((format-uri "/sap/bc/adt/abapsource/prettyprinter"))
    (abaplib--rest-api-call format-uri
                            nil
                            :type "POST"
                            :data source-code
                            :parser 'abaplib-util-sourcecode-parser)))

;;========================================================================
;; Module - Core Services - Code Completion
;;========================================================================
(defun abaplib-do-codecompletion-proposal (full-source-uri pos_row pos_col source-code)
  "Request for code completion."
  (message "Requesting proposal completion from server...")
  (let* ((request-uri "/sap/bc/adt/abapsource/codecompletion/proposal")
         (params `((uri . ,(format "%s#start=%d,%d"
                                   full-source-uri pos_row pos_col))
                   (signalCompleteness . true)))
         (completion-result (abaplib--rest-api-call request-uri
                                                    nil
                                                    :type "POST"
                                                    :data source-code
                                                    :params params
                                                    :parser 'abaplib-util-xml-parser)))

    (when completion-result
      (let* ((values-node (car (xml-get-children completion-result 'values)))
             (data-node   (car (xml-get-children values-node 'DATA))))
        (xml-get-children data-node 'SCC_COMPLETION)))))

(defun abaplib-do-codecompletion-insert (full-source-uri pos_row pos_col pattern-key source-code)
  "Insert code completion to source."
  (let* ((request-uri "/sap/bc/adt/abapsource/codecompletion/insertion")
         (params `((uri . ,(format "%s#start=%d,%d"
                                   full-source-uri pos_row pos_col))
                   (patternKey . ,pattern-key))))
    (abaplib--rest-api-call request-uri
                            nil
                            :type "POST"
                            :data source-code
                            :params params
                            :parser 'abaplib-util-sourcecode-parser)))

;;========================================================================
;; Module - Core Services - Code Navigation
;;========================================================================
(defun abaplib-get-navigation-target (full-source-uri pos_row pos_col source-code)
  "Navigate to matching statement."
  (message "Navigating...")
  (let* ((request-uri "/sap/bc/adt/navigation/target")
         (headers `(("x-csrf-token" . ,(abaplib-get-csrf-token))
                    ("x-sap-adt-sessiontype" . "stateful")
                    ("Content-Type" . "text/plain")
                    ("Accept" . "application/xml")))
         (params `((uri . ,(format "%s#start=%d,%d"
                                   full-source-uri pos_row pos_col))
                   (filter . "implementation")
                   (filter . "matchingStatement")))
         (navigation-result (abaplib--rest-api-call request-uri
                                                    nil
                                                    :parser 'abaplib-util-xml-parser
                                                    :type "POST"
                                                    :headers headers
                                                    :data source-code
                                                    :params params)))
    (when navigation-result
      (let ((uri (xml-get-attribute navigation-result 'uri)))
        uri))
    ))

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
  (let* ((target-source-uri (abaplib-get-target-source-uri target-navi-uri))
         (target-object-info (abaplib-get-object-info target-source-uri))
         (object-path     (cdr (assoc 'path target-object-info)))
         (obj-fname-base  (cdr (assoc 'filename-base target-object-info)))
         (object-filename (file-name-completion obj-fname-base object-path))
         (object-filepath))
    (unless object-filename
      (message "Fetching source from server...")
      (abaplib-do-retrieve (cdr (assoc 'name target-object-info))
                           (cdr (assoc 'type target-object-info))
                           (cdr (assoc 'uri  target-object-info)))
      (while (not object-filename)
        (sit-for 1)
        (setq object-filename (file-name-completion obj-fname-base object-path))))
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
                                             :parser 'abaplib-util-xml-parser
                                             :type "POST"
                                             :headers headers
                                             :data post-data
                                             :params params)))
    ))

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
                                                  :parser 'abaplib-util-xml-parser))
         (includes (xml-get-children metadata-server 'include))
         (include (car (-filter (lambda (include) (cl-search (xml-get-attribute include 'includeType) source-name)) includes)))
         (last-author (xml-get-attribute include 'changedBy))
         (links (xml-get-children include 'link))
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/html")) links)))
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
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/html")) links)))
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
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
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
                                                  :parser 'abaplib-util-xml-parser))
         (links (xml-get-children metadata-server 'link))
         (link (car (-filter (lambda (link) (string= (xml-get-attribute link 'type) "text/plain")) links)))
         )
    (list (xml-get-attribute link 'etag) (xml-get-attribute metadata-server 'changedBy))))

(provide 'abaplib)
;;; abaplib.el ends here
