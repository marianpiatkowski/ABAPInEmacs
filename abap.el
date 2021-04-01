;;; abap.el --- ABAP programs            -*- lexical-binding: t; -*-

;; Copyright (C) 2020-  Marian Piatkowski
;; Copyright (C) 2018  Marvin Qian

;; Author: Marian Piatkowski <marianpiatkowski@web.de>
;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords:

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

(require 'subr-x)
(require 'abaplib)


;; (defvar-local abap--abap-object nil
;; "ABAP development Object
;; alist with name and type")
;;==============================================================================
;; Project
;;==============================================================================

;;;###autoload
(defun abap-create-project ()
  "Create new ABAP project."
  (interactive)
  (let* ((project-name (or (read-string "Project name (without blank): " )
                           (error "project name can't be empty")))
         (parent-dir (read-directory-name "Workspace directory:" abap-workspace-dir))
         (project (expand-file-name project-name parent-dir)))
    (unless (file-directory-p parent-dir)
      (make-directory parent-dir))
    (abaplib-create-project project)
    (message "Project %s created and added to workspace." project)
    (abap-open-project project)))

(defun abap-add-project ()
  "Add ABAP project into workspace."
  (interactive)
  (let* ((current-dir (abaplib-util-current-dir))
         (project(expand-file-name
                  (read-directory-name "Init project: "
                               (abaplib-project-init-propose current-dir)))))
    (abaplib-create-project project)
    (message "Project %s added to workspace." project)))

(defun abap-remove-project ()
  "Remove ABAP project from workspace.
  `Note:' This operation will not physically delete the project files."
  (interactive)
  (let ((project (completing-read "Select Project: "
                                  (abaplib-get-project-list))))
    (abaplib-remove-project project)
    (message "Project %s removed from workspace." project)))

;;;###autoload
(defun abap-open-project (&optional project)
  "Open ABAP project."
  (interactive)
  (let ((project (or project
                     (completing-read "Select Project: " (abaplib-get-project-list)))))
    (abaplib-switch-project project)
    (dired project)))

(defun abap-get-current-project ()
  "Get current project, prompt user action project if none has been chosen."
  (unless abaplib--current-project
    (call-interactively 'abap-open-project))
  abaplib--current-project)

(defun abap-add-server ()
  "Add server to current project."
  (interactive)
  (let ((project (abap-get-current-project))
        (server (read-string "Server https url: ")))
    (abaplib-add-server-to-project project server)
    (message "Server URL %s added to current project" server)))

(defun abap-login ()
  "Login to server."
  (interactive)
  (let* ((project (abap-get-current-project))
         (username (upcase (read-string "Username: ")))
         (password (read-passwd "Password: "))
         (client   (read-string "SAP Client: "  ))
         (login-token (format "Basic %s" (base64-encode-string
                                          (concat username ":" password)))))
    (message "Connecting...")
    (abaplib-auth-login-with-token project login-token client abap-save-login-credential)))

;;;###autoload
(defun abap-search-object ()
  "Search and retrieve ABAP development objects."
  (interactive)
  (let* ((project (abap-get-current-project))
         (query-string (read-string "Enter Search String: "))
         (search-result (abaplib-do-search query-string))
         ;; (completing-list (mapcar (lambda(object-node)
         ;;                            (let ((name (xml-get-attribute object-node 'name))
         ;;                                  (type (xml-get-attribute object-node 'type))
         ;;                                  (description (xml-get-attribute object-node
         ;;                                                                  'description)))
         ;;                              (format "%-8s %-30s %s" type name description)))
         ;;                          search-result))
         ;; (selected-item (split-string (completing-read "Matching Items: " completing-list)
         ;; " " t))
         ;; (item-type (car selected-item))
         ;; (item-name (nth 1 selecetd-item))
         (completing-list)
         (index 1))
    (dolist (object-node search-result)
      (let ((name (xml-get-attribute object-node 'name))
            (description (xml-get-attribute object-node 'description)))
        (setq completing-list
              (append  completing-list
                       `((,(format "%-3s %-30s %s" index name description)))))
        (setq index (+ index 1))))
    (let* ((selected-item (completing-read "Matching Items: " completing-list))
           (selected-index (string-to-number (car (split-string selected-item " " t))))
           (selected-object (nth (- selected-index 1) search-result))
           (object-node (xml-get-children selected-object 'objectReference))
           (object-path (abaplib-do-retrieve (xml-get-attribute selected-object 'name)
                                             (xml-get-attribute selected-object 'type)
                                             (xml-get-attribute selected-object 'uri))))
      (dired object-path))))

(defun abap-retrieve-source ()
  "Retrieve source of a ABAP development object."
  (interactive)
  (let ((source-name (file-name-nondirectory (buffer-file-name)))
        (object-name (abaplib-get-property 'name))
        (object-type (abaplib-get-property 'type))
        (object-uri  (abaplib-get-property 'uri)))
    ;; (abaplib-service-dispatch 'retrieve abap-object)
    (abaplib-do-retrieve object-name
                         object-type
                         object-uri
                         source-name)))

(defun abap-check-source-uptodate ()
  "Check whether local source is up to date with server."
  (interactive)
  (let* ((object-path (file-name-directory (buffer-file-name)))
         (source-name (file-name-nondirectory (buffer-file-name)))
         (object-info `((path . ,object-path)
                        (file . ,source-name)
                        (name . ,(abaplib-get-property 'name))
                        (type . ,(abaplib-get-property 'type))
                        (uri  . ,(abaplib-get-property 'uri)))))
    (abaplib-do-compare-wserver object-info)))

(defun abap-syntax-check (&optional dont-show-error?)
  "Syntax check of source."
  (interactive)
  (let* ((curr-buffer (current-buffer))
         (source-name (file-name-nondirectory (buffer-file-name)))
         (object-info `((file        . ,source-name)
                        (name        . ,(abaplib-get-property 'name))
                        (type        . ,(abaplib-get-property 'type))
                        (uri         . ,(abaplib-get-property 'uri))
                        (src-uri     . ,(abaplib-get-property 'source-uri source-name))
                        (src-version . ,(abaplib-get-property 'version source-name))))
         (source-code (abaplib-buffer-whole-string curr-buffer)))
    (abaplib-do-check object-info source-code dont-show-error?)))

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
         (source-etag (abaplib-get-etag full-source-uri))
         (package     (abaplib-get-property 'package))
         (tr-number)
         (object-info `((path . ,object-path)
                        (file . ,source-name)
                        (full-source-uri . ,full-source-uri))))
    (abaplib-check-version source-etag (abaplib-get-property 'sources) source-name)
    (unless (string= package "$TMP")
      (let* ((requests (abaplib-retrieve-trans-request full-source-uri))
             (selected-req (completing-read "Change Request: " requests)))
        (setq tr-number (string-trim (car (split-string selected-req "|"))))
        (message (abaplib-post-cm-checkrun tr-number full-source-uri))))
    (abaplib-do-submit source-code object-info tr-number)))

(defun abap-activate-source ()
  "Activate source."
  (interactive)
  (let ((object-info `((name . ,(abaplib-get-property 'name))
                       (type . ,(abaplib-get-property 'type))
                       (uri  . ,(abaplib-get-property 'uri)))))
    (abaplib-do-activate object-info)))

(defun abap-format-source ()
  "Format source - `pretty print'."
  (interactive)
  (let* ((curr-buffer (current-buffer))
         (object-name (abaplib-get-property 'name))
         (source-code (abaplib-buffer-whole-string curr-buffer))
         (formated-source (abaplib-do-format source-code)))
    (unless (or (not formated-source)
                (string= formated-source ""))
      (progn
        (set-buffer curr-buffer)
        (erase-buffer)
        (goto-char (point-min))
        (insert formated-source)))))

(defun abap-code-completion ()
  "Invoke ABAP code completion."
  (interactive)
  (let* ((curr-buffer (current-buffer))
         (source-name (file-name-nondirectory (buffer-file-name)))
         (object-uri (abaplib-get-property 'uri))
         (source-uri (abaplib-get-property 'source-uri source-name))
         (full-source-uri (concat object-uri "/" source-uri))
         (source-code (abaplib-buffer-whole-string curr-buffer))
         (completion-result (abaplib-do-codecompletion-proposal full-source-uri
                                                                (line-number-at-pos)
                                                                (current-column)
                                                                source-code)))
    (unless completion-result
      (error "No matches"))
    (let* ((prompt-list (mapcar
                         (lambda (completion)
                           `(,(intern (abaplib-util-get-xml-value completion 'IDENTIFIER))
                             . ,completion))
                         completion-result))
           (selected-key (completing-read "Matches: " prompt-list))
           (completion (alist-get (intern selected-key) prompt-list))
           (prefix-length (string-to-number (abaplib-util-get-xml-value completion
                                                                     'PREFIXLENGTH)))
           (completion-source (abaplib-do-codecompletion-insert full-source-uri
                                                                (line-number-at-pos)
                                                                (current-column)
                                                                selected-key
                                                                source-code)))
      (unless (or (not completion-source)
                  (string= completion-source ""))
        (let ((source (substring completion-source prefix-length)))
          (set-buffer curr-buffer)
          (insert source))))))

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
         (other-window (not (string= (abaplib-get-target-source-uri target-navi-uri)
                                     full-source-uri))))
    (abaplib-do-navigate target-navi-uri other-window)))

(defun abap-execute-object ()
  "Execute ABAP development object."
  (interactive)
  (let* ((source-name (file-name-nondirectory (buffer-file-name)))
         (object-uri (abaplib-get-property 'uri))
         (object-name (abaplib-get-property 'name))
         (source-uri (abaplib-get-property 'source-uri source-name))
         (full-source-uri (concat object-uri "/" source-uri))
         )
    (abaplib-do-execute full-source-uri object-name)))

(defun abap-where-used ()
  "Get Where-Used List of object under cursor."
  (interactive)
  (let* ((source-name (file-name-nondirectory (buffer-file-name)))
         (object-uri (abaplib-get-property 'uri))
         (source-uri (abaplib-get-property 'source-uri source-name))
         (full-source-uri (concat object-uri "/" source-uri)))
    (abaplib-where-used full-source-uri (line-number-at-pos) (current-column))))

(provide 'abap)
;;; abap.el ends here
