# ABAP In Emacs (AiE)

**TODO: Integrate `sap-abap-mode` to have `ABAP Mode` available**

## Introduction

This project implements a Emacs plugin which allows to access development artifacts like reports,
classes, interfaces etc. of a ABAP system. It uses the ABAP Development Tools (ADT) HTTP protocol
to access the ABAP workbench functionality. Currently, the following commands are provided by this
package:

- Retrieve (Pull) source from ABAP server
- Offline edit
- Server side formating & auto-complete
- Server side syntax check
- Submit (push) source to ABAP server
- Activate source at ABAP server
- Run source (Not yet)

## Installation

### Within Spacemacs

You should have already created a private layer. In this layer add code below to `packages.el`:

```cl
(defconst <your-layer-name>-packages
  '(
    (abap-mode
     :location (recipe :fetcher github :repo "qianmarv/sap-abap-mode"))

    (abap
     :location (recipe :fetcher github :repo "qianmarv/ABAPInEmacs"))
    ))

(defun qianmarv-sap/init-abap-mode ()
  (use-package abap-mode
    :mode ("\\.abap\\'" . abap-mode)
    :init
        ;;    (add-to-list 'auto-mode-alist '("\\.abap\\'" . abap-mode))
    ))

(defun qianmarv-sap/init-abap ()
  (use-package abap
    :defer t
    :after (request)
    :init
    (progn
       (spacemacs/declare-prefix  "aa" "ABAP Development")
       (spacemacs/set-leader-keys "aac" 'abap-create-project)
       (spacemacs/set-leader-keys "aaa" 'abap-add-project)
       (spacemacs/set-leader-keys "aar" 'abap-remove-project)
       (spacemacs/set-leader-keys "aao" 'abap-open-project)
       (spacemacs/set-leader-keys "aas" 'abap-add-server)
       (spacemacs/set-leader-keys "aaf" 'abap-search-object)
       (spacemacs/set-leader-keys "aal" 'abap-login)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "p" 'abap-open-project)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "f" 'abap-search-object)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "r" 'abap-retrieve-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "c" 'abap-check-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "r" 'abap-retrieve-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "s" 'abap-submit-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "a" 'abap-activate-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "=" 'abap-format-source)
       (global-set-key (kbd "<C-return>") 'abap-code-completion)
       )))
```

### As a package manually

Clone this repository and add `ABAPInEmacs` directory to your `load-path`.
```cl
(add-to-list 'load-path "path/to/ABAPInEmacs")
(require 'abap)
(require 'abap-flycheck)
```
Specify your workspace directory by setting the variable `abap-workspace-dir`.
```cl
(setq abap-workspace-dir "path/to/ABAPWorkspace")
```

## Usage

### Project setup

Steps to set up a ABAP project:

- Create a project with `M-x abap-create-project`.
  If you already have a project, with `M-x abap-add-project`.
  Remove an existing project with `M-x abap-remove-project`. This will not physically remove the
  project files.
  At any time, you can open or switch to an existing project with `M-x abap-open-project`.
- Add a ABAP server to project with `M-x abap-add-server`.

### Start development

The following steps are an essential part of the workflow:

1. `M-x abap-login` - Logon to ABAP server\
    By default, the encoded credential will be persisted so that a login on each action is not
    required.
2. `M-x abap-search-object` - Search ABAP development object\
   The source code of the selected item is pulled from the ABAP server afterwards and a `Dired`
   buffer opens up showing the corresponding files.

Once a ABAP development object has been successfully fetched by Step 2, the following steps can be
done when working on `.abap`-files:

3. `M-x abap-retrieve-source` - Update content of ABAP development object in current buffer from server\
    Note that after a successful fetch of the data the buffer has to be reverted manually!
    (`M-x revert-buffer`)
4. `M-x abap-check-source` - Perform syntax check of object in current buffer
    If errors and/or warnings occur, they are displayed in the \***ABAP Log**\* buffer.
5. `M-x abap-format-source` - Do source code formatting of object (pretty print)
6. `M-x abap-submit-source` - Submit local content in current buffer to ABAP server\
    Unless the object resides server-side in the `$TMP` package, a transport request has to be
    chosen to for the current version of the source code.
7. `M-x abap-activate-source` - Activate (compile) server-side object in current buffer
8. `M-x abap-code-completion` - Request proposals for code completion from ABAP server

You can also create a Git repository with the retrieved source code and collaborate with other colleagues.
