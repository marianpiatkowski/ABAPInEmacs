# ABAP In Emacs (AiE)

## Introduction

This project implements a Emacs plugin which allows to access development artifacts like reports,
classes, interfaces etc. of a ABAP system. It uses the ABAP Development Tools (ADT) HTTP protocol
to access the ABAP workbench functionality. Currently, the package provides the following:

- Retrieve (Pull) source from ABAP server
- Offline edit\
  To have `ABAP Mode` available, a major mode for editing/viewing development files in Emacs,
  install [sap-abap-mode](https://github.com/marianpiatkowski/sap-abap-mode) (see description below).
- Server side formating & auto-complete
- Server side syntax check
- Submit (push) source to ABAP server
- Activate source at ABAP server
- Run source

## Installation

The current version can only be installed manually. Clone this repository with the `--recursive` option, i.e.
```bash
$ git clone --recursive https://github.com/marianpiatkowski/ABAPInEmacs.git
```
to get additional packages like [sap-abap-mode](https://github.com/marianpiatkowski/sap-abap-mode) as well.
Add `ABAPInEmacs` directory to your `load-path`
```cl
(add-to-list 'load-path "path/to/ABAPInEmacs")
;; sap-abap-mode includes
(require 'abap-mode)
(add-to-list 'auto-mode-alist '("\\.abap\\'" . abap-mode))
;; ABAP CDS Mode
(require 'abap-cds-mode)
(add-to-list 'auto-mode-alist '("\\.cds\\'" . abap-cds-mode))
;; ADT files as well
(add-to-list 'auto-mode-alist '("\\.\\(asprog\\|asinc\\|aclass\\)\\'" . abap-mode))
(add-to-list 'auto-mode-alist '("\\.asddls\\'" . abap-cds-mode))
;; ABAPInEmacs includes
(require 'abap)
(require 'abap-flycheck)
```
Specify your workspace directory by setting the variable `abap-workspace-dir`.
```cl
(setq abap-workspace-dir "path/to/ABAPWorkspace")
```

With the configuration above

- `ABAP Mode` will be loaded automatically once a file with suffix `.abap` has been opened
- `ABAP CDS Mode` will be loaded automatically once a file with suffix `.cds` has been opened.

## Usage

### Project setup

Steps to set up a ABAP project:

- Create a project with `M-x abap-create-project`.\
  If you already have a project, it can be added with `M-x abap-add-project`.\
  Remove an existing project with `M-x abap-remove-project`. This will not physically remove the
  project files.\
  At any time, you can open or switch to an existing project with `M-x abap-open-project`.
- Add a ABAP server to project with `M-x abap-add-server`.

### Start development

The following steps are an essential part of the workflow:

1. `M-x abap-login` - Logon to ABAP server\
    By default, the encoded credential will be persisted so that a login on each action is not
    required.
2. `M-x abap-search-object` - Search ABAP development object\
    The source code of the selected search result is then pulled from the ABAP server and a
    directory buffer opens up showing the corresponding files.

Once a ABAP development object has been successfully fetched by Step 2, the following steps can be
done when working on `.abap`-files:

3. `M-x abap-retrieve-source` - Get content of ABAP development object in current buffer from server\
    Note that after a successful fetch of the data the buffer has to be reverted manually!
    (`M-x revert-buffer`)
4. `M-x abap-syntax-check` - Perform syntax check of object in current buffer\
    If errors and/or warnings occur, they are displayed in the \***ABAP Log**\* buffer.
5. `M-x abap-format-source` - Do source code formatting of object (pretty print)
6. `M-x abap-submit-source` - Submit local content in current buffer to ABAP server\
    Unless the object is local on the server side, a transport request has to be chosen for the
    current version of the source code.
7. `M-x abap-activate-source` - Activate (compile) object in current buffer on the server
8. `M-x abap-code-completion` - Request proposals for code completion from ABAP server
9. `M-x abap-navigate-code` - Navigate to object under cursor in current buffer, i.e. find
   definition/implementation of the object
10. `M-x abap-execute-object` - Execute source code in current buffer

You can also create a Git repository with the retrieved source code and collaborate with other colleagues.
