;;; lsp-java-treemacs.el --- LSP Java treemacs integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: lsp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides integration between `lsp-java' and `treemacs' which aims to provide
;; ide like project explorer.

;;; Code:

(require 'dash)
(require 'lsp-mode)
(require 'lsp-java)
(require 'dash-functional)
(require 'treemacs-extensions)

(defface lsp-java-treemacs-directory-face
  '((t :inherit  treemacs-directory-face))
  "Face used by treemacs for lsp-java directories and packages."
  :group 'lsp-java-treemacs)

(defface lsp-java-treemacs-extension-root-face
  '((t :inherit treemacs-directory-face ))
  "Face used for the extension root."
  :group 'lsp-java-treemacs)

(defface lsp-java-treemacs-file-face
  '((t :inherit  treemacs-file-face))
  "Face used by treemacs for files."
  :group 'lsp-java-treemacs)


(defun lsp-java-treemacs--get-libraries (project-uri)
  "Get the list of buffers, grouped by their major mode.
PROJECT-URI is the project id of the active project."
  (when-lsp-workspace (lsp-java--find-workspace project-uri)
    (lsp-send-execute-command "che.jdt.ls.extension.externalLibraries"
                              (list :projectUri project-uri))))

(defun lsp-java-treemacs--library-children (project-uri node-id)
  "Get the list of library children.
PROJECT-URI and NODE-ID are the details for the current node."
  (when-lsp-workspace (lsp-java--find-workspace project-uri)
    (lsp-send-execute-command "che.jdt.ls.extension.libraryChildren"
                              (list :projectUri project-uri
                                    :nodeId node-id))))

(defun lsp-java-treemacs--external-library-children (project-uri node-id path)
  "Get the list of external library children.
PROJECT-URI, NODE-ID and PATH are the details for the current node."
  (when-lsp-workspace (lsp-java--find-workspace project-uri)
    (lsp-send-execute-command "che.jdt.ls.extension.externalLibrariesChildren"
                              (list :projectUri project-uri
                                    :nodeId node-id
                                    :nodePath path))))

(defun lsp-java-treemacs--open-file (_arg)
  "Open resource file."
  (when-lsp-workspace (lsp-java--find-workspace (treemacs--prop-at-point :project-uri))
    (->> (treemacs--prop-at-point :uri)
         lsp--uri-to-path
         find-file)))

(treemacs-define-leaf-node lsp-file treemacs-icon-java
                           :ret-action 'lsp-java-treemacs--open-file)

(defmacro lsp-java-treemacs--setup-icon (var file-name &rest extensions)
  "Define VAR with its display property being the image created from FILE-NAME.
Insert VAR into `treemacs-icon-hash' for each of the given file EXTENSIONS."
  (unless (treemacs--is-image-creation-impossible?)
    `(progn
       (defvar ,var
         (let* ((image-unselected (treemacs--create-image (f-join (f-dirname (or load-file-name buffer-file-name)) ,file-name)))
                (image-selected   (treemacs--create-image (f-join (f-dirname (or load-file-name buffer-file-name)) ,file-name))))
           (treemacs--set-img-property image-selected   :background treemacs--selected-icon-background)
           (treemacs--set-img-property image-unselected :background treemacs--not-selected-icon-background)
           (concat (propertize " "
                               'display image-unselected
                               'img-selected image-selected
                               'img-unselected image-unselected)
                   " ")))
       (push ,var treemacs--created-icons)
       (--each (quote ,extensions) (ht-set! treemacs-icons-hash it ,var))
       ,var)))

(if (treemacs--is-image-creation-impossible?)
    (progn
      (lsp-java-treemacs--setup-icon lsp-java-treemacs-jar "icons/vscode/file_type_jar.png")
      (lsp-java-treemacs--setup-icon lsp-java-treemacs-jar-opened "icons/vscode/file_type_jar.png")
      (lsp-java-treemacs--setup-icon lsp-java-treemacs-package "icons/vscode/folder_type_package.png")
      (lsp-java-treemacs--setup-icon lsp-java-treemacs-package-opened "icons/vscode/folder_type_package_opened.png")
      (lsp-java-treemacs--setup-icon lsp-java-treemacs-jar-folder "icons/vscode/folder_type_component.png")
      (lsp-java-treemacs--setup-icon lsp-java-treemacs-jar-folder-opened "icons/vscode/folder_type_component_opened.png")
      (lsp-java-treemacs--setup-icon lsp-java-treemacs-library-folder "icons/vscode/folder_type_library.png")
      (lsp-java-treemacs--setup-icon lsp-java-treemacs-library-folder-opened "icons/vscode/folder_type_library_opened.png")
      (lsp-java-treemacs--setup-icon lsp-java-treemacs-class "icons/vscode/file_type_class.png" "class"))
  (treemacs--set-icon-save-default
   lsp-java-treemacs-jar treemacs-icon-closed-text
   lsp-java-treemacs-jar-opened treemacs-icon-open-text
   lsp-java-treemacs-package treemacs-icon-closed-text
   lsp-java-treemacs-package-opened treemacs-icon-open-text
   lsp-java-treemacs-jar-folder treemacs-icon-closed-text
   lsp-java-treemacs-jar-folder-opened treemacs-icon-open-text
   lsp-java-treemacs-library-folder treemacs-icon-closed-text
   lsp-java-treemacs-library-folder-opened treemacs-icon-open-text
   lsp-java-treemacs-class treemacs-icon-tag-leaf-text))

(defmacro treemacs--lsp-node-or-folder ()
  "Extract common code from nodes."
  (let ((more-properties '(:project-uri (button-get btn :project-uri)
                                        :node-path node-path
                                        :node-id (button-get btn :node-id)
                                        :uri uri)))
    `(-let (((&hash "name" "entryType" "uri" "path" node-path) item))
       (pcase entryType
         ("PACKAGE"
          (treemacs-render-node :icon treemacs-icon-package-closed
                                :label-form name
                                :state treemacs-package-closed-state
                                :key-form node-path
                                :face 'lsp-java-treemacs-directory-face
                                :more-properties ,more-properties))
         ("FOLDER"
          (treemacs-render-node :icon treemacs-icon-folder-closed
                                :label-form name
                                :state treemacs-folder-closed-state
                                :key-form node-path
                                :face 'lsp-java-treemacs-directory-face
                                :more-properties ,more-properties))
         ((or "CLASS_FILE" "FILE")
          (treemacs-render-node :icon (treemacs-icon-for-file name)
                                :label-form name
                                :state treemacs-lsp-file-state
                                :key-form uri
                                :face 'lsp-java-treemacs-file-face
                                :more-properties ,more-properties))))))

(treemacs-define-expandable-node package
  :icon-open lsp-java-treemacs-package-opened
  :icon-closed lsp-java-treemacs-package
  :query-function (lsp-java-treemacs--external-library-children
                   (button-get btn :project-uri)
                   (button-get btn :node-id)
                   (button-get btn :node-path))
  :render-action (treemacs--lsp-node-or-folder))

(treemacs-define-expandable-node folder
  :icon-open lsp-java-treemacs-jar-folder-opened
  :icon-closed lsp-java-treemacs-jar-folder
  :query-function (lsp-java-treemacs--external-library-children
                   (button-get btn :project-uri)
                   (button-get btn :node-id)
                   (button-get btn :node-path))
  :render-action (treemacs--lsp-node-or-folder))

(treemacs-define-expandable-node jar-file
  :icon-open lsp-java-treemacs-jar-opened
  :icon-closed lsp-java-treemacs-jar
  :query-function (lsp-java-treemacs--library-children (button-get btn :project-uri)
                                                       (button-get btn :key))
  :render-action (treemacs--lsp-node-or-folder))

(treemacs-define-expandable-node external-library
  :icon-open lsp-java-treemacs-library-folder-opened
  :icon-closed lsp-java-treemacs-library-folder
  :query-function (-> btn
                      (button-get :parent)
                      (button-get :path)
                      file-name-as-directory
                      lsp--path-to-uri
                      lsp-java-treemacs--get-libraries)
  :root-label "External Libraries"
  :root-marker t
  :root-face 'lsp-java-treemacs-extension-root-face
  :root-key-form (lsp--path-to-uri (button-get parent :path))
  :render-action (-let ((project-uri (-> btn
                                         (button-get :parent)
                                         (button-get :path)
                                         lsp--path-to-uri))
                        ((&hash "name" "id") item))
                   (treemacs-render-node
                    :icon treemacs-icon-jar-file-closed
                    :label-form name
                    :state treemacs-jar-file-closed-state
                    :face 'lsp-java-treemacs-directory-face
                    :key-form id
                    :more-properties (:node-id id :project-uri project-uri
                                               :workspace (lsp-java--find-workspace project-uri)))))

(defun lsp-java-treemacs--folders-change (added removed)
  "Handler for `lsp-workspace-folders-change' hook.
ADDED and REMOVED are pointing which are the changed folders."
  (--each added
    (treemacs-do-add-project-to-workspace it (f-filename it)))
  (--each removed
    (-some->> (treemacs-current-workspace)
              treemacs-workspace->projects
              (-first (lambda (project) (f-equal? it (treemacs-project->path project))))
              treemacs-do-remove-project-from-workspace)))

(defun lsp-java-treemacs--is-root (dir-or-project)
  "Return whether DIR-OR-PROJECT is root of a project."
  (let ((dir (if (stringp dir-or-project)
                 dir-or-project
               (treemacs-project->path dir-or-project))))
    (-some-> dir
             lsp-java--find-workspace
             lsp-java--get-project-uris
             (-contains? (lsp--path-to-uri dir)))))

(defun lsp-java-treemacs-register ()
  "Register `lsp-java' extension."
  (interactive)
  (treemacs-define-directory-extension
   :extension 'treemacs-EXTERNAL-LIBRARY-extension
   :position 'directory-start
   :predicate 'lsp-java-treemacs--is-root)
  (treemacs-define-project-extension
   :extension 'treemacs-EXTERNAL-LIBRARY-extension
   :position 'project-start
   :predicate 'lsp-java-treemacs--is-root)

  (require 'treemacs)
  (unless (eq 'visible (treemacs-current-visibility))
    (treemacs))

  (maphash (lambda (root-path _workspace)
             (unless (or (s-equals? (f-canonical root-path) (f-canonical lsp-java-workspace-dir))
                         (s-equals? (f-canonical root-path) (f-canonical lsp-java-workspace-cache-dir)))
               (treemacs-do-add-project-to-workspace root-path (f-filename root-path))))
           lsp--workspaces)
  (add-hook 'lsp-workspace-folders-change 'lsp-java-treemacs--folders-change))

(defun lsp-java-treemacs-unregister ()
  "Unregister extension."
  (interactive)
  (remove-hook 'lsp-workspace-folders-change 'lsp-java-treemacs--folders-change)
  (treemacs-remove-project-extension 'treemacs-EXTERNAL-LIBRARY-extension
                                     'project-start)
  (treemacs-remove-directory-extension 'treemacs-EXTERNAL-LIBRARY-extension
                                       'directory-start))

(provide 'lsp-java-treemacs )
;;; lsp-java-treemacs.el ends here
