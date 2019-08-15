;;; lsp-java-dependency.el --- LSP Java treemacs integration  -*- lexical-binding: t; -*-

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

(with-eval-after-load 'treemacs
  (defcustom lsp-java-dependency-theme "Default"
    "The `lsp-java-dependency' theme."
    :type 'string
    :group 'lsp-java-dependency)

  (defun lsp-java-dependency--goto-element (&rest _args)
    (if-let ((dep (-some-> (treemacs-node-at-point)
                           (button-get :dep))))
        (--doto (find-file-noselect (or (-some-> (gethash "uri" dep)
                                                 (lsp--uri-to-path))
                                        (gethash "path" dep)))

          (select-window (get-mru-window nil nil t))
          (switch-to-buffer it))
      (user-error "No element under point.")))

  (treemacs-modify-theme "Default"
    :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/vscode")
    :config
    (progn
      (treemacs-create-icon :file "classfile.png" :extensions (classfile) :fallback "-")
      (treemacs-create-icon :file "default_folder_opened.png" :extensions (default-folder-opened) :fallback "-")
      (treemacs-create-icon :file "default_folder.png" :extensions (default-folder) :fallback "-")
      (treemacs-create-icon :file "default_root_folder_opened.png" :extensions (default-root-folder-opened) :fallback "-")
      (treemacs-create-icon :file "default_root_folder.png" :extensions (default-root-folder) :fallback "-")
      (treemacs-create-icon :file "file_type_class.png" :extensions ("class") :fallback "-")
      (treemacs-create-icon :file "file_type_jar.png" :extensions (file-type-jar) :fallback "-")
      (treemacs-create-icon :file "folder-open.png" :extensions (folder-open) :fallback "-")
      (treemacs-create-icon :file "folder.png" :extensions (folder) :fallback "-")
      (treemacs-create-icon :file "folder_type_component_opened.png" :extensions (folder-type-component-opened) :fallback "-")
      (treemacs-create-icon :file "folder_type_component.png" :extensions (folder-type-component) :fallback "-")
      (treemacs-create-icon :file "folder_type_library_opened.png" :extensions (folder-type-library-opened) :fallback "-")
      (treemacs-create-icon :file "folder_type_library.png" :extensions (folder-type-library) :fallback "-")
      (treemacs-create-icon :file "folder_type_maven_opened.png" :extensions (folder-type-maven-opened) :fallback "-")
      (treemacs-create-icon :file "folder_type_maven.png" :extensions (folder-type-maven) :fallback "-")
      (treemacs-create-icon :file "folder_type_package_opened.png" :extensions (folder-type-package-opened) :fallback "-")
      (treemacs-create-icon :file "folder_type_package.png" :extensions (folder-type-package) :fallback "-")
      (treemacs-create-icon :file "icon-create.png" :extensions (icon-create) :fallback "-")
      (treemacs-create-icon :file "icon-flat.png" :extensions (icon-flat) :fallback "-")
      (treemacs-create-icon :file "icon-hierarchical.png" :extensions (icon-hierarchical) :fallback "-")
      (treemacs-create-icon :file "icon-link.png" :extensions (icon-link) :fallback "-")
      (treemacs-create-icon :file "icon-refresh.png" :extensions (icon-refresh) :fallback "-")
      (treemacs-create-icon :file "icon-unlink.png" :extensions (icon-unlink) :fallback "-")
      (treemacs-create-icon :file "jar.png" :extensions (jar) :fallback "-")
      (treemacs-create-icon :file "library.png" :extensions (library) :fallback "-")
      (treemacs-create-icon :file "packagefolder-open.png" :extensions (packagefolder-open) :fallback "-")
      (treemacs-create-icon :file "packagefolder.png" :extensions (packagefolder) :fallback "-")
      (treemacs-create-icon :file "package.png" :extensions (package) :fallback "-")
      (treemacs-create-icon :file "project.png" :extensions (java-project) :fallback "-")))

  (defun lsp-java-dependency--icon (dep expanded)
    "Get the symbol for the the kind."
    (-let (((&hash "uri" "name" "kind" "entryKind" entry-kind) dep))
      (concat
       (if (or (= kind 6)
               (= kind 8))
           "   "
         (treemacs-get-icon-value
          (if expanded 'expanded 'collapsed)
          nil
          lsp-java-dependency-theme))
       (if (or (= kind 8)
               (= kind 6))
           (treemacs-icon-for-file uri)
         (treemacs-get-icon-value
          (cond
           ((eq kind 5) 'package)
           ((eq kind 7) 'folder)
           ((eq kind 4) 'packagefolder)
           ((eq kind 2) 'java-project)
           ((eq entry-kind 1) 'package)
           ((eq entry-kind 3) 'packagefolder)
           ((eq entry-kind 5) 'library))
          nil
          lsp-java-dependency-theme)))))

  (defun lsp-java-dependency--get-children (dep)
    (lsp-java-with-jdtls
      (-let (((&hash "projectUri" project-uri "rootPath" root-path "path" "kind" "name") dep))
        (unless (or (= kind 6)
                    (= kind 8))
          (->> (lsp-send-execute-command
                "java.getPackageData"
                (vector (ht ("kind"  kind)
                            ("path"  (unless (eq kind 2)
                                       (if (= 5 kind)
                                           name
                                         path)))
                            ("rootPath" (unless (eq kind 2)
                                          (or root-path path)))
                            ("projectUri"  project-uri))))
               (-mapcat (lambda (inner-dep)
                          (puthash "projectUri" project-uri inner-dep)
                          (when (= kind 4)
                            (puthash "rootPath" path inner-dep))
                          (if (eq (gethash "entryKind" inner-dep) 3)
                              (lsp-java-dependency--get-children inner-dep)
                            (list inner-dep)))))))))

  (treemacs-define-expandable-node lsp-java-dependency
    :icon-open-form (lsp-java-dependency--icon (treemacs-button-get node :dep) t)
    :icon-closed-form (lsp-java-dependency--icon (treemacs-button-get node :dep) nil)
    :query-function (lsp-java-dependency--get-children (treemacs-button-get node :dep))
    :ret-action 'lsp-java-dependency--goto-element
    :render-action
    (treemacs-render-node
     :icon (lsp-java-dependency--icon  item nil)
     :label-form (propertize (gethash "name" item) 'face 'default)
     :state treemacs-lsp-java-dependency-closed-state
     :key-form (gethash "name" item)
     :more-properties (:dep item)))

  (defun lsp-java-dependency--root-folders ()
    (lsp-java-with-jdtls
      (-mapcat (lambda (root-path)
                 (let ((project-uri (lsp--path-to-uri root-path)))
                   (->> project-uri
                        (lsp-send-execute-command "java.project.list")
                        (--map (--doto it (puthash "projectUri" project-uri it))))))
               (lsp-session-folders (lsp-session)))))

  (treemacs-define-variadic-node lsp-java-dependency-list
    :query-function (lsp-java-dependency--root-folders)
    :render-action
    (treemacs-render-node
     :icon (lsp-java-dependency--icon item nil)
     :label-form (propertize (gethash "name" item) 'face 'default)
     :state treemacs-lsp-java-dependency-closed-state
     :key-form item
     :more-properties (:dep item))
    :root-key-form 'LSP-Java-Dependency)

  (defun lsp-java-dependency-refresh ()
    "Refresh dependecy list."
    (interactive)
    (condition-case _err
        (let ((inhibit-read-only t))
          (with-current-buffer "*LSP Dependency List*"
            (treemacs-update-node '(:custom LSP-Symbols) t)))
      (error)))

  (defun lsp-java-dependency-list ()
    "Display error list."
    (interactive)
    (lsp-java-with-jdtls
      (let* ((buffer (get-buffer-create "*LSP Dependency List*"))
             (window (display-buffer-in-side-window buffer '((side . right)))))
        (select-window window)
        (set-window-dedicated-p window t)
        (treemacs-initialize)
        (setq-local treemacs-default-visit-action 'treemacs-RET-action)
        (treemacs-LSP-JAVA-DEPENDENCY-LIST-extension)))))

(provide 'lsp-java-dependency)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
