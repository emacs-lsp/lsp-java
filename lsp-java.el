;;; lsp-java.el --- Java support for lsp-mode

;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.0"))
;; Keywords: java
;; URL: https://github.com/emacs-lsp/lsp-java

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cc-mode)
(require 'lsp-mode)

;;;###autoload
(defcustom lsp-java-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/")
  "Install directory for eclipse.jdt.ls-server.
The slash is expected at the end."
  :group 'lsp-mode
  :risky t
  :type 'directory )

(defcustom lsp-java-workspace-dir (expand-file-name (locate-user-emacs-file "workspace/"))
  "LSP java workspace directory."
  :group 'lsp-java-mode
  :risky t
  :type 'directory)

(defcustom lsp-java--workspace-folders ()
  "LSP java workspace folders."
  :group 'lsp-java-mode
  :risky t)

;; TODO create customization for each of the settings
(defun lsp-java--settings ()
  "JDT settings."
  '((java
     (jdt
      (ls
       (vmargs . "-noverify -Xmx1G -XX:+UseG1GC -XX:+UseStringDeduplication")))
     (errors
      (incompleteClasspath
       (severity . "warning")))
     (configuration
      (updateBuildConfiguration . "interactive")
      (maven))
     (trace
      (server . "off"))
     (import
      (gradle
       (enabled . t))
      (maven
       (enabled . t))
      (exclusions . ["**/node_modules/**"
                     "**/.metadata/**"
                     "**/archetype-resources/**"
                     "**/META-INF/maven/**"]))
     (referencesCodeLens
      (enabled . t))
     (signatureHelp
      (enabled . t))
     (implementationsCodeLens
      (enabled . t))
     (format
      (enabled . t))
     (saveActions
      (organizeImports . :json-false))
     (contentProvider)
     (autobuild
      (enabled . t))
     (completion
      (favoriteStaticMembers .
                             ["org.junit.Assert.*"
                              "org.junit.Assume.*"
                              "org.junit.jupiter.api.Assertions.*"
                              "org.junit.jupiter.api.Assumptions.*"
                              "org.junit.jupiter.api.DynamicContainer.*"
                              "org.junit.jupiter.api.DynamicTest.*"])
      (importOrder . ["java" "javax" "com" "org"]))
     (test
      (report
       (position . "sideView"))))))

(defun lsp-java--locate-server-jar ()
  "Return the jar file location of the language server.

The entry point of the language server is in `lsp-java-server-install-dir'/plugins/org.eclipse.equinox.launcher_`version'.jar."
  (ignore-errors
    (let* ((plugindir (expand-file-name "plugins" lsp-java-server-install-dir))
           (server-jar-filenames (directory-files plugindir t "org.eclipse.equinox.launcher_.*")))
      (if (not (= (length server-jar-filenames) 1))
          (message (format "Found more than one java language server entry points: %s" server-jar-filenames))
        (car server-jar-filenames)))))

(defun lsp-java--locate-server-config ()
  "Return the server config based on OS."
  (let ( (config (cond
                  ((string-equal system-type "windows-nt") ; Microsoft Windows
                   "config_win")
                  ((string-equal system-type "darwin") ; Mac OS X
                   "config_mac")
                  ((string-equal system-type "gnu/linux") ; linux
                   "config_linux"))))
    (message (format "using config for %s" config))
    (expand-file-name config lsp-java-server-install-dir)))

(defun lsp-java--get-workspace-dir ()
  "Gets the workspace directory. Directory will be created if it doesn't exists."
  (unless (file-directory-p lsp-java-workspace-dir)
    (make-directory lsp-java-workspace-dir))
  lsp-java-workspace-dir)

(defun lsp-java--ls-command ()
  "LS startup command."
  (let ((server-jar (lsp-java--locate-server-jar))
        (server-config (lsp-java--locate-server-config))
        (root-dir (lsp-java--get-root)))
    `( "java"
       "-Declipse.application=org.eclipse.jdt.ls.core.id1"
       "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
       "-Dosgi.bundles.defaultStartLevel=4"
       "-Declipse.product=org.eclipse.jdt.ls.core.product"
       "-Dlog.protocol=true"
       "-Dlog.level=ALL"
       "-noverify"
       "-Xmx1G"
       "-jar"
       ,server-jar
       "-configuration"
       ,server-config
       "-data"
       ,(lsp-java--get-workspace-dir))))

(defun lsp-java--get-root ()
  "Retrieves the root directory of the java project root if available.

The current directory is assumed to be the java project’s root otherwise."
  (cond
   ((and (featurep 'projectile) (projectile-project-p)) (projectile-project-root))
   ((vc-backend default-directory) (expand-file-name (vc-root-dir)))
   (t (let ((project-types '("pom.xml" "build.gradle" ".project")))
	(or (seq-some (lambda (file) (locate-dominating-file default-directory file)) project-types)
	    default-directory)))))

(defun lsp-java--language-status-callback (workspace params)
  "Callback for client initialized."
  (setq-local lsp-status (concat "::" (gethash "type" params)))
  (message "%s[%s]" (gethash "message" params) (gethash "type" params)))

(defun lsp-java--client-initialized (client)
  "Callback for client initialized."
  (lsp-client-on-notification client "language/status" 'lsp-java--language-status-callback))

(lsp-define-stdio-client lsp-java "java" (lambda () lsp-java-workspace-dir)
                         (lsp-java--ls-command)
                         :ignore-regexps
                         '("^SLF4J: "
                           "^Listening for transport dt_socket at address: ")
                         :extra-init-params (list :workspaceFolders (mapcar
                                                                     'lsp--path-to-uri
                                                                     lsp-java--workspace-folders)
                                                  :settings (lsp-java--settings))
                         :initialize 'lsp-java--client-initialized)

(defun lsp-java--after-start (&rest args)
 "Runs after `lsp-java-enable' to configure workspace folders."
  ;; TODO temporary explicitly initialize lsp--workspaces with the workspace folders
  ;; until lsp-mode provides facilities for managing folders
 (mapcar (lambda (root)
           (puthash root lsp--cur-workspace lsp--workspaces))
         lsp-java--workspace-folders))

(add-function :after (symbol-function 'lsp-java-enable) #'lsp-java--after-start)

(provide 'lsp-java)
;;; lsp-java.el ends here
