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
  :type 'directory)

(defcustom lsp-java-workspace-dir (expand-file-name (locate-user-emacs-file "workspace/"))
  "LSP java workspace directory."
  :group 'lsp-java-mode
  :risky t
  :type 'directory)

(defcustom lsp-java-workspace-cache-dir (expand-file-name (locate-user-emacs-file "workspace/.cache/"))
  "LSP java workspace cache directory."
  :group 'lsp-java-mode
  :risky t
  :type 'directory)

(defcustom lsp-java--workspace-folders ()
  "LSP java workspace folders storing files downloaded from JDT."
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
     (progressReports
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
      (importOrder . ["java" "javax" "com" "org"])))))

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

(defun lsp-java-organize-imports ()
  "Organize java imports."
  (interactive)
  (lsp--send-execute-command
   "java.edit.organizeImports"
   (list (lsp--path-to-uri buffer-file-name))))

(defun lsp-java--ensure-dir (path)
  "Ensure that directory PATH exists."
  (unless (file-directory-p path)
    (make-directory path)))

(defun lsp-java--ls-command ()
  "LS startup command."
  (let ((server-jar (lsp-java--locate-server-jar))
        (server-config (lsp-java--locate-server-config))
        (root-dir (lsp-java--get-root)))
    (lsp-java--ensure-dir lsp-java-workspace-dir)
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
       ,lsp-java-workspace-dir)))

(defun lsp-java--get-root ()
  "Retrieves the root directory of the java project root if available.

The current directory is assumed to be the java project’s root otherwise."
  (cond
   ;; the cache directory root
   ((string= default-directory lsp-java-workspace-cache-dir) default-directory)
   ((and (featurep 'projectile) (projectile-project-p)) (projectile-project-root))
   ((vc-backend default-directory) (expand-file-name (vc-root-dir)))
   (t (let ((project-types '("pom.xml" "build.gradle" ".project")))
	(or (seq-some (lambda (file) (locate-dominating-file default-directory file)) project-types)
	    default-directory)))))

(defun lsp-java--language-status-callback (workspace params)
  "Callback for client initialized."
  (let ((status (gethash "type" params))
        (current-status (lsp-workspace-get-metadata "status" workspace)))
    ;; process the status message only if there is no status or if the status is
    ;; starting (workaround for bug https://github.com/eclipse/eclipse.jdt.ls/issues/651)
    (when (not (and (or (string= current-status "Error" )
                        (string= current-status "Started" ))
                    (string= "Starting" status)))
      (lsp-workspace-status (concat "::" status) workspace)
      (lsp-workspace-set-metadata "status" status workspace)
      (message "%s[%s]" (gethash "message" params) (gethash "type" params)))))

(defun lsp-java--apply-workspace-edit (action)
  "Callback for java/applyWorkspaceEdit."
  (lsp--apply-workspace-edit (car (gethash "arguments" action))))

(defun lsp-java--actionable-notification-callback (workspace params)
  "Handler for actionable notifications."
  (let ((notifications (or (lsp-workspace-get-metadata
                            "actionable-notifications"
                            workspace)
                           (make-hash-table :test #'equal))))
    (puthash (gethash "message" params) params notifications)
    (lsp-workspace-set-metadata "actionable-notifications"
                                notifications
                                workspace)
    (lsp-workspace-status
     (concat "::"
             (propertize
              (concat (lsp-workspace-get-metadata "status" workspace) "[!]")
              'face 'warning)))))

(defun lsp-java--progress-report (_workspace params)
  "Progress report handling."
  (message "%s%s" (gethash "status" params) (if (gethash "complete" params) " (done)" "")))

(defun lsp-java--client-initialized (client)
  "Callback for CLIENT initialized."
  (lsp-client-on-notification client "language/status" 'lsp-java--language-status-callback)
  (lsp-client-on-notification client "language/actionableNotification" 'lsp-java--actionable-notification-callback)
  (lsp-client-on-notification client "language/progressReport" 'lsp-java--progress-report)
  (lsp-client-on-action client "java.apply.workspaceEdit" 'lsp-java--apply-workspace-edit)
  (lsp-client-register-uri-handler client "jdt" 'lsp-java--resolve-uri))

(defun lsp-java--get-filename (url)
  "Get the name of the buffer calculating it based on URL."
  (let ((regexp "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?"))
    (save-match-data
      (string-match regexp url)
      (format "%s.java"
              (replace-regexp-in-string "/" "." (match-string 2 url) t t)))))

(defun lsp-java--get-metadata-location (file-location)
  "Given a FILE-LOCATION return the file containing the metadata for the file."
  (format "%s.%s.metadata"
          (file-name-directory file-location)
          (file-name-base file-location)))

(defun lsp-java--resolve-uri (uri)
  "Load a file corresponding to URI executing request to the jdt server."
  (let* ((buffer-name (lsp-java--get-filename uri))
         (file-location (concat lsp-java-workspace-cache-dir buffer-name)))
    (unless (file-readable-p file-location)
      (lsp-java--ensure-dir lsp-java-workspace-cache-dir)
      (let ((content (lsp-send-request (lsp-make-request
                                        "java/classFileContents"
                                        (list :uri uri)))))
        (with-temp-file file-location
          (insert content))
                                        ; store uri
        (with-temp-file (lsp-java--get-metadata-location file-location)
          (insert uri))))
    file-location))

(defun lsp-java-actionable-notifications ()
  "Lists current actionable notifications."
  (interactive)
  (when-let ((notifications (lsp-workspace-get-metadata "actionable-notifications"))
             (selected-notification (completing-read
                                     "Select notification to fix:"
                                     notifications
                                     nil
                                     t))
             (commands (gethash "commands"
                                (gethash selected-notification notifications))))
    (lsp-execute-code-action (lsp--select-action commands))
    (remhash selected-notification notifications)
    (when (= (hash-table-count notifications) 0)
      (lsp-workspace-status (concat "::" (lsp-workspace-get-metadata "status"))))))

(lsp-define-stdio-client lsp-java "java" (lambda () lsp-java-workspace-dir)
                         (lsp-java--ls-command)
                         :ignore-regexps
                         '("^SLF4J: "
                           "^Listening for transport dt_socket at address: ")
                         :extra-init-params (list :workspaceFolders (mapcar
                                                                     'lsp--path-to-uri
                                                                     lsp-java--workspace-folders)
                                                  :settings (lsp-java--settings)
                                                  :extendedClientCapabilities (list :progressReportProvider t))
                         :initialize 'lsp-java--client-initialized)

(defun lsp-java--after-start (&rest _args)
  "Run after `lsp-java-enable' to configure workspace folders."
  ;; TODO temporary explicitly initialize lsp--workspaces with the workspace folders
  ;; until lsp-mode provides facilities for managing folders
  (mapc (lambda (root)
          (puthash root lsp--cur-workspace lsp--workspaces))
        lsp-java--workspace-folders)
  (puthash lsp-java-workspace-cache-dir lsp--cur-workspace lsp--workspaces))

(defun lsp-java--before-start (&rest _args)
  "Initialize lsp java variables."
  (let ((metadata-file-name (lsp-java--get-metadata-location buffer-file-name)))
    (setq-local lsp-buffer-uri
                (when (file-exists-p metadata-file-name)
                  (with-temp-buffer (insert-file-contents metadata-file-name)
                    (buffer-string)))))

  ;; disable editing in case file coming from a jar has been opened.
  (when lsp-buffer-uri (read-only-mode 1)))

(add-function :after (symbol-function 'lsp-java-enable) #'lsp-java--after-start)
(add-function :before (symbol-function 'lsp-java-enable) #'lsp-java--before-start)

(provide 'lsp-java)
;;; lsp-java.el ends here
