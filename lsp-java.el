;;; lsp-java.el --- Java support for lsp-mode

;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.0") (markdown-mode "2.3") (dash "2.14.1") (f "0.20.0") (ht "2.0") (dash-functional "1.2.0"))
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

;;; Commentary: Java specific adapter for LSP mode

;;; Code:
(require 'cc-mode)
(require 'lsp-mode)
(require 'markdown-mode)
(require 'lsp-methods)
(require 'dash)
(require 'ht)
(require 'f)
(require 'tree-widget)

(defgroup lsp-java nil
  "JDT emacs frontend."
  :prefix "lsp-java-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/emacs-lisp/lsp-java"))

(defcustom lsp-java-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/")
  "Install directory for eclipse.jdt.ls-server.
The slash is expected at the end."
  :group 'lsp-java
  :risky t
  :type 'directory)

(defcustom lsp-java-java-path "java"
  "Path of the java executable."
  :group 'lsp-java
  :type 'string)

(defcustom lsp-java-workspace-dir (expand-file-name (locate-user-emacs-file "workspace/"))
  "LSP java workspace directory."
  :group 'lsp-java
  :risky t
  :type 'directory)

(defcustom lsp-java-workspace-cache-dir (expand-file-name (locate-user-emacs-file "workspace/.cache/"))
  "LSP java workspace cache directory."
  :group 'lsp-java
  :risky t
  :type 'directory)

(defcustom lsp-java--workspace-folders ()
  "(Deprecated) - use `lsp-workspace-folders-add' and `lsp-workspace-folders-remove'."
  :group 'lsp-java
  :risky t
  :type '(repeat directory))

(defcustom lsp-java-themes-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons")
  "Directory containing themes."
  :type 'directory
  :group 'lsp-java)

(defcustom lsp-java-theme "vscode"
  "Theme to use."
  :type 'string
  :group 'lsp-java)

(defcustom lsp-java-pop-buffer-function 'lsp-java-show-buffer
  "The function which will be used for showing the helper windows."
  :type 'function
  :group 'lsp-java)

(defcustom lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication")
  "Specifies extra VM arguments used to launch the Java Language Server.

Eg. use `-noverify -Xmx1G -XX:+UseG1GC
-XX:+UseStringDeduplication` to bypass class
verification,increase the heap size to 1GB and enable String
deduplication with the G1 Garbage collector"
  :group 'lsp-java
  :risky t
  :type '(repeat string))

(defcustom lsp-java-9-args '("--add-modules=ALL-SYSTEM" "--add-opens java.base/java.util=ALL-UNNAMED" "--add-opens java.base/java.lang=ALL-UNNAMED")
  "Specifies arguments specific to java 9 and later."
  :group 'lsp-java
  :risky t
  :type '(repeat string))

(defcustom lsp-java-incomplete-classpath 'warning
  "Specifies the severity of the message when the classpath is incomplete for a Java file."
  :group 'lsp-java
  :type '(choice (const ignore)
                 (const info)
                 (const warning)
                 (const error)))

(defcustom lsp-java-update-build-configuration 'automatic
  "Specifies how modifications on build files update the Java classpath/configuration."
  :group 'lsp-java
  :type '(choice
          (const disabled)
          (const interactive)
          (const automatic)))

(defcustom lsp-java-import-exclusions '("**/node_modules/**"
                                        "**/.metadata/**"
                                        "**/archetype-resources/**"
                                        "**/META-INF/maven/**")
  "Configure glob patterns for excluding folders."
  :group 'lsp-java
  :type '(repeat string))

(defcustom lsp-java-favorite-static-members
  '("org.junit.Assert.*"
    "org.junit.Assume.*"
    "org.junit.jupiter.api.Assertions.*"
    "org.junit.jupiter.api.Assumptions.*"
    "org.junit.jupiter.api.DynamicContainer.*"
    "org.junit.jupiter.api.DynamicTest.*")
  "Defines a list of static members or types with static members.

 Content assist will propose those static members even if the
 import is missing."
  :group 'lsp-java
  :type '(repeat string))

(defcustom lsp-java-import-order
  '("java" "javax" "com" "org")
  "Defines the sorting order of import statements.

A package or type name prefix (e.g. 'org.eclipse') is a valid entry. An import is always added to the most specific group."
  :group 'lsp-java
  :type '(repeat string))

(defcustom lsp-java-trace-server 'off
  "Traces the communication between Emacs and the Java language server."
  :group 'lsp-java
  :type '(choice
          (const off)
          (const messages)
          (const verbose)))

(defcustom lsp-java-enable-file-watch nil
  "Defines whether the client will monitor the files for changes."
  :group 'lsp-java
  :type 'boolean)

(defcustom lsp-java-format-enabled t
  "Specifies whether or not formatting is enabled on the language server."
  :group 'lsp-java
  :type 'boolean)

(defcustom lsp-java-format-settings-url nil
  "Specifies the file path to the formatter xml url."
  :group 'lsp-java
  :type 'string)

(defcustom lsp-java-format-settings-profile nil
  "Specifies the formatter profile name."
  :group 'lsp-java
  :type 'string)

(defcustom lsp-java-format-comments-enabled nil
  "Preference key used to include the comments during the formatting."
  :group 'lsp-java
  :type 'boolean)

(defcustom  lsp-java-save-action-organize-imports t
  "Organize imports on save."
  :group 'lsp-java
  :type 'boolean)

(defcustom lsp-java-bundles nil
  "List of bundles that will be loaded in the JDT server."
  :group 'lsp-java
  :type 'list)

(defcustom lsp-java-import-gradle-enabled t
  "Enable/disable the Gradle importer."
  :group 'lsp-java
  :type 'boolean)

(defcustom lsp-java-import-maven-enabled t
  "Enable/disable the Maven importer."
  :group 'lsp-java
  :type 'boolean)

(defcustom lsp-java-auto-build t
  "Enable/disable the 'auto build'."
  :group 'lsp-java
  :type 'boolean)

(defcustom lsp-java-progress-report t
  "[Experimental] Enable/disable progress reports from background processes on the server."
  :group 'lsp-java
  :type 'boolean)

(defcustom lsp-java-completion-guess-arguments nil
  "When set to true, method arguments are guessed when a method is selected from as list of code assist proposals."
  :group 'lsp-java
  :type 'boolean)

(defvar lsp-java--download-root "https://raw.githubusercontent.com/emacs-lsp/lsp-java/master/install/")

(defun lsp-java--json-bool (param)
  "Return a PARAM for setting parsable by json.el for booleans."
  (or param :json-false))

(defun lsp-java--list-or-empty (param)
  "Return either PARAM or empty vector in case PARAM is nil."
  (or param (vector)))

(defun lsp-java--settings ()
  "JDT settings."
  `((java
     (jdt
      (ls
       (vmargs . ,(string-join lsp-java-vmargs " "))))
     (errors
      (incompleteClasspath
       (severity . ,lsp-java-incomplete-classpath)))
     (configuration
      (updateBuildConfiguration . ,lsp-java-update-build-configuration)
      (maven))
     (trace
      (server . ,lsp-java-trace-server))
     (import
      (gradle
       (enabled . ,(lsp-java--json-bool lsp-java-import-gradle-enabled)))
      (maven
       (enabled . ,(lsp-java--json-bool lsp-java-import-maven-enabled)))
      (exclusions . ,(lsp-java--list-or-empty lsp-java-import-exclusions)))
     (referencesCodeLens
      (enabled . t))
     (progressReports
      (enabled . ,(lsp-java--json-bool lsp-java-progress-report)))
     (signatureHelp
      (enabled . t))
     (implementationsCodeLens
      (enabled . t))
     (format
      (enabled . ,(lsp-java--json-bool lsp-java-format-enabled))
      (settings
       (profile . ,lsp-java-format-settings-profile)
       (url . ,lsp-java-format-settings-url))
      (comments
       (enabled . ,(lsp-java--json-bool lsp-java-format-comments-enabled))))
     (saveActions
      (organizeImports . ,(lsp-java--json-bool lsp-java-save-action-organize-imports)))
     (contentProvider)
     (autobuild
      (enabled . ,(lsp-java--json-bool lsp-java-auto-build)))
     (completion
      (favoriteStaticMembers . ,(lsp-java--list-or-empty lsp-java-favorite-static-members))
      (importOrder . ,(lsp-java--list-or-empty lsp-java-import-order))
      (guessMethodArguments . ,lsp-java-completion-guess-arguments)))))

(defvar lsp-java-buffer-configurations
  `(("*classpath*" . ((side . right) (slot . 10) (window-width . 0.20)))))

(defun lsp-java-show-buffer (buf)
  "Show BUF according to defined rules."
  (let ((win (display-buffer-in-side-window buf
                                            (or (-> buf
                                                    buffer-name
                                                    (assoc lsp-java-buffer-configurations)
                                                    rest)
                                                '((side . right)
                                                  (slot . 1)
                                                  (window-width . 0.20))))))
    (set-window-dedicated-p win t)
    (select-window win)))

(defun lsp-java--locate-server-jar ()
  "Return the jar file location of the language server.

The entry point of the language server is in `lsp-java-server-install-dir'/plugins/org.eclipse.equinox.launcher_`version'.jar."
  (let ((plugindir (expand-file-name "plugins" lsp-java-server-install-dir)))
    (unless (file-directory-p plugindir)
      (if (yes-or-no-p "Server is not installed. Do you want to install it?")
          (lsp-java--ensure-server)
        (error "LSP Java cannot be started without JDT LS Server")))
    (let ((server-jar-filenames (directory-files plugindir t "org.eclipse.equinox.launcher_.*.jar$")))
      (if (not (= (length server-jar-filenames) 1))
          (error (format "Unable to find single point of entry %s" server-jar-filenames))
        (car server-jar-filenames)))))

(defun lsp-java--locate-server-config ()
  "Return the server config based on OS."
  (let ((config (cond
                 ((string-equal system-type "windows-nt") ; Microsoft Windows
                  "config_win")
                 ((string-equal system-type "darwin") ; Mac OS X
                  "config_mac")
                 (t "config_linux"))))
    (let ((inhibit-message t))
      (message (format "using config for %s" config)))
    (expand-file-name config lsp-java-server-install-dir)))

(defun lsp-java-organize-imports ()
  "Organize java imports."
  (interactive)
  (lsp--send-execute-command
   "java.edit.organizeImports"
   (list (lsp--path-to-uri buffer-file-name))))

(defun lsp-java-build-project (&optional full)
  "Perform project build action.

FULL specify whether full or incremental build will be performed."
  (interactive "P" )
  (lsp-send-notification
   (lsp-make-request "java/buildWorkspace" (if full t :json-false))))

(defun lsp-java-update-project-configuration ()
  "Update project configuration."
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (if-let ((lsp--cur-workspace (or lsp--cur-workspace
                                     (gethash (lsp-java--get-root) lsp--workspaces))))
        (if (or (string= file-name "pom.xml") (string-match "\\.gradle\\'" file-name))
            (lsp-send-notification
             (lsp-make-request "java/projectConfigurationUpdate"
                               (list :uri (lsp--buffer-uri))))
          (error "Update configuration could be called only from build file(pom.xml or gradle build file)"))
      (error "Unable to find workspace"))))

(defun lsp-java--ensure-dir (path)
  "Ensure that directory PATH exists."
  (unless (file-directory-p path)
    (make-directory path t)))

(defun lsp-java--get-java-version ()
  "Retrieve the java version from shell command."
  (let* ((java-version-output (shell-command-to-string (concat lsp-java-java-path " -version")))
         (version-string (nth 2 (split-string java-version-output))))
    (string-to-number (replace-regexp-in-string "\"" "" version-string))))

(defun lsp-java--java-9-plus-p ()
  "Check if java version is greater than or equal to 9."
  (let ((java-version (lsp-java--get-java-version)))
    (>= java-version 9)))

(defun lsp-java--ls-command ()
  "LS startup command."
  (let ((server-jar (lsp-java--locate-server-jar))
        (server-config (lsp-java--locate-server-config))
        (java-9-args (when (lsp-java--java-9-plus-p)
                       lsp-java-9-args)))
    (lsp-java--ensure-dir lsp-java-workspace-dir)
    `(,lsp-java-java-path
      "-Declipse.application=org.eclipse.jdt.ls.core.id1"
      "-Dosgi.bundles.defaultStartLevel=4"
      "-Declipse.product=org.eclipse.jdt.ls.core.product"
      "-Dlog.protocol=true"
      "-Dlog.level=ALL"
      ,@lsp-java-vmargs
      "-jar"
      ,server-jar
      "-configuration"
      ,server-config
      "-data"
      ,lsp-java-workspace-dir
      ,@java-9-args)))

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
  "Callback for client initialized.

WORKSPACE is the currently active workspace.
PARAMS the parameters for language status notifications."
  (let ((status (gethash "type" params))
        (current-status (lsp-workspace-get-metadata "status" workspace)))
    ;; process the status message only if there is no status or if the status is
    ;; starting (workaround for bug https://github.com/eclipse/eclipse.jdt.ls/issues/651)
    (when (not (and (or (string= current-status "Error" )
                        (string= current-status "Started" ))
                    (string= "Starting" status)))
      (lsp-workspace-status (concat "::" status) workspace)
      (lsp-workspace-set-metadata "status" status workspace)
      (let ((inhibit-message lsp-inhibit-message))
        (message "%s[%s]" (gethash "message" params) (gethash "type" params))))))

(defun lsp-java--apply-workspace-edit (action)
  "Callback for java/applyWorkspaceEdit.

ACTION is the action to execute."
  (lsp--apply-workspace-edit (car (gethash "arguments" action))))

(defun lsp-java--actionable-notification-callback (workspace params)
  "Handler for actionable notifications.

WORKSPACE is the currently active workspace.
PARAMS the parameters for actionable notifications."
  (let* ((project-root (lsp-java--get-root))
         (classpath-incomplete-p (cl-find-if (lambda (command)
                                               (string= (gethash "command" command)
                                                        "java.ignoreIncompleteClasspath.help"))
                                             (gethash "commands" params)))
         (choices (list (format "Import project \"%s.\"" project-root)
                        "Import project by selecting root directory interactively."
                        (format "Do not ask more for the current project(add \"%s\" to lsp-project-blacklist)" project-root)
                        "Do nothing."))
         (action-index (when (and classpath-incomplete-p (not (member project-root lsp-project-blacklist)))
                         (condition-case nil
                             (cl-position
                              (completing-read (format "%s is not part of any project. Select action: "
                                                       (buffer-name))
                                               choices
                                               nil
                                               t)
                              choices
                              :test 'equal)
                           ('quit)))))
    (case action-index
      (0 (lsp-workspace-folders-add (list project-root)))
      (1 (call-interactively 'lsp-workspace-folders-add))
      (2 (add-to-list 'lsp-project-blacklist project-root))
      (t (let ((notifications (or (lsp-workspace-get-metadata
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
                     'face 'warning))))))))

(defun lsp-java--progress-report (_workspace params)
  "Progress report handling.

PARAMS progress report notification data."
  (let ((inhibit-message lsp-inhibit-message))
    (message "%s%s" (gethash "status" params) (if (gethash "complete" params) " (done)" ""))))

(defun lsp-java--render-string (str)
  "Render STR with `java-mode' syntax highlight."
  (condition-case nil
      (with-temp-buffer
        (delay-mode-hooks (java-mode))
        (insert str)
        (font-lock-ensure)
        (buffer-string))
    (error str)))

(defun lsp-java--render-markup (string)
  "Render STRING as markup."
  (string-trim-right
   (with-temp-buffer
     (insert string)
     (delay-mode-hooks
       (make-local-variable 'markdown-code-lang-modes)
       (add-to-list 'markdown-code-lang-modes (cons "java" 'java-mode))
       (setq-local markdown-fontify-code-blocks-natively t)
       (setq-local markdown-fontify-code-block-default-mode "java")
       (setq-local markdown-hide-markup t)

       (let ((inhibit-message t)) (gfm-view-mode))
       (ignore-errors (font-lock-ensure)))
     (buffer-string))))

(defun lsp-java--prepare-mvnw ()
  "Download mvnw."
  (let ((mvn-executable (if (string-equal system-type "windows-nt")
                            "mvnw.cmd"
                          "mvnw"))
        (downloader ".mvn/wrapper/MavenWrapperDownloader.java")
        (properties ".mvn/wrapper/maven-wrapper.properties"))
    (mkdir ".mvn/wrapper/" t)
    (url-copy-file (concat lsp-java--download-root mvn-executable)
                   mvn-executable
                   t)
    (url-copy-file (concat lsp-java--download-root downloader) downloader t)
    (url-copy-file (concat lsp-java--download-root properties) properties t)
    (if (string= system-type "windows-nt")
        mvn-executable
      (concat "sh " mvn-executable))))

(defun lsp-java--bundles-dir ()
  "Get default bundles dir."
  (concat (file-name-as-directory lsp-java-server-install-dir) "bundles"))

(defun lsp-java--ensure-server ()
  "Ensure that JDT server and the other configuration."
  (let* ((default-directory (concat temporary-file-directory "lsp-java-install/")))
    (when (file-directory-p default-directory)
      (delete-directory default-directory t))
    (when (file-directory-p lsp-java-server-install-dir)
      (delete-directory lsp-java-server-install-dir t))
    (mkdir default-directory t)
    (url-copy-file (concat lsp-java--download-root "pom.xml") "pom.xml" t)
    (let ((full-command (format
                         "%s -Djdt.js.server.root=%s -Djunit.runner.root=%s -Djunit.runner.fileName=%s -Djava.debug.root=%s clean package"
                         (or (lsp-java--prepare-mvnw) (executable-find "mvn"))
                         (expand-file-name lsp-java-server-install-dir)
                         (expand-file-name
                          (if (boundp 'dap-java-test-runner)
                              (file-name-directory dap-java-test-runner)
                            (concat (file-name-directory lsp-java-server-install-dir) "test-runner")))
                         (if (boundp 'dap-java-test-runner)
                             (file-name-nondirectory (directory-file-name dap-java-test-runner))
                           "junit-platform-console-standalone.jar")
                         (expand-file-name (lsp-java--bundles-dir)))))
      (message "Running %s" full-command)
      (shell-command full-command))))

(defun lsp-java-update-server ()
  "Update LDT LS server."
  (interactive)
  (message "Server update started...")
  (lsp-java--ensure-server)
  (message "Server update finished..."))

(defun lsp-java--folders-change (&rest _)
  "Handler for folder's change."
  (lsp-java-update-project-uris lsp--cur-workspace))

(defun lsp-java--workspace-notify (&rest _args)
  "Workspace notify handler."
  (lsp-java-update-project-uris lsp--cur-workspace))

(defun lsp-java--client-initialized (client)
  "Callback for CLIENT initialized."
  (lsp-client-on-notification client "language/status" 'lsp-java--language-status-callback)
  (lsp-client-on-notification client "language/actionableNotification" 'lsp-java--actionable-notification-callback)
  (lsp-client-on-notification client "language/progressReport" 'lsp-java--progress-report)
  (lsp-client-on-notification client "workspace/notify" 'lsp-java--workspace-notify)
  (lsp-client-on-action client "java.apply.workspaceEdit" 'lsp-java--apply-workspace-edit)
  (lsp-client-register-uri-handler client "jdt" 'lsp-java--resolve-uri)
  (lsp-client-register-uri-handler client "chelib" 'lsp-java--resolve-uri)

  (lsp-provide-marked-string-renderer client "java" #'lsp-java--render-string)
  (lsp-provide-default-marked-string-renderer client #'lsp-java--render-markup)
  (add-hook 'lsp-workspace-folders-change 'lsp-java--folders-change))

(defun lsp-java--get-filename (url)
  "Get the name of the buffer calculating it based on URL."
  (or (save-match-data
        (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" url)
          (format "%s.java"
                  (replace-regexp-in-string "/" "." (match-string 2 url) t t))))
      (save-match-data
        (when (string-match "chelib://\\(.*\\)" url)
          (let ((matched (match-string 1 url)))
            (replace-regexp-in-string (regexp-quote ".jar") "jar" matched t t))))
      (error "Unable to match %s" url)))

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
      (lsp-java--ensure-dir (file-name-directory file-location))
      (let ((content (lsp-send-request (lsp-make-request
                                        "java/classFileContents"
                                        (list :uri uri)))))
        (with-temp-file file-location
          (insert content))
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

(defun lsp-java-execute-matching-action (regexp &optional not-found-message)
  "Execute the code action which title match the REGEXP.
NOT-FOUND-MESSAGE will be used if there is no matching action."
  (let ((actions (cl-remove-if-not
                  (lambda (item) (string-match regexp (gethash "title" item)))
                  (lsp-get-or-calculate-code-actions))))
    (case (length actions)
      (0 (error (or not-found-message "Unable to find action")))
      (1 (lsp-execute-code-action (car actions)))
      (t (lsp-execute-code-action (lsp--select-action actions))))))

(defun lsp-java-extract-to-local-variable (arg)
  "Extract local variable refactoring.
The prefix ARG and `cider-prompt-for-symbol' decide whether to
extract all or only the current occurrence."
  (interactive "P")
  (lsp-java-execute-matching-action
   (if arg
       "Extract to local variable$"
     "Extract to local variable (replace all occurrences)")))

(defun lsp-java-extract-to-constant ()
  "Extract constant refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Extract to constant"))

(defun lsp-java-add-unimplemented-methods ()
  "Extract constant refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Add unimplemented methods"))

(defun lsp-java-create-parameter ()
  "Create parameter refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Create parameter '"))

(defun lsp-java-create-field ()
  "Create field refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Create field '"))

(defun lsp-java-create-local ()
  "Create local refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Create local variable"))

(defun lsp-java-extract-method ()
  "Extract method refactoring."
  (interactive)
  (lsp-java-execute-matching-action "Extract to method"))

(defun lsp-java-add-import ()
  "Add missing import."
  (interactive)
  (lsp-java-execute-matching-action "Import '.*'"))

(defun lsp-java--bundles ()
  "Get lsp java bundles."
  (let ((bundles-dir (lsp-java--bundles-dir)))
    (append lsp-java-bundles (when (file-directory-p bundles-dir)
                               (directory-files bundles-dir t "\\.jar$")))))

(lsp-define-stdio-client lsp-java "java" (lambda () lsp-java-workspace-dir)
                         (lsp-java--ls-command)
                         :ignore-regexps
                         '("^SLF4J: "
                           "^Listening for transport dt_socket at address: ")
                         :extra-init-params (list :settings (lsp-java--settings)
                                                  :extendedClientCapabilities (list :progressReportProvider t
                                                                                    :classFileContentsSupport t)
                                                  :bundles (lsp-java--bundles))
                         :initialize 'lsp-java--client-initialized)

(defun lsp-java-update-user-settings ()
  "Update user settings.

The method could be called after changing configuration
property (e. g. `lsp-java-organize-imports') to update the
server."
  (interactive)
  (lsp--set-configuration (lsp-java--settings)))

(defun lsp-java--after-start (&rest _args)
  "Run after `lsp-java-enable' to configure workspace folders."
  ;; patch server capabilities since jdt server does not declare
  ;; executeCommandProvider capability required by `lsp-mode'
  (puthash "executeCommandProvider" t (lsp--server-capabilities))
  ;; mark the cache dir as part of the current project
  (puthash lsp-java-workspace-cache-dir lsp--cur-workspace lsp--workspaces)

  (when lsp-java-enable-file-watch
    (with-demoted-errors
        "Failed to register watches with following message: %S "
      (lsp-workspace-register-watch
       (mapcar (lambda (folder)
                 (list folder
                       '("**/*.java"
                         "**/pom.xml"
                         "**/*.gradle"
                         "**/.project"
                         "**/.classpath"
                         "**/settings/*.prefs")))
               lsp-java--workspace-folders))))
  (unless (gethash "initialized" (lsp--workspace-metadata lsp--cur-workspace))
    (lsp-java-update-user-settings)
    (puthash "initialized" t (lsp--workspace-metadata lsp--cur-workspace)))

  (lsp-java-update-project-uris lsp--cur-workspace))

(defun lsp-java-update-project-uris (&optional workspace)
  "Update WORKSPACE project uris."
  (interactive)
  (setq workspace (or workspace lsp--cur-workspace))
  (with-lsp-workspace workspace
    (->> workspace
         lsp--workspace-workspace-folders
         (--map (or (lsp-send-execute-command "che.jdt.ls.extension.mavenProjects"
                                              (lsp--path-to-uri it))
                    (lsp--path-to-uri (file-name-as-directory it))))
         -flatten
         -uniq
         (lsp-java--set-project-uris workspace))))

(defun lsp-java--set-project-uris (workspace project-uris)
  "Set WORKSPACE project uri list to PROJECT-URIS."
  (puthash "project-uris" project-uris (lsp--workspace-metadata workspace)))

(defun lsp-java--get-project-uris (workspace)
  "Get WORKSPACE maven projects."
  (let ((current-workspace-folders (lsp--workspace-workspace-folders workspace))
        (workspace-metadata (lsp--workspace-metadata workspace)))
    ;; update the project uri only if the workspace folders has changed after
    ;; the last call.
    ;; TODO investigate a better way to do that.
    (if (eq current-workspace-folders (gethash "last-workspace-folders" workspace-metadata))
        (gethash "project-uris" workspace-metadata)
      (lsp-java-update-project-uris workspace)
      (puthash "last-workspace-folders" current-workspace-folders workspace-metadata))))

(defun lsp-java--find-workspace (file-uri)
  "Return the workspace corresponding FILE-URI."
  (->> lsp--workspaces
       ht-values
       -uniq
       (--first (-some? (lambda (project-uri)
                          (s-starts-with? (lsp--uri-to-path project-uri)
                                          (lsp--uri-to-path file-uri)))
                        (lsp-java--get-project-uris it)))))

(defun lsp-java--find-project-uri (file-uri)
  "Return the java project corresponding FILE-URI."
  (->> lsp--workspaces
       ht-values
       -uniq
       (-map 'lsp-java--get-project-uris)
       -flatten
       (--filter (s-starts-with? (lsp--uri-to-path it)
                                 (lsp--uri-to-path file-uri)))
       (-max-by (lambda (project-a project-b)
                  (> (length project-a)
                     (length project-b))))))

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

(defun lsp-java--nearest-widget ()
  "Return widget at point or next nearest widget."
  (or (widget-at)
      (ignore-errors
        (let ((pos (point)))
          (widget-forward 1)
          (and (< pos (point))
               (widget-at))))))

(defun lsp-java--tree-under-cursor ()
  "Get tree under cursor."
  (-when-let (widget-under-cursor (lsp-java--nearest-widget))
    (if (tree-widget-p widget-under-cursor )
        widget-under-cursor
      (widget-get widget-under-cursor :parent))))

(defun lsp-java-classpath-open ()
  "Open object at path."
  (interactive)
  (if-let ((file (widget-get (lsp-java--tree-under-cursor) :file)))
      (if (file-exists-p file)
          (find-file file)
        (user-error "File %s does not exists" file))
    (user-error "No file under cursor")))

(defvar lsp-java-classpath-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'lsp-java-classpath-open)
    map))

(define-derived-mode lsp-java-classpath-mode special-mode "lsp-java-classpath"
  "Minor mode for browsing classpath.")

(define-widget 'lsp-java-widget-guide 'item
  "Vertical guide line."
  :tag       " "
  :format    "%t")

(define-widget 'lsp-java-widget-end-guide 'item
  "End of a vertical guide line."
  :tag       " "
  :format    "%t")

(define-widget 'lsp-java-widget-handle 'item
  "Horizontal guide line that joins a vertical guide line to a node."
  :tag       " "
  :format    "%t")

(defmacro lsp-java-define-widget (name &optional image-open image-closed image-empty)
  "Helper for defining widget icons.
NAME will be the name of the widget icon.
IMAGE-OPEN will be used when the widget is open.
IMAGE-CLOSED will be when the tree is closed.
IMAGE-EMPTY will be used when the tree widget is empty."
  (let ((open-icon (make-symbol (format "lsp-java-%s-open" name)))
        (close-icon (make-symbol (format "lsp-java-%s-close" name)))
        (empty-icon (make-symbol (format "lsp-java-%s-empty" name)))
        (leaf-icon (make-symbol (format "lsp-java-%s-leaf" name))))
    `(progn
       (define-widget (quote ,open-icon) 'tree-widget-icon
         "Icon for a open tree-widget node."
         :tag        "[+]"
         :glyph-name ,(or image-open name))
       (define-widget (quote ,close-icon) 'tree-widget-icon
         "Icon for a closed tree-widget node."
         :tag        "[-]"
         :glyph-name ,(or image-closed image-open name))
       (define-widget (quote ,empty-icon) 'tree-widget-icon
         "Icon for a closed tree-widget node."
         :tag        "[.]"
         :glyph-name ,(or image-empty image-open name))
       (list :open-icon (quote ,open-icon)
             :close-icon (quote ,close-icon)
             :empty-icon (quote ,empty-icon)
             :leaf-icon (quote ,leaf-icon)
             :handle 'lsp-java-widget-handle
             :end-guide 'lsp-java-widget-end-guide
             :guide 'lsp-java-widget-guide))))

(defvar lsp-java-icons-file-type-jar (lsp-java-define-widget "file_type_jar"))
(defvar lsp-java-icons-file-type-class (lsp-java-define-widget "file_type_class"))
(defvar lsp-java-icons-folder-type-component (lsp-java-define-widget "folder_type_component"
                                                                     "folder_type_component_opened"
                                                                     "folder_type_component"))
(defvar lsp-java-icons-default-folder (lsp-java-define-widget "default_folder"
                                                              "default_folder_opened"
                                                              "default_folder"))
(defvar lsp-java-icons-folder-type-library (lsp-java-define-widget "folder_type_library"
                                                                   "folder_type_library_opened"
                                                                   "folder_type_library"))
(defvar lsp-java-icons-default-root-folder (lsp-java-define-widget "default_root_folder"
                                                                   "default_root_folder_opened"
                                                                   "default_root_folder"))
(defvar lsp-java-icons-folder-type-maven (lsp-java-define-widget "folder_type_maven"
                                                                 "folder_type_maven_opened"
                                                                 "folder_type_maven"))

(defvar lsp-java-icons-error (lsp-java-define-widget "error"))
(defvar lsp-java-icons-warning (lsp-java-define-widget "warning"))
(defvar lsp-java-icons-info (lsp-java-define-widget "info"))

(defun lsp-java--classpath-get-icon (kind)
  "Get the icon corresponding to KIND."
  (pcase kind
    (1 lsp-java-icons-file-type-jar)
    (2 lsp-java-icons-default-root-folder)
    (3 lsp-java-icons-default-folder)
    (5 lsp-java-icons-folder-type-library)))

(defun lsp-java--classpath-get-node (path kind project-uri)
  "Get the icon corresponding to KIND.
PATH to the item.
PROJECT-URI uri of the item."
  (pcase kind
    (1 `(push-button :format "%[%t%]\n"
                     :tag ,(-> path lsp--path-to-uri f-filename)))
    (2 `(push-button :format "%[%t%]\n"
                     :tag ,(-> path lsp--path-to-uri f-filename)))
    (3 `(push-button :format "%[%t%]\n"
                     :tag ,(f-relative (lsp--uri-to-path path) (lsp--uri-to-path project-uri))))
    (5 `(push-button :format "%[%t%]\n"
                     :tag ,path))))

(defun lsp-java--classpath-render-classpath (classpath-entry project-uri)
  "Render CLASSPATH-ENTRY comming from PROJECT-URI."
  (-let* (((&hash "path" "children" "entryKind" kind) classpath-entry)
          (icons (lsp-java--classpath-get-icon kind))
          (node (lsp-java--classpath-get-node path kind project-uri)))
    `(tree-widget :node ,node
                  :open t
                  :file ,(lsp--uri-to-path path)
                  ,@icons
                  ,@(--map (lsp-java--classpath-render-classpath it project-uri) children))))

(defun lsp-java-classpath-browse ()
  "Show currently active sessions."
  (interactive)
  (let ((uri (lsp--path-to-uri (or buffer-file-name (f-canonical default-directory)))))
    (--if-let (or lsp--cur-workspace (lsp-java--find-workspace uri))
        (with-lsp-workspace it
          (if-let (project-uri (lsp-java--find-project-uri buffer-file-name))
              (let ((inhibit-read-only t)
                    (buf (get-buffer-create "*classpath*")))
                (with-current-buffer buf
                  (erase-buffer)
                  (lsp-java-classpath-mode)
                  (setq-local lsp--cur-workspace it)

                  (when lsp-java-themes-directory
                    (setq-local tree-widget-themes-directory lsp-java-themes-directory))

                  (when lsp-java-theme
                    (tree-widget-set-theme lsp-java-theme))

                  (widget-create
                   `(tree-widget
                     :node (push-button :format "%[%t%]\n"
                                        :tag ,(f-filename (lsp--uri-to-path project-uri)))
                     :open t
                     :file ,(lsp--uri-to-path project-uri)
                     ,@lsp-java-icons-default-root-folder
                     ,@(--map (lsp-java--classpath-render-classpath it project-uri)
                              (lsp-send-execute-command "che.jdt.ls.extension.classpathTree" project-uri))))
                  (run-hooks 'lsp-java-classpath-mode-hook))
                (funcall lsp-java-pop-buffer-function buf)
                (goto-char (point-min)))
            (user-error "Failed to calculate project for buffer %s" (buffer-name))))
      (user-error "Unable to find workspace for buffer %s" (buffer-name)))))

(provide 'lsp-java)
;;; lsp-java.el ends here
