;;; lsp-java-test.el --- java test support           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs frontend of https://github.com/Microsoft/vscode-java-test

;;; Code:

(require 'lsp-mode)
(require 'lsp-java)
(require 'treemacs)

(defvar lsp-java-test--last-result nil)
(defvar lsp-java-test--state (ht))
(defvar lsp-java-test--last-run-state (ht))

(defcustom lsp-java-test-theme "Default"
  "The `lsp-java-test' theme."
  :type 'string)

(defvar lsp-test-java-browser-position-params
  `((side . ,treemacs-position)
    (slot . 4)
    (window-width . ,treemacs-width)))

(defvar lsp-test-java-report-position-params
  `((side . right)
    (slot . 5)
    (window-width . ,treemacs-width)))



(defun lsp-java-test--process-test-lens (lens)
  (-let [(test-data &as &hash "location" (&hash "range") "children") lens]
    (cons (-doto test-data
            (ht-set "range" range))
          (-mapcat #'lsp-java-test--process-test-lens children))))

(defface lsp-java-test-error-face
  '((t :height 1.0 :inherit error))
  "The face used for code lens overlays."
  :group 'lsp-faces)

(defface lsp-java-test-success-face
  '((t :height 1.0 :inherit success))
  "The face used for code lens overlays."
  :group 'lsp-faces)

(defface lsp-java-test-in-progress-face
  '((t :height 1.0 :inherit warn))
  "The face used for code lens overlays."
  :group 'lsp-faces)

(defvar-local lsp-java-test--last-callback nil)

(defun lsp-java-test-lens-backend (modified? callback)
  (setq-local lsp-java-test--last-callback callback)
  (lsp-request-async
   "workspace/executeCommand"
   (list :command "vscode.java.test.search.codelens"
         :arguments (->> (buffer-file-name)
                         lsp--path-to-uri
                         vector))
   (lambda (result)
     (let* ((lenses (-mapcat #'lsp-java-test--process-test-lens result))
            (all-lenses (append
                         (-map
                          (lambda (lens)
                            (-doto lens
                              (ht-set "command" (ht ("title" "Debug test")
                                                    ("command" (lambda ()
                                                                 (interactive)
                                                                 (lsp-java-test--start-test lens nil)))))))
                          lenses)
                         (-map
                          (lambda (lens)
                            (-doto (ht-copy lens)
                              (ht-set "command" (ht ("title" "Run test")
                                                    ("command" (lambda ()
                                                                 (interactive)
                                                                 (lsp-java-test--start-test lens t)))))))
                          lenses)
                         (-keep
                          (-lambda ((lens &as &hash "fullName" full-name))
                            (when-let (lens-properties (lsp-java-test--status full-name))
                              (-let [(title . face) lens-properties]
                                (-doto (ht-copy lens)
                                  (ht-set "command" (ht ("title" title)
                                                        ("face" face)
                                                        ("command" #'lsp-java-test-show-report)))))))
                          lenses))))
       (funcall callback all-lenses lsp--cur-version)))
   :mode 'detached))

;;;###autoload
(define-minor-mode lsp-java-test-lens-mode
  "Toggle code-lens overlays."
  :group 'lsp-java-test
  :global nil
  :init-value nil
  :lighter nil
  (cond
   (lsp-java-test-lens-mode
    (setq-local lsp-lens-backends (pushnew 'lsp-java-test-lens-backend lsp-lens-backends))
    (lsp--lens-refresh t))
   (t (setq-local lsp-lens-backends (delete 'lsp-java-test-lens-backend lsp-lens-backends))
      (setq-local lsp-java-boot--callback nil))))

(defun lsp-java-test-search (root level full-name)
  (lsp-java-with-jdtls
    (lsp-send-execute-command
     "vscode.java.test.search.items"
     (vector (json-serialize `(:uri ,root
                                    :level ,level
                                    ,@(when full-name (list :fullName full-name))))))))

(defun lsp-java-test-goto (&rest _)
  "Goto the symbol at point."
  (interactive)
  (-if-let ((&hash? "location" (&hash? "uri" "range" (&hash? "start"))) (-some-> (treemacs-node-at-point)
                                                                                 (button-get :item)))
      (progn
        (select-window (get-mru-window (selected-frame) nil :not-selected))
        (find-file (lsp--uri-to-path uri))
        (when start
          (goto-char (lsp--position-to-point start))))
    (user-error "No test under point.")))

(treemacs-define-expandable-node java-tests
  :icon-open-form (lsp-java-test--icon (-some-> (treemacs-node-at-point)
                                                (treemacs-button-get :item))
                                       t)
  :icon-closed-form (lsp-java-test--icon (-some-> (treemacs-node-at-point)
                                                  (treemacs-button-get :item))
                                         nil)
  :query-function (lsp-java-test-search (treemacs-button-get node :key)
                                        (or (treemacs-button-get node :level)
                                            1)
                                        (treemacs-button-get node :full-name))
  :ret-action 'lsp-java-test-goto
  :render-action
  (treemacs-render-node
   :icon (lsp-java-test--icon item nil)
   :label-form (gethash "displayName" item)
   :state treemacs-java-tests-closed-state
   :key-form (->> item
                  (gethash "location")
                  (gethash "uri"))
   :more-properties (:level (1+  (or (treemacs-button-get node :level)
                                     1))
                            :full-name (gethash "fullName" item)
                            :item item)))

(defun java-tests--roots ()
  (-uniq (gethash 'jdtls (lsp-session-server-id->folders (lsp-session)))))

(treemacs-modify-theme "Default"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/vscode")
  :config
  (progn
    (treemacs-create-icon :file "class.png" :extensions (java-test-class) :fallback "-")
    (treemacs-create-icon :file "debug.png" :extensions (java-test-debug) :fallback "-")
    (treemacs-create-icon :file "method.png" :extensions (java-test-method) :fallback "-")
    (treemacs-create-icon :file "package.png" :extensions (java-test-package) :fallback "-")
    (treemacs-create-icon :file "placeholder.png" :extensions (java-test-placeholder) :fallback "-")
    (treemacs-create-icon :file "refresh.png" :extensions (java-test-refresh) :fallback "-")
    (treemacs-create-icon :file "run.png" :extensions (java-test-run) :fallback "-")))

(defun lsp-java-test-right-click (event)
  (interactive "e")
  (let* ((ec (event-start event))
         (p1 (posn-point ec))
         (w1 (posn-window ec)))
    (select-window w1)
    (goto-char p1)
    (hl-line-highlight)
    (run-with-idle-timer
     0.001 nil
     (lambda ()
       (cl-labels ((check (value) (not (null value))))
         (let* ((node    (treemacs-node-at-point))
                (state   (-some-> node (treemacs-button-get :state)))
                (project (treemacs-project-at-point))
                (menu
                 (easy-menu-create-menu
                  nil
                  `(["Run Test"   lsp-java-test-run]
                    ["Debug Test" lsp-java-test-debug]
                    ["Refresh"    lsp-java-test-refresh])))
                (choice (x-popup-menu event menu)))
           (when choice (call-interactively (lookup-key menu (apply 'vector choice))))
           (hl-line-highlight)))))))

(defun lsp-java-test--wrap-icon (icon open? has-children?)
  (concat
   (cond
    ((and open? has-children?) " ▾ ")
    (has-children? " ▸ ")
    (t "   "))
   icon))

(defun lsp-java-test--icon (item open?)
  (lsp-java-test--wrap-icon
   (if item
       (cl-case (gethash "level" item)
         (1 (treemacs-get-icon-value 'java-test-package nil lsp-java-test-theme))
         (2 (treemacs-get-icon-value 'java-test-package nil lsp-java-test-theme))
         (3 (treemacs-get-icon-value 'java-test-class nil lsp-java-test-theme))
         (4 (treemacs-get-icon-value 'java-test-method nil lsp-java-test-theme)))
     (treemacs-get-icon-value 'root nil lsp-java-test-theme))
   open?
   (or (not item)
       (not (eq (gethash "level" item) 4)))))

(defvar lsp-java-test-mode-map
  (-doto (make-sparse-keymap)
    (define-key (kbd "x") #'lsp-java-test-run)
    (define-key (kbd "d") #'lsp-java-test-debug)
    (define-key (kbd "R") #'lsp-java-test-refresh)
    (define-key [mouse-1]  #'treemacs-TAB-action)
    (define-key [mouse-3]  #'lsp-java-test-right-click)
    (define-key [double-mouse-1]  #'treemacs-RET-action))
  "Keymap for `lsp-java-test-mode'.")

(define-minor-mode lsp-java-test-mode "Java Test Mode"
  nil nil lsp-java-test-mode-map)

(treemacs-define-variadic-node java-tests-list
  :query-function (java-tests--roots)
  :render-action
  (treemacs-render-node
   :icon
   (lsp-java-test--icon (-some-> (treemacs-node-at-point)
                                 (treemacs-button-get :item))
                        nil)
   :label-form (f-filename item)
   :state treemacs-java-tests-closed-state
   :key-form (lsp--path-to-uri item))
  :root-key-form 'LSP-Java-Tests)

(defun lsp-java-test--start-from-browser (no-debug)
  (if-let ((node (treemacs-node-at-point)))
      (lsp-java-test--start-test (or (treemacs-button-get node :item)
                                     (ht ("project" (treemacs-button-get node :key))
                                         ("level" 1)
                                         ("location" (ht ("uri" (treemacs-button-get node :key))))))
                                 t)
    (user-error "No test under point")))

(defun lsp-java-test-run ()
  "Run test under point."
  (interactive)
  (lsp-java-test--start-from-browser nil))

(defun lsp-java-test-debug ()
  (interactive)
  (lsp-java-test--start-from-browser t))

(defun lsp-java-test-refresh ()
  (interactive)
  (condition-case _err
      (let ((inhibit-read-only t))
        (with-current-buffer "*Java Tests*"
          (treemacs-update-node '(:custom LSP-Java-Tests) t)))
    (error)))

(defun lsp-java-test--update-test-content (test-data test-state)
  (-let [(&alist 'name state 'attributes (&alist 'name test-name)) test-data]
    (when (and (-contains? '("testFailed"
                             "testFinished"
                             "testStarted"
                             "suiteTreeNode")
                           state)
               (not (and (string= "testFinished" state)
                         (string= (gethash test-name test-state)
                                  "testFailed"))))
      (when (string= "suiteTreeNode" state)
        (setq test-name (concat (->> lsp-java-test--last-result
                                     (-keep (-lambda ((&alist 'name 'attributes (&alist 'name test-name)))
                                              (when (string= name "suiteTreeStarted")
                                                test-name)))
                                     (cl-first))
                                "#"
                                test-name)))
      (puthash test-name state test-state))))

(defun lsp-java-test--filter-function (line)
  (let ((json (cl-second (s-match "@@<TestRunner-\\(.*\\)-TestRunner>" line))))
    (cond
     (json (-let [(test-data &as &alist 'name) (json-read-from-string json)]
             (when (string= name "testReporterAttached")
               (setq lsp-java-test--last-result nil)
               (setq lsp-java-test--last-run-state (ht)))
             (push test-data lsp-java-test--last-result)

             (lsp-java-test--update-test-content test-data lsp-java-test--state)
             (lsp-java-test--update-test-content test-data lsp-java-test--last-run-state)

             (lsp-java-test--schedule-refresh-lens)
             (lsp-java-test--update-report)
             nil))
     ((s-equals? "\n" line) nil)
     (t line))))

(defvar lsp-java-test--refresh-lens-timer nil)

(defun lsp-java-test--do-refresh-lenses ()
  (->>
   (lsp-find-workspace 'jdtls nil)
   (lsp--workspace-buffers)
   (-map (lambda (buffer)
           (with-current-buffer buffer
             (when (and lsp-java-test-lens-mode lsp-java-test--last-callback)
               (lsp-java-test-lens-backend nil lsp-java-test--last-callback)))))))

(defun lsp-java-test--schedule-refresh-lens ()
  (when lsp-java-test--refresh-lens-timer
    (cancel-timer lsp-java-test--refresh-lens-timer))
  (setq lsp-java-test--refresh-lens-timer
        (run-at-time 0.2 nil #'lsp-java-test--do-refresh-lenses)))


(defconst lsp-java-test-kind-root 0)
(defconst lsp-java-test-kind-folder 1)
(defconst lsp-java-test-kind-package 2)
(defconst lsp-java-test-kind-class 3)
(defconst lsp-java-test-kind-method 4)

(defun lsp-java-test--get-tests  (test)
  (-let [(&hash "level" "location" (&hash? "uri") "fullName") test]
    (cond
     ((or (eq level lsp-java-test-kind-method)
          (eq level lsp-java-test-kind-class))
      (gethash "fullName" test))
     (t
      (s-join " " (-map #'lsp-java-test--get-tests (lsp-java-test-search uri level fullName)))))))

(defun lsp-java-test--start-test (test no-debug)
  (with-lsp-workspace (lsp-find-workspace 'jdtls nil)
    (dap-debug
     `(
       :type "java"
       :name ,(format "Running %s" (gethash "displayName" test))
       :mainClass "com.microsoft.java.test.runner.Launcher"
       :projectName ,(gethash "project" test)
       :output-filter-function lsp-java-test--filter-function
       :args ,(format "%s %s"
                      "junit"
                      (lsp-java-test--get-tests test))
       :cwd  ,(->> test
                   (gethash "location")
                   (gethash "uri")
                   (lsp--uri-to-path)
                   (lsp-workspace-root))
       :classPaths ,(apply #'vector
                           (cl-list*
                            (f-join lsp-java-test-root "com.microsoft.java.test.runner.jar")
                            (f-join lsp-java-test-root "/lib/")
                            (lsp-send-execute-command "vscode.java.test.runtime.classpath"
                                                      (->> test
                                                           (gethash "location")
                                                           (gethash "uri")
                                                           (lsp--uri-to-path)
                                                           (vector)
                                                           (vector)))))
       ,@(when no-debug `(:noDebug t))))))

(defun lsp-java-test-browser ()
  (interactive)
  (if-let ((buf (get-buffer "*Java Tests*")))
      (select-window (display-buffer-in-side-window buf lsp-test-java-browser-position-params))
    (let* ((buf (get-buffer-create "*Java Tests*"))
           (window (display-buffer-in-side-window buf lsp-test-java-browser-position-params)))
      (select-window window)
      (set-window-dedicated-p window t)
      (treemacs-initialize)
      (treemacs-JAVA-TESTS-LIST-extension)
      (lsp-java-test-mode)
      (setq-local header-line-format "TEST EXPLORER: "))))

(defun lsp-java-test--test-kind (path)
  (if (s-contains? "#" path)
      'java-test-method
    'java-test-class))

(defun lsp-java-test--duration (test)
  (->> lsp-java-test--last-result
       (-keep (-lambda ((&alist 'name 'attributes (&alist 'duration 'name test-name)))
                (when (and (or (string= name "testFinished")
                               (string= name "testFailed"))
                           (string= test-name test))
                  duration)))
       cl-first))

(defun lsp-java-test--trace (test)
  (->> lsp-java-test--last-result
       (-keep (-lambda ((&alist 'name 'attributes (&alist 'trace 'name test-name)))
                (when (and (string= name "testFailed")
                           (string= test-name test))
                  trace)))
       cl-first))

(defun lsp-java-test-report-open (&rest _rest)
  (interactive)
  (when-let (test (-some-> (treemacs-node-at-point)
                           (treemacs-button-get :key)))
    (let ((buf (get-buffer-create "*Test Stack*")))
      (let ((inhibit-read-only t))
        (with-current-buffer buf
          (erase-buffer)
          (insert (lsp-java-test--trace test))
          (view-mode t)
          (select-window (get-mru-window (selected-frame) nil :not-selected))
          (switch-to-buffer buf))))))

(defun lsp-java-test--status (test-name &optional state)
  (setq state (or state lsp-java-test--state))
  (if (s-contains? "#" test-name)
      (pcase (gethash test-name state)
        ("testFailed"
         (cons "❌" 'lsp-java-test-error-face))
        ("testFinished"
         (cons "✔" 'lsp-java-test-success-face))
        ("testStarted"
         (cons "⌛" 'lsp-java-test-in-progress-face))
        ("suiteTreeNode"
         (cons "⌚" 'lsp-java-test-in-progress-face))
        (_
         (cons "?" 'lsp-java-test-in-progress-face)))
    (let ((inner-test-status (-keep (-lambda ((method-name . test-status))
                                      (when (s-starts-with?
                                             (concat test-name "#")
                                             method-name)
                                        test-status))
                                    (ht->alist state))))
      (cond
       ((not inner-test-status) nil)
       ((-contains? inner-test-status "testFailed") (cons "❌" 'lsp-java-test-error-face))
       ((-contains? inner-test-status "testStarted") (cons "⌛" 'lsp-java-test-in-progress-face))
       ((-contains? inner-test-status "suiteTreeNode") (cons "⌚" 'lsp-java-test-in-progress-face))
       ((-contains? inner-test-status "testFinished") (cons "✔" 'lsp-java-test-success-face))))))

(treemacs-define-expandable-node java-test-report-node
  :icon-open-form
  (let ((kind (lsp-java-test--test-kind
               (treemacs-button-get node :key))))
    (lsp-java-test--wrap-icon
     (treemacs-get-icon-value kind
                              nil
                              lsp-java-test-theme)
     t
     (eq kind 'java-test-class)))
  :ret-action 'lsp-java-test-report-open
  :icon-closed-form
  (let ((kind (lsp-java-test--test-kind
               (treemacs-button-get node :key))))
    (lsp-java-test--wrap-icon
     (treemacs-get-icon-value kind
                              nil
                              lsp-java-test-theme)
     nil
     (eq kind 'java-test-class)))
  :query-function
  (let ((item (treemacs-button-get node :key)))
    (->> lsp-java-test--last-result
         (reverse)
         (-drop-while (-lambda ((&alist 'name 'attributes (&alist 'name node-name)))
                        (not (and (string= name "suiteTreeStarted")
                                  (string= node-name item)))))
         (cl-rest)
         (-take-while (-lambda ((&alist 'name))
                        (string= name "suiteTreeNode")))
         (-map (-lambda ((&alist 'attributes (&alist 'name node-name)))
                 node-name))))
  :render-action
  (treemacs-render-node
   :icon (lsp-java-test--wrap-icon
          (treemacs-get-icon-value 'java-test-method
                                   nil
                                   lsp-java-test-theme)
          nil
          nil)
   :label-form (-let* ((test (concat (treemacs-button-get (treemacs-node-at-point) :key) "#" item))
                       ((content . face) (lsp-java-test--status test lsp-java-test--last-run-state)))
                 (concat (propertize item 'face 'default)
                         (propertize " " 'face 'default)
                         (propertize content 'face face)
                         (condition-case _
                             (when-let (duration (lsp-java-test--duration test))
                               (propertize (concat " " duration " ms")
                                           'face 'default))
                           (error (message (error-message-string _))))))
   :state treemacs-java-test-report-node-closed-state
   :key-form (concat (treemacs-button-get (treemacs-node-at-point) :key) "#" item)))

(treemacs-define-variadic-node java-tests-report
  :query-function (-keep (-lambda ((&alist 'name 'attributes (&alist 'name node-name)))
                           (when (string= name "suiteTreeStarted")
                             node-name))
                         (reverse lsp-java-test--last-result))
  :render-action
  (treemacs-render-node
   :icon (lsp-java-test--wrap-icon
          (treemacs-get-icon-value 'java-test-class nil lsp-java-test-theme)
          t
          nil)
   :label-form (-let [(content . face) (lsp-java-test--status item lsp-java-test--last-run-state)]
                 (concat (propertize item 'face 'default)
                         (propertize " " 'face 'default)
                         (propertize content 'face face)))
   :state treemacs-java-test-report-node-closed-state
   :key-form item)
  :root-key-form 'LSP-Java-Test-Report)

(defun lsp-java-test--expand-recursively (root)
  (-map
   (lambda (btn)
     (unless (treemacs-is-node-expanded? btn)
       (save-excursion
         (goto-char (marker-position btn))
         (funcall (alist-get (treemacs-button-get btn :state) treemacs-TAB-actions-config))))
     (lsp-java-test--expand-recursively btn))
   (treemacs--get-children-of root)))

(defun lsp-java-test--expand (root-key)
  (-when-let (root (treemacs-dom-node->position (treemacs-find-in-dom root-key)))
    (treemacs-save-position
     (lsp-java-test--expand-recursively root))))

(defun lsp-java-test--update-report-modeline ()
  (setq-local mode-line-format
              (or (->> lsp-java-test--last-result
                       (-keep (-lambda ((&alist 'name 'attributes (&alist 'message)))
                                (when (string= "testSummary" name)
                                  message)))
                       cl-first)
                  (->> lsp-java-test--last-result
                       (-keep (-lambda ((&alist 'name 'attributes (&alist 'message)))
                                (when (string= "testSummary" name)
                                  message)))
                       cl-first)
                  "Running...")))

(defun lsp-java-test--update-report ()
  (when (buffer-live-p (get-buffer "*Java Tests Results*"))
    (condition-case _err
        (let ((inhibit-read-only t))
          (with-current-buffer "*Java Tests Results*"
            ;; (lsp-java-test--update-report-modeline)
            (treemacs-update-node '(:custom LSP-Java-Test-Report) t)
            (lsp-java-test--expand '(:custom LSP-Java-Test-Report))))
      (error))))


(defun lsp-java-test-show-report ()
  (interactive)
  (let ((original-buffer (current-buffer)))
    (let* ((buf (get-buffer-create "*Java Tests Results*"))
           (window (display-buffer-in-side-window buf lsp-test-java-report-position-params)))
      (select-window window)
      (set-window-dedicated-p window t)
      (treemacs-initialize)
      (treemacs-JAVA-TESTS-REPORT-extension)
      (setq-local header-line-format "TEST RESULTS:   ")
      (lsp-java-test--update-report-modeline)
      (lsp-java-test--expand '(:custom LSP-Java-Test-Report)))))

(provide 'lsp-java-test)
;;; lsp-java-test.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
