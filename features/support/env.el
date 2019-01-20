(require 'f)

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (defvar lsp-java-root-path project-directory)
  (defvar lsp-java-test-root (f-join temporary-file-directory "tests")))

(defvar lsp-java-support-path
  (f-dirname load-file-name))

(defvar lsp-java-features-path
  (f-parent lsp-java-support-path))

(defvar lsp-java-root-path
  (f-parent lsp-java-features-path))

(add-to-list 'load-path lsp-java-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'espuds)
  (require 'ert)
  (require 'lsp-java))

(Setup
 (unless (file-directory-p (expand-file-name "plugins" lsp-java-server-install-dir))
   (lsp-java-update-server)))

(Before
 (require 'lsp-java)
 (setq lsp-java-workspace-dir (f-join lsp-java-test-root "workspace")
       lsp-java-pop-buffer-function 'pop-to-buffer
       lsp-print-io t
       lsp-inhibit-message nil
       lsp-java-workspace-cache-dir (f-join lsp-java-test-root "workspace-cache")
       lsp-java-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/")
       lsp-response-timeout 30)
 (when (file-exists-p lsp-java-test-root)
   (delete-directory lsp-java-test-root t))
 (setq lsp--session (make-lsp-session)))

(After
 (mapc 'kill-buffer
       (seq-filter
        (lambda (b)
          (with-current-buffer b
            (equal 'java-mode major-mode)))
        (buffer-list))))

(Teardown
 ;; After when everything has been run
 )
