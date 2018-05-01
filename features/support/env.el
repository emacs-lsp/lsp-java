(require 'f)

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq lsp-java-root-path project-directory))

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
  (require 'lsp-java)
  (require 'lsp-mode)
  (require 'lsp-java))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
