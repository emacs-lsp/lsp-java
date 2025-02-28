(package "lsp-java"
         "3.0"
         "Java support for lsp-mode")

(website-url "https://github.com/emacs-lsp/lsp-java")
(keywords "languages" "tools")

;; XXX: Linter will report errors because of the package main file; let's
;; disable this for now!
;;(package-file "lsp-java.el")

(files "*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "28.1")
(depends-on "lsp-mode")
(depends-on "markdown-mode")
(depends-on "dash")
(depends-on "f")
(depends-on "ht")
(depends-on "request")
(depends-on "treemacs")
(depends-on "dap-mode")

(development
 (depends-on "spinner")
 (depends-on "ecukes")
 (depends-on "espuds"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
