[![MELPA](https://melpa.org/packages/lsp-java-badge.svg)](https://melpa.org/#/lsp-java)

Java support for lsp-mode using the [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls).

## Installation
Clone this repository `https://github.com/emacs-lsp/lsp-mode` to a suitable path, and add
```emacs-lisp
(add-to-list 'load-path "<path to lsp-java>")
(add-to-list 'load-path "<path to lsp-mode>")
(require 'lsp-java)
(require 'lsp-mode)
(add-hook 'java-mode-hook #'lsp-mode)
```
to your `.emacs`.

It needs the eclipse jdt server in `~/.emacs.d/eclipse.jdt.ls/server/`

Download either [latest](http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz) or [a specific version](http://download.eclipse.org/jdtls/snapshots/?d) of Eclipse JDT Language Server distribution to `~/.emacs.d/eclipse.jdt.ls/server/`

If you choose to have the server installed in a different directory, set `lsp-java-server-install-dir`
