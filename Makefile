SHELL=/usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

INIT="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (package-refresh-contents))"

LINT="(progn \
		(unless (package-installed-p 'package-lint) \
		  (package-install 'package-lint)) \
		(require 'package-lint) \
		(package-lint-batch-and-exit))"

build:
	cask install

unix-ci: build unix-compile checkdoc lint

windows-ci: CASK=
windows-ci: clean windows-compile checkdoc lint

unix-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		*.el

windows-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-l test/windows-bootstrap.el \
		-L . -L clients \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(LSP-FILES)

lint:
	@echo "package linting..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval $(INIT) \
		--eval $(LINT) \
		*.el

clean:
	rm -rf .cask

.PHONY: build test compile checkdoc lint
