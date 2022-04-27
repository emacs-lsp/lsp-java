SHELL=/usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

build:
	$(EASK) package
	$(EASK) install

ci: build compile checkdoc lint

compile:
	@echo "Compiling..."
	@$(EASK) compile

# TODO: We cannot lint dap-mode
lint:
	@echo "package linting..."
	@$(EASK) lint lsp-java.el lsp-java-boot.el

clean:
	$(EASK) clean-all

# TODO: do we have tests?
test:
	$(EASK) install-deps --dev

.PHONY: build test compile checkdoc lint
