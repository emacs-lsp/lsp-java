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

lint:
	@echo "package linting..."
	@$(EASK) lint

clean:
	$(EASK) clean-all

# TODO: do we have tests?
test:
	$(EASK) install-deps --dev

.PHONY: build test compile checkdoc lint
