[![MELPA](https://melpa.org/packages/lsp-java-badge.svg)](https://melpa.org/#/lsp-java)
[![Build Status](https://travis-ci.com/emacs-lsp/lsp-java.svg?branch=master)](https://travis-ci.com/emacs-lsp/lsp-java)

Java support for lsp-mode using the [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls).

## Features
LSP java mode supports the following JDT Features:
* As you type reporting of parsing and compilation errors (via [flycheck](https://github.com/flycheck/flycheck)/[lsp-ui](https://github.com/emacs-lsp/lsp-ui))
* Code completion - using [company-lsp](https://github.com/tigersoldier/company-lsp) or builtin ```complete-at-point```
* Javadoc hovers - using [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
* Code actions - using [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
* Code outline - using builtin [imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html)
* Code navigation - using builtin [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
* Code lens (references/implementations) - using builtin [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
* Highlights
* Code formatting
* Maven pom.xml project support
* Limited Gradle support

## Installation
### Install LSP Java
The recommended way to install LSP Java is via `package.el` - the built-in package
manager in Emacs.
LSP Java is available on the two major `package.el` community
maintained repos -
[MELPA Stable](http://stable.melpa.org)
and [MELPA](http://melpa.org).

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `lsp-java` <kbd>[RET]</kbd>

Then add the following lines to your `.emacs` and open a file from the any of the specified projects.
```emacs-lisp
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp-java-enable)

;; set the projects that are going to be imported into the workspace.
(setq lsp-java--workspace-folders (list "/path/to/project1"
                                        "/path/to/project2"
                                        ...))
```

### Install [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls)
Download either [latest](http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz) or [a specific version](http://download.eclipse.org/jdtls/snapshots/?d) of Eclipse JDT Language Server distribution to `~/.emacs.d/eclipse.jdt.ls/server/`

If you choose to have the server installed in a different directory, set `lsp-java-server-install-dir`.

On Linux/MacOS you could install/update [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls) via running the following commands:

```bash
rm -rf ~/.emacs.d/eclipse.jdt.ls/server/
mkdir -p ~/.emacs.d/eclipse.jdt.ls/server/
wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz -O /tmp/jdt-latest.tar
tar xf /tmp/jdt-latest.tar -C ~/.emacs.d/eclipse.jdt.ls/server/
```

## Supported commands
### LSP Mode commands
  | Command name                | Description                                                  |
  |-----------------------------|--------------------------------------------------------------|
  | lsp-execute-code-action     | Execute code action.                                         |
  | lsp-rename                  | Rename symbol at point                                       |
  | lsp-describe-thing-at-point | Display help for the thing at point.                         |
  | lsp-workspace-restart       | Restart project                                              |
  | lsp-format-buffer           | Format current buffer                                        |
  | lsp-symbol-highlight        | Highlight all relevant references to the symbol under point. |
### LSP Java commands

  | Command name                          | Description                                                   |
  |---------------------------------------|---------------------------------------------------------------|
  | lsp-java-organize-imports             | Organize imports                                              |
  | lsp-java-build-project                | Perform partial or full build for the projects                |
  | lsp-java-update-project-configuration | Update project configuration                                  |
  | lsp-java-actionable-notifications     | Resolve actionble notifications                               |
  | lsp-java-update-user-settings         | Update user settings (Check the options in the table bellow.) |
#### Refactoring

LSP Java provides rich set of refactorings via [Eclipse JDT Language
Server](https://projects.eclipse.org/projects/eclipse.jdt.ls) code actions and
some of them are bound to Emacs commands:

  | Command name                      | Description                  |
  |-----------------------------------|------------------------------|
  | lsp-java-extract-to-constant      | Extract constant refactoring |
  | lsp-java-add-unimplmented-methods | Extract constant refactoring |
  | lsp-java-create-parameter         | Create parameter refactoring |
  | lsp-java-create-field             | Create field refactoring     |
  | lsp-java-create-local             | Create local refactoring     |
  | lsp-java-extract-method           | Extract method refactoring   |
  | lsp-java-add-import               | Add missing import           |
## Supported settings
  | Setting                               | Description                                                                            |
  |---------------------------------------|----------------------------------------------------------------------------------------|
  | lsp-java-server-install-dir           | Install directory for eclipsejdtls-server                                              |
  | lsp-java-java-path                    | Path of the java executable                                                            |
  | lsp-java-workspace-dir                | LSP java workspace directory                                                           |
  | lsp-java-workspace-cache-dir          | LSP java workspace cache directory                                                     |
  | lsp-java--workspace-folders           | LSP java workspace folders storing files downloaded from JDT                           |
  | lsp-java-vmargs                       | Specifies extra VM arguments used to launch the Java Language Server                   |
  | lsp-java-incomplete-classpath         | Specifies the severity of the message when the classpath is incomplete for a Java file |
  | lsp-java-update-build-configuration   | Specifies how modifications on build files update the Java classpath/configuration     |
  | lsp-java-import-exclusions            | Configure glob patterns for excluding folders                                          |
  | lsp-java-favorite-static-members      | Defines a list of static members or types with static members                          |
  | lsp-java-import-order                 | Defines the sorting order of import statements                                         |
  | lsp-java-trace-server                 | Traces the communication between Emacs and the Java language server                    |
  | lsp-java-enable-file-watch            | Defines whether the client will monitor the files for changes                          |
  | lsp-java-format-enabled               | Specifies whether or not formatting is enabled on the language server                  |
  | lsp-java-format-settings-url          | Specifies the file path to the formatter xml url                                       |
  | lsp-java-format-settings-profile      | Specifies the formatter profile name                                                   |
  | lsp-java-format-comments-enabled      | Preference key used to include the comments during the formatting                      |
  | lsp-java-save-action-organize-imports | Organize imports on save                                                               |
  | lsp-java-organize-imports             | Specifies whether or not organize imports is enabled as a save action                  |
  | lsp-java-bundles                      | List of bundles that will be loaded in the JDT server                                  |
  | lsp-java-import-gradle-enabled        | Enable/disable the Gradle importer                                                     |
  | lsp-java-import-maven-enabled         | Enable/disable the Maven importer                                                      |
  | lsp-java-auto-build                   | Enable/disable the 'auto build'                                                        |
  | lsp-java-progress-report              | [Experimental] Enable/disable progress reports from background processes on the server |
## Screenshot
![demo](images/demo.png)
## Additional packages
* [lsp-ui](https://github.com/emacs-lsp/lsp-ui) : Flycheck, documentation and code actions support.
* [company-lsp](https://github.com/tigersoldier/company-lsp) : LSP company backend.
## FAQ
* LSP Java is showing to many debug messages, how to stop that?

Add the following configuration.
```emacs-lisp
(setq lsp-inhibit-message t)
```
* [lsp-ui](https://github.com/emacs-lsp/lsp-ui) does not display all of the actions on the current point(e. g "Extract constant")?

LSP UI by default sends current line bounds for action region which breaks forces JDT server to return only "Extract method action."
```emacs-lisp
(setq lsp-ui-sideline-update-mode 'point)
```
* LSP Java does not provide completion, go to definition for some of the files?

Make sure the project is properly imported according to instructions. When particular file is not part of imported project [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls) could not calculate the current classpath.
