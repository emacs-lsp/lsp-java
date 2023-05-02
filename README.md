[![MELPA](https://melpa.org/packages/lsp-java-badge.svg)](https://melpa.org/#/lsp-java)
[![MELPA Stable](https://stable.melpa.org/packages/lsp-java-badge.svg)](https://stable.melpa.org/#/lsp-java)
[![Build Status](https://github.com/emacs-lsp/lsp-java/workflows/CI/badge.svg?branch=master)](https://github.com/emacs-lsp/lsp-java/actions)
[![Join the chat at https://gitter.im/emacs-lsp/lsp-mode](https://badges.gitter.im/emacs-lsp/lsp-mode.svg)](https://gitter.im/emacs-lsp/lsp-mode?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

lsp-java
========

Emacs Java IDE using [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls).

## Features
LSP java mode supports the following JDT Features:

* As you type reporting of parsing and compilation errors (via [flycheck](https://github.com/flycheck/flycheck)/[lsp-ui](https://github.com/emacs-lsp/lsp-ui))
* Code completion - using [company-capf](https://github.com/company-mode/company-mode) or builtin ```complete-at-point```
* Javadoc hovers - using [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
* Code actions - using [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
* Code outline - using builtin [imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html)
* Code navigation - using builtin [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
* Code lens (references/implementations) - using builtin [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
* Highlights
* Code formatting
* Maven pom.xml project support
* Limited Gradle support
* Visual debugger - [dap-mode](https://github.com/yyoncho/dap-mode/)
* Test runner - [dap-mode](https://github.com/yyoncho/dap-mode/)
* Project explorer integration - [treemacs](https://github.com/Alexander-Miller/treemacs)
* Integration with [Spring Initializr](https://start.spring.io/)
## Demo
Here it is a demo from EmacsConf2019 https://www.youtube.com/watch?v=Bbjxn9yVNJ8
## Tutorial
Here is a tutorial that covers setup and use https://xpressrazor.wordpress.com/2020/11/04/java-programming-in-emacs/
## Screenshot
![demo](images/demo.png)
## Installation
### Spacemacs
[lsp-java](https://github.com/emacs-lsp/lsp-java) is included in spacemacs (for now only on the dev branch). If you are using the development version of
spacemacs you can simply add `(java :variables java-backend 'lsp)` to `dotspacemacs-configuration-layers`.

### Install via melpa
The recommended way to install LSP Java is via `package.el` - the built-in package
manager in Emacs. LSP Java is available on the two major `package.el` community
maintained repos - [MELPA Stable](http://stable.melpa.org) and [MELPA](http://melpa.org).

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `lsp-java` <kbd>[RET]</kbd>

Then add the following lines to your `.emacs` and open a file from the any of the specified projects.
```emacs-lisp
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
```
### [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls)

[lsp-java](https://github.com/emacs-lsp/lsp-java) will automatically detect when the server is missing and it will download [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls) before the first startup. The server installation will be in `lsp-java-server-install-dir`. It will detect whether [dap-mode](https://github.com/yyoncho/dap-mode/) is present and it will download the required server side plugins/components. If you want to update the server you can run `lsp-java-update-server`. To run specific version of [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls) use `lsp-java-server-install-dir`.

### Quick start
Minimal configuration with [company-capf](https://github.com/company-mode/company-mode) and [lsp-ui](https://github.com/emacs-lsp/lsp-ui) and [dap-mode](https://github.com/yyoncho/dap-mode/). Set `lsp-java-workspace-dir` in case you have existing Java projects. Now you can explore the methods under `lsp-java-*`, `dap-java-*`, `dap-*`, and `lsp-*`.

```elisp
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration)))
(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))
(use-package lsp-treemacs)
```
## Supported commands
### LSP Mode commands
* `lsp-execute-code-action` - Execute code action.
* `lsp-rename` - Rename symbol at point
* `lsp-describe-thing-at-point` - Display help for the thing at point.
* `lsp-goto-type-definition` - Go to type definition
* `lsp-goto-implementation` - Go to implementation
* `lsp-restart-workspace` - Restart project
* `lsp-format-buffer` - Format current buffer
* `lsp-symbol-highlight` - Highlight all relevant references to the symbol under point.
* `lsp-workspace-folders-add` - Add workspace folder
* `lsp-workspace-folders-remove` - Remove workspace folder
* `lsp-workspace-folders-switch` - Switch workspace folder
### LSP Java commands
* `lsp-java-organize-imports` - Organize imports
* `lsp-java-build-project` - Perform partial or full build for the projects
* `lsp-java-update-project-configuration` - Update project configuration
* `lsp-java-actionable-notifications` - Resolve actionable notifications
* `lsp-java-update-user-settings` - Update user settings (Check the options in the table bellow.)
* `lsp-java-update-server` - Update server instalation.
* `lsp-java-generate-to-string` - Generate `toString` method.
* `lsp-java-generate-equals-and-hash-code` - Generate `equals` and `hashCode` methods.
* `lsp-java-generate-overrides` - Generate method `overrides`
* `lsp-java-generate-getters-and-setters` - Generate getters and setters.
* `lsp-java-type-hierarchy` - Open type hierarchy. Use prefix arg to specify the type of the hierarchy.
#### Refactoring
LSP Java provides rich set of refactorings via [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls) code actions and
some of them are bound to Emacs commands:

* `lsp-java-extract-to-constant` - Extract constant refactoring
* `lsp-java-add-unimplemented-methods` - Extract constant refactoring
* `lsp-java-create-parameter` - Create parameter refactoring
* `lsp-java-create-field` - Create field refactoring
* `lsp-java-create-local` - Create local refactoring
* `lsp-java-extract-method` - Extract method refactoring
* `lsp-java-add-import` - Add missing import
#### Testing support
* `lsp-jt-browser` - Browse tests and run/debug them.
** Use `x` to run the test(s) under point; `d` to debug the tests under point. `R` to refresh.
** Support for GUI operations.
* `lsp-jt-report-open` - open test report
* `lsp-jt-lens-mode` - test lenses mode(requires `lsp-lens-mode` to be enabled.)

#### Dependency viewer
* `lsp-java-dependency-list` - View java dependencies

#### STS4 Integration (experimental)

LSP java has integration with [STS4](https://github.com/spring-projects/sts4/) providing the following functionality.

## Spring boot support (Experimental)

In addition to the integration with [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls) [lsp-java](http://github.com/emacs-lsp/lsp-java) provides integration with [STS4](https://github.com/spring-projects/sts4/)  which covers Spring Boot
`application.properties`, `application.yml` and `.java` files.

## Usage:
Make sure that you have configured `JAVA_HOME`. `lsp-java` will automatically download the [STS4](https://github.com/spring-projects/sts4/) when you call `lsp-java-update-server`. In order to enable [STS4](https://github.com/spring-projects/sts4/)  integration add the following lines to your config:
``` emacs-lisp
(require 'lsp-java-boot)

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
```

## Functionality for `.java`

### Navigating the source code - Go to symbol in file/workspace
Easy navigation to Spring-specific elements of your source code.

![Go to Symbol in workspace](images/java-navigation.png)

#### Commands
`lsp-workspace-symbol` - (works better usign [helm-lsp](https://github.com/yyoncho/helm-lsp))

#### Examples
* `@/` shows all defined request mappings (mapped path, request method, source location)
* `@+` shows all defined beans (bean name, bean type, source location)
* `@>` shows all functions (prototype implementation)
* `@` shows all Spring annotations in the code

### Quick-access for running apps
Easy navigation to the provided request mappings of running apps.

![accessing running apps quickly](images/running-apps.png)

#### Commands
`lsp-workspace-symbol` - (works better usign [helm-lsp](https://github.com/yyoncho/helm-lsp))

#### Examples
* `//` shows all request mappings of all running Spring Boot apps and opens a browser for the selected endpoint

### Live application information hovers
STS4 automatically detects JVM processes for running boot applications on your local machine.

For some types of information, STS 4 may also show a 'quick summary' as a codelens.

If there are multiple instances of the app running on your machine, the live data from all those instances will show up in the hover information.

``` emacs-lisp
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
```
![live data from running apps as hover on source code](images/live-hovers.png)

#### Examples
* `@Profile`: shows information about the active profiles on the running apps
* `@Component`, `@Bean`, `@Autowired`: shows detailed information about the beans and their wiring from the live app
* `@ContidionalOn...`: shows information about the conditions and their evaluation at runtime

### Code templates
Write Spring code with templates, available via regular code completion.

#### Examples
* `@GetMapping`
* `@PostMapping`
* `@PutMapping`

### Smart code completions
Additional code completions for Spring-specific annotations

![Smart code completion for boot properties](images/validation-completion.png)

## Functionality for `.properties` and `.yml`

This extension analyzes your project's classpath and parses and indexes any [Spring Boot
Properties Metadata](https://docs.spring.io/spring-boot/docs/current/reference/html/configuration-metadata.html) it finds. Both Maven and Gradle projects are supported.

The data in the index is used to provide validation, code completions and information
hovers while editing Spring Boot Properties in either `.properties` or `.yml` format.

### Validation and code completion in properties file
![application-properties-validation](images/validation-completion.png)

### Validation and code completion in yaml file
![application-properties-validation](images/yaml-completion-and-help.png)

#### Spring Initializr
`lsp-java` provides a frontend for [Spring Initializr](https://start.spring.io/) which simplifies the creation of Spring Boot projects directly from Emacs via `lsp-java-spring-initializr`.
![Create Spring boot project](images/boot.png)
## Supported settings
* `lsp-java-server-install-dir` -
  "Install directory for eclipse.jdt.ls-server.
The slash is expected at the end."
* `lsp-java-jdt-download-url` - JDT JS download url.
Use http://download.eclipse.org/che/che-ls-jdt/snapshots/che-jdt-language-server-latest.tar.gz if you want to use Eclipse Che JDT LS.
* `lsp-java-java-path` - Path of the java executable.
* `lsp-java-progress-string` - Java progress status as reported by the language server.
* `lsp-java-workspace-dir` - LSP java workspace directory.
* `lsp-java-workspace-cache-dir` - LSP java workspace cache directory.
* `lsp-java-themes-directory` - Directory containing themes.
* `lsp-java-theme` - Theme to use.
* `lsp-java-pop-buffer-function` - The function which will be used for showing the helper windows.
* `lsp-java-vmargs` - Specifies extra VM arguments used to launch the Java Language Server. Eg. use `-noverify -Xmx1G -XX:+UseG1GC -XX:+UseStringDeduplication` to bypass class verification,increase the heap size to 1GB and enable String deduplication with the G1 Garbage collector
* `lsp-java-9-args` - Specifies arguments specific to java 9 and later.
* `lsp-java-errors-incomplete-classpath-severity` - Specifies the severity of the message when the classpath is incomplete for a Java file
* `lsp-java-configuration-check-project-settings-exclusions` - Checks if the extension-generated project settings files (.project, .classpath, .factorypath, .settings/) should be excluded from the file explorer.
* `lsp-java-configuration-update-build-configuration` - Specifies how modifications on build files update the Java classpath/configuration
* `lsp-java-trace-server` - Traces the communication between VS Code and the Java language server.
* `lsp-java-import-gradle-enabled` - Enable/disable the Gradle importer.
* `lsp-java-import-maven-enabled` - Enable/disable the Maven importer.
* `lsp-java-maven-download-sources` - Enable/disable eager download of Maven source artifacts.
* `lsp-java-references-code-lens-enabled` - Enable/disable the references code lens.
* `lsp-java-signature-help-enabled` - Enable/disable the signature help.
* `lsp-java-implementations-code-lens-enabled` - Enable/disable the implementations code lens.
* `lsp-java-configuration-maven-user-settings` - Path to Maven's settings.xml
* `lsp-java-format-enabled` - Enable/disable default Java formatter
* `lsp-java-save-actions-organize-imports ` - Enable/disable auto organize imports on save action
* `lsp-java-import-exclusions` - Configure glob patterns for excluding folders
* `lsp-java-content-provider-preferred` - Preferred content provider (a 3rd party decompiler id, usually). We support https://github.com/dgileadi/vscode-java-decompiler. To enable it, add:
    ```elisp
    (setq lsp-java-content-provider-preferred "fernflower")
    ```
* `lsp-java-autobuild-enabled` - Enable/disable the 'auto build'
* `lsp-java-max-concurrent-builds` - Max simultaneous project builds
* `lsp-java-completion-enabled` - Enable/disable code completion support
* `lsp-java-completion-overwrite` - When set to true, code completion overwrites the current text. When set to false, code is simply added instead.
* `lsp-java-completion-guess-method-arguments` - When set to true, method arguments are guessed when a method is selected from as list of code assist proposals.
* `lsp-java-completion-favorite-static-members` - Defines a list of static members or types with static members. Content assist will propose those static members even if the import is missing.
* `lsp-java-completion-import-order` - Defines the sorting order of import statements. A package or type name prefix (e.g. 'org.eclipse') is a valid entry. An import is always added to the most specific group.
* `lsp-java-folding-range-enabled` - Enable/disable smart folding range support. If disabled, it will use the default indentation-based folding range provided by VS Code.
* `indentation-based` - [Experimental] Enable/disable progress reports from background processes on the server.
* `lsp-java-progress-reports-enabled` - [Experimental] Enable/disable progress reports from background processes on the server.
* `lsp-java-format-settings-url` - Specifies the url or file path to the [Eclipse formatter xml settings](https://github.com/redhat-developer/vscode-java/wiki/Formatter-settings).
* `lsp-java-format-settings-profile` - Optional formatter profile name from the Eclipse formatter settings.
* `lsp-java-format-comments-enabled` - Includes the comments during code formatting.
* `lsp-java-format-on-type-enabled` - Enable/disable automatic block formatting when typing `;`, `<enter>` or `}`
* `lsp-java-bundles` - List of bundles that will be loaded in the JDT server.
* `lsp-java-code-generation-hash-code-equals-use-java7objects` - Use Objects.hash and Objects.equals when generating the hashCode and equals methods. This setting only applies to Java 7 and higher.
* `lsp-java-code-generation-hash-code-equals-use-instanceof` - Use 'instanceof' to compare types when generating the hashCode and equals methods.
* `lsp-java-code-generation-use-blocks` - Use blocks in 'if' statements when generating the methods.
* `lsp-java-code-generation-generate-comments` - Generate method comments when generating the methods.
* `lsp-java-code-generation-to-string-template` - The template for generating the toString method.
* `lsp-java-code-generation-to-string-code-style` - The code style for generating the toString method.
* `lsp-java-code-generation-to-string-skip-null-values` - Skip null values when generating the toString method.
* `lsp-java-code-generation-to-string-list-array-contents` - List contents of arrays instead of using native toString().
* `lsp-java-code-generation-to-string-limit-elements` - Limit number of items in arrays/collections/maps to list, if 0 then list all.
* `lsp-java-inhibit-message` - If non-nil, inhibit java messages echo via `inhibit-message'.

## Additional packages
* [lsp-ui](https://github.com/emacs-lsp/lsp-ui) : Flycheck, documentation and code actions support.
* [company-capf](https://github.com/company-mode/company-mode) : Company backend support.
* [treemacs](https://github.com/Alexander-Miller/treemacs) : Project viewer.
* [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs) : `lsp-mode` GUI controls implemented using treemacs.
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

When particular file is not part of imported project [Eclipse JDT Language Server](https://projects.eclipse.org/projects/eclipse.jdt.ls) could not calculate the current classpath.

* How do I change the version of java used by LSP?

Have a look at `~/.emacs.d/workspace/.metadata/.plugins/org.eclipse.jdt.launching/libraryInfos.xml`. If you updated your local java path and want LSP to use the new version, try removing the `~/.emacs.d/workspace/` directory and relaunch LSP. Also see [#114](https://github.com/emacs-lsp/lsp-java/issues/114).
If you have multiple java JDK versions installed and want to change the version of java used by LSP, also need to set `lsp-java-configuration-runtimes`. An example for setting `lsp-java-configuration-runtimes`:

```lisp
(setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
						:path "/home/kyoncho/jdk1.8.0_201.jdk/")
					(:name "JavaSE-11"
						:path "/home/kyoncho/jdk-11.0.1.jdk/"
						:default t)])
```

* How do I change JVM args passed to JDT server?
LSP slowness could be caused by slow JDT server, especially on large JAVA projects. Bump up the heap size maybe a good idea.

```lisp
;; current VSCode defaults
(setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))
```
