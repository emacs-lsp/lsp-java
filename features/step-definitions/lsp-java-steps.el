;;; lsp-java-steps.el --- Step definitions for lsp-java  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <ivan.yonchovski@tick42.com>

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


;;; Code:

(require 'f)
(require 's)

(defun lsp-java-steps-async-wait (pred callback)
  "Call CALLBACK when PRED becomes true."
  (let (timer
        (retry-count 0))
    (setq timer (run-with-timer
                 1
                 1
                 (lambda (&rest rest)
                   (if (funcall pred)
                       (progn
                         (cancel-timer timer)
                         (funcall callback))
                     (setq retry-count (1+ retry-count))
                     (message "The function failed, attempt %s" retry-count)))))))

(Given "^I have maven project \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
  (lambda (project-name dir-name)
    (setq default-directory lsp-java-root-path)

    ;; delete old directory
    (when (file-exists-p dir-name)
      (delete-directory dir-name t))

    ;; create directory structure
    (mkdir (expand-file-name
            (f-join lsp-java-root-path dir-name project-name "src" "main" "java" "temp")) t)

    (mkdir (expand-file-name project-name (expand-file-name "src")) t)

    (message lsp-java-root-path)

    ;; add pom.xml
    (with-temp-file (expand-file-name "pom.xml" (f-join dir-name project-name))
      (insert "
<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd\">
  <modelVersion>4.0.0</modelVersion>
  <groupId>test</groupId>
  <artifactId>test-project</artifactId>
  <packaging>jar</packaging>
  <version>1</version>
  <name>test-project</name>
  <url>http://maven.apache.org</url>

  <properties>
      <maven.compiler.source>1.8</maven.compiler.source>
      <maven.compiler.target>1.8</maven.compiler.target>
  </properties>

  <dependencies>
    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>4.11</version>
      <scope>test</scope>
    </dependency>
  </dependencies>
</project>"))))

(And "^I have a java file \"\\([^\"]+\\)\"$"
  (lambda (file-name)
    (setq default-directory lsp-java-root-path)
    (find-file file-name)
    (save-buffer)))

(And "^I add project \"\\([^\"]+\\)\" folder \"\\([^\"]+\\)\" to the list of workspace folders$"
  (lambda (project dir-name)
    (add-to-list 'lsp-java--workspace-folders (f-join lsp-java-root-path dir-name project))))

(And "^I start lsp-java$"
  (lambda ()
    (lsp-java-enable)))

(Then "^The server status must become \"\\([^\"]+\\)\"$"
  (lambda (status callback)
    (lsp-java-steps-async-wait
     (lambda ()
       (if (s-equals? (s-trim (lsp-mode-line)) status)
           t
         (progn
           (message "Server status is %s" (lsp-mode-line))
           nil)))
     callback)))


(And "^There must be \"\\([^\"]+\\)\" actionable notification$"
  (lambda (count callback)
    (lsp-java-steps-async-wait
     (lambda ()
       (= (string-to-number count)
          (hash-table-count (lsp-workspace-get-metadata "actionable-notifications"))))
     callback)))

(provide 'lsp-java-steps)
;;; lsp-java-steps.el ends here
