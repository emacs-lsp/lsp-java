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
(require 'lsp-java)

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

(defun lsp-java-steps-project (project-name dir-name config-name config)
         (setq default-directory lsp-java-test-root)

         ;; delete old directory
         (when (file-exists-p dir-name)
           (delete-directory dir-name t))

         ;; create directory structure
         (mkdir (expand-file-name
                 (f-join   dir-name project-name "src" "main" "java" "temp")) t)

         ;; add pom.xml
         (with-temp-file (expand-file-name config-name (f-join dir-name project-name))
           (insert config)))

(Given "^I have maven project \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
       (lambda (project-name dir-name)
				 (lsp-java-steps-project project-name dir-name "pom.xml" "
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
</project>")))

(Given "^I have gradle groovy DSL project \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
       (lambda (project-name dir-name)
				 (lsp-java-steps-project project-name dir-name "build.gradle" "
plugins {
    id 'java'
}
repositories {
    jcenter()
}
dependencies {
    implementation 'com.google.guava:guava:27.0.1-jre'
}")))

(Given "^I have gradle kotlin DSL project \"\\([^\"]+\\)\" in \"\\([^\"]+\\)\"$"
       (lambda (project-name dir-name)
				 (lsp-java-steps-project project-name dir-name "build.gradle.kts" "
plugins {
    java
}
repositories {
    jcenter()
}
dependencies {
    implementation(\"com.google.guava:guava:27.0.1-jre\")
}")))

(And "^I have a java file \"\\([^\"]+\\)\"$"
     (lambda (file-name)
       (setq default-directory lsp-java-test-root)
       (find-file file-name)
       (save-buffer)))

(And "^I add project \"\\([^\"]+\\)\" folder \"\\([^\"]+\\)\" to the list of workspace folders$"
     (lambda (project dir-name)
       (mkdir lsp-java-workspace-dir t)
       (lsp-workspace-folders-add (f-join lsp-java-test-root dir-name project))))

(And "^I start lsp-java$"
     (lambda () (lsp)))

(Then "^The server status must become \"\\([^\"]+\\)\"$"
      (lambda (status callback)
        (lsp-java-steps-async-wait
         (lambda ()
           (--some? (equal (lsp--workspace-status-string it) "::Started")
                    (lsp--session-workspaces (lsp-session))))
         callback)))

(And "^I use formatter profile \"\\([^\"]+\\)\" from \"\\([^\"]+\\)\"$"
     (lambda (formatter-name formatter-file)
       (setq lsp-java-format-settings-url (lsp--path-to-uri
                                           (f-join lsp-java-root-path "features/fixtures" formatter-file)))
       (setq lsp-java-format-settings-profile formatter-name)))

(And "^There must be \"\\([^\"]+\\)\" actionable notification$"
     (lambda (count callback)
       (lsp-java-steps-async-wait
        (lambda ()
          (= (string-to-number count)
             (hash-table-count (lsp-workspace-get-metadata "actionable-notifications"))))
        callback)))

(When "^I indent buffer$"
      (lambda ()
        (indent-region (point-min) (point-marker))))

(When "^I invoke \"\\([^\"]+\\)\" I should see error message \"\\([^\"]+\\)\"$"
      (lambda (command message)
        (condition-case err
            (funcall (intern command))
          (error (cl-assert (string= message (error-message-string err)) t (error-message-string err))))))

(When "^I call \"\\([^\"]+\\)\" and see:$"
      (lambda (command text callback)
        (lsp-java-steps-async-wait
         (lambda ()
           (save-window-excursion
             (funcall (intern command))
             (s-contains? text (buffer-string) t)))
         callback)))

(provide 'lsp-java-steps)
;;; lsp-java-steps.el ends here
