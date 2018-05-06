Feature: Importing projects

  Scenario: Maven projects
    Given I have maven project "m" in "tmp"
    And I have a java file "tmp/m/src/main/java/temp/App.java"
    And I clear the buffer
    And I add project "m" folder "tmp" to the list of workspace folders
    And I start lsp-java
    Then The server status must become "LSP::Started"
