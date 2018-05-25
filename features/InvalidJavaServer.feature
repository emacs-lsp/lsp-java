Feature: Missing server.
  Scenario: Wrong server install configuration
    Given I have maven project "m" in "tmp"
    And I have a java file "tmp/m/src/main/java/temp/App.java"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I clear the buffer
    And I insert:
    """
    package temp; import java.util.ArrayList; class App {ArrayList<String> al = new ArrayList<>();}
    """
    And I call "save-buffer"
    And I set lsp-java-server-install-dir to "/non-existing"
    When I invoke "lsp-java-enable" I should see error message "Failed to find server installation with following message: Opening directory: No such file or directory, /non-existing/plugins"
