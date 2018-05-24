Feature: Formatting

  Scenario: Changing formatter
    Given I have maven project "m" in "tmp"
    And I have a java file "tmp/m/src/main/java/temp/App.java"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I clear the buffer
    And I insert:
    """
    package temp; import java.util.ArrayList; class App {ArrayList<String> al = new ArrayList<>();}
    """
    And I call "save-buffer"
    And I use formatter profile "profileName" from "testFormatter.xml"
    And I start lsp-java
    And The server status must become "LSP::Started"
    When I indent buffer
    And I call "save-buffer"
    Then I should see:
    """
    package temp;
    import java.util.ArrayList;
    class App
    {
    	ArrayList<String> al = new ArrayList<>();
    }
    """
