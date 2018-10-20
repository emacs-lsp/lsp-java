Feature: Organize imports
  @Imports
  Scenario: Organize imports
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I have a java file "tmp/m/src/main/java/temp/App.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    import java.util.HashMap;

    class App {
      ArrayList<String> al = new ArrayList<>();
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "LSP::Started"
    When I call "lsp-java-organize-imports"
    Then I should see:
    """
    package temp;

    import java.util.ArrayList;

    class App {
      ArrayList<String> al = new ArrayList<>();
    }
    """

  @Imports
  Scenario: Organize imports save action
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I have a java file "tmp/m/src/main/java/temp/App.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    import java.util.HashMap;

    class App {
      ArrayList<String> al = new ArrayList<>();
    }
    """
    And I start lsp-java
    And The server status must become "LSP::Started"
    And I call "save-buffer"
    Then I should see:
    """
    package temp;

    import java.util.ArrayList;

    class App {
      ArrayList<String> al = new ArrayList<>();
    }
    """

  @Imports
  Scenario: Disable organize imports save action
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I have a java file "tmp/m/src/main/java/temp/App.java"
    And I set lsp-java-save-action-organize-imports to nil
    And I clear the buffer
    And I insert:
    """
    package temp;

    import java.util.HashMap;

    class App {
      ArrayList<String> al = new ArrayList<>();
    }
    """
    And I start lsp-java
    And The server status must become "LSP::Started"
    And I call "save-buffer"
    Then I should see:
    """
    package temp;

    import java.util.HashMap;

    class App {
      ArrayList<String> al = new ArrayList<>();
    }
    """
    And I set lsp-java-save-action-organize-imports to 't
