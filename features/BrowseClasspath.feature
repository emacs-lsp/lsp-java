Feature: Browse classpaths
  @Classpath
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
    When I call "lsp-java-classpath-browse"
    Then I should see:
    """
    org.eclipse.m2e.MAVEN2_CLASSPATH_CONTAINER
    """
