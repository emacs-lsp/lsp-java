Feature: Actionable notifications
  @Notifications
  Scenario: Ignore classpath warnings
    Given I have maven project "m" in "tmp"
    And I have a java file "tmp/m/App.java"
    And I clear the buffer
    And I add project "m" folder "tmp" to the list of workspace folders
    And I start lsp-java
    Then The server status must become "LSP::Started[!]"
    And There must be "1" actionable notification
    And I start an action chain
    When I press "M-x"
    When I type "lsp-java-actionable-notifications"
    And I press "<return>"
    # Execute the first actionable notification
    When I type "App.java isn't on the classpath"
    And I press "<tab>"
    And I press "<return>"
    # Execute the first item from the resolution
    When I type "More"
    And I press "<tab>"
    And I press "RET"
    And I execute the action chain
    And There must be "0" actionable notification
    And The server status must become "LSP::Started"
