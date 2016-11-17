Scenario: Starting interleave-mode
  Given I open the file "notes.org"
  When I start interleave-mode
  Then I should see buffer "notes.org"
  And I should see buffer "quotes.pdf"
  And I should be in buffer "quotes.pdf"
  And I quit interleave-mode

Scenario: Add note
  Given I open the file "notes.org"
  When I start interleave-mode
  And I press "i"
  And I insert "some notes"
  Then I should be in buffer "notes.org"
  And I should see "some notes"
  And I quit interleave-mode

Scenario: Switch between notes
  Given I open the file "notes.org"
  When I start interleave-mode
  And I press "i"
  And I insert "great"
  And I switch to buffer "quotes.pdf"
  And I press "n"
  And I press "i"
  Then I should not see "great" 
  When I insert "including Common Lisp"
  And I switch to buffer "quotes.pdf"
  And I press "p"
  And I switch to buffer "notes.org"
  Then I should not see "including Common Lisp"
  And I should see "great"
  And I quit interleave-mode
