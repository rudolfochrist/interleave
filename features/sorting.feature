Scenario: Sorting headlines
  Given I open the file "notes.org"
  When I start interleave-mode
  And I press "n"
  And I press "n"
  And I press "i" 
  And I insert "some note"
  And I switch to buffer "quotes.pdf"
  And I press "p"
  And I press "i"
  And I insert "another note"
  And I switch to buffer "quotes.pdf"
  And I quit interleave-mode
  And I switch to buffer "notes.org"
  Then the headline "Notes for page 2" should be before headline "Notes for page 3"

Scenario: Sorting headlines
  Given I open the file "notes.org"
  And I go to end of buffer
  And I insert "* Introduction"
  When I start interleave-mode
  And I press "n"
  And I press "n"
  And I press "i" 
  And I insert "some note"
  And I switch to buffer "quotes.pdf"
  And I press "p"
  And I press "i"
  And I insert "another note"
  And I switch to buffer "quotes.pdf"
  And I quit interleave-mode
  And I switch to buffer "notes.org"
  Then the headline "Introduction" should be before headline "Notes for page 2"
  And the headline "Notes for page 2" should be before headline "Notes for page 3"
