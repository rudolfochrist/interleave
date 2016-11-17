
Scenario: Notes synchronization-navigation in normal notes documents
  Given I open the file "sync-normal.org"
  When I start interleave-mode
  And I press "M-n"
  Then the current page should be "2"
  When I switch to buffer "sync-normal.org"
  Then I should see "notes 2"
  When I switch to buffer "quotes.pdf"
  And I press "M-n"
  Then the current page should be "4"
  When I switch to buffer "sync-normal.org"
  Then I should see "notes 4"
  And I should not see "notes 2"
  When I switch to buffer "quotes.pdf"
  And I press "M-p"
  Then the current page should be "2"
  When I switch to buffer "sync-normal.org"
  Then I should see "notes 2"
  And I should not see "notes 4"
  And I quit interleave-mode

@wip
Scenario: Sync pdf page to current set of notes
  Given I open the file "sync-normal.org"
  And I start interleave-mode
  When I switch to buffer "sync-normal.org"
  And I widen the buffer
  And I go to word "find me"
  And I press "M-."
  And I switch to buffer "quotes.pdf"
  Then the current page should be "4"
  And I quit interleave-mode

