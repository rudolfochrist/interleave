
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

Scenario: Notes synchronization-navigation in multi-pdf notes documents
  Given I open the file "sync-multi.org"
  And I go to word "Quotes"
  And I start interleave-mode
  Then I should see buffer "quotes.pdf"
  When I press "M-n"
  Then the current page should be "2"
  When I switch to buffer "sync-multi.org"
  Then I should see "1 notes 2"
  When I switch to buffer "quotes.pdf"
  And I press "M-n"
  Then the current page should be "4"
  When I switch to buffer "sync-multi.org"
  Then I should see "1 notes 4"
  And I should not see "1 notes 2"
  When I switch to buffer "quotes.pdf"
  And I press "M-p"
  Then the current page should be "2"
  When I switch to buffer "sync-multi.org"
  Then I should see "1 notes 2"
  And I should not see "1 notes 4"
  And I quit interleave-mode

Scenario: Don't slip into other heading while notes syncing
  Given I open the file "sync-multi.org"
  And I go to word "Slides"
  And I start interleave-mode
  Then I should see buffer "slides.pdf"
  When I press "M-n"
  Then the current page should be "1"
  When I switch to buffer "sync-multi.org"
  Then I should see "2 notes 1"
  When I switch to buffer "slides.pdf"
  And I press "M-p"
  Then I should be in buffer "slides.pdf"
  And the current page should be "1"
  When I switch to buffer "sync-multi.org"
  Then I should see "2 notes 1"
  And I quit interleave-mode

Scenario: Sync pdf page to notes in current subtree of multi-pdf notes document
  Given I open the file "sync-multi.org"
  And I go to word "Slides"
  And I start interleave-mode
  And I switch to buffer "sync-multi.org"
  And I widen the buffer 
  And I go to word "find me 2"
  When I press "M-."
  Then the current page should be "3"
  And I quit interleave-mode

