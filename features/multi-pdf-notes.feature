Scenario: Starting interleave-mode on multi-pdf notes
  Given I open the file "multi-pdf.org"
  When I go to word "Quotes"
  And I start interleave-mode
  Then I should see buffer "multi-pdf.org"
  And I should see buffer "quotes.pdf"
  When I quit interleave-mode
  And I switch to buffer "multi-pdf.org"
  And I go to word "Slides"
  And I start interleave-mode
  Then I should see buffer "multi-pdf.org"
  And I should see buffer "slides.pdf"
  And I quit interleave-mode

Scenario: Add note in multi-pdf notes
  Given I open the file "multi-pdf.org"
  When I go to word "Quotes"
  And I start interleave-mode
  And I press "i"
  And I insert "note 1"
  Then I should be in buffer "multi-pdf.org"
  And I should see "note 1"
  And I quit interleave-mode

Scenario: Navigation in multi-pdf notes
  Given I open the file "multi-pdf.org" 
  When I go to word "Quotes"
  And I start interleave-mode
  And I press "i"
  And I insert "note 1"
  And I switch to buffer "quotes.pdf"
  And I press "n"
  And I press "i"
  And I insert "note 2"
  And I switch to buffer "quotes.pdf"
  And I press "p"
  And I switch to buffer "multi-pdf.org"
  Then I should see "note 1"
  And I should not see "note 2"
  When I switch to buffer "quotes.pdf"
  And I press "n"
  And I switch to buffer "multi-pdf.org"
  Then I should see "note 2"
  And I should not see "note 1"
  And I quit interleave-mode
  
