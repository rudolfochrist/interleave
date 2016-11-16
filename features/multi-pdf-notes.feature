Scenario: Starting interleave-mode on multi-pdf notes
  Given I open the file "multi-pdf.org"
  When I go to word "Quotes"
  And I start interleave-mode
  Then I should see buffer "multi-pdf.org"
  And I should see buffer "quotes.org"
  When I quit interleave-mode
  And I switch to buffer "multi-pdf.org"
  And I go to word "Slides"
  And I start interleave-mode
  Then I should see buffer "multi-pdf.org"
  And I should see buffer "slides.pdf"

