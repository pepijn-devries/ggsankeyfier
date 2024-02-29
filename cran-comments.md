## Second submission

My apologies for the oversight. This is now fixed.

 > Found the following (possibly) invalid URLs:
     URL: [https:://doi.org/10.2139/ssrn.4450241]https:://doi.org/10.2139/ssrn.4450241
       From: inst/doc/data_management.html
       Status: Error
       Message: URL rejected: Port number was not a decimal number
       between 0 and 65535
 > Indeed, there is a doubled colon in "https::"
 > Pls write this as https://doi.org/10.2139/ssrn.4450241
 > Please fix and resubmit.

## Test environments

 * local Windows 10 install, R 4.3.2
 * winbuilder
 * rhub check_for_cran

 > Possibly misspelled words in DESCRIPTION:
   Sankey (3:15, 24:14)

This is a common name for a particular type of
diagrams, see:
<https://en.wikipedia.org/wiki/Sankey_diagram>

 > Found the following (possibly) invalid URLs:
   URL: https:://doi.org/10.2139/ssrn.4450241
     From: inst/doc/data_management.html
     Status: Error
     Message: URL rejected: Port number was not a decimal number between 0 and 65535

This is most likely a temporary issue on the DOI
server. The URL does work in a web browser.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
