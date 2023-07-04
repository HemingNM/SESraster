## R CMD check results

0 errors | 0 warnings | 1 note

Maintainer: ‘Neander Marcel Heming <neanderh@yahoo.com.br>’
  
  Found the following (possibly) invalid URLs:
    
    URL: doi:10.1046/j.1365-2699.2003.00875.x
      From: README.md
      Message: Invalid URI scheme (use \doi for DOIs in Rd markup)
    URL: doi:10.2307/177478
      From: README.md
      Message: Invalid URI scheme (use \doi for DOIs in Rd markup)
    URL: https://doi.org/10.2307/1936961
      From: inst/doc/S1-get-started.html
      Status: 403
      Message: Forbidden
      
## Maintainer comments
Fixed codecov URL

I was not able to solve the above NOTE about possibly invalid URLs from DOIs. I 
know they are correct, so I am submitting as it is. 

Main changes are listed below:

* This is a new release.

* bux fixes
  - fixed error in `.str.sample()` to avoid negative probabilities when all 
species were absent from a cell (i.e. all values are zero)

* new function
  - added `bootspat_ff()` to include Fixed-Fixed algorithm

* enhancements
  - added new vignette
  - added references and improved accuracy of algorithm description in DESCRIPTION, 
  README, and vignettes
  - improved accuracy of null model descriptions in vignettes
  - added link to functions in documentation
