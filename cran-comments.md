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

# SESraster 0.7.1

* bug fixes
  - bug fix on SESraster() that was creating only a single temporary raster for 
  FUN metrics results. Now it creates "n = aleats" temporary rasters.

* function changes
  - SESraster() now returns "co_lower", "co_upper", "p_lower", "p_upper" values
  in addition to the former resulting values

