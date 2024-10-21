# SESraster 0.7.1

* bug fixes
  - bug fix on SESraster() that was creating only a single temporary raster for 
  FUN metrics results. Now it creates "n = aleats" temporary rasters.

* function changes
  - SESraster() now also returns p values for the upper and lower tails (i.e. "p_lower", "p_upper")

# SESraster 0.7.0

* new functions
  - added `SESraster()` to handle aleatorizations
  - added `algorithm_metrics()` to compare original and randomized rasters
  - added `plot_alg_metrics()` to plot site and species difference metrics 
  between original and randomized rasters

* function changes
  - changed function name from .fit.memory() to fit.memory() and added argument "n"
  - Added cross links to functions

* vignette
  - added vignette to exemplify how `SESraster()` works
  - changed order of vignettes
  
# SESraster 0.6.2

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

# SESraster 0.6.1

Released 2023-06-23

* Initial CRAN submission.



