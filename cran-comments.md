## R CMD check results

0 errors | 0 warnings | 1 note


❯ checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found


## Maintainer comments
Main changes are listed below and in the NEWS file:

* This is a new release.

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
