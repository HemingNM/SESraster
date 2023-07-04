<!-- badges: start -->
[![CRAN-status](https://www.r-pkg.org/badges/version/SESraster?color=green)](https://cran.r-project.org/package=SESraster)
[![](http://cranlogs.r-pkg.org/badges/grand-total/SESraster?color=green)](https://cran.r-project.org/package=SESraster)
[![](http://cranlogs.r-pkg.org/badges/SESraster?color=green)](https://cran.r-project.org/package=SESraster)
[![R-CMD-check](https://github.com/HemingNM/SESraster/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HemingNM/SESraster/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/HemingNM/SESraster/branch/master/graph/badge.svg?token=YJZHUXU5R7)](https://app.codecov.io/gh/HemingNM/SESraster)
<!-- badges: end -->

# SESraster <a href="https://hemingnm.github.io/SESraster/"><img src="man/figures/logo.png" align="right" height="139" alt="SESraster website" /></a>

Randomization of presence/absence species distribution raster data with or without including spatial structure for calculating standardized effect sizes and testing null hypothesis.
The spatially unstructured randomization algorithms are based on classical algorithms for matrices (Gotelli 2000, <doi:10.2307/177478>) but implemented for raster data.

### Installation
To install the package, run the following code:
```
install.packages("SESraster")
```
The development version of `SESraster` can be installed from the [`SESraster repository`](https://github.com/HemingNM/SESraster) in Github:
```
require(devtools)
devtools::load_all()
devtools::install_github("HemingNM/SESraster", build_vignettes = TRUE)
library(SESraster)
```


### Examples
Vignettes can be found at the [package's webpage](https://hemingnm.github.io/SESraster/) or loading:
```
browseVignettes("SESraster")
```


### Issues
If you have any question or find any bug, let us know through the topic ["Issues"](https://github.com/HemingNM/SESraster/issues).
