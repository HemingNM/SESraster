<!-- badges: start -->

[![CRAN-status](https://www.r-pkg.org/badges/version/SESraster?color=green)](https://cran.r-project.org/package=SESraster) [![](http://cranlogs.r-pkg.org/badges/grand-total/SESraster?color=green)](https://cran.r-project.org/package=SESraster) [![](http://cranlogs.r-pkg.org/badges/SESraster?color=green)](https://cran.r-project.org/package=SESraster) [![R-CMD-check](https://github.com/HemingNM/SESraster/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HemingNM/SESraster/actions/workflows/R-CMD-check.yaml) [![codecov](https://codecov.io/gh/HemingNM/SESraster/branch/master/graph/badge.svg?token=YJZHUXU5R7)](https://app.codecov.io/gh/HemingNM/SESraster)

<!-- badges: end -->

# SESraster <a href="https://hemingnm.github.io/SESraster/"><img src="man/figures/logo.png" alt="SESraster website" align="right" height="139"/></a>

Randomization of presence/absence species distribution raster data with or without including spatial structure for calculating standardized effect sizes and testing null hypothesis. The randomization algorithms are based on classical algorithms for matrices (Gotelli 2000, <doi:10.2307/177478>) implemented for raster data.

<br>

### Installation

To install the package, run:

```         
install.packages("SESraster")
```

The development version can be installed from the [`Github`](https://github.com/HemingNM/SESraster) repository:

```         
require(devtools)
install_github("HemingNM/SESraster", build_vignettes = TRUE)
```

<br>

### SESraster basics

Basic information about the package can be found below, at the [package's webpage](https://hemingnm.github.io/SESraster/), or as vignettes:

```         
browseVignettes("SESraster")
```

-   

    #### Null model algorithms

    An overview of the [null model algorithms](vignette(%22null-models%22)) for species co-occurrence analysis summarized in (Gotelli 2000, <doi:10.2307/177478>).

-   

    #### Spatial null model algorithms in SESraster

    For installation instructions and to see how the null model algorithms implemented in `SESraster` work for spatial data, just [get started](vignette(%22spatial-null-models%22)).

-   

    #### Standardized effect sizes (SES)

    For computing standardized effect sizes (SES) using `SESraster()` function and the null model algorithms implemented in the package, take a look at [SES computation](vignette(%22SES%22)).

<br>

### Citation

If this package is useful to you, please cite it in your publications. Find more information using:

```         
citation("SESraster")
```

<br>

### Issues

If you have any question or find any bug, let us know through the topic ["Issues"](https://github.com/HemingNM/SESraster/issues).

<br>
