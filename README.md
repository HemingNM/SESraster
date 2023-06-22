<!-- badges: start -->
[![R-CMD-check](https://github.com/HemingNM/SESraster/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HemingNM/SESraster/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# SESraster <a href="https://hemingnm.github.io/SESraster/"><img src="man/figures/logo.png" align="right" height="139" alt="SESraster website" /></a>

Randomization of presence/absence species distribution raster data with or without including spatial structure for calculating standardized effect sizes and testing null hypothesis.
The spatially unstructured randomization algorithms are based on fixed-fixed algorithms for matrices (Gotelli 2000, <doi:10.2307/177478>).
The spatially structured randomization algorithm is based on the preserved richness null model (Laffan & Crisp 2003, <doi:10.1046/j.1365-2699.2003.00875.x>).

If you have any questions, let us know through the topic ["Issues"](https://github.com/HemingNM/SESraster/issues). To install the package, run the following code:

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

A vignette with examples can be found at the [package's webpage](https://hemingnm.github.io/SESraster/) or loading:

```
browseVignettes("SESraster")
```
