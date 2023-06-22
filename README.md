<!-- badges: start -->
[![R-CMD-check](https://github.com/HemingNM/SESraster/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HemingNM/SESraster/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# SESraster
The SESraster randomizes presence/absence species distribution raster data with or without including spatial structure for calculating Standardized Effect Sizes (SES) necessary for null hypothesis testing.
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
