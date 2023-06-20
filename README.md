# SESraster
The SESraster randomizes presence/absence species distribution raster data with or without including spatial structure for calculating Standardized Effect Sizes (SES) necessary for null hypothesis testing.
If you have any questions, let us know through the topic ["Issues"](https://github.com/HemingNM/SESraster/issues). To install the package, run the following code:

```{r}
install.packages("SESraster")
```

The development version of `SESraster` can be installed from the [`SESraster repository in Github`](https://github.com/HemingNM/SESraster):

```{r}
require(devtools)
devtools::load_all()
devtools::install_github("HemingNM/SESraster")
library(SESraster)
```
