test_that("function bootspat_str works", {

  # creating data
  library(terra)
  set.seed(100)
  bin1 <- terra::rast(ncol = 5, nrow = 5, nlyr = 5)
  values(bin1) <- round(runif(ncell(bin1) * nlyr(bin1)))
  names(bin1) <- paste0("sp", 1:5)

  # applying the function
  rand.str <- bootspat_str(bin1)

  # testing
  expect_true(class(rand.str) == "SpatRaster", "TRUE")
})
