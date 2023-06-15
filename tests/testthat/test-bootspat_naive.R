test_that("function bootspat_naive works", {

  # creating data
  library(terra)
  set.seed(100)
  bin1 <- terra::rast(ncol = 5, nrow = 5, nlyr = 5)
  values(bin1) <- round(runif(ncell(bin1) * nlyr(bin1)))
  names(bin1) <- paste0("sp", 1:5)

  # applying the function
  rand.site <- bootspat_naive(bin1, "site")
  rand.sp <- bootspat_naive(bin1, "species")
  rand.both <- bootspat_naive(bin1, "both")

  # testing
  expect_true(class(rand.site) == "SpatRaster", "TRUE")
  expect_true(class(rand.sp) == "SpatRaster", "TRUE")
  expect_true(class(rand.both) == "SpatRaster", "TRUE")
  expect_error(bootspat_naive(bin1, "x"))
  expect_error(bootspat_naive(bin1, "site", memory = NA))
})
