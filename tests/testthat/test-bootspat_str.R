test_that("function bootspat_str works", {

  # creating data
  library(terra)
  set.seed(100)
  bin1 <- terra::rast(ncol = 5, nrow = 5, nlyr = 5)
  values(bin1) <- round(runif(ncell(bin1) * nlyr(bin1)))
  names(bin1) <- paste0("sp", 1:5)
  bin2 <- terra::rast(ncol = 5, nrow = 5, nlyr = 1)
  values(bin2) <- runif(ncell(bin2) * nlyr(bin2))
  bin1[1] <- NA

  # applying the function
  set.seed(100)
  rand.str <- bootspat_str(bin1)
  rand.str2 <- bootspat_str(bin1, rprob = bin2)

  # testing
  expect_true(class(rand.str) == "SpatRaster", "TRUE")
  expect_true(class(rand.str2) == "SpatRaster", "TRUE")
  expect_equal(unlist(rand.str2[2]), setNames(c(1,1,0,0,0), names(bin1)))
  expect_true(all(is.na(unlist(rand.str2[1]))))
})
