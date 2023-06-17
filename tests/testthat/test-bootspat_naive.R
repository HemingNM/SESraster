test_that("function bootspat_naive works", {

  # creating data
  library(terra)
  set.seed(100)
  bin1 <- terra::rast(ncol = 5, nrow = 5, nlyr = 5)
  values(bin1) <- round(runif(ncell(bin1) * nlyr(bin1)))
  names(bin1) <- paste0("sp", 1:5)
  bin1[1] <- NA

  # applying the function
  set.seed(100)
  rand.site <- bootspat_naive(bin1, "site")
  names(rand.site) <- names(bin1)
  rand.site2 <- bootspat_naive(bin1, "site", memory = F)
  set.seed(100)
  rand.sp <- bootspat_naive(bin1, "species")
  names(rand.sp) <- names(bin1)
  set.seed(100)
  rand.both <- bootspat_naive(bin1, "both")
  names(rand.both) <- names(bin1)
  rand.both2 <- bootspat_naive(bin1, "both", memory = F)

  # testing
  expect_true(class(rand.site) == "SpatRaster", "TRUE")
  expect_true(class(rand.sp) == "SpatRaster", "TRUE")
  expect_true(class(rand.both) == "SpatRaster", "TRUE")

  expect_error(bootspat_naive(bin1, "x"))
  expect_error(bootspat_naive(bin1, "site", memory = NA))


  expect_equal(unlist(rand.site[2]), setNames(c(0,0,1,0,1), names(bin1)))

  expect_equal(unlist(rand.sp[2]), setNames(c(1,0,0,1,0), names(bin1)))

  expect_equal(unlist(rand.both[2]), setNames(c(1,0,0,1,1), names(bin1)))
})
