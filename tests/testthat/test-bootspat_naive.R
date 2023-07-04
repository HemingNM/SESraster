test_that("bootspat_naive throws error when object classes are not appropriate", {
  # loading data
  bin1 <- load_ext_data() #terra::rast(system.file("extdata", "spp_sites.tif", package="SESraster"))

  expect_error(bootspat_naive(bin1, "x"))
  expect_error(bootspat_naive(bin1, "site", memory = NA))
})

test_that("function bootspat_naive works", {
  set.seed(100)

  # loading data
  bin1 <- load_ext_data() #terra::rast(system.file("extdata", "spp_sites.tif", package="SESraster"))

  # applying the function
  rand.site <- bootspat_naive(bin1, "site")
  rand.site2 <- bootspat_naive(bin1, "site", memory = F)
  rand.sp <- bootspat_naive(bin1, "species")
  rand.sp2 <- bootspat_naive(bin1, "species", memory = F, filename=paste0(tempfile(), ".tif"))
  rand.both <- bootspat_naive(bin1, "both")
  rand.both2 <- bootspat_naive(bin1, "both", memory = F)

  # testing
  expect_true(inherits(rand.site, "SpatRaster"), "TRUE")
  expect_true(inherits(rand.sp, "SpatRaster"), "TRUE")
  expect_true(inherits(rand.both, "SpatRaster"), "TRUE")

  expect_equal(unlist(rand.site[2]), setNames(rep(0, terra::nlyr(bin1)), names(bin1)))
  expect_equal(unlist(rand.sp[2]), setNames(c(0,0,0,0,1,0,1), names(bin1)))
  expect_equal(unlist(rand.both[2]), setNames(c(0,1,1,0,0,1,1), names(bin1)))

  expect_equal(unlist(rand.site[1]), setNames(as.double(rep(NA, terra::nlyr(bin1))), names(bin1)))
  expect_equal(unlist(rand.sp[1]), setNames(as.double(rep(NA, terra::nlyr(bin1))), names(bin1)))
  expect_equal(unlist(rand.both[1]), setNames(as.double(rep(NA, terra::nlyr(bin1))), names(bin1)))

  expect_equal(sum(rand.site)[1:8], sum(bin1)[1:8]) # test richness
})
