test_that("function bootspat_FF works", {
  set.seed(100)

  # loading data
  bin1 <- load_ext_data() # terra::rast(system.file("extdata", "spp_sites.tif", package="SESraster"))

  rprobnull <- terra::app(bin1,
                          function(x){
                            ifelse(is.na(x), NA, 1)
                          })

  # applying the function
  rand.str <- bootspat_ff(bin1)
  rand.str2 <- bootspat_ff(bin1, rprob = rprobnull)

  # testing
  expect_true(inherits(rand.str, "SpatRaster"), "TRUE")
  expect_true(inherits(rand.str2, "SpatRaster"), "TRUE")

  expect_equal(unlist(rand.str2[1]), setNames(as.double(rep(NA, terra::nlyr(bin1))), names(bin1)))
  expect_equal(unlist(rand.str2[2]), setNames(rep(0, terra::nlyr(bin1)), names(bin1)))
  expect_equal(unlist(rand.str2[3]), setNames(c(0,1,0,0,1,1,1), names(bin1)))
  expect_equal(unlist(terra::global(rand.str2, function(x) sum(x, na.rm = TRUE)))[1:2],
               unlist(terra::global(bin1, function(x) sum(x, na.rm = TRUE)))[1:2])
  expect_equal(sum(rand.str)[1:6], sum(bin1)[1:6])
  expect_equal(sum(rand.str2)[1:6], sum(bin1)[1:6])
  expect_true(all(is.na(unlist(rand.str2[1]))))
})
