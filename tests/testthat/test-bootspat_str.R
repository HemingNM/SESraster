test_that("function bootspat_str works", {
  set.seed(100)

  # loading data
  bin1 <- load_ext_data() # terra::rast(system.file("extdata", "spp_sites.tif", package="SESraster"))
  rprobnull <- terra::app(bin1,
                          function(x){
                            ifelse(is.na(x), NA, 1)
                          })

    # applying the function
  rand.str <- bootspat_str(bin1)
  rand.str2 <- bootspat_str(bin1, rprob = rprobnull)

  # testing
  expect_true(class(rand.str) == "SpatRaster", "TRUE")
  expect_true(class(rand.str2) == "SpatRaster", "TRUE")
  expect_equal(unlist(rand.str2[2]), setNames(c(1,1,0,0,0), names(bin1)))
  expect_true(all(is.na(unlist(rand.str2[1]))))
})
