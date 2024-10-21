test_that("SES works", {
  r <- load_ext_data()
  aleats <- 5

  appmean <- function(x, ...){
                        terra::app(x, "mean", ...)
  }

  set.seed(10)
  # test ses by species
  ses <- SESraster(r, FUN=appmean, spat_alg = "bootspat_naive", spat_alg_args=list(random="species"),
                   aleats = aleats)
  nms <- paste0(c("Observed", "Null_Mean", "Null_SD", "SES", "p_lower", "p_upper"), ".mean")

  expect_true(inherits(ses, "SpatRaster"), "TRUE")
  expect_true(terra::nlyr(ses) == length(nms))
  expect_named(ses, nms)

  expect_equal(unlist(ses[1]), setNames(as.double(rep(c(NA, 0), c(4, 2))), nms))

  expect_equal(unlist(ses[2]), setNames(c(0, 0.4, 0.11952286, -3.34664011, 1, 0), nms))

  expect_equal(round(sd(terra::values(ses[[2]]), na.rm = TRUE), 3), as.double(0.081)) # test spat variation

  ## test ses by site
  ses <- SESraster(r, FUN=appmean, spat_alg = "bootspat_naive", spat_alg_args=list(random="site"),
                   aleats = aleats)
  # nms <- c("Observed.mean", "Null_Mean.mean", "Null_SD.std", "SES.mean")

  expect_true(inherits(ses, "SpatRaster"), "TRUE")
  expect_true(terra::nlyr(ses) == length(nms))
  expect_named(ses, nms)

  expect_equal(unlist(ses[1]), setNames(as.double(rep(c(NA, 0), c(4, 2))), nms))

  expect_equal(unlist(ses[2]), setNames(rep(0, length(nms)), nms))

  expect_equal(as.vector(terra::values(ses[[1]])), as.vector(terra::values(ses[[2]]))) # test spat variation

  ses <- SESraster(r, FUN=appmean, spat_alg = "bootspat_naive",
                   spat_alg_args=list(random="species"),
                   aleats = aleats,
                   force_wr_aleat_file = TRUE)

  expect_true(inherits(ses, "SpatRaster"), "TRUE")
  expect_true(terra::nlyr(ses) == length(nms))
  expect_named(ses, nms)

  expect_equal(unlist(ses[1]), setNames(as.double(rep(c(NA, 0), c(4, 2))), nms))

  ## example with 'vec_alg'
  appsv <- function(x, lyrv, na.rm=T, ...){
                        sumw <- function(x, lyrv, na.rm, ...){
                          ifelse(all(is.na(x)), NA,
                                 sum(x*lyrv, na.rm=na.rm, ...))
                        }
                        stats::setNames(terra::app(x, sumw, lyrv = lyrv, na.rm=na.rm, ...), "sumw")
  }
  n2 <- names(appsv(r, seq_len(terra::nlyr(r))))
  ses <- SESraster(r, FUN=appsv, FUN_args = list(lyrv = seq_len(terra::nlyr(r)), na.rm = TRUE),
                   Fa_sample = "lyrv",
                   Fa_alg = "sample", Fa_alg_args = list(replace=TRUE),
                   aleats = aleats,
                   filename = paste0(tempfile(),"ses.tif"))
  # names(ses)
  nms <- paste0(c("Observed", "Null_Mean", "Null_SD" ,"SES", "p_lower", "p_upper"), ".", n2)
  expect_equal(unlist(ses[1]), setNames(as.double(rep(c(NA, 0), c(4, 2))), nms))

  expect_equal(unlist(ses[2]), setNames(rep(0, length(nms)), nms))

  expect_equal(round(sd(terra::values(ses[[2]]), na.rm = TRUE), 3), as.double(4.413)) # test spat variation

  expect_error(SESraster(r, FUN=appsv, FUN_args = list(lyrv = seq_len(terra::nlyr(r)), na.rm = TRUE),
                         Fa_sample = numeric(1),
                         Fa_alg = "sample", Fa_alg_args = list(replace=TRUE),
                         aleats = aleats))

  expect_warning(SESraster(r, FUN=appsv, FUN_args = list(lyrv = seq_len(terra::nlyr(r)), na.rm = TRUE),
                         Fa_sample = c("lyrv", "appmean"),
                         Fa_alg = "sample", Fa_alg_args = list(replace=TRUE),
                         aleats = aleats))

  expect_error(SESraster(r, FUN=appsv, FUN_args = list(lyrv = seq_len(terra::nlyr(r)), na.rm = TRUE),
                         Fa_sample = c("appmean"),
                         Fa_alg = "sample", Fa_alg_args = list(replace=TRUE),
                         aleats = aleats))
  unlink(paste0(tempfile(),"ses.tif"))
})


test_that("algorithm_metrics works", {
  r <- load_ext_data()
  aleats <- 5
  set.seed(10)
  ambspp <- algorithm_metrics(r, spat_alg = "bootspat_naive", spat_alg_args=list(random="species"),
                              aleats = aleats)
  ambsite <- algorithm_metrics(r, spat_alg = "bootspat_naive", spat_alg_args=list(random="site"),
                               aleats = aleats)

  nms <- c("mean_diff", "sd_diff", "min_diff", "max_diff")

  expect_named(ambspp, c("spp_metrics", "spat_rich_diff"))
  expect_named(ambsite, c("spp_metrics", "spat_rich_diff"))

  ## DF tests
  expect_true(inherits(ambspp$spp_metrics, "data.frame"), "TRUE")
  expect_true(inherits(ambsite$spp_metrics, "data.frame"), "TRUE")

  expect_equal(dim(ambspp$spp_metrics), c(terra::nlyr(r), 11))
  expect_equal(dim(ambsite$spp_metrics), c(terra::nlyr(r), 11))

  expect_equal(ambspp$spp_metrics$actual, ambspp$spp_metrics$rand_avg)
  expect_equal(ambspp$spp_metrics$rand_avg, c(1, 14,  9, 12, 12, 10, 23))

  ## rast tests
  expect_true(inherits(ambspp$spat_rich_diff, "SpatRaster"), "TRUE")
  expect_true(inherits(ambsite$spat_rich_diff, "SpatRaster"), "TRUE")

  expect_true(terra::nlyr(ambsite$spat_rich_diff)==4)
  expect_true(terra::nlyr(ambspp$spat_rich_diff)==4)

  expect_equal(unlist(ambsite$spat_rich_diff[1]), setNames(as.double(rep(NA, 4)), nms))
  expect_equal(unlist(ambsite$spat_rich_diff[2]), setNames(rep(0, 4), nms))

  expect_equal(unlist(ambspp$spat_rich_diff[1]), setNames(as.double(rep(NA, 4)), nms))
  expect_equal(unlist(ambspp$spat_rich_diff[2]), setNames(c(2.8, 0.83666003, 2, 4), nms))

  expect_equal(round(mean(terra::values(ambspp$spat_rich_diff[[2]]), na.rm = TRUE), 3), as.double(1.144)) # test spat variation
  expect_equal(round(mean(terra::values(ambspp$spat_rich_diff[[1]]), na.rm = TRUE), 3), as.double(0)) # test spat variation

  ambff <- algorithm_metrics(r, spat_alg = "bootspat_naive", spat_alg_args=list(random="species"),
                              aleats = aleats,
                              force_wr_aleat_file = TRUE)

  expect_named(ambff, c("spp_metrics", "spat_rich_diff"))

  ## DF tests
  expect_true(inherits(ambff$spp_metrics, "data.frame"), "TRUE")

  ## rast tests
  expect_true(inherits(ambff$spat_rich_diff, "SpatRaster"), "TRUE")
})

test_that("algorithm_metrics works", {
  r <- load_ext_data()
  aleats <- 5
  set.seed(10)
  ambspp <- algorithm_metrics(r, spat_alg = "bootspat_naive", spat_alg_args=list(random="species"),
                              aleats = aleats)

  img <- function() {
    metrics <- ambspp[["spp_metrics"]]
    oldpar <- graphics::par()
    graphics::par(mfrow=c(3,1), mar=c(4,5,1,1))

    plot(metrics[,1], metrics[,"sp_reldiff"], pch=19,
         ylim=range(c(metrics[,"sp_reldiff_l"], metrics[,"sp_reldiff_u"])),
         xlab="Actual frequency", ylab="Species relative difference \n in frequency")

    plot(metrics[,1], metrics[,"global_reldiff"], pch=19, xlab="Actual frequency", ylab="Global relative difference \n in frequency",
         ylim=range(c(metrics[,"global_reldiff_l"], metrics[,"global_reldiff_u"])))

    plot(metrics[order(metrics[,1]),1], pch=19, ylab="Frequency", xlab="Species i")

    suppressWarnings(graphics::par(oldpar))
  }

  expect_identical(plot_alg_metrics(ambspp) , img())

  expect_identical(plot_alg_metrics(ambspp, what = "site") , terra::plot(ambspp[["spat_rich_diff"]]))
})

