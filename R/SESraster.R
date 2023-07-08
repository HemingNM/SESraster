#' Standardized effect sizes for SpatRaster objects
#'
#' @description Calculates the standardized effect sizes using a custom function
#' and a null model algorithm.
#'
#' @param x SpatRaster. A SpatRaster containing presence-absence data (0 or 1)
#' for a set of species.
#' @param aleats positive integer. A positive integer indicating how many times
#' the calculation should be repeated.
#' @param FUN The function to be applied. It must work with SpatRaster objects.
#' See examples.
#' @param algorithm The function implementing the desired randomization method.
#' It must work with SpatRaster objects. See examples. Example of functions that
#' work are: \code{\link{bootspat_naive}}, \code{\link{bootspat_str}}, \code{\link{bootspat_ff}}.
#' @param FUN_args List of arguments passed to the FUN
#' @param alg_args List of arguments passed to the randomization method chosen
#' in 'algorithm'.
#' See \code{\link{bootspat_naive}}, \code{\link{bootspat_str}}, \code{\link{bootspat_ff}}
#' @inheritParams terra::app
#' @param ... additional arguments passed to 'terra::app()' function.
#'
#' @return SpatRaster. The function returns the observed metric, the mean of the
#'  simulations calculated over n=aleats times, the standard deviation of the
#'  simulations, and the standardized effect size (SES) for the metric defined in FUN.
#'
#' @details Perform n=aleats spatial randomizations based on the randomization
#' method defined in 'algorithm' argument and calculates the metric
#' defined in 'FUN' argument. The function (FUN) to calculate the desired metric
#' must work with any of \link[terra]{app}, \link[terra]{focal},
#' \link[terra]{focal3D} family of functions.
#'
#' @seealso \code{\link{bootspat_str}}, \code{\link{bootspat_naive}},
#' \code{\link{bootspat_ff}}, \code{\link{algorithm_metrics}}
#'
#' @author Neander M. Heming and Gabriela Alves-Ferreira
#' @references Gotelli 2000
#'
#' @examples
#' library(SESraster)
#' library(terra)
#' r <- load_ext_data()
#' appmean <- function(x, ...){
#'                       terra::app(x, "mean", ...)
#'                     }
#' ses <- SESraster(r, FUN=appmean, algorithm = "bootspat_naive", alg_args=list(random="species"))
#' plot(ses)
#' ses <- SESraster(r, FUN=appmean, algorithm = "bootspat_naive", alg_args=list(random="site"))
#' plot(ses)
#'
#' ## example of how to use 'FUN_args'
#' r[7][1] <- NA
#' plot(r)
#' sesNA <- SESraster(r, FUN=appmean, algorithm = "bootspat_naive",
#'                  FUN_args = list(na.rm = FALSE), alg_args=list(random = "species"))
#' plot(sesNA)
#'
#' ses <- SESraster(r, FUN=appmean, algorithm = "bootspat_naive",
#'                FUN_args = list(na.rm = TRUE), alg_args=list(random = "species"))
#' plot(ses)
#'
#' @export
SESraster <- function(x,
                      FUN = NULL,
                      algorithm = NULL,
                      FUN_args = NULL, alg_args = NULL,
                      aleats=10,
                      cores = 1, filename = "",
                      overwrite = FALSE, ...){

  FUN <- match.fun(FUN)
  algorithm <- match.fun(algorithm)

  # x rasters will be generated in this function, let's see if there is enough memory in the user's pc
  mi <- fit.memory(c(x, x[[1]]), n=(aleats+3))

  temp.filename <- tempfile()
  temp.raster <- paste0(temp.filename, ".tif") # temporary names to rasters
  temp.a <- paste0(temp.filename, 1:aleats, ".tif") # create a vector with filenames for random rasters
  temp.r <- paste0(tempfile(), "r", 1:aleats, ".tif") # create a vector with filenames for random FUN rasters

  add_fn <- FALSE
  if(isFALSE(mi)) {
    ## get argument names and include "filename = ifelse(mi, "", temp.a[i])" into alg_args
    add_fn <- any(grepl("filename", methods::formalArgs(args(algorithm)))) #& # check if algorithm has 'filename' arg
                 # !any(grepl("filename", names(alg_args))) # check if 'filename' is in supplied arguments for algorithm
    # add_m <- (any(grepl("filename", frl_alg)) & !any(grepl("filename", g_aa)))
    if(add_fn){
      alg_args[["filename"]] <- ""
    }
  }
  ## add filename item to FUN args
  FUN_args[["filename"]] <- ""

  ## Null model (bootstrap structure)
  rast.rand <- list() # store rasters from loop

  for(i in 1:aleats){
    if(add_fn){ ## use temporary file
      alg_args$filename[] <- temp.a[i]
    }

    ### null distribution # TODO -
    pres.site.null <- rlang::exec(algorithm, x, !!!alg_args)


    # calculate metric
    FUN_args[["filename"]][] <- ifelse(mi, "", temp.r[i])
    rast.rand[[i]] <- rlang::exec(FUN, pres.site.null, !!!FUN_args)
  }

  rast.rand <- terra::rast(rast.rand) # transform a list into a SpatRaster

  ### Randomized mean value
  rast.rand.avg <- terra::mean(rast.rand, na.rm = TRUE, #cores = cores,
                               overwrite = overwrite,
                               filename = ifelse(mi, "", paste0(temp.filename, "avg.tif")))
  ### Randomized stdev value
  rast.rand.sd <- terra::stdev(rast.rand, na.rm = TRUE, #cores = cores,
                               overwrite = overwrite,
                               filename = ifelse(mi, "", paste0(temp.filename, "sd.tif")))

  ### Observed value
  FUN_args[["filename"]][] <- ifelse(mi, "", temp.raster)
  rast.obs <- rlang::exec(FUN, x, !!!FUN_args)
    # FUN(x, filename = ifelse(mi, "", temp.raster), FUN_args)

  ## Calculating the standardized effect size (SES)
  out <- terra::app(c(rast.obs, rast.rand.avg, rast.rand.sd),
                    fun=function(x){
                      return(c(Observed = x[1],
                               Null_Mean = x[2],
                               Null_SD = x[3],
                               SES = (x[1]-x[2])/x[3]))
                    }, cores = cores, filename = filename,
                    overwrite = overwrite, ...)

  ## HD Cleanup
  unlink(temp.a) # delete the file that will not be used
  unlink(temp.raster) # delete the file that will not be used
  unlink(paste0(temp.filename, "avg.tif"))
  unlink(paste0(temp.filename, "sd.tif"))

  return(out)
}



#' Performance of randomization algorithms
#'
#' @description Compares the richness and occurrence incidence across species
#' between actual and randomized species distributions
#'
#' @inheritParams SESraster
# #' @param plot logical. Should results be plotted?
#'
#' @return a list with two components.
#' - spp_metrics: a matrix with metrics comparing actual and randomized frequency
#' of species occurrence. Metrics are average, sd, min, and max frequency across
#' randomizations, sp_reldiff (average difference relative to species frequency),
#' global_reldiff (average difference relative to the number of available cells),
#' upper and lower confidence intervals for sp_reldiff and global_reldiff.
#' - spat_rich_diff: a SpatRaster with summary statistics about differences
#' between actual and bootstrapped site (cell) richness
#'
#'#' @seealso \code{\link{bootspat_str}}, \code{\link{bootspat_naive}},
#' \code{\link{bootspat_ff}}, \code{\link{SESraster}}, \code{\link{plot_alg_metrics}}
#'
#' @author Neander M. Heming
#' @examples
#' library(SESraster)
#' library(terra)
#' r <- load_ext_data()
#' algorithm_metrics(r, algorithm = "bootspat_naive", alg_args=list(random="species"))
#' algorithm_metrics(r, algorithm = "bootspat_naive", alg_args=list(random="site"))
#' # algorithm_metrics(r, algorithm = "bootspat_naive", alg_args=list(random="both"))
#'
#' @export
algorithm_metrics <- function(x,
                              algorithm = NULL, alg_args = NULL,
                              aleats = 10, # plot = FALSE,
                              filename = "", ...){

  ## check algorithm function
  algorithm <- match.fun(algorithm)

  ## test if rasters fit in RAM memory,  n=aleats*2+1 rasters will be generated in this function
  mi <- fit.memory(x[[1]], n=(aleats*2+1))
  ## if doesn't fit in memory,
  # - add filename into args
  # - create temporary raster files for aleats, then delete them to clean up HD
  add_fn <- FALSE
  if(isFALSE(mi)){
    temp.a <- paste0(tempfile(), 1:aleats, ".tif") # create a vector with filenames for random rasters
    temp.r <- paste0(tempfile(), "r", 1:aleats, ".tif") # create a vector with filenames for random richness rasters

    ## get argument names and include "filename = ifelse(mi, "", temp.a[i])" into alg_args
    add_fn[] <- any(grepl("filename", methods::formalArgs(args(algorithm)))) #& # check if algorithm has 'filename' arg
                   # !any(grepl("filename", names(alg_args))) # check if 'filename' is in supplied arguments for algorithm
    # add_m <- (any(grepl("filename", frl_alg)) & !any(grepl("filename", g_aa)))
    if(add_fn){
      alg_args[["filename"]] <- ""
      #   alg_args$filename <- ""
    }
  }

  ## null raster characterization
  actual <- stats::setNames(sapply(x, function(x) terra::freq(x)[2,3]), names(x))
  actual_rich <- terra::app(x, "sum", na.rm=T, filename = ifelse(mi, "", paste0(tempfile(), "ar.tif")))
  all <- unlist(terra::global(x[[1]], function(x)sum(!is.na(x), na.rm=T)))

  ## store bootstrapped frequencies
  res <- matrix(nrow = terra::nlyr(x), ncol = aleats)

  ## store Null model (bootstrap) rasters
  null.rich.diff <- list() # store rasters from loop

  for(i in 1:aleats){
    ## add filename.i to algorithm args
    if(add_fn){
      alg_args$filename[] <- temp.a[i]
    }

    ### null distribution
    pres.site.null <- rlang::exec(algorithm, x, !!!alg_args) # algorithm(x = x, unlist(alg_args))

    ## calculate null distribution species incidence
    res[,i] <- sapply(pres.site.null, function(x) terra::freq(x)[2,3])

    ## calculate number of pixels with difference from actual richness
    null.rich.diff[[i]] <- terra::app(c(actual_rich, pres.site.null),
                                      function(x){
                                        return(sum(x[-1], na.rm = T) - x[1])
                                      }, filename = ifelse(mi, "", temp.r[i]))
  }

  ## get randomized values for incidence
  comp_unstr <- as.data.frame(t(rbind(actual_freq=actual, apply(res, 1, function(x){
    c(rand_avg = mean(x, na.rm=T),
      rand_sd = stats::sd(x, na.rm=T),
      rand_min = min(x, na.rm=T),
      rand_max = max(x, na.rm=T))
  }))))

  ## compute relative difference
  comp_unstr$sp_reldiff <- (comp_unstr[,"rand_avg"] - comp_unstr[,"actual_freq"])/comp_unstr[,"actual_freq"]
  comp_unstr$global_reldiff <- (comp_unstr[,"rand_avg"] - comp_unstr[,"actual_freq"])/all
  comp_unstr$sp_reldiff_l <- comp_unstr$sp_reldiff-comp_unstr[,"rand_sd"]/comp_unstr[,"actual_freq"]
  comp_unstr$sp_reldiff_u <- comp_unstr$sp_reldiff+comp_unstr[,"rand_sd"]/comp_unstr[,"actual_freq"]
  comp_unstr$global_reldiff_l <- comp_unstr$global_reldiff-comp_unstr[,"rand_sd"]/all
  comp_unstr$global_reldiff_u <- comp_unstr$global_reldiff+comp_unstr[,"rand_sd"]/all

  ## compute spatial metrics for richness differences
  # transform the list into a SpatRaster and compute mean and sd
  spat.rich.diff <- terra::app(terra::rast(null.rich.diff),
                               function(x, rdiff){
                                 if(all(is.na(x))){
                                   rdiff[] <- NA
                                 } else {
                                   rdiff[] <- c(mean(x, na.rm=TRUE), stats::sd(x, na.rm=TRUE),
                                                range(x, na.rm=TRUE))
                                 }
                                 return(rdiff)
                               }, rdiff = stats::setNames(vector("double", 4),
                                                   paste(c("mean", "sd", "min", "max"), "_diff")),
                               filename = filename, ...)

  # Clean up files from HD
  if(isFALSE(mi)){
    unlink(c(temp.a, temp.r, paste0(tempfile(), "ar.tif")))
  }
  return(list(spp_metrics = comp_unstr,
              spat_rich_diff = spat.rich.diff))
}


#' Plot performance of randomization algorithms
#'
#' @description Plots objects returned by \code{\link{algorithm_metrics}}
#'
#' @param x list. Object returned by \code{\link{algorithm_metrics}}
#' @param what What should be plotted, "species" or "site" metrics?
#' @param ... Additional parameters passed to \code{\link[terra]{plot}}
#'
#' @seealso \code{\link{algorithm_metrics}}
#'
#' @author Neander M. Heming
#' @examples
#' library(SESraster)
#' library(terra)
#' r <- load_ext_data()
#' am1 <- algorithm_metrics(r, algorithm = "bootspat_naive", alg_args=list(random="species"))
#' am2 <- algorithm_metrics(r, algorithm = "bootspat_naive", alg_args=list(random="site"))
#' plot_alg_metrics(am1)
#' plot_alg_metrics(am2)
#' plot_alg_metrics(am1, "site")
#'
#' @export
plot_alg_metrics <- function(x, what="spp", ...) {
  if(what == "site"){
    terra::plot(x[["spat_rich_diff"]], ...)
  } else {
    comp_unstr <- x[["spp_metrics"]]
    oldpar <- graphics::par()
    graphics::par(mfrow=c(3,1), mar=c(4,5,1,1))

    plot(comp_unstr[,1], comp_unstr[,"sp_reldiff"], pch=19,
         ylim=range(c(comp_unstr[,"sp_reldiff_l"], comp_unstr[,"sp_reldiff_u"])),
         xlab="Actual frequency", ylab="Species relative difference \n in frequency")
    graphics::segments(x0=comp_unstr[,1],
                       y0=comp_unstr[,"sp_reldiff_l"], y1=comp_unstr[,"sp_reldiff_u"],
                       col="gray", lwd=1)
    graphics::points(comp_unstr[,1], comp_unstr[,"sp_reldiff_l"], pch="-", col="gray", cex=2.5)
    graphics::points(comp_unstr[,1], comp_unstr[,"sp_reldiff_u"], pch="-", col="gray", cex=2.5)
    graphics::points(comp_unstr[,1], comp_unstr[,"sp_reldiff"], pch=19)

    plot(comp_unstr[,1], comp_unstr[,"global_reldiff"], pch=19, xlab="Actual frequency", ylab="Global relative difference \n in frequency",
         ylim=range(c(comp_unstr[,"global_reldiff_l"], comp_unstr[,"global_reldiff_u"])))
    graphics::segments(x0=comp_unstr[,1],
                       y0=comp_unstr[,"global_reldiff_l"], y1=comp_unstr[,"global_reldiff_u"],
                       col="gray", lwd=1)
    graphics::points(comp_unstr[,1], comp_unstr[,"global_reldiff_l"], pch="-", col="gray", cex=2.5)
    graphics::points(comp_unstr[,1], comp_unstr[,"global_reldiff_u"], pch="-", col="gray", cex=2.5)
    graphics::points(comp_unstr[,1], comp_unstr[,"global_reldiff"], pch=19)

    plot(comp_unstr[order(comp_unstr[,1]),1], pch=19, ylab="Frequency", xlab="Species i")
    graphics::segments(x0=seq_len(nrow(comp_unstr)),
                       y0=comp_unstr[order(comp_unstr[,1]),4], y1=comp_unstr[order(comp_unstr[,1]),5],
                       col="red", lwd=2)
    graphics::points(comp_unstr[order(comp_unstr[,1]),2], pch="--", col="black", cex=2)
    graphics::points(comp_unstr[order(comp_unstr[,1]),4], pch="-", col="red", cex=2.5)
    graphics::points(comp_unstr[order(comp_unstr[,1]),5], pch="-", col="red", cex=2.5)
    graphics::legend("topleft", legend=c("Actual", "Sampled"), title = "Frequency of occupied pixels", pch=c(19,3), col=c("black", "red"))

    suppressWarnings(graphics::par(oldpar))
  }
}
