#' Standardized effect sizes
#'
#' @description Calculates the standardized effect size using a custom function.
#'
#' @param x SpatRaster. A SpatRaster containing presence-absence data (0 or 1)
#' for a set of species.
#' @param aleats positive integer. A positive integer indicating how many times
#' the calculation should be repeated.
#' @param FUN custom function that works with SpatRaster objects. See examples
#' @param algorithm character. A character indicating the randomization method.
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
#' \code{\link{bootspat_ff}}, \code{\link{algorithm_performance}}
#'
#' @author Neander M. Heming and Gabriela Alves-Ferreira
#' @references Gotelli 2000
#'
#' @examples
#' library(SESraster)
#' library(terra)
#' r <- load_ext_data()
#' appmean <- function(x, ...){
#' terra::app(x, "mean", ...)
#' }
#' ses <- SESraster(r, FUN=appmean, algorithm = "SIM1", alg_args=list(random="species"))
#' plot(ses)
#' ses <- SESraster(r, FUN=appmean, algorithm = "SIM1", alg_args=list(random="site"))
#' plot(ses)
#' ses <- SESraster(r, FUN=appmean, algorithm = "SIM1",
#'                   alg_args=list(random="site"), FUN_args=list(na.rm=TRUE))
#' plot(ses)
#'
#' @export
SESraster <- function(x,
                      FUN=NULL,
                      algorithm = "SIM1",
                      FUN_args = NULL, alg_args = NULL,
                      aleats=10,
                      cores = 1, filename = "",
                      overwrite = FALSE, ...){

  if(!any(algorithm %in% paste0("SIM", 1:9))){
    stop("Please chose a proper algorithm")
  } else if(algorithm == "SIM1"){
    algorithm_f <- SESraster::bootspat_naive
  }

  # x rasters will be generated in this function, let's see if there is enough memory in the user's pc
  mi <- fit.memory(x[[1]], n=(aleats+3))

  temp.filename <- tempfile()
  temp.raster <- paste0(temp.filename, ".tif") # temporary names to rasters
  temp.a <- paste0(temp.filename, 1:aleats, ".tif") # create a vector with filenames for random rasters

  ## Null model (bootstrap structure)
  rast.rand <- list() # store rasters from loop

  # ## null raster characterization
  # res <- matrix(nrow = terra::nlyr(x), ncol = aleats)
  # actual <- setNames(sapply(x, function(x) terra::freq(x)[2,3]), names(x))

  for(i in 1:aleats){
    ### null distribution # TODO - use temporary file
    pres.site.null <- algorithm_f(x = x, unlist(alg_args))

    # calculate metric
    rast.rand[[i]] <- FUN(pres.site.null,
                          filename = ifelse(mi, "", temp.a[i]), FUN_args)

    # # calculate null distribution species incidence
    # res[,i] <- sapply(pres.site.null, function(x) terra::freq(x)[2,3])
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
  rast.obs <- FUN(x, filename = ifelse(mi, "", temp.raster), FUN_args)

  ## Calculating the standardized effect size (SES)
  # ses <-
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
#' @param plot logical. Should results be plotted?
#' @inheritParams SESraster
#'
#' @return a list with three components.
#' - randomization_results: a matrix with frequency of species occurrence on
#' each randomization
#' - spp_metrics: a matrix with metrics comparing actual and randomized frequency
#' of species occurrence. Metrics are average, sd, min, and max frequency across
#' randomizations, sp_reldiff (average difference relative to species frequency),
#' global_reldiff (average difference relative to the number of available cells),
#' upper and lower confidence intervals for sp_reldiff and global_reldiff.
#'
#'#' @seealso \code{\link{bootspat_str}}, \code{\link{bootspat_naive}},
#' \code{\link{bootspat_ff}}, \code{\link{SESraster}}
#'
#' @author Neander M. Heming
#' @examples
#' library(SESraster)
#' library(terra)
#' r <- load_ext_data()
#' appmean <- function(x, ...){
#' terra::app(x, "mean", ...)
#' }
#' algorithm_performance(r, FUN=appmean, algorithm = "SIM1", alg_args=list(random="species"))
#' algorithm_performance(r, FUN=appmean, algorithm = "SIM1", alg_args=list(random="site"))
#' #' algorithm_performance(r, FUN=appmean, algorithm = "SIM1", alg_args=list(random="both"))
#' @export
algorithm_performance <- function(x,
                                  algorithm, alg_args = NULL,
                                  aleats=10, plot=F, ...){

  ## algorithm selection
  if(algorithm == "SIM1"){
    algorithm_f <- SESraster::bootspat_naive
  }

  ## null raster characterization
  actual <- stats::setNames(sapply(x, function(x) terra::freq(x)[2,3]), names(x))
  actual_rich <- terra::app(x, "sum", na.rm=T)
  all <- unlist(terra::global(x[[1]], function(x)sum(!is.na(x), na.rm=T)))

  res <- matrix(nrow = terra::nlyr(x), ncol = aleats)
  rich_diff <- matrix(nrow = aleats, ncol = 2,
                      dimnames = list(1:aleats, c("N_cells_diff", "Proportion_cells_diff")))

  for(i in 1:aleats){
    ### null distribution
    pres.site.null <- algorithm_f(x = x, unlist(alg_args))

    ## calculate null distribution species incidence
    res[,i] <- sapply(pres.site.null, function(x) terra::freq(x)[2,3])

    ## calculate number of pixels with difference from actual richness
    null.rich.diff <- terra::app(c(actual_rich, pres.site.null),
                            function(x){
                              return(sum(x[-1], na.rm = T) - x[1])
                            })
    rich_diff[i, 1] <- unlist(terra::global(null.rich.diff, function(x)sum(x!=0, na.rm=T) ))
    rich_diff[i, 2] <- rich_diff[i, 1]/all
  }

  ## get randomized values for incidence
  comp_unstr <- as.data.frame(t(rbind(actual, apply(res, 1, function(x){
    c(rand_avg = mean(x, na.rm=T),
      rand_sd = stats::sd(x, na.rm=T),
      rand_min = min(x, na.rm=T),
      rand_max = max(x, na.rm=T))
  }))))

  ## compute relative difference
  comp_unstr$sp_reldiff <- (comp_unstr[,"rand_avg"] - comp_unstr[,"actual"])/comp_unstr[,"actual"]
  comp_unstr$global_reldiff <- (comp_unstr[,"rand_avg"] - comp_unstr[,"actual"])/all
  comp_unstr$sp_reldiff_l <- comp_unstr$sp_reldiff-comp_unstr[,"rand_sd"]/comp_unstr[,"actual"]
  comp_unstr$sp_reldiff_u <- comp_unstr$sp_reldiff+comp_unstr[,"rand_sd"]/comp_unstr[,"actual"]
  comp_unstr$global_reldiff_l <- comp_unstr$global_reldiff-comp_unstr[,"rand_sd"]/all
  comp_unstr$global_reldiff_u <- comp_unstr$global_reldiff+comp_unstr[,"rand_sd"]/all

  ## plotting results
  if(plot){
    # jpeg("../figs/freq_comparison.jpeg", width = 480*3, height = 480*2*2,
    #      quality = 100, res=300)
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

    graphics::par(oldpar)
  }

  return(list(randomization_results=res, spp_metrics=comp_unstr, rich_metrics=rich_diff))
}
