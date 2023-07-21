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
#' @param FUN_args Named list of arguments passed to the FUN
#' @param spat_alg A function with the algorithm implementing the desired
#' randomization method. It must work with SpatRaster objects. See examples.
#' Example of functions that work are: \code{\link{bootspat_naive}},
#' \code{\link{bootspat_str}}, \code{\link{bootspat_ff}}.
#'
#' @param spat_alg_args List of arguments passed to the randomization method
#' chosen in 'spat_alg'. See \code{\link{bootspat_naive}}, \code{\link{bootspat_str}},
#' \code{\link{bootspat_ff}}
#' @param Fa_sample Named list of length 1 with a FUN argument (e.g. a vector)
#' to be randomized
#' @param Fa_alg function to randomize any non spatial argument to be passed
#' to 'FUN'.
#' @param Fa_alg_args Named list of arguments passed to the function in 'Fa_alg'
#' @param force_wr_aleat_file logical. Force writing bootstrapped rasters, even if
#' files fit in memory. Mostly used for internal test units.
#' @inheritParams terra::app
#' @param ... additional arguments passed to 'terra::app()' function.
#'
#' @return SpatRaster. The function returns the observed metric, the mean of the
#'  simulations calculated over n=aleats times, the standard deviation of the
#'  simulations, and the standardized effect size (SES) for the metric defined in FUN.
#'
#' @details Perform n=aleats spatial randomizations based on the randomization
#' method defined in 'spat_alg' argument and calculates the metric
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
#' ses <- SESraster(r, FUN=appmean, spat_alg = "bootspat_naive", spat_alg_args=list(random="species"),
#'                  aleats = 4)
#' plot(ses)
#' ses <- SESraster(r, FUN=appmean, spat_alg = "bootspat_naive", spat_alg_args=list(random="site"),
#'                  aleats = 4)
#' plot(ses)
#'
#' ## example of how to use 'FUN_args'
#' r[7][1] <- NA
#' plot(r)
#' set.seed(10)
#' sesNA <- SESraster(r, FUN=appmean, FUN_args = list(na.rm = FALSE),
#'                  spat_alg = "bootspat_naive", spat_alg_args=list(random = "species"),
#'                  aleats = 4)
#' plot(sesNA)
#'
#' set.seed(10)
#' ses <- SESraster(r, FUN=appmean, FUN_args = list(na.rm = TRUE),
#'                 spat_alg = "bootspat_naive", spat_alg_args=list(random = "species"),
#'                  aleats = 4)
#' plot(ses)
#'
#' ## example with 'Fa_alg'
#' appsv <- function(x, lyrv, na.rm = FALSE, ...){
#'                       sumw <- function(x, lyrv, na.rm, ...){
#'                             ifelse(all(is.na(x)), NA,
#'                                     sum(x*lyrv, na.rm=na.rm, ...))
#'                       }
#'                       stats::setNames(terra::app(x, sumw, lyrv = lyrv, na.rm=na.rm, ...), "sumw")
#'                     }
#' set.seed(10)
#' ses <- SESraster(r, FUN=appsv,
#'                  FUN_args = list(lyrv = seq_len(nlyr(r)), na.rm = TRUE),
#'                     Fa_sample = "lyrv",
#'                     Fa_alg = "sample", Fa_alg_args = list(replace=FALSE),
#'                     aleats = 4)
#' plot(ses)
#'
#' set.seed(10)
#' ses <- SESraster(r, FUN=appsv,
#'                  FUN_args = list(lyrv = seq_len(nlyr(r)), na.rm = TRUE),
#'                     Fa_sample = "lyrv",
#'                     Fa_alg = "sample", Fa_alg_args = list(replace=TRUE),
#'                     aleats = 4)
#' plot(ses)
#'
#' @export
SESraster <- function(x,
                      FUN = NULL, FUN_args = list(),
                      spat_alg = NULL, spat_alg_args = list(),
                      Fa_sample = NULL, Fa_alg = NULL, Fa_alg_args = list(),
                      aleats = 10,
                      cores = 1, filename = "",
                      overwrite = FALSE,
                      force_wr_aleat_file = FALSE, ...){

  ## Find the corresponding functions
  FUN <- match.fun(FUN)
  if(is.null(spat_alg)){
    spat_alg <- function(x)x
  }
  spat_alg <- match.fun(spat_alg)

  # create file names for temporary raster files for aleats, then delete them to clean up HD
  temp.filename <- tempfile()
  temp.raster <- paste0(temp.filename, ".tif") # temporary names to rasters
  temp.a <- paste0(temp.filename, "a", ".tif") # create a vector with filenames for random rasters
  temp.r <- paste0(tempfile(), "r", 1:aleats, ".tif") # create a vector with filenames for random FUN rasters

  # x rasters will be generated in this function, let's see if there is enough memory in the user's pc
  mi <- fit.memory(c(x, x[[1]]), n=(aleats+3))

  add_fn <- FALSE
  if(isFALSE(mi)) {
    ## get argument names and include "filename = ifelse(mi, "", temp.a[i])" into spat_alg_args
    add_fn[] <- any(grepl("filename", methods::formalArgs(args(spat_alg)))) #& # check if spat_alg has 'filename' arg
    # !any(grepl("filename", names(spat_alg_args))) # check if 'filename' is in supplied arguments for spat_alg
  }

  ## if needs to create a file, add temporary empty element to be filled on aleats loop
  if(add_fn | force_wr_aleat_file){
    spat_alg_args[["filename"]] <- temp.a # ""
    spat_alg_args[["overwrite"]] <- TRUE # ""
  }

  ## add filename item to FUN args
  FUN_args[["filename"]] <- ifelse(mi, "", temp.raster)

  ## argument used by FUN that will be randomized
  if(!is.null(Fa_sample)){
    if(!inherits(Fa_sample, "character")){
      stop("Fa_sample needs to be of class 'character'")
    }
    if(length(Fa_sample)>1) {
      Fa_sample <- Fa_sample[1]
      warning("Only the first element of 'Fa_sample' will be used")
    }
    if(!Fa_sample %in% names(FUN_args)){
      stop(paste(Fa_sample, "not found in 'FUN_args'"))
    }

    ## Find the corresponding function
    Fa_alg <- match.fun(Fa_alg)

  }

  ### Observed value
  rast.obs <- rlang::exec(FUN, x, !!!FUN_args)

  ## Null model (bootstrap structure)
  rast.rand <- list() # store rasters from loop

  for(i in 1:aleats){

    ### null distribution
    pres.site.null <- rlang::exec(spat_alg, x, !!!spat_alg_args)

    ## Randomize one FUN_arg to compute metric
    if(!is.null(Fa_sample)){
      FUN_args[[Fa_sample]] <- rlang::exec(Fa_alg, FUN_args[[Fa_sample]], !!!Fa_alg_args)
    }

    ### calculate metric
    rast.rand[[i]] <- rlang::exec(FUN, pres.site.null, !!!FUN_args)

  }

  ### SES
  rcomb <- matrix(seq_len(aleats*terra::nlyr(rast.obs)), ncol = aleats)

  rast.rand <- terra::rast(rast.rand) # transform a list into a SpatRaster

  ## vector to store results
  resu <- stats::setNames(as.double(rep(NA, 4)),
                          c("Observed", "Null_Mean", "Null_SD", "SES"))

  ## SES for multiple layers
  ses <- terra::rast(lapply(seq_len(nrow(rcomb)),
                            function(l, ro, rr, rcomb, resu,
                                     mi, cores, temp.filename, overwrite, ...){

                              lnm <- names(ro[[rcomb[l,1]]])
                              resu <- stats::setNames(resu, paste(names(resu), lnm, sep = "."))

                              ## Calculating the standardized effect size (SES)
                              return(terra::app(c(ro[[rcomb[l,1]]], rr[[rcomb[l,]]]),
                                                fun=function(x, lnm, resu){

                                                  nm <- mean(x[-1], na.rm=TRUE) ### Randomized mean value
                                                  nsd <- stats::sd(x[-1], na.rm=TRUE) ### Randomized stdev value

                                                  resu[] <- c(x[1], nm, nsd, ifelse(nsd==0, (x[1]-nm), (x[1]-nm)/nsd))

                                                  return(resu)

                                                },
                                                lnm = lnm, resu = resu,
                                                cores = cores,
                                                filename = ifelse(mi, "", paste0(temp.filename, l, "out.tif")),
                                                overwrite = overwrite, ...))

                            }, ro = rast.obs, rr = rast.rand, rcomb = rcomb, resu = resu,
                            mi = mi,
                            cores = cores, temp.filename = temp.filename,
                            overwrite = overwrite, ...))

  if(filename != ""){
    ses <- terra::writeRaster(ses, filename, overwrite = overwrite, ...)
  }

  ## HD Cleanup
  unlink(temp.a) # delete the file that will not be used
  unlink(temp.raster) # delete the file that will not be used
  # unlink(paste0(temp.filename, "avg.tif"))
  # unlink(paste0(temp.filename, "sd.tif"))
  unlink(paste0(temp.filename, seq_len(nrow(rcomb)), "out.tif"))

  return(ses)
}



#' Performance of randomization algorithms
#'
#' @description Compares the richness and occurrence incidence across species
#' between actual and randomized species distributions
#'
#' @inheritParams SESraster
#'
#' @return a list with two components:
#' \itemize{
##'    \item{spp_metrics: a matrix with metrics comparing actual and randomized frequency
##'    of species occurrence. Metrics are average, sd, min, and max frequency across
##'    randomizations, sp_reldiff (average difference relative to species frequency),
##'    global_reldiff (average difference relative to the number of available cells),
##'    upper and lower confidence intervals for sp_reldiff and global_reldiff.}
##'    \item{spat_rich_diff: a SpatRaster with summary statistics about differences
##'    between actual and bootstrapped site (cell) richness}
##'}
#'
#' @seealso \code{\link{bootspat_str}}, \code{\link{bootspat_naive}},
#' \code{\link{bootspat_ff}}, \code{\link{SESraster}}, \code{\link{plot_alg_metrics}}
#'
#' @author Neander M. Heming
#' @examples
#' library(SESraster)
#' library(terra)
#' r <- load_ext_data()
#' algorithm_metrics(r, spat_alg = "bootspat_naive", spat_alg_args=list(random="species"), aleats = 3)
#' algorithm_metrics(r, spat_alg = "bootspat_naive", spat_alg_args=list(random="site"), aleats = 3)
#' # algorithm_metrics(r, spat_alg = "bootspat_naive", spat_alg_args=list(random="both"))
#'
#' @export
algorithm_metrics <- function(x,
                              spat_alg = NULL, spat_alg_args = NULL,
                              aleats = 10, # plot = FALSE,
                              filename = "",
                              force_wr_aleat_file = FALSE, ...){

  ## check spat_alg function
  spat_alg <- match.fun(spat_alg)

  # create file names for temporary raster files for aleats, then delete them to clean up HD
  temp.a <- paste0(tempfile(), "a", ".tif") # create a vector with filenames for random rasters
  temp.r <- paste0(tempfile(), "r", 1:aleats, ".tif") # create a vector with filenames for random richness rasters

  ## test if rasters fit in RAM memory,  n=aleats*2+1 rasters will be generated in this function
  mi <- fit.memory(x[[1]], n=(aleats*2+1))

  ## if doesn't fit in memory,
  # - add filename into args
  add_fn <- FALSE
  if(isFALSE(mi)){
    ## get argument names and include "filename = ifelse(mi, "", temp.a[i])" into spat_alg_args
    add_fn[] <- any(grepl("filename", methods::formalArgs(args(spat_alg)))) #& # check if spat_alg has 'filename' arg
                   # !any(grepl("filename", names(spat_alg_args))) # check if 'filename' is in supplied arguments for spat_alg
  }

  ## if needs to create a file, add temporary empty element to be filled on aleats loop
  if(add_fn | force_wr_aleat_file){
    spat_alg_args[["filename"]] <- temp.a # ""
    spat_alg_args[["overwrite"]] <- TRUE # ""
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
    ## add filename.i to spat_alg args
    # if(add_fn){
    #   spat_alg_args$filename[] <- temp.a[i]
    # }

    ### null distribution
    pres.site.null <- rlang::exec(spat_alg, x, !!!spat_alg_args)

    ## calculate null distribution species incidence
    res[,i] <- sapply(pres.site.null, function(x) terra::freq(x)[2,3])

    ## calculate number of pixels with difference from actual richness
    null.rich.diff[[i]] <- terra::app(c(actual_rich, pres.site.null),
                                      function(x){
                                        return(sum(x[-1], na.rm = T) - x[1])
                                      }, filename = ifelse(mi, "", temp.r[i]))
  }

  ## get randomized values for incidence
  ## compute relative difference
  metrics <- as.data.frame(t(apply(cbind(actual, res), 1,
                                   function(x, all){
                                     sm <- c(x[1], # actual values
                                             rand_avg = mean(x[-1], na.rm=T),
                                             rand_sd  = stats::sd(x[-1], na.rm=T),
                                             rand_min = min(x[-1], na.rm=T),
                                             rand_max = max(x[-1], na.rm=T))
                                     sm_d <- c(stats::setNames( (sm[2] - x[1])/x[1], "sp_reldiff"),
                                               stats::setNames( (sm[2] - x[1])/all, "global_reldiff") )
                                     sm_ci <- c(stats::setNames( sm_d[1] - sm[3]/x[1], "sp_reldiff_l"),
                                                stats::setNames( sm_d[1] + sm[3]/x[1], "sp_reldiff_u"),
                                                stats::setNames( sm_d[2] - sm[3]/all, "global_reldiff_l"),
                                                stats::setNames( sm_d[2] + sm[3]/all, "global_reldiff_u") )
                                     return(c(sm, sm_d, sm_ci))
                                   }, all=all)
  ))

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
                                                   paste0(c("mean", "sd", "min", "max"), "_diff")),
                               filename = filename, ...)

  # Clean up files from HD
  unlink(c(temp.a, temp.r, paste0(tempfile(), "ar.tif")))

  return(list(spp_metrics = metrics,
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
#' am1 <- algorithm_metrics(r, spat_alg = "bootspat_naive", spat_alg_args=list(random="species"))
#' am2 <- algorithm_metrics(r, spat_alg = "bootspat_naive", spat_alg_args=list(random="site"))
#' plot_alg_metrics(am1)
#' plot_alg_metrics(am2)
#' plot_alg_metrics(am1, "site")
#'
#' @export
plot_alg_metrics <- function(x, what="spp", ...) {
  if(what == "site"){
    terra::plot(x[["spat_rich_diff"]], ...)
  } else {
    metrics <- x[["spp_metrics"]]
    oldpar <- graphics::par()
    graphics::par(mfrow=c(3,1), mar=c(4,5,1,1))

    plot(metrics[,1], metrics[,"sp_reldiff"], pch=19,
         ylim=range(c(metrics[,"sp_reldiff_l"], metrics[,"sp_reldiff_u"])),
         xlab="Actual frequency", ylab="Species relative difference \n in frequency")
    graphics::segments(x0=metrics[,1],
                       y0=metrics[,"sp_reldiff_l"], y1=metrics[,"sp_reldiff_u"],
                       col="gray", lwd=1)
    graphics::points(metrics[,1], metrics[,"sp_reldiff_l"], pch="-", col="gray", cex=2.5)
    graphics::points(metrics[,1], metrics[,"sp_reldiff_u"], pch="-", col="gray", cex=2.5)
    graphics::points(metrics[,1], metrics[,"sp_reldiff"], pch=19)

    plot(metrics[,1], metrics[,"global_reldiff"], pch=19, xlab="Actual frequency", ylab="Global relative difference \n in frequency",
         ylim=range(c(metrics[,"global_reldiff_l"], metrics[,"global_reldiff_u"])))
    graphics::segments(x0=metrics[,1],
                       y0=metrics[,"global_reldiff_l"], y1=metrics[,"global_reldiff_u"],
                       col="gray", lwd=1)
    graphics::points(metrics[,1], metrics[,"global_reldiff_l"], pch="-", col="gray", cex=2.5)
    graphics::points(metrics[,1], metrics[,"global_reldiff_u"], pch="-", col="gray", cex=2.5)
    graphics::points(metrics[,1], metrics[,"global_reldiff"], pch=19)

    plot(metrics[order(metrics[,1]),1], pch=19, ylab="Frequency", xlab="Species i")
    graphics::segments(x0=seq_len(nrow(metrics)),
                       y0=metrics[order(metrics[,1]),4], y1=metrics[order(metrics[,1]),5],
                       col="red", lwd=2)
    graphics::points(metrics[order(metrics[,1]),2], pch="--", col="black", cex=2)
    graphics::points(metrics[order(metrics[,1]),4], pch="-", col="red", cex=2.5)
    graphics::points(metrics[order(metrics[,1]),5], pch="-", col="red", cex=2.5)
    graphics::legend("topleft", legend=c("Actual", "Sampled"), title = "Frequency of occupied pixels", pch=c(19,3), col=c("black", "red"))

    suppressWarnings(graphics::par(oldpar))
  }
}
