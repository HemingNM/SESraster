
#' Adjust probability of sampling based on frequency of occurrences.
#'
#' This function is used to adjust the probability of a species to be sampled
#' across the raster, so that the sampled frequency of occurrence of the
#' species is closer to the observed
#'
#' @param x SpatRaster. A presence-absence raster (stack).
#' @param rprob SpatRaster. A raster (stack) of probabilities.
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' \dontrun{
#' library(SESraster)
#' library(terra)
#' # creating random species distributions
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- rast(f)
#' set.seed(510)
#' r10 <- rast(lapply(1:18,
#'                 function(i, r, mn, mx){
#'                   app(r, function(x, t){
#'                     sapply(x, function(x, t){
#'                        x<max(t) & x>min(t)
#'                     }, t=t)
#'                   }, t=sample(seq(mn, mx), 2))
#'                 }, r=r, mn=minmax(r)[1]+10, mx=minmax(r)[2]-10))
#'
#' names(r10) <- paste("sp", 1:nlyr(r10))
#' fr2prob(r10)
#' # raw frequencies
#' unlist(terra::global(x, function(x)sum(x, na.rm=T)))
#' }
fr2prob <- function(x, rprob=NULL){
  fr <- unlist(terra::global(x, function(x)sum(x, na.rm=T)))
  all <- unlist(terra::global(x[[1]], function(x)sum(!is.na(x), na.rm=T)))
  p <- fr/all

  if(is.null(rprob)){
    # in frp*(1/all), fpr=all  # probability across all raster
    # so all*(1/all), and
    # ppr <- 1
    p/(1-p)
  } else {
    frp <- unlist(terra::global(rprob, function(x)sum(x, na.rm=T)))
    ppr <- frp/all # probability when there are constraints (not all raster is available)
    # frp*(1/all) # probability when there are constraints (not all raster is available)
    (p/(1-p))/ppr
  }
}

#' Vectorized structured sample
#'
#' @param x vector containing sample size (i.e. richness) in the first element
#' and probabilities on the remaining
#' @param sp elements to be sampled
#' @param resu vector of results, must be the same length of sp
#' @param fr_prob Frequency or probability of each sp element. This will be multiplied by the
#' probability vector
#' @return vector of sampled sp elements
#' @author Neander Marcel Heming
# #' @examples
#'
#' @keywords internal
.str.sample <- function(x, sp, resu, fr_prob){
  if(is.na(x[1])) {
    resu[] <- NA
  } else {
    resu[sample(sp, size = x[1], prob = x[-1]*fr_prob)] <- 1
  }
  resu
}

#' Spatially structured sample
#'
#' Randomizes a raster stack with fixed richness.
#' Randomizations are based on frequencies (given or calculated from x)
#' and, optionally, a probability raster stack.
#' Both, frequencies and probability raster stack, control the probability of a
#' given species is sampled in each cell raster. Frequency control the probability
#' of each species compared to all others. Probability raster stack control the
#' probability that each species is sampled in a given raster cell.
#'
#' @param x SpatRaster. A presence-absence SpatRaster.
#' @param rprob SpatRaster. Stack of probability values. Structures the spatial
#' pattern of each randomized species.
#' @param rich SpatRaster. Richness pattern structuring the sample size of
#' each cell randomization. Calculated if not provided.
#' @param fr_prob Either frequency of pixels or probability that a species is
#' observed across the whole layer.
#' @inheritParams terra::app
# #' @param cores
# #' @param filename Character. Filename for output SpatRaster.
#' @param memory logical. Checks if there is enough available RAM memory. Calculated if NULL
#' @param ... additional parameters for terra::app
#' @author Neander Marcel Heming
#'
#' @examples
#' \dontrun{
#' library(SESraster)
#' library(terra)
#' # creating random species distributions
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- rast(f)
#' set.seed(510)
#' r10 <- rast(lapply(1:18,
#'                 function(i, r, mn, mx){
#'                   app(r, function(x, t){
#'                     sapply(x, function(x, t){
#'                        x<max(t) & x>min(t)
#'                     }, t=t)
#'                   }, t=sample(seq(mn, mx), 2))
#'                 }, r=r, mn=minmax(r)[1]+10, mx=minmax(r)[2]-10))
#'
#' names(r10) <- paste("sp", 1:nlyr(r10))
#' plot(r10)
#'
#' # bootstrapping once
#' randr10 <- bootspat_str(r10, rprobnull)
#' plot(randr10)
#' plot(r10)
#' plot(c(sum(r10), sum(randr10)))
#' cbind(rand=sapply(randr10, function(x)freq(x)[2,3]),
#'       actual=sapply(r10, function(x)freq(x)[2,3]))
#' }
#' @return SpatRaster object
#' @export
bootspat_str <- function(x, rprob=NULL, rich=NULL, fr_prob=NULL, cores = 1, filename = "", memory = NULL, ...){

  if(is.null(memory)){
    memory <- .fit.memory(x)
  }

  if(is.null(fr_prob)){
    fr_prob <- fr2prob(x, rprob)
  }

  if(is.null(rprob)){
    rprob <- terra::app(x,
                       function(x){
                         ifelse(is.na(x), 0, 1)
                       })
  }

  if(is.null(rich)){
    rich <- terra::app(x, sum, na.rm=T)
  }

  sp <- seq_len(terra::nlyr(x))
  resu <- vector("numeric", length(sp))

  r <- terra::app(c(rich, rprob),
                  .str.sample,
                  sp=sp, resu=resu, fr_prob=fr_prob,
  cores = cores, filename = filename, overwrite = T)

  terra::set.names(r, names(x))

  return(r)
}
