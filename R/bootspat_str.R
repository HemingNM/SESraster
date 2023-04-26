#' Vectorized structured sample
#'
#' @param x vector containing sample size (i.e. richness) in the first element
#' and probabilities on the remaining
#' @param sp elements to be sampled
#' @param resu vector of results, must be the same length of sp
#' @param fr frequency of each sp element. This will be multiplied by the
#' probability vector
#' @return vector of sampled sp elements
# #' @examples
#'
#' @keywords internal
.str.sample <- function(x, sp, resu, fr){
  if(is.na(x[1])) {
    resu[] <- NA
  } else {
    resu[sample(sp, size = x[1], prob = x[-1]*fr)] <- 1
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
#' @param prob SpatRaster. Stack of probability values. Structures the spatial
#' pattern of each randomized species.
#' @param rich SpatRaster. Richness pattern structuring the sample size of
#' each cell randomization. Calculated if not provided.
#' @param fr frequency of pixels that a species is observed within each layer of x
#' @inheritParams terra::app
# #' @param cores
# #' @param filename Character. Filename for output SpatRaster.
#' @param memory logical. Checks if there is enough available RAM memory. Calculated if NULL
#' @param ... additional parameters for terra::app
#'
#' @examples
#' \dontrun{
#' library(SESraster)
#' library(terra)
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- rast(f)
#' set.seed(510)
#' r10 <- rast(lapply(1:10,
#'                 function(i, r, mn, mx){
#'                   app(r, function(x, t){
#'                     sapply(x, function(x, t){
#'                       x>t
#'                     }, t=t)
#'                   }, t=sample(seq(mn, mx), 1))
#'                 }, r=r, mn=minmax(r)[1]+10, mx=minmax(r)[2]-10))
#' r10 <- c(r10,
#'       rast(lapply(1:4,
#'                   function(i, r, mn, mx){
#'                     app(r, function(x, t){
#'                       sapply(x, function(x, t){
#'                         x<t
#'                       }, t=t)
#'                     }, t=sample(seq(mn, mx), 1))
#'                   }, r=r, mn=minmax(r)[1]+10, mx=minmax(r)[2]-10)))
#' names(r10) <- paste("sp", 1:nlyr(r10))
#' plot(r10)
#' randr10 <- bootspat_str(r10, rprobnull)
#' plot(randr10)
#' plot(r10)
#' plot(c(sum(r10), sum(randr10)))
#' cbind(rand=sapply(randr10, function(x)freq(x)[2,3]),
#'       actual=sapply(r10, function(x)freq(x)[2,3]))
#' }
#' @return SpatRaster object
#' @export
bootspat_str <- function(x, prob=NULL, rich=NULL, fr=NULL, cores = 1, filename = "", memory = NULL, ...){

  if(is.null(memory)){
    memory <- .fit.memory(x)
  }

  if(is.null(fr)){
    # uses utils::globalVariables because of https://github.com/r-lib/devtools/issues/1714
    # utils::globalVariables(value)
    value <- NULL
    fr <- subset(terra::freq(x), value==1)[,"count"]
  }

  if(is.null(prob)){
    prob <- terra::app(x,
                       function(x){
                         ifelse(is.na(x), 0, 1)
                       })
  }

  if(is.null(rich)){
    rich <- terra::app(x, sum, na.rm=T)
  }

  sp <- seq_len(terra::nlyr(x))
  resu <- vector("numeric", length(sp))

  r <- terra::app(c(rich, prob),
                  .str.sample,
                  sp=sp, resu=resu, fr=fr,
  cores = cores, filename = filename, overwrite = T)

  terra::set.names(r, names(prob))
  r
}
