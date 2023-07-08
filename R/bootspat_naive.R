#' Internal function to resample a vector according to the observed frequency
#'
#' @param x numeric. A vector containing values to resampling.
#' @param fr data.frame A data.frame with 3 columns (layer, value, count).
#' @return vector
#' @author Neander Marcel Heming
#'
# #' @examples
#' @keywords internal
.lyr.sample <- function(x, fr){
  sapply(x, function(x, fr){
    if(is.na(x)){
      return(NA)
    } else {
      return(sample(fr$value, 1, prob = fr$count))
    }
  }, fr = fr)
}

#' Internal function to sample vectors with non-NA values
#'
#' @param x  numeric. A vector containing values to resampling.
#'
#' @return vector
#' @author Neander Marcel Heming
#'
# #' @examples
#' @keywords internal
.sample.not.NA <- function(x){
  s <- !is.na(x)
  x[s] <- sample(x[s])
  x
}

#' Randomize a set of rasters according to the observed frequency.
#'
#' Randomize a set of rasters according to the observed frequency using the methods: sites (by cells), species (by layer) or both (layers and cells). The randomization not assign values to cells with nodata.
#' @param x SpatRaster. A presence-absence SpatRaster.
#' @param memory logical.
#' @param random character. Character indicating the type of randomization to be used. The available types are by "site", "specie" or "both". The first method (site) keeps species richness constant within each site (cell)pixel by randomizing the position (presence/absence) of the species within each cell of the stack.
#' @inheritParams terra::app
# #' @param cores positive integer. If cores > 1, a 'parallel' package cluster with that many cores is created and used.
# #' @param filename character. Output filename.
#' @details The first method (site) is performed within each site (cell) by randomizing the position (presence/absence) of the species within each cell of the stack. This method keeps species richness constant at each cell but the size of the species distribution might change. The second method (species) is performed at each layer (species) of the stack by randomizing the position of species presences in space. This method changes the species richness at each cell but the size of the species distribution is held constant (except if randomization is performed by frequency). The third method (both) combines randomization by site and species at the same time. This method will shuffle all presences across cells and layers, changing site richness and species distribution sizes and location at the same time.
#' @param ... additional arguments to be passed passed down from a calling function.
#' @return SpatRaster object
#' @seealso \code{\link{bootspat_str}}, \code{\link{bootspat_ff}},
#' \code{\link{SESraster}}, \code{\link{algorithm_metrics}}
#' @author Neander Marcel Heming and Gabriela Alves-Ferreira
#'
#' @examples
#' library(terra)
#' # load random species distributions
#' r <- load_ext_data()
#' plot(r)
#'
#' # randomize pres/abs data by site
#' rn <- bootspat_naive(r, "site")
#' plot(rn)
#'
#'
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
#' randr10 <- bootspat_naive(r10, "site")
#' plot(randr10)
#'
#' plot(c(sum(r10), sum(randr10)), main=c("observed", "randomized"))
#' cbind(observed=sapply(r10, function(x)freq(x)[2,3]),
#'       randomized=sapply(randr10, function(x)freq(x)[2,3]))
#'
#' @export
bootspat_naive <- function(x, random = c("site", "species", "both"),
                      filename = "", memory = NULL, cores = 1, ...){


  if(is.null(memory)){
    memory <- fit.memory(x, n=2)
  }

  if (random == "species"){ # randomize sites within species (layers)

    if(memory){ # when fits on the memory

      resu <- terra::rast(sapply(names(x),#lapply(1:terra::nlyr(x),
                                                  function(i, r){
                                                    terra::app(r[[i]],
                                                               fun = .sample.not.NA)
                                                  }, r = x))
    } else { # when does not fit on the memory

      message("The file does not fit on the memory. Randomization will be done by probability.")
      fr <- terra::freq(x)
      resu <- terra::rast(sapply(names(x),#seq_len(terra::nlyr(x)),
                                 function(i, r, fr){
                                   terra::app(r[[i]], fun = .lyr.sample,
                                              fr = fr[fr$layer==which(names(r) == i),])
                                 }, r = x, fr = fr))
      if(filename != ""){
        resu <- terra::writeRaster(resu, filename = filename)
      }

    }

  } else if (random == "site") { ### randomize species within each site (cells)

    if(memory){

      resu <- terra::app(x, sample, cores = cores, overwrite = TRUE)

    } else {

      ### randomize by cells- species in each site
      resu <- terra::app(x, sample, cores = cores, filename = filename, overwrite = TRUE)

    }

  } else if (random == "both") {

    if(memory){
      ### randomize by sites and species!
      resu <- x
      resu[] <- .sample.not.NA(x[])

    } else {
      message("The file does not fit on the memory. Randomization will be done by probability.")
      fr <- terra::freq(x)
      ### randomize by sites and species!
      resu <- terra::app(x, fun = .lyr.sample, fr = fr, cores = cores,
                         filename = filename, overwrite = TRUE)

    }

  } else {
    stop("Choose a valid randomization method! The methods currently available are: 'site', 'species', 'both'.")
  }

  terra::set.names(resu, names(x))
  return(resu)
}
