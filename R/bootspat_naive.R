#' Intern function to resampling a vector according to the observed frequency
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

#' Intern function to sample vectors with non-NA values
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
#' @details The first method (site) is performed within each site (cell) by randomizing the position (presence/absence) of the species within each cell of the stack. This method keeps species richness constant at each cell but the size of the species distribution might change. The second method (species) is performed at each layer (species) of the stack by randomizing the position of species presences in space (Figure 1b). This method changes the species richness at each cell but the size of the species distribution is held constant (except if randomization is performed by frequency). The third method (both) combines randomization by site and species at the same time. This method will shuffle all presences across cells and layers, changing site richness and species distribution sizes and location at the same time.
#' @param ... additional arguments to be passed passed down from a calling function.
#' @return SpatRaster
#' @author Neander Marcel Heming and Gabriela Alves-Ferreira
#'
#' @examples
#' \dontrun{
#' ras <- terra::rast(system.file("extdata", "rast.presab.tif", package="phylogrid"))
#' sr <- bootspat_naive(ras, random = "site")
#' plot(sr)
#' }
#' @export
bootspat_naive <- function(x, random = c("site", "species", "both"),
                      filename = "", memory = NULL, cores = 1, ...){


  if(is.null(memory)){
    memory <- .fit.memory(x)
  }

  if (random == "species"){ # randomize sites within species (layers)

    if(memory){ # when fits on the memory

      resu <- terra::rast(lapply(1:terra::nlyr(x),
                                 function(i, r){
                                   terra::app(r[[i]],
                                              fun = .sample.not.NA)
                                 }, r = x))
    } else { # when does not fit on the memory

      message("The file does not fit on the memory. Randomization will be done by probability.")
      fr <- terra::freq(x)
      resu <- terra::writeRaster(terra::rast(lapply(1:terra::nlyr(x),
                                                    function(i, r, fr){
                                                      terra::app(r[[i]], fun = .lyr.sample,
                                                                 fr = fr[fr$layer==i,])
                                                    }, r = x, fr = fr)),
                                 filename = filename)

    }

  } else if (random == "site") { ### randomize species within each site (cells)

    if(memory){

      resu <- terra::app(x, sample, cores = cores, overwrite = T)

    } else {

      ### randomize by cells- species in each site
      resu <- terra::app(x, sample, cores = cores, filename = filename, overwrite = T)

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
                         filename = filename, overwrite = T)

    }

  } else {
    stop("Choose a valid randomization method! The methods currently available are: 'site', 'species', 'both'.")
  }

  return(resu)

}
