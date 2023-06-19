#' Load SESraster external datasets
#'
#' @description This function loads external datasets available at extdata package folder
#' @param x dataset to be loaded
#' @details
#' These are the available datasets:
#' - spp_sites: a SpatRaster with randomly generated presence-absence data for five species.
#' @return SpatRaster object
#' @usage load_ext_data(x = "spp_sites")
#' @examples
#' # load random species distributions
#' library(SESraster)
#' library(terra)
#'
#' r <- load_ext_data()
#'
#' plot(r)
#'
#' @export
load_ext_data <- function(x = "spp_sites"){
  terra::rast(system.file("extdata", paste0(x, ".tif"), package="SESraster"))
}
