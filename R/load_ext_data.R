#' Load SESraster external datasets
#'
#' @description This function loads external datasets available at extdata package folder
#' @param x dataset to be loaded
#' @details
#' These are the available datasets:
#' - spp_sites: a SpatRaster with randomly generated presence-absence data for five species.
#' @return SpatRaster
#' @usage load_ext_data(x = "spp_sites")
#' @export
#' @source Rosauer, 2017. Available on: \href{https://github.com/DanRosauer/phylospatial/tree/master/PhyloEndemism_in_R/Tree%20Frog%20Data/}{Github}
#' @source IUCN. 2022. The IUCN Red List of Threatened Species (spatial data). Version 2022-1. \href{https://www.iucnredlist.org}{IUCN}
#' @export
load_ext_data <- function(x = "spp_sites"){
  terra::rast(system.file("extdata", paste0(x, ".tif"), package="SESraster"))
}
