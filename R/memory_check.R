#' Function to evaluate if the rasters generated in the function fits on memory
#'
#' @param x number of rasters generated in the function
#'
#' @return logical
#' @author Neander Marcel Heming and Gabriela Alves-Ferreira
#'
# #' @examples
#' @keywords internal
.fit.memory <- function(x){
  # x rasters will be generated in this function, let's see if there is enough memory in the user's pc
  sink(nullfile())    # suppress output
  mi <- terra::mem_info(x, 1)[5] != 0 # proc in memory = T TRUE means that it fits in the pc's memory, so you wouldn't have to use temporary files
  sink()
  return(mi)
}
