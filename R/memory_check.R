#' Function to evaluate if the rasters generated in the function fit on RAM memory
#'
#' @inheritParams terra::mem_info
#'
#' @return logical
#' @author Neander Marcel Heming and Gabriela Alves-Ferreira
# #' @examples
# #' @keywords internal
#' @export
fit.memory <- function(x, n=1){
  # x rasters will be generated in this function, let's see if there is enough memory in the user's pc
  sink(nullfile())    # suppress output
  mi <- terra::mem_info(x, n)[5] != 0 # proc in memory = T TRUE means that it fits in the pc's memory, so you wouldn't have to use temporary files
  names(mi) <- NULL
  sink(file = NULL)
  # sink(file = NULL)
  return(mi)
}
