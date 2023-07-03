#' Spatially structured fixed-fixed sample
#'
#' Randomizes a raster stack with fixed richness and species frequency of incidence.
#' Randomizations are based on frequencies (given or calculated from x)
#' and, optionally, a probability raster stack.
#' The probability raster stack controls the probability that a
#' given species is sampled in each cell raster. Frequency controls the number of cells
#' being sampled for each species.
#'
#' @details The algorithm is based on the algorithm of Connor & Simberloff (1979).
#' It takes each species at a time and placed on Nj (species frequency of
#' incidence) randomly chosen sites (cells). The original algorithm randomly
#' chooses the sequence of species and fills sites (originally islands) until they
#' reach the observed species richness. However, as sites (cells) are filled with
#' species, some species do not have enough available sites to be placed, and
#' their sampled frequency is smaller than observed. Additionally, some sites cannot
#' be completely filled because duplicated species are not allowed in the same site.
#' Their solution was to increase the number of sites to place the species.
#' Here, we opted to order the sequence of species from the largest Nj to the
#' smallest. Also, the probability of occupying a site is given by cell
#' expected richness and on each round (i.e. species placement), the expected
#' richness of newly occupied sites is reduced. This ensures that there will be
#' available sites for all species and the randomized frequency of
#' incidence equals the observed frequency of incidence (Nj).
#'
#'
#' @param fr The observed frequency of incidence (i.e. number of occupied pixels)
#' of each species is across the study area.
#' @param glob_fr The size (i.e. number of pixels) of the study area.
#' @inheritParams bootspat_str
#' @inheritParams terra::app
#' @param ... additional parameters for terra::app
#' @seealso \code{\link{bootspat_str}}, \code{\link{bootspat_naive}}
#' @author Neander Marcel Heming
#'
#' @references Connor, E. F., & Simberloff, D. (1979). The Assembly of Species Communities: Chance or Competition? Ecology, 60(6), 1132–1140.
#'
#' @examples
#' # load random species distributions
#' library(SESraster)
#' library(terra)
#' r <- load_ext_data()
#' plot(r)
#'
#' # applying the function
#' rand.str <- bootspat_str(r)
#' plot(rand.str)
#'
#' # With null probability raster
#' rprobnull <- terra::app(r,
#'                        function(x){
#'                        ifelse(is.na(x), NA, 1)
#'                        })
#' rand.str2 <- bootspat_str(r, rprob = rprobnull)
#'
#'
#' library(SESraster)
#' library(terra)
#' # creating random species distributions
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- rast(f)
#' set.seed(510)
#' r10 <- rast(lapply(1:8,
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
#' rprobnull <- terra::app(r10,
#'                        function(x){
#'                        ifelse(is.na(x), NA, 1)
#'                        })
#'
#' # bootstrapping once
#' randr10 <- bootspat_ff(r10, rprobnull)
#' plot(randr10)
#' plot(c(sum(r10), sum(randr10)), main=c("observed", "randomized"))
#' plot(sum(r10)-sum(randr10))
#' cbind(observed=sapply(r10, function(x)freq(x)[2,3]),
#'       randomized=sapply(randr10, function(x)freq(x)[2,3]))
#'
#' @return SpatRaster object
#' @export
bootspat_ff <- function(x, rprob=NULL, rich=NULL, fr=NULL, glob_fr=NULL, cores = 1, filename = "", overwrite = FALSE, ...){
  method <- c("weights", "random")[1] ## method random, randomizes the species richness (SIM2); method weights, retains richness (SIM9)

  if(is.null(rprob)){
    rprob <- terra::app(x,
                        function(x){
                          ifelse(is.na(x), 0, 1)
                        })
  }

  if(is.null(rich)){
    rich <- terra::app(x, sum, na.rm=TRUE)
  }

  if(is.null(fr)){
    fr <- unlist(terra::global(x, function(x) sum(x, na.rm = TRUE)))
  }

  if(is.null(glob_fr)){
    glob_fr <- unlist(terra::global(rich, function(x) sum(!is.na(x))))
  }

  ## species sequence for bootstraping
  # v_seq <- seq_along(size)
  v_seq <- order(fr, decreasing = T)

  ## rerun every boostrap
  # v_seq <- sample(length(fr))
  # resulting raster stack
  r <- terra::app(x,
                  function(x){
                    return(ifelse(is.na(x), NA, 0))
                  }, cores = cores, filename = filename, overwrite = overwrite, ...)
  rich_next <- rich

  for(i in v_seq){
    if(glob_fr > fr[i]){
      terra::set.values(r,
                 cells = terra::spatSample(rich_next,
                                           size=fr[i], method=method,
                                           na.rm=TRUE, cells=TRUE, values=FALSE, #
                                           as.raster=FALSE, exhaustive=TRUE)[,"cell"],
                 values = 1, layer = i)
    } else {
      terra::set.values(r,
                 cells = terra::cells(r[[i]]),
                 values = 1, layer = i)
    }

    ## reducing available pixels for next species to be sampled
    rich_next <- terra::app(c(rich_next, r[[i]]),
                            function(x){
                              if(is.na(x[1])) return(NA)
                              v <- x[1]-x[2]
                              return(ifelse(v<0, 0, v))
                            })
  }

  return(r)
}
