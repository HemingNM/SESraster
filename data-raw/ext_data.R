## code to prepare external dataset
# creating data
set.seed(100)
spp_sites <- terra::rast(ncol = 5, nrow = 5, nlyr = 7)
terra::values(spp_sites) <- round(runif(terra::ncell(spp_sites) * terra::nlyr(spp_sites)))
terra::values(spp_sites[[1]]) <- rep(c(0,1), c(24,1))
terra::values(spp_sites[[7]]) <- 1
spp_sites[1] <- NA
spp_sites[2] <- 0
names(spp_sites) <- paste0("sp", 1:terra::nlyr(spp_sites))
# as.data.frame(spp_sites)

# if(!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = T)
terra::writeRaster(spp_sites, "inst/extdata/spp_sites.tif", overwrite=T)

# usethis::use_data(spp_sites, overwrite = TRUE)
