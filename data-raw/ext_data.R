## code to prepare external dataset
# creating data
set.seed(100)
spp_sites <- terra::rast(ncol = 5, nrow = 5, nlyr = 5)
terra::values(spp_sites) <- round(runif(terra::ncell(spp_sites) * terra::nlyr(spp_sites)))
spp_sites[1] <- NA
names(spp_sites) <- paste0("sp", 1:5)
# as.data.frame(spp_sites)

# if(!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = T)
# terra::writeRaster(spp_sites, "inst/extdata/spp_sites.tif", overwrite=T)

# usethis::use_data(spp_sites, overwrite = TRUE)
