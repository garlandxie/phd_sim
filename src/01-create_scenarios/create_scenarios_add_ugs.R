# libraries ----
library(terra)


## reclassify land cover ----

# convert into terra class objects
pl_r_lc <- terra::vect(pl_tidy)
lc_spatrast <- terra::rast(lc_3m)
lc_spatrast2 <- terra::rast(lc_3m) # to avoid overwriting lc_spatrast

# remove color table for each landcover raster
terra::coltab(lc_spatrast) <- NULL
terra::coltab(lc_spatrast2) <- NULL

# create a raster with assigned green spaces values for each parking lot
pl_r <- terra::rasterize(pl_r_lc, lc_spatrast, field = "landcover")

# use raster algebra to replace impervious surface values
# with green spaces values for each parking lot 
lc_spatrast2[!is.na(pl_r[])] <- pl_r[!is.na(pl_r[])]

# save to disk -----

## reclassify land cover ----
writeRaster(
  x = lc_spatrast2, 
  filename = here("data", "intermediate_data", "sc1_add_ugs_lc.tiff")
)


