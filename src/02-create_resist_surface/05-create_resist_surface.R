# libraries --------------------------------------------------------------------
library(terra)        # for manipulating rasters
library(here)         # for creating relative file-paths
library(ggplot2)      # for visualizing data
library(spatialEco)   # for inverting raster values
library(sf)           # for importing ESRI shapefiles

# import -----------------------------------------------------------------------

sc1_sdm <- terra::rast(here(
  "data/intermediate_data/maxent_sdm/DSV.tgb/Final/CurrentEM_final_tgb.tif")
  )

sc2_sdm <- terra::rast(here(
  "data/intermediate_data/maxent_sdm/DSV.tgb/Final/FutureEM_final_tgb.tif")
)

# clean: resistance surface ----------------------------------------------------

sc1_resist_surf <- spatialEco::raster.invert(sc1_sdm)
sc2_resist_surf <- spatialEco::raster.invert(sc2_sdm)

# visualize --------------------------------------------------------------------

# double check to see if raster inversion worked
(sc1_resist_surf_map <- ggplot() + 
    tidyterra::geom_spatraster(data = sc1_resist_surf) 
)

(sc2_resist_surf_map <- ggplot() + 
    tidyterra::geom_spatraster(data = sc2_resist_surf) 
)

# save to disk -----------------------------------------------------------------

# tif files --------------------------------------------------------------------

terra::writeRaster(
  x = sc1_resist_surf, 
  filename = here("data", "intermediate_data", "resist_surfaces",
                  "sc1_resist_surf.tif"),
  gdal=c("COMPRESS=DEFLATE", "TFW=YES"),
  overwrite = TRUE
)

terra::writeRaster(
  x = sc2_resist_surf, 
  filename = here("data", "intermediate_data", "resist_surfaces",
                  "sc2_resist_surf.tif"),
  gdal=c("COMPRESS=DEFLATE", "TFW=YES"),
  overwrite = TRUE
)

