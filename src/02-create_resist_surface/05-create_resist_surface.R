# libraries --------------------------------------------------------------------
library(terra)        # for manipulating rasters
library(here)         # for creating relative file-paths
library(ggplot2)      # for visualizing data
library(spatialEco)   # for inverting raster values
library(sf)           # for importing ESRI shapefiles

# import -----------------------------------------------------------------------

## continuous values -----------------------------------------------------------
sc1_sdm <- terra::rast(here(
  "data/intermediate_data/maxent_sdm/DSV.tbg/final/Sc1_CurrentEM_final_tgb.tif")
  )

sc2_sdm <- terra::rast(here(
  "data/intermediate_data/maxent_sdm/DSV.tbg/Final/Sc2_FutureEM_final_tgb.tif")
)

sc3_sdm <- terra::rast(here(
  "data/intermediate_data/maxent_sdm/DSV.tbg/Final/Sc3_FutureEM_final_tgb.tif")
)

## binary: suitable vs unsuitable ----------------------------------------------

#sc1_binary <- terra::rast(here(
#  "data/intermediate_data/maxent_sdm/DSV.tgb/proj_CurrentEM_tgb/proj_CurrentEM_tgb_DSV.tgb_ensemble_ROCbin.tif")
#)

#sc2_binary <- terra::rast(here(
# "data/intermediate_data/maxent_sdm/DSV.tgb/proj_FutureEM_tgb/proj_FutureEM_tgb_DSV.tgb_ensemble_ROCbin.tif")
#)

# clean: resistance surface ----------------------------------------------------

## continuous
sc1_resist_surf <- spatialEco::raster.invert(sc1_sdm)
sc2_resist_surf <- spatialEco::raster.invert(sc2_sdm)
sc3_resist_surf <- spatialEco::raster.invert(sc3_sdm)

## binary
# I'm using Omniscape from the SyncroSim GUI software
# which does not allow zero or negative values
# in this case, convert raster values accordingly: 
# 0 = 1 (suitable habitats)
# 1 = 9999 (unsuitable habitats)

#sc1_resist_surf_binary <- lapp(sc1_binary, fun=function(x){
#  return(ifelse(x == 0, 1, 9999))
#}
#)

#sc2_resist_surf_binary <- lapp(sc2_binary, fun=function(x){
#  return(ifelse(x == 0, 1, 9999))
#}
#)

# clip
#sc1_resist_surf_binary <- terra::mask(sc1_resist_surf_binary, sc1_resist_surf)
#sc2_resist_surf_binary <- terra::mask(sc2_resist_surf_binary, sc2_resist_surf)

# visualize --------------------------------------------------------------------

# double check to see if raster inversion worked
(sc1_resist_surf_map <- ggplot() + 
    tidyterra::geom_spatraster(data = sc1_resist_surf) 
)

(sc2_resist_surf_map <- ggplot() + 
    tidyterra::geom_spatraster(data = sc2_resist_surf) 
)

(sc3_resist_surf_map <- ggplot() + 
    tidyterra::geom_spatraster(data = sc3_resist_surf) 
)

# save to disk -----------------------------------------------------------------

# tif files --------------------------------------------------------------------

## continious ------------------------------------------------------------------
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

terra::writeRaster(
  x = sc3_resist_surf, 
  filename = here("data", "intermediate_data", "resist_surfaces",
                  "sc3_resist_surf.tif"),
  gdal=c("COMPRESS=DEFLATE", "TFW=YES"),
  overwrite = TRUE
)

## binary ----------------------------------------------------------------------

#terra::writeRaster(
#  x = sc1_resist_surf_binary, 
#  filename = here("data", "intermediate_data", "resist_surfaces",
#                  "sc1_resist_surf_binary.tif"),
#  gdal=c("COMPRESS=DEFLATE", "TFW=YES"),
#  overwrite = TRUE
#)

#terra::writeRaster(
#  x = sc2_resist_surf_binary, 
#  filename = here("data", "intermediate_data", "resist_surfaces",
#                  "sc2_resist_surf_binary.tif"),
#  gdal=c("COMPRESS=DEFLATE", "TFW=YES"),
#  overwrite = TRUE
#)
