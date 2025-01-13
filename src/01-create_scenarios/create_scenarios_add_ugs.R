# libraries ----
library(terra)    # for manipulating raster datasets
library(raster)   # for manipulating raster datasets
library(sf)       # for manipulating geospatial (vector) files
library(here)     # for creating relative file-paths

# import data ----

## parking lot polygons ----

pl <- read_sf(here(
  "data", "intermediate_data", "parking lots",
  "pl_green_space_type.shp"))

## remote sensing variables (raster) ----
lc_3m <- raster(here(
  "data", "input_data", "resist_surfaces", 
  "landcover_to_3m_res.tif")
)

sw_3m <- raster(here(
  "data", "input_data", "resist_surfaces",
  "soilwater_to_3m_res.tif")
)

ph_3m <- raster(here(
  "data", "input_data", "resist_surfaces",
  "soil_pH_to_3m_res.tif")
)

slope_3m <- raster(here(
  "data", "input_data", "resist_surfaces",
  "slope_to_3m_res.tif")
)

clay_3m <- raster(here(
  "data", "input_data", "resist_surfaces",
  "soil_clay_to_3m_res.tif")
)

sand_3m <- raster(here(
  "data", "input_data", "resist_surfaces",
  "soil_sand_to_3m_res.tif")
)

wind_3m <- raster(here(
  "data", "input_data", "resist_surfaces",
  "wind_to_3m_res.tif")
)

ndvi_3m <- raster(here(
  "data", "input_data", "resist_surfaces",
  "ndvi_to_2016-2024_3m_res.tif")
)

# clean data ----

# do quality control
# ensure consistent projected coordinate reference system
epsg_2617 <- CRS('+init=EPSG:26917')
crs(lc_3m) <- epsg_2617
crs(sw_3m) <- epsg_2617
crs(ph_3m) <- epsg_2617
crs(slope_3m) <- epsg_2617
crs(clay_3m) <- epsg_2617
crs(sand_3m) <- epsg_2617
crs(wind_3m) <- epsg_2617
crs(ndvi_3m) <- epsg_2617 

# perform raster algebra ----

## reclassify land cover ----

# convert into terra class objects
pl_r_lc <- terra::vect(pl)
lc_spatrast <- terra::rast(lc_3m)
lc_spatrast2 <- terra::rast(lc_3m) # to avoid overwriting lc_spatrast

# remove color table for each landcover raster
terra::coltab(lc_spatrast) <- NULL
terra::coltab(lc_spatrast2) <- NULL

# create a raster with assigned green spaces values for each parking lot
pl_lc <- terra::rasterize(pl_r_lc, lc_spatrast, field = "landcover")

# use raster algebra to replace impervious surface values
# with green spaces values for each parking lot 
lc_spatrast2[!is.na(pl_lc[])] <- pl_lc[!is.na(pl_lc[])]

## reclassify soil water ----

sw_spatrast <- terra::rast(sw_3m)
sw_spatrast2 <- terra::rast(sw_3m) 
pl_sw <- terra::rasterize(pl_r_lc, sw_spatrast, field = "soilwater")
sw_spatrast2[!is.na(pl_sw[])] <- pl_sw[!is.na(pl_sw[])]

## reclassify soil pH ----

ph_spatrast <- terra::rast(ph_3m)
ph_spatrast2 <- terra::rast(ph_3m) 
pl_ph <- terra::rasterize(pl_r_lc, ph_spatrast, field = "soilph")
ph_spatrast2[!is.na(pl_ph[])] <- pl_ph[!is.na(pl_ph[])]

## reclassify soil clay ----

clay_spatrast <- terra::rast(clay_3m)
clay_spatrast2 <- terra::rast(clay_3m) 
pl_clay <- terra::rasterize(pl_r_lc, clay_spatrast, field = "soilclay")
clay_spatrast2[!is.na(pl_clay[])] <- pl_clay[!is.na(pl_clay[])]

## reclassify slope ----

slope_spatrast <- terra::rast(slope_3m)
slope_spatrast2 <- terra::rast(slope_3m) 
pl_slope <- terra::rasterize(pl_r_lc, slope_spatrast, field = "slope")
slope_spatrast2[!is.na(pl_slope[])] <- pl_slope[!is.na(pl_slope[])]

## reclassify sand ----

sand_spatrast <- terra::rast(sand_3m)
sand_spatrast2 <- terra::rast(sand_3m) 
pl_sand <- terra::rasterize(pl_r_lc, sand_spatrast, field = "soilsand")
sand_spatrast2[!is.na(pl_sand[])] <- pl_sand[!is.na(pl_sand[])]

## reclassify wind ----

wind_spatrast <- terra::rast(wind_3m)
wind_spatrast2 <- terra::rast(wind_3m) 
pl_wind <- terra::rasterize(pl_r_lc, wind_spatrast, field = "windspeed")
wind_spatrast2[!is.na(pl_wind[])] <- pl_wind[!is.na(pl_wind[])]

# reclassify ndvi ----

ndvi_spatrast <- terra::rast(ndvi_3m)
ndvi_spatrast2 <- terra::rast(ndvi_3m) 
pl_ndvi <- terra::rasterize(pl_r_lc, ndvi_spatrast, field = "ndvi")
ndvi_spatrast2[!is.na(pl_ndvi[])] <- pl_ndvi[!is.na(pl_ndvi[])]

# save to disk -----

## reclassify land cover ----
writeRaster(
  x = lc_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios",
    "sc1_add_ugs_lc.tiff")
)

## reclassify soil water ----
writeRaster(
  x = sw_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios",
    "sc1_add_ugs_sw.tiff")
)

## reclassify soil ph ----
writeRaster(
  x = ph_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios",
    "sc1_add_ugs_ph.tiff")
)

## reclassify soil clay ----
writeRaster(
  x = clay_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios",
    "sc1_add_ugs_clay.tiff")
)


## reclassify slope ----
writeRaster(
  x = slope_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios",
    "sc1_add_ugs_slope.tiff")
)

## reclassify sand ----
writeRaster(
  x = sand_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios",
    "sc1_add_ugs_sand.tiff")
)

## reclassify wind ----
writeRaster(
  x = wind_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios",
    "sc1_add_ugs_wind.tiff")
)

## reclassify ndvi ----

writeRaster(
  x = ndvi_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios",
    "sc1_add_ugs_ndvi.tiff")
)
