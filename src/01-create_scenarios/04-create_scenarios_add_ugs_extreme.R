# libraries --------------------------------------------------------------------
library(terra)    # for manipulating raster datasets
library(raster)   # for manipulating raster datasets
library(sf)       # for manipulating geospatial (vector) files
library(here)     # for creating relative file-paths

# import data ------------------------------------------------------------------

## parking lot polygons --------------------------------------------------------

pl <- read_sf(here(
  "data", "intermediate_data", "parking lots",
  "pl_extreme_ugs_type.shp"))

## remote sensing variables (raster) ----
lc <- raster(here(
  "data", "input_data", "resist_surfaces", "converted",
  "landcover_to_categorical_20m_res.tif")
)

sw <- raster(here(
  "data", "input_data", "resist_surfaces", "converted",
  "soil_water_to_20m_converted.tif")
)

ph <- raster(here(
  "data", "input_data", "resist_surfaces", "converted",
  "soil_pH_to_20m_converted.tif")
)

slope <- raster(here(
  "data", "input_data", "resist_surfaces", "converted",
  "slope_to_20m_converted.tif")
)

clay <- raster(here(
  "data", "input_data", "resist_surfaces", "converted",
  "soil_clay_to_20m_converted.tif")
)

sand <- raster(here(
  "data", "input_data", "resist_surfaces", "converted",
  "soil_sand_to_20m_converted.tif")
)

wind <- raster(here(
  "data", "input_data", "resist_surfaces", "converted",
  "wind_to_20m_converted.tif")
)

ndvi <- raster(here(
  "data", "input_data", "resist_surfaces", "converted",
  "ndvi_to_2016-2024_20m_converted.tif")
)

# clean data ----

# do quality control
# ensure consistent projected coordinate reference system
epsg_2617 <- CRS('+init=EPSG:26917')
crs(lc) <- epsg_2617
crs(sw) <- epsg_2617
crs(ph) <- epsg_2617
crs(slope) <- epsg_2617
crs(clay) <- epsg_2617
crs(sand) <- epsg_2617
crs(wind) <- epsg_2617
crs(ndvi) <- epsg_2617 

# perform raster algebra ----

## reclassify land cover ----

# convert into terra class objects
pl_r_lc <- terra::vect(pl)
lc_spatrast <- terra::rast(lc)
lc_spatrast2 <- terra::rast(lc) # to avoid overwriting lc_spatrast

# remove color table for each landcover raster
terra::coltab(lc_spatrast) <- NULL
terra::coltab(lc_spatrast2) <- NULL

# create a raster with assigned green spaces values for each parking lot
pl_lc <- terra::rasterize(pl_r_lc, lc_spatrast, field = "landcover")

# use raster algebra to replace impervious surface values
# with green spaces values for each parking lot 
lc_spatrast2[!is.na(pl_lc[])] <- pl_lc[!is.na(pl_lc[])]

## reclassify soil water ----

sw_spatrast <- terra::rast(sw)
sw_spatrast2 <- terra::rast(sw) 
pl_sw <- terra::rasterize(pl_r_lc, sw_spatrast, field = "soilwater")
sw_spatrast2[!is.na(pl_sw[])] <- pl_sw[!is.na(pl_sw[])]

## reclassify soil pH ----

ph_spatrast <- terra::rast(ph)
ph_spatrast2 <- terra::rast(ph) 
pl_ph <- terra::rasterize(pl_r_lc, ph_spatrast, field = "soilph")
ph_spatrast2[!is.na(pl_ph[])] <- pl_ph[!is.na(pl_ph[])]

## reclassify soil clay ----

clay_spatrast <- terra::rast(clay)
clay_spatrast2 <- terra::rast(clay) 
pl_clay <- terra::rasterize(pl_r_lc, clay_spatrast, field = "soilclay")
clay_spatrast2[!is.na(pl_clay[])] <- pl_clay[!is.na(pl_clay[])]

## reclassify slope ----

slope_spatrast <- terra::rast(slope)
slope_spatrast2 <- terra::rast(slope) 
pl_slope <- terra::rasterize(pl_r_lc, slope_spatrast, field = "slope")
slope_spatrast2[!is.na(pl_slope[])] <- pl_slope[!is.na(pl_slope[])]

## reclassify sand ----

sand_spatrast <- terra::rast(sand)
sand_spatrast2 <- terra::rast(sand) 
pl_sand <- terra::rasterize(pl_r_lc, sand_spatrast, field = "soilsand")
sand_spatrast2[!is.na(pl_sand[])] <- pl_sand[!is.na(pl_sand[])]

## reclassify wind ----

wind_spatrast <- terra::rast(wind)
wind_spatrast2 <- terra::rast(wind) 
pl_wind <- terra::rasterize(pl_r_lc, wind_spatrast, field = "windspeed")
wind_spatrast2[!is.na(pl_wind[])] <- pl_wind[!is.na(pl_wind[])]

# reclassify ndvi ----

ndvi_spatrast <- terra::rast(ndvi)
ndvi_spatrast2 <- terra::rast(ndvi) 
pl_ndvi <- terra::rasterize(pl_r_lc, ndvi_spatrast, field = "ndvi")
ndvi_spatrast2[!is.na(pl_ndvi[])] <- pl_ndvi[!is.na(pl_ndvi[])]

# save to disk -----

## reclassify land cover ----
writeRaster(
  x = lc_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios", "sc3_extreme",
    "sc3_add_ugs_lc.tiff"), 
  overwrite = TRUE
)

## reclassify soil water ----
writeRaster(
  x = sw_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios", "sc3_extreme",
    "sc3_add_ugs_sw.tiff"),
  overwrite = TRUE
)

## reclassify soil ph ----
writeRaster(
  x = ph_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios", "sc3_extreme",
    "sc3_add_ugs_ph.tiff"),
  overwrite = TRUE
)

## reclassify soil clay ----
writeRaster(
  x = clay_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios", "sc3_extreme",
    "sc3_add_ugs_clay.tiff"),
  overwrite = TRUE
)

## reclassify slope ----
writeRaster(
  x = slope_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios", "sc3_extreme",
    "sc3_add_ugs_slope.tiff"),
  overwrite = TRUE
)

## reclassify sand ----
writeRaster(
  x = sand_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios", "sc3_extreme",
    "sc3_add_ugs_sand.tiff"),
  overwrite = TRUE
)

## reclassify wind ----
writeRaster(
  x = wind_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios", "sc3_extreme",
    "sc3_add_ugs_wind.tiff"),
  overwrite = TRUE
)

## reclassify ndvi ----

writeRaster(
  x = ndvi_spatrast2, 
  filename = here(
    "data", "intermediate_data", "ugs_scenarios", "sc3_extreme",
    "sc3_add_ugs_ndvi.tiff"),
  overwrite = TRUE
)