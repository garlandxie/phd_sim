# libraries ----
library(terra)
library(raster)
library(sf)
library(here)

# import data ----

## parking lot polygons ----

pl <- read_sf(here("data", "intermediate_data", "pl_green_space_type.shp"))

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

# save to disk -----

## reclassify land cover ----
writeRaster(
  x = lc_spatrast2, 
  filename = here("data", "intermediate_data", "sc1_add_ugs_lc.tiff")
)

## reclassify soil water ----
writeRaster(
  x = sw_spatrast2, 
  filename = here("data", "intermediate_data", "sc1_add_ugs_sw.tiff")
)

## reclassify soil ph ----
writeRaster(
  x = ph_spatrast2, 
  filename = here("data", "intermediate_data", "sc1_add_ugs_ph.tiff")
)

