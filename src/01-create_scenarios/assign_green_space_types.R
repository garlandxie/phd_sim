# libraries ----
library(here)             # for creating relative file-paths
library(exactextractr)    # for calculating zonal statistics
library(dplyr)            # for manipulating data frames
library(opendatatoronto)  # for downloading open data from the web 
library(sf)               # for manipulating geospatial (vector) data
library(raster)           # for manipulating geospatial (raster) data  
library(exactextractr)    # for calculating zonal statistics
library(ggplot2)          # for visualizing data 
library(fasterize)        # for rasterizing polygons

# import data ----

## existing green spaces (vector) ----
ugs <- list_package_resources("9a284a84-b9ff-484b-9e30-82f22c1780b9") %>%
  dplyr::filter(name == "Green Spaces - 4326.zip") %>%
  get_resource()

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

## parking lots ----
pl <- read_sf(here("data", "intermediate_data", "parking-lots-in-parkland-priority.shp"))

# clean data -----

## get consistent crs ----
crs_lc <- st_crs(lc_3m)
ugs_transform <- st_transform(ugs, crs_lc)
pl_transform <- st_transform(pl, crs_lc)

## apply zonal statistics ----

# a bit slow: 10 minute run or so 
# could try to clip the rasters using the UGS shapefiles to make it faster
lc_bv <- exact_extract(lc_3m, ugs_transform, "majority")
sw_bv <- exact_extract(sw_3m, ugs_transform, "majority")
ph_bv <- exact_extract(ph_3m, ugs_transform, "majority")
sl_bv <- exact_extract(slope_3m, ugs_transform, "majority")
sc_bv <- exact_extract(clay_3m, ugs_transform, "majority")
ss_bv <- exact_extract(sand_3m, ugs_transform, "majority")
ws_bv <- exact_extract(wind_3m, ugs_transform, "majority")
ndvi_bv <- exact_extract(ndvi_3m, ugs_transform, "majority")

## combine remote sensing variables  ----
ugs_tidy <- ugs_transform %>% 
  cbind(lc_bv) %>%
  cbind(sw_bv) %>%
  cbind(ph_bv) %>%
  cbind(sl_bv) %>%
  cbind(sc_bv) %>%
  cbind(ss_bv) %>%
  cbind(ws_bv) %>%
  cbind(ndvi_bv) %>%
  dplyr::select(
    ugs_type = AREA_CL6, 
    property = AREA_NA9, 
    landcover_bv = lc_bv, 
    soilwater_bv = sw_bv, 
    soilph_bv = ph_bv, 
    slope_bv = sl_bv, 
    soilclay_bv = sc_bv,
    soilsand_bv = ss_bv,
    windspeed_bv = ws_bv,
    ndvi_bv, 
    geometry) %>%
  filter(landcover_bv %in% c(10, 30, 40, 60)) 

# save to disk 
# st_write(ugs_tidy, dsn = here("data", "intermediate_data", "ugs_quality.shp"))
# ugs_tidy <- read_sf(here("data", "intermediate_data", "ugs_quality.shp"))

# remove zero values for some environmental variables
soil_water_dist <- ugs_tidy %>%
  dplyr::filter(soilwater_bv > 0) %>%
  pull(soilwater_bv)

soil_ph_dist <- ugs_tidy %>%
  dplyr::filter(soilph_bv > 0) %>%
  pull(soilph_bv)

soil_clay_dist <- ugs_tidy %>%
  dplyr::filter(soilclay_bv > 0) %>%
  pull(soilclay_bv)

soil_sand_dist <- ugs_tidy %>%
  dplyr::filter(soilsand_bv > 0) %>%
  pull(soilsand_bv)

## assign values to parking lots ----
set.seed(10)

pl_tidy <- pl_transform %>%
  dplyr::mutate(
    landcover = sample(ugs_tidy$landcover_bv, nrow(pl), replace = TRUE),
    soilwater = sample(soil_water_dist, size = nrow(pl), replace = TRUE),
    soilph = sample(soil_ph_dist, size = nrow(pl), replace = TRUE),
    slope = sample(ugs_tidy$slope_bv, size = nrow(pl), replace = TRUE),
    soilclay = sample(soil_clay_dist, size = nrow(pl), replace = TRUE),
    soilsand = sample(soil_sand_dist, size = nrow(pl), replace = TRUE),
    windspeed = sample(ugs_tidy$windspeed_bv, size = nrow(pl), replace = TRUE),
    ndvi = sample(ugs_tidy$ndvi_bv, size = nrow(pl), replace = TRUE)
  ) %>%
  dplyr::select(
    objectd, 
    landcover, 
    soilwater, 
    soilph,
    slope,
    soilclay,
    soilsand,
    ndvi, 
    windspeed
  )

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
  filename = here("data", "intermediate_data", "lc_reclassify.tiff")
  )
