# libraries ----
library(here)             # for creating relative file-paths
library(exactextractr)    # for calculating zonal statistics
library(dplyr)            # for manipulating data frames
library(opendatatoronto)  # for downloading open data from the web 
library(sf)               # for manipulating geospatial (vector) data
library(raster)           # for manipulating geospatial (raster) data  
library(exactextractr)    # for calculating zonal statistics
library(ggplot2)          # for visualizing data 

# import data ----

## existing green spaces (vector) ----
ugs <- list_package_resources("9a284a84-b9ff-484b-9e30-82f22c1780b9") %>%
  dplyr::filter(name == "Green Spaces - 4326.zip") %>%
  get_resource()

## remote sensing variables (raster) ----

## I got a bit lazy here and just added each raster individually
## at least it's reproducible
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

## parking lots (vector) ----

## realistic scenario of converting parking lots into green spaced
## within Parkland Area of Needs
pl_realistic <- read_sf(here(
  "data", "intermediate_data", "parking lots",
  "parking-lots-in-parkland-priority.shp")
  )

## extreme scenario ----- 

## obtain directly from open data Toronto 
pl_extreme <- list_package_resources("bb408f36-6824-4158-8a12-d4efe6465959") %>%
  dplyr::filter(name == "Parking Lot WGS84") %>%
  get_resource()

# clean data -----

## get consistent crs ----
epsg_2617 <- CRS('+init=EPSG:26917')
crs(lc) <- epsg_2617
crs(sw) <- epsg_2617
crs(ph) <- epsg_2617
crs(slope) <- epsg_2617
crs(clay) <- epsg_2617
crs(sand) <- epsg_2617
crs(wind) <- epsg_2617
crs(ndvi) <- epsg_2617 

# assign crs to green space and parking lot dataset
crs_lc <- st_crs(lc)
ugs_transform <- st_transform(ugs, crs_lc)
pl_realistic_transform <- st_transform(pl_realistic, crs_lc)
pl_extreme_transform <- st_transform(pl_extreme, crs_lc)

## apply zonal statistics ----

# a bit slow: 10 minute run or so 
# could try to clip the rasters using the UGS shapefiles to make it faster
lc_bv <- exact_extract(lc, ugs_transform, "majority")
sw_bv <- exact_extract(sw, ugs_transform, "majority")
ph_bv <- exact_extract(ph, ugs_transform, "majority")
sl_bv <- exact_extract(slope, ugs_transform, "majority")
sc_bv <- exact_extract(clay, ugs_transform, "majority")
ss_bv <- exact_extract(sand, ugs_transform, "majority")
ws_bv <- exact_extract(wind, ugs_transform, "majority")
ndvi_bv <- exact_extract(ndvi, ugs_transform, "majority")

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
    geometry) 

# subset to get environmental variables for ONLY green spaces
ugs_tidy <- dplyr::filter(ugs_tidy, landcover_bv %in% c(10, 30, 40, 60)) 

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

# realistic scenario of adding UGS
pl_realistic_tidy <- pl_realistic_transform %>%
  dplyr::mutate(
    landcover = sample(ugs_tidy$landcover_bv, nrow(pl_realistic), replace = TRUE),
    soilwater = sample(soil_water_dist, size = nrow(pl_realistic), replace = TRUE),
    soilph = sample(soil_ph_dist, size = nrow(pl_realistic), replace = TRUE),
    slope = sample(ugs_tidy$slope_bv, size = nrow(pl_realistic), replace = TRUE),
    soilclay = sample(soil_clay_dist, size = nrow(pl_realistic), replace = TRUE),
    soilsand = sample(soil_sand_dist, size = nrow(pl_realistic), replace = TRUE),
    windspeed = sample(ugs_tidy$windspeed_bv, size = nrow(pl_realistic), replace = TRUE),
    ndvi = sample(ugs_tidy$ndvi_bv, size = nrow(pl_realistic), replace = TRUE)
  ) %>%
  dplyr::select(
    objectid, 
    landcover, 
    soilwater, 
    soilph,
    slope,
    soilclay,
    soilsand,
    ndvi, 
    windspeed
  )

# extreme scenario of adding UGS
pl_extreme_tidy <- pl_extreme_transform %>%
  dplyr::mutate(
    landcover = sample(ugs_tidy$landcover_bv, nrow(pl_extreme), replace = TRUE),
    soilwater = sample(soil_water_dist, size = nrow(pl_extreme), replace = TRUE),
    soilph = sample(soil_ph_dist, size = nrow(pl_extreme), replace = TRUE),
    slope = sample(ugs_tidy$slope_bv, size = nrow(pl_extreme), replace = TRUE),
    soilclay = sample(soil_clay_dist, size = nrow(pl_extreme), replace = TRUE),
    soilsand = sample(soil_sand_dist, size = nrow(pl_extreme), replace = TRUE),
    windspeed = sample(ugs_tidy$windspeed_bv, size = nrow(pl_extreme), replace = TRUE),
    ndvi = sample(ugs_tidy$ndvi_bv, size = nrow(pl_extreme), replace = TRUE)
  ) %>%
  dplyr::select(
    objectid, 
    landcover, 
    soilwater, 
    soilph,
    slope,
    soilclay,
    soilsand,
    ndvi, 
    windspeed
  )

# save to disk -----

st_write(
  obj = pl_realistic_tidy, 
  dsn = here(
    "data", "intermediate_data", "parking lots", 
    "pl_realistic_ugs_type.shp"),
  append = TRUE
)

st_write(
  obj = pl_extreme_tidy, 
  dsn = here(
    "data", "intermediate_data", "parking lots", 
    "pl_extreme_ugs_type.shp"),
  append = TRUE
)
