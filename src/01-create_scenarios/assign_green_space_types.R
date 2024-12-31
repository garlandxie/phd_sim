# libraries ----
library(here)             # for creating relative file-paths
library(exactextractr)    # for calculating zonal statistics
library(dplyr)            # for manipulating data frames
library(opendatatoronto)  # for downloading open data from the web 
library(sf)               # for manipulating geospatial (vector) data
library(raster)           # for manipulating geospatial (raster) data  
library(exactextractr)    # for calculating zonal statistics
library(ggplot2)          # for visualizing data 

# import ----

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

# clean -----

## get consistent crs ----
crs_lc <- st_crs(lc_3m)
ugs_transform <- st_transform(ugs, crs_lc)

## apply zonal statistics ----

# a bit slow: 10 minute run or so 
lc_bv <- exact_extract(lc_3m, ugs_transform, "majority")
sw_bv <- exact_extract(sw_3m, ugs_transform, "majority")
ph_bv <- exact_extract(ph_3m, ugs_transform, "majority")
sl_bv <- exact_extract(slope_3m, ugs_transform, "majority")
sc_bv <- exact_extract(clay_3m, ugs_transform, "majority")
ss_bv <- exact_extract(sand_3m, ugs_transform, "majority")
ws_bv <- exact_extract(wind_3m, ugs_transform, "majority")
ndvi_bv <- exact_extract(ndvi_3m, ugs_transform, "majority")

## tidy things up ----
ugs_tidy <- ugs %>% 
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
