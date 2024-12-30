# libraries ----
library(opendatatoronto)  # for importing Open Toronto datasets
library(dplyr)            # for manipulating data
library(here)             # for creating relative file-paths
library(raster)           # for manipulating raster data

# import ----

## forest and land cover 2018 ----

# note: the Forest and Land Cover 2018 data was rasterized by N. Sookhan
# using QGIS in the BUGS lab at the University of Toronto Scarborough
# this process is probably much faster to do in QGIS than R given 
# the fine-scale pixel resolution (0.6 m) and spatial extent (City of TO)
lc_2018 <- raster(
  here("data", "input_data", 
       "resist_surf", "to_lc_2018", "to_lc_2018.tif")
  )

## elevation ----

# metadata from open.toronto.ca/dataset/triangular-irregular-network-tin

# Triangular Irregular Network (TIN) polygon elevation data
# derived from 2023 LiDAR-derived 2.5 m resolution bare-earth elevation 

# attributes:
# Avg_Elev: Average Elevation of the polygon
# Aspect: Directional measure of slope
# Slope: Rate of change of elevation at a surface location in degrees
# Ptg_Slope: Rate of change of elevation at surface location as percentage

# datum:
# Horizontal: NAD83 (CSRS) / MTM zone 10 (EPSG:2952)
# Vertical: CGVD2013

tin_elev <- show_package("4ab91af6-6b08-4318-aee0-87f163891e53") %>%
  dplyr::filter(row_number()==1) %>%
  list_package_resources() %>%
  get_resource()

# could also import shapefile from disk if the above code doesn't work
# sf::st_read(here(
#  "data", "input_data", "resist_surf", "to_tin_elev_2023", 
#  "2023_TIN.shp")
#  )

## precipitation (rain gauge locations) ----
rain_gauges <- search_packages("precipitation") %>%
  dplyr::filter(row_number()==1) %>%
  list_package_resources() %>%
  dplyr::filter(format == "CSV") 

rain_gauge_2015 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2015")
  )

rain_gauge_2016 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2016")
)

rain_gauge_2017 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2017")
)

rain_gauge_2018 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2018")
)

rain_gauge_2019 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2019")
)

rain_gauge_2020 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2020")
)

rain_gauge_2021 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2021")
)

rain_gauge_2022 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2022")
)

rain_gauge_2023 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2023")
)

rain_gauge_2024 <- get_resource(
  dplyr::filter(rain_gauges, name == "precipitation-data-2024")
)

rain_gauge_10yr <- rain_gauge_2015 %>%
  rbind(rain_gauge_2016) %>%
  rbind(rain_gauge_2017) %>%
  rbind(rain_gauge_2018) %>%
  rbind(rain_gauge_2019) %>%
  rbind(rain_gauge_2020) %>%
  rbind(rain_gauge_2021) %>%
  rbind(rain_gauge_2022) %>%
  rbind(rain_gauge_2023) %>%
  rbind(rain_gauge_2024)

# save to disk ----

# convert to RDS objects to save memory space for whoever needs it
saveRDS(
  obj = tin_elev, 
  file = here("data", "intermediate_data", "tin_elev_2018.RDS")
  )

saveRDS(
  obj = rain_gauge_10yr, 
  file = here("data", "intermediate_data", "rain_gauge_2015-2024.RDS")
)