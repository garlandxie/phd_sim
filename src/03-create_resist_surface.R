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

tin_elev <- show_package("4ab91af6-6b08-4318-aee0-87f163891e53") %>%
  dplyr::filter(row_number()==1) %>%
  list_package_resources() %>%
  get_resource()




