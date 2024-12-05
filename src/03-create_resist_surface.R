# libraries ----
library(opendatatoronto)  # for importing Open Toronto datasets
library(dplyr)            # for manipulating data
library(here)             # for creating relative file-paths
library(raster)           # for manipulating raster data

# import ----

# note: the Forest and Land Cover 2018 data was rasterized by N. Sookhan
# using QGIS in the BUGS lab at the University of Toronto Scarborough
# this process is probably much faster to do in QGIS than R given 
# the fine-scale pixel resolution (0.6 m) and spatial extent (City of TO)
lc_2018 <- raster(
  here("data", "input_data", 
       "resist_surf", "to_lc_2018", "to_lc_2018.tif")
  )

