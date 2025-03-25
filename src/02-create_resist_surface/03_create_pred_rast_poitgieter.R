# libraries ----
library(raster)
library(here)

# import -----
priority_tif <- raster(here(
  "data", "input_data", "poitgieter_prioritization",
  "Model_Final.tif")
  )

# clean data ----

# create binary raster
threshold <- 7
binary_priority <- priority_tif < threshold

# fill missing values
NAvalue(binary_priority) <- -9999

# save to disk ----
writeRaster(
  x = binary_priority, 
  options = c("COMPRESS=NONE", "TFW=YES"),
  filename = here(
    "data", "intermediate_data", "potgieter_prioritization",
    "potgieter_prioritization.tif"),
  overwrite = TRUE
)