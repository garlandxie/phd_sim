# libraries --------------------------------------------------------------------
library(rgbif)    # for querying GBIF records
library(here)     # for creating relative file-paths
library(sf)       # for manipulating geospatial files
library(wk)       # for manipulating WTK polygons - needed for GBIF queries
library(janitor)

# import -----------------------------------------------------------------------

# load green space dataset from City of Toronto 
# this is done to query the GBIF occurrence records using a polygon
ugs <- sf::read_sf(here("data", "input_data", "green_spaces_4326"))

# get occurrence records (with geospatial coordinates)
# of species within the Apocynaceae family
# the collective records of all species within this family
# will form the target-ground background option
keys_id <- rgbif::name_backbone_checklist(
  c("Apocynum androsaemifolium",
  "Apocynum cannabinum",
  "Asclepias exaltata", 
  "Asclepias incarnata",
  "Asclepias sullivantii",
  "Asclepias syriaca",
  "Asclepias tuberosa",
  "Gonolobus niger",
  "Vinca minor",
  "Vincetoxicum nigrum",
  "Vincetoxicum rossicum"
  )
)

# create WTK polygon of the City of Toronto boundary
# this polygon should reduce the number of imported queries from GBIF
wkt_to_boundary <- ugs %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_text() %>%
  wk::wkt() %>%
  wk::wk_orient()

to_bound <- wkt_to_boundary <- ugs %>%
  sf::st_bbox() %>%
  sf::st_as_sfc()

# run GBIF query from API
occ_download(
  pred_in("speciesKey", keys_id$speciesKey),
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_within(wkt_to_boundary)
)

to_occ <- occ_download_get(
  key = '0061415-241126133413365',
  path = here("data", "input_data"),
  overwrite = TRUE
) %>%
  occ_download_import(
    select = c(
      "gbifID", 
      "occurrenceID", 
      "occurrenceStatus",
      "year", 
      "decimalLatitude", 
      "decimalLongitude",
      "coordinateUncertaintyInMeters",
      "species", 
      "acceptedScientificName")
  )

# clean data ----

# check for duplicate records
duplicates <- janitor::get_dupes(to_occ)

# remove focal species (Vincetoxicum rossicum)
trgt_grp <- dplyr::filter(to_occ, species != "Vincetoxicum rossicum")

# create target group bias raster ----

# aggregate by coordinates
sum_records <- as.data.frame(dplyr::count(trgt_grp, decimalLatitude, decimalLongitude))

# extract coordinates
coords <- cbind(sum_records[, "decimalLatitude"], sum_records[, "decimalLongitude"])

# do a 2D kernel density estimation
target_density <- ks::kde(coords)

# create raster 
target_raster <- raster::raster(target_density)

# define in UTM 17N
crs(target_raster) <- '+init=EPSG:26917'

# 
target_raster <- target_raster - minValue(target_raster)
target_raster <- terra::rast(target_raster)
target_raster <- spatialEco::raster.transformation(target_raster, trans="norm")

# save to disk ----

ggsave(
  filename = here("output", "data_appendix", "GBIFrecs_trgtgrp.png"), 
  plot = plot_trgtgrp, 
  device = "png", 
  units = "in", 
  height = 5, 
  width = 7
)

  
 



