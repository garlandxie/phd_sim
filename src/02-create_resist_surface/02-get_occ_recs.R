# libraries --------------------------------------------------------------------
library(rgbif)    # for querying GBIF records
#library(usethis) # for getting GBIF credentials
library(here)     # for creating relative file-paths
library(sf)       # for manipulating geospatial files
library(wk)       # for manipulating WTK polygons - needed for GBIF queries
library(dplyr)
library(CoordinateCleaner)

# import -----------------------------------------------------------------------

# load green space dataset from City of Toronto 
# this is done to query the GBIF occurrence records using a polygon
ugs <- sf::read_sf(here("data", "input_data", "green_spaces_4326"))

# clean data -------------------------------------------------------------------

# get list of speciesKeys for invasive plants in TO
# see Potgieter et al. (2022). J Appl Ecol
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.14103
keys_id <- name_backbone_checklist("Vincetoxicum rossicum")

# create WTK polygon of the City of Toronto boundary
# this polygon should reduce the number of imported queries from GBIF
wkt_to_boundary <- ugs %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_as_text() %>%
  wk::wkt() %>%
  wk::wk_orient()

# run GBIF query from API
occ_download(
  pred_in("speciesKey", keys_id$speciesKey),
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_within(wkt_to_boundary)
)
  
# download queried occurrence record
to_occ <- occ_download_get(
  key = '0003346-241007104925546',
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

# more filters
to_occ_clean <- to_occ %>%
  
  # remove geodefault values
  dplyr::filter(!coordinateUncertaintyInMeters %in% c(301,3036,999,9999)) %>%
  
  # remove values with very high coordinate uncertainty
  dplyr::filter(coordinateUncertaintyInMeters < 1000 | is.na(coordinateUncertaintyInMeters))

# sanity checks

# note: I ran two tests to determine bias in coordinate conversion 
# and rasterized sampling on October, 9th, 2024. 
# Both tests show no deviations from tested assumptions, and thus
# zero flagged records within datasets of a particular invasive plant 

# I have kept code below, mainly for transparency but 
# other readers can easily reproduce the code (if need be)

## coordinate conversion bias
#out.ddmm <- to_occ_clean %>%
#    dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
#    cd_ddmm(lon = "decimalLongitude", lat = "decimalLatitude", 
#        ds = "species", diff = 1, min_span = 0.1,
#        value = "dataset")

## rasterized sampling bias
#out.round <- to_occ_clean %>%
#  dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
#  cd_round(
#  lon = "decimalLongitude", 
#  lat = "decimalLatitude", 
#  ds = "species",
#  value = "dataset",
#  T1 = 7,
#  verbose = TRUE,
#  graphs = F)

# save to disk -----------------------------------------------------------------

write.csv(
  x = to_occ_clean, 
  file = here("data", "intermediate_data", "occ_tidy.csv")
)
