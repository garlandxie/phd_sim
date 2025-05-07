# libraries --------------------------------------------------------------------
library(rgbif)    # for querying GBIF records
#library(usethis) # for getting GBIF credentials
library(here)     # for creating relative file-paths
library(sf)       # for manipulating geospatial files
library(wk)       # for manipulating WTK polygons - needed for GBIF queries
library(dplyr)
library(CoordinateCleaner)

# import -----------------------------------------------------------------------

## get Toronto green space shapefiles ------------------------------------------
ugs <- sf::read_sf(here("data", "input_data", "green_spaces_4326"))

## get GBIF records ------------------------------------------------------------
keys_id <- name_backbone_checklist(
  c("Vincetoxicum rossicum", "Cynanchum rossicum")
  )

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
gbif_occ <- occ_download_get(
  key = '0067143-241126133413365',
  path = here("data", "input_data", "occ_recs_gbif"),
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

## get EDDMaps records ---------------------------------------------------------

# obtained records online as of Jan 14, 2025
# there's no R API available for EDDMAps right now  
edd_occ <- read.csv(here(
  "data", "input_data", "occ_recs_eddmaps", 
  "mappings_2025-01-13.csv")
  )

# clean data -------------------------------------------------------------------

## geospatial coordinates ------------------------------------------------------

# GBIF
gbif_occ_clean <- gbif_occ %>%
  
  # remove geodefault values
  dplyr::filter(!coordinateUncertaintyInMeters %in% c(301,3036,999,9999)) %>%
  
  # get coordinate accuracy of 10 m or below
  dplyr::filter(coordinateUncertaintyInMeters <= 20) %>%
  dplyr::filter(!is.na(coordinateUncertaintyInMeters))

# EDDMaps
edd_occ_clean <- edd_occ %>%
  
  # get records within Toronto, Canada
  dplyr::filter(Location ==  "\"Toronto, Ontario, Canada\"") %>%
  
  # remove to avoid overlap with GBIF records 
  # GBIF has iNaturalist Records in their database when you do queries
  dplyr::filter(reporter != "iNaturalist Database  ") %>% 
  
  # get coordinate accuracy of 10 m or below
  dplyr::filter(CoordAcc <= 20) 

## set up for WISDM ------------------------------------------------------------

gbif_occ_final <- gbif_occ_clean %>%
  dplyr::select(
    id = gbifID, 
    lat = decimalLatitude, 
    lon = decimalLongitude
    ) 

edd_occ_final <- edd_occ_clean %>%
  dplyr::select(
  id = objectid,
  lat = Latitude, 
  lon = Longitude
  )

occ_tidy_wgs84 <- rbind(gbif_occ_final, edd_occ_final)

# convert lat longs to utm17n --------------------------------------------------

occ_tidy_utm17n <- occ_tidy_wgs84 %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326)) %>%
  st_transform(crs = 26917) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

# save to disk -----------------------------------------------------------------

write.csv(
  x = occ_tidy_wgs84, 
  file = here(
    "data", "intermediate_data", "occurrence_records", 
    "occ_tidy_wgs84.csv")
)

write.csv(
  x = occ_tidy_utm17n, 
  file = here(
    "data", "intermediate_data", "occurrence_records", 
    "occ_tidy_utm17n.csv")
)
