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

## get EDDMaps records ---------------------------------------------------------

# obtained records online as of Jan 14, 2025
# there's no R API available for EDDMAps right now  
edd_occ <- read.csv(here("data", "input_data", "eddmaps", "mappings.csv"))

# clean data -------------------------------------------------------------------

## geospatial coordinates ------------------------------------------------------

# positional uncertainty should be twice lower (10 m) than the grain size (20 m)
# references: 
# (1) Gábor et al. (2022). MEE. 
# (2) Moudrý et al. (2024). Ecography. 
# (3) 

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

occ_final <- rbind(gbif_occ_final, edd_occ_final)

# sanity checks ----------------------------------------------------------------

# I have kept code below, mainly for transparency but 
# other readers can easily reproduce the code (if need be)

## coordinate conversion bias --------------------------------------------------
out.ddmm <- to_occ_clean %>%
    dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
    cd_ddmm(lon = "decimalLongitude", lat = "decimalLatitude", 
        ds = "species", diff = 1, min_span = 0.1,
        value = "dataset")

## rasterized sampling bias ----------------------------------------------------
out.round <- to_occ_clean %>%
  dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  cd_round(
  lon = "decimalLongitude", 
  lat = "decimalLatitude", 
  ds = "species",
  value = "dataset",
  T1 = 7,
  verbose = TRUE,
  graphs = F
  )

# save to disk -----------------------------------------------------------------

write.csv(
  x = occ_final, 
  file = here("data", "intermediate_data", "occ_tidy.csv")
)
