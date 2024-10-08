# libraries --------------------------------------------------------------------
library(rgbif)    # for querying GBIF records
#library(usethis) # for getting GBIF credentials
library(here)     # for creating relative file-paths
library(sf)       # for manipulating geospatial files
library(wk)       # for manipulating WTK polygons - needed for GBIF queries
library(opendatatoronto) # for importing open data through API
library(dplyr)

# import -----------------------------------------------------------------------

# load green space dataset from City of Toronto 
# this is done to query the GBIF occurrence records using a polygon
package <- show_package("9a284a84-b9ff-484b-9e30-82f22c1780b9")
resources <- list_package_resources("9a284a84-b9ff-484b-9e30-82f22c1780b9")
datastore_resources <- dplyr::filter(resources, tolower(format) %in% c('csv', 'geojson'))
ugs <- dplyr::filter(datastore_resources, row_number()==4) %>% get_resource()

# clean data -------------------------------------------------------------------

# get list of speciesKeys for invasive plants in TO
# see Potgieter et al. (2022). J Appl Ecol
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.14103
keys_id <- name_backbone_checklist(
  c("Vincetoxicum rossicum",
  "Convolvulus arvensis",
  "Taraxacum officinale",
  "Lonicera tatarica",
  "Solanum dulcamara",
  "Ligustrum vulgare",
  "Rhamnus cathartica",
  "Echium vulgare",
  "Celastrus orbiculatus",
  "Rosa multiflora",
  "Impatiens glandulifera",
  "Phragmites australis",
  "Cirsium arvense",
  "Vinca minor",
  "Elaeagnus angustifolia",
  "Reynoutria japonica",
  "Acer platanoides",
  "Elaeagnus umbellata",
  "Barbarea vulgaris",
  "Rumex crispus",
  "Alliaria petiolata",
  "Potentilla recta",
  "Phleum pratense",
  "Crataegus monogyna",
  "Hypericum perforatum",
  "Plantago major",
  "Tussilago farfara",
  "Torilis japonica",
  "Elymus repens",
  "Lythrum salicaria",
  "Vicia cracca",
  "Dactylis glomerata",
  "Pastinaca sativa",
  "Trifolium repens",
  "Linaria vulgaris",
  "Campanula rapunculoides",
  "Convallaria majalis",
  "Daucus carota",
  "Trifolium pratense",
  "Cichorium intybus",
  "Ranunculus acris",
  "Verbascum thapsus",
  "Robinia pseudoacacia",
  "Hesperis matronalis",
  "Epipactis helleborine",
  "Glechoma hederacea",
  "Cirsium vulgare",
  "Aegopodium podagraria",
  "Chelidonium majus",
  "Leonurus cardiaca")
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
  pred_gte("year", 2016),
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
      "species", 
      "acceptedScientificName")
      )
