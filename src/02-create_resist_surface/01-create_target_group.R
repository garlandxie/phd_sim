# libraries --------------------------------------------------------------------
library(rgbif)            # for querying GBIF records
library(here)             # for creating relative file-paths
library(sf)               # for manipulating geospatial files
library(wk)               # for manipulating WTK polygons - needed for GBIF queries
library(janitor)          # for cleaning column names
library(opendatatoronto)  # for getting open data from City of Toronto
library(dplyr)            # for manipulating data 

# import -----------------------------------------------------------------------

## boundary of Toronto ---------------------------------------------------------
# this is done to query the GBIF occurrence records using a polygon
to_bound <- list_package_resources("841fb820-46d0-46ac-8dcb-d20f27e57bcc") %>%
  get_resource()

# occurrence records -----------------------------------------------------------

# GBIF of species within the Apocynaceae family
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
wkt_to_boundary <- to_bound %>%
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

to_occ <- occ_download_get(
  key = '0067091-241126133413365',
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

## add raster for resampling ---------------------------------------------------
soil_ph <- terra::rast(here(
  "data", "input_data", "resist_surfaces",
  "soil_pH_to_20m_res.tif")
  )

# clean data -------------------------------------------------------------------

## check for duplicate records -------------------------------------------------
duplicates <- janitor::get_dupes(to_occ)

## remove focal species --------------------------------------------------------
trgt_grp <- dplyr::filter(to_occ, species != "Vincetoxicum rossicum") 

## get occurrence records within Toronto ---------------------------------------

to_bound_utm <- st_transform(to_bound, 26917)

trgt_grp_sf <- trgt_grp %>%
  st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"), 
    crs = "EPSG:4326") %>%
  st_transform(26917) %>%
  mutate(
    lat = sf::st_coordinates(.)[,1], 
    lon = sf::st_coordinates(.)[,2]
  )

trgt_grp_2 <- st_intersection(trgt_grp_sf, to_bound_utm) %>%
  st_drop_geometry()
  
# create target group bias raster ----------------------------------------------

# aggregate by coordinates
sum_records <- as.data.frame(dplyr::count(trgt_grp_2, lat, lon))

# extract coordinates
coords <- cbind(sum_records[, "lat"], sum_records[, "lon"])

# perform 2D kernel density estimation
target_density <- ks::kde(coords)

# create raster 
target_raster <- raster::raster(target_density)

# define in UTM 17N
raster::crs(target_raster) <- '+init=EPSG:26917'

# create probability surface surface
# range of 0-1 for the pixels 
target_raster <- target_raster - raster::minValue(target_raster)
target_raster <- target_raster %>%
  terra::rast() %>%
  spatialEco::raster.transformation(trans = "norm")

# clip new raster to TO boundary
target_raster2 <- raster::mask(target_raster, to_bound_utm)


# save to disk -----------------------------------------------------------------

terra::writeRaster(
  x = target_raster2,
  filename = here(
    "trgt_prob_raster.tiff")
  )
