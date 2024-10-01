# libraries --------------------------------------------------------------------
library(sf)     # for manipulating geospatial data
library(here)   # for creating relative file-paths
library(dplyr)
library(ggplot2)

# import -----------------------------------------------------------------------

ugs <- sf::read_sf(here("data", "input_data", "green_spaces_4326"))

gbif_alpe <- readr::read_delim(
  file = here(
  "data", "input_data", "gbif_dca_2024-sep-19",
  "alliara_petiolata", 
  "occurrence.txt"), 
  delim = "\t", 
  
  # select relevant columns to speed up parsing
  col_select = c(
    "occurrenceStatus", 
    "year", 
    "continent", 
    "decimalLatitude",
    "decimalLongitude", 
    "coordinateUncertaintyInMeters")
)

# clean data -------------------------------------------------------------------

# dissolve administrative boundaries of multiple polygons
ugs_dissolved <- ugs %>%
  st_union(by_feature = FALSE) %>%
  st_cast("POLYGON") %>%
  as.data.frame() %>% # each row is a polygon
  st_as_sf() %>%
  st_transform(crs = 32617)

# get occurrence records within North America
alpe_recs <- gbif_alpe %>%
  dplyr::filter(continent == "NORTH_AMERICA") %>%
  dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(4326) %>%
  st_transform(crs = 32617)

# perform geospatial analysis --------------------------------------------------

# create bounding box for the City of Toronto
bbx <- st_as_sfc(st_bbox(ugs_dissolved))
to_recs <- st_intersection(alpe_occ, bbx)

# count number of GBIF records per green space
# does not account for false absences in stochastic patch occupancy modelling
occ_tidy <- ugs_dissolved %>%
  mutate(
    area_m2 = st_area(geometry), 
    count_intersect = lengths(st_intersects(., to_recs))
    )
