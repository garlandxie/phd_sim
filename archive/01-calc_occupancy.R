# libraries --------------------------------------------------------------------
library(sf)        # for manipulating geospatial data
library(here)      # for creating relative file-paths
library(dplyr)     # for manipulating data
library(ggplot2)   # for visualizing data
library(tidyr)     # for making long to wide tables

# import -----------------------------------------------------------------------


gbif_alpe <- readr::read_delim(
  file = here(
  "data", "input_data", "gbif_dca_2024-sep-19",
  "alliara_petiolata", 
  "occurrence.txt"), 
  delim = "\t", 
  
  # select relevant columns to speed up parsing
  col_select = c(
    "gbifID",
    "occurrenceStatus", 
    "year", 
    "continent", 
    "decimalLatitude",
    "decimalLongitude")
) 

# clean data -------------------------------------------------------------------

# dissolve administrative boundaries of multiple polygons
ugs_dissolved <- ugs %>%
  st_union(by_feature = FALSE) %>%
  st_cast("POLYGON") %>%
  as.data.frame() %>% # each row is a polygon
  st_as_sf() %>%
  st_transform(crs = 32617) %>%
  mutate(id = c(1:nrow(.)))

# get GBIF records within North America
alpe_recs <- gbif_alpe %>%
  dplyr::filter(continent == "NORTH_AMERICA") %>%
  dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(4326) %>%
  st_transform(crs = 32617)
  
# perform geospatial analysis --------------------------------------------------

# create bounding box for the City of Toronto
bbx <- st_as_sfc(st_bbox(ugs_dissolved))
to_recs <- st_intersection(alpe_recs, bbx)

# get present-absence only data for the last eight years
# where each survey year is a column 
to_occ <- to_recs %>%
  dplyr::filter(year %in% c(2016:2024)) %>%
  mutate(
    occurrenceStatus = case_when(
      occurrenceStatus == "PRESENT" ~ "1", 
      occurrenceStatus == "ABSENT" ~ "0", 
      TRUE ~ occurrenceStatus),
    occurrenceStatus = as.numeric(occurrenceStatus)
  ) %>%
  group_by(gbifID, geometry) %>%
  tidyr::pivot_wider(names_from = year, values_from = occurrenceStatus) %>%
  ungroup() %>%
  mutate(across(
    c(`2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`), 
    ~tidyr::replace_na(., 0))) %>%
  select(
    gbifID, 
    geometry, 
    y2016 = `2016`,
    y2017 = `2017`,
    y2018 = `2018`, 
    y2019 = `2019`, 
    y2020 = `2020`, 
    y2021 = `2021`, 
    y2022 = `2022`, 
    y2023 = `2023`, 
    y2024 = `2024`
  )

# calculate patch occupancy and size for each green space 
# does not account for false absences in stochastic patch occupancy modelling
# this is required to parameterize the IFM with empirical data 
occ_tidy <- ugs_dissolved %>%
  
  # add GBIF observations if coordinates intersect a given green space 
  st_join(to_occ) %>%
  
  # create presence-only data across 8 years for a given green space 
  mutate(across(paste0("y", c(2016:2024)), ~tidyr::replace_na(., 0))) %>%
  group_by(id) %>%
  summarize(across(paste0("y", c(2016:2024)), ~ifelse(sum(.) >=1, 1, 0))) %>%
  ungroup() %>%
  
  # add patch size for a given green space 
  mutate(
    area_m2 = st_area(geometry),
    area_km2 = as.numeric(area_m2)/1000000, 
    
  # create x and y coordinates of the centroids for each green space
    centroid = st_centroid(geometry) %>% st_coordinates(),
    x_crds = centroid[,"X"], 
    y_crds = centroid[,"Y"]
    ) %>%
  
  # remove the spatial geometry inherent to sf class objects
  as.data.frame() %>%
  
  # clean up a bit
  dplyr::select(
    ugs_id = id, 
    x_crds, 
    y_crds, 
    area_km2,
    paste0("y", c(2016:2024))
  ) 

# save to disk -----------------------------------------------------------------

write.csv(
  occ_tidy, 
  file = here("data", "intermediate_data", "occ_tidy.csv")
)
