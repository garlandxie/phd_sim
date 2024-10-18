# libraries --------------------------------------------------------------------
library(iNEXT)    # for adjusting sampling efforts in GBIF data
library(here)     # for creating relative file-paths
library(sf)       # for manipulating geospatial files
library(dplyr)    # for manipulating data
library(ggplot2)  # for visualizing data

# import -----------------------------------------------------------------------
occ_tidy <- read.csv(here("data", "intermediate_data", "occ_tidy.csv"))
ugs <- sf::read_sf(here("data", "input_data", "green_spaces_4326"))

# clean data -------------------------------------------------------------------

# dissolve administrative boundaries of multiple polygons
ugs_dissolved <- ugs %>%
  st_union(by_feature = FALSE) %>%
  st_cast("POLYGON") %>%
  as.data.frame() %>% # each row is a polygon
  st_as_sf() %>%
  st_transform(crs = 32617) %>%
  mutate(id = c(1:nrow(.)))

# transform GBIF records into species-site matrix for iNEXT
occ_crs <- occ_tidy %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(4326) %>%
  st_transform(crs = 32617)

occ_ind <- ugs_dissolved %>%
  
  # collect all GBIF records that intersect within a given green space
  sf::st_join(occ_crs) %>%
  sf::st_drop_geometry() %>%
  
  # get number of GBIF records per species for each green space
  # aggregate across all years (for now)
   dplyr::filter(!is.na(year)) %>%
   mutate(
    occurrenceStatus = case_when(
      occurrenceStatus == "PRESENT" ~ "1", 
      occurrenceStatus == "ABSENT" ~ "0", 
      TRUE ~ occurrenceStatus), 
    occurrenceStatus = as.numeric(occurrenceStatus)
  ) %>%
  group_by(id, species) %>%
  summarize(num_records = sum(occurrenceStatus)) %>%
  ungroup() %>%
  tidyr::pivot_wider(values_from = num_records, names_from = id) %>%
  ungroup() %>%
  mutate(across(everything(.), ~tidyr::replace_na(., 0))) %>%
  tibble::column_to_rownames(var = "species") %>%
  as.data.frame() 

# calculate sample coverage for each green space -------------------------------

next_gbif <- iNEXT(x = occ_ind, datatype = "abundance", q = 0)

# remove single records
next_gbif_summ <- next_gbif$DataInfo  %>%
  dplyr::select(
    id = Assemblage, 
    num_gbif_records = n, 
    species_richness = S.obs, 
    sample_coverage = SC
    ) %>%
  mutate(
    id = as.integer(id)
  )
                
# find sites with high sample coverage -----------------------------------------

ugs_inext <- full_join(ugs_dissolved, next_gbif_summ, by = "id")

plot_inext <- ugs_inext %>%
  dplyr::filter(num_gbif_records >1 & species_richness >1) %>%
  ggplot() + 
    geom_sf(aes(fill = sample_coverage)) + 
    scale_fill_distiller(name = "SC", palette = "Spectral") + 
    theme_bw()

# save to disk -----------------------------------------------------------------

st_write(
  obj = ugs_inext, 
  dsn = here("data", "intermediate_data", "ugs_inext.shp")
  )

ggsave(
  filename = here("plot_inext.png"),
  plot = plot_inext, 
  device = "png",
  units = "in",
  height = 6, 
  width = 6  
)
                     