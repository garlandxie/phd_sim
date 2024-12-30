# function was adapted from Graham et al. 2015. Biol Conserv
# https://doi.org/10.1016/j.biocon.2015.09.002
# see also https://github.com/laurajanegraham/ifm_r/blob/master/R/model.fit.R

library(here)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)

# prep -------------------------------------------------------------------------

# used QGIS to create a buffer around sites that had 
# relatively high sample coverage (> 80%)

buffer <- sf::read_sf(here("data", "input_data", "buffer", "buffer.shp"))
ugs_inext <- sf::read_sf(here("data", "intermediate_data", "ugs_inext.shp"))
occ_tidy <- read.csv(here("data", "intermediate_data", "occ_tidy.csv"))

# clean data -------------------------------------------------------------------

## calculate patch occupancy ---------------------------------------------------

# get overlapping polygons within 2km buffer
out_overlap <- ugs_inext[st_intersects(ugs_inext, buffer, sparse = FALSE), ]

# get GBIF records for DSV from 2016-2024
dsv_gbif <- occ_tidy %>%
  dplyr::filter(species == "Vincetoxicum rossicum") %>%
  dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(4326) %>%
  st_transform(crs = 32617)

# get occupied polygons 
dsv_occ <- out_overlap %>%
  sf::st_join(dsv_gbif, join = st_intersects, left = TRUE) %>%
  mutate(
    occurrenceStatus = case_when(
      occurrenceStatus == "PRESENT" ~ "1", 
      is.na(occurrenceStatus) ~ "0", 
      TRUE ~ occurrenceStatus
    ) %>% as.numeric(),
  ) %>%
  dplyr::filter(!is.na(year)) %>%
  group_by(id, geometry) %>%
  tidyr::pivot_wider(values_from = occurrenceStatus, names_from = year) %>%
  ungroup() %>%
  mutate(across(`2023`:`2024`, ~tidyr::replace_na(., 0))) %>%
  select(
    id, 
    yr_2018 = `2018`, 
    yr_2019 = `2019`, 
    yr_2020 = `2020`, 
    yr_2021 = `2021`,
    yr_2022 = `2022`, 
    yr_2023 = `2023`, 
    yr_2024 = `2024`
  ) 
  
# get unoccupied polygons 
dsv_unocc <- out_overlap %>%
  sf::st_join(dsv_gbif, join = st_intersects, left = TRUE) %>%
  filter(is.na(year)) %>%
  mutate(
    yr_2018 = 0,
    yr_2019 = 0, 
    yr_2020 = 0, 
    yr_2021 = 0, 
    yr_2022 = 0, 
    yr_2023 = 0,
    yr_2024 = 0
  ) %>%
  select(colnames(dsv_occ))

dsv <- rbind(dsv_occ, dsv_unocc) %>%
  group_by(id) %>%
  summarize(across(yr_2018:yr_2024, ~ifelse(sum(.) >=1, 1, 0)))

## calculate patch area ---------------------------------------------------------

dsv <- dsv %>%
  mutate(
    area_m2 = st_area(geometry),
    area_m2 = as.numeric(area_m2),
    area_km2 = area_m2/1000000
    ) 

# assign variables -------------------------------------------------------------

# create a matrix to hold parameters x, y, and u
params <- matrix(0, nrow = 10, ncol = 4)

# get patch area
A <- dsv$area_km2

# estimate strength of the distance decay from empirical dispersal kernels
# so far, maximum dispersal distance for Vincetoxicum rossicum
alpha <- 70

# calculate interpatch distances as the minimum edge-edge distance between 
# each patch and the nearest patch 
# gDistance using rgeos is now out of service, plus terra R package
# uses different class objects than sf, so this sf solution should work 
dist_matrix <- st_distance(
  st_cast(dsv, "MULTILINESTRING"), 
  which = "Euclidean"
  )

# calculate connectivity -------------------------------------------------------

# for IFM, connectivity (S) is the sum of each patch
# where the migration of each patch is the product of patch size, occupancy, 
# and interpatch distance (weighted by dispersal distance) 
edis <- as.matrix(exp(-(1/alpha)*as.dist(dist_matrix)))
edis <- sweep(edis, 2, A, "*")

# this is for a single snapshot of occupancies 
# but could include multiple surveys from GBIF?
p <- st_drop_geometry(dsv[, c("yr_2018", "yr_2019", "yr_2020", "yr_2021", "yr_2022", "yr_2023", "yr_2024")])
P <- rowSums(p)
S <- rowSums(sweep(edis, 2, P/ncol(p), "*"))

# estimate parameters ----------------------------------------------------------

# run logistic regression 
# incidence function model can be parameterized as a linear model of 
# log-odds incidence
mod <- glm(cbind(P, ncol(p) - P) ~ offset(2*log(S)) + log(A), family = binomial(link = "logit"))

# estimate parameters x 
# x represents the extent to which a species’ survival is dependent on patch
# size (larger x represents weaker dependence)
beta <- coef(mod)
xhat <- beta[2]

# estimate parameters y and u from log(uy)
# species was present is of the size where extinction probability E = 1
ey <- exp(-beta[1])
etilde <- min(A[P>0])^xhat
ytilde <- ey/etilde

# summarize parameters ---------------------------------------------------------
params <- data.frame(x = x, e = e, y = y) 

# plot -------------------------------------------------------------------------

site_selection <- ggplot() + 
  geom_sf(data = st_intersection(dsv_gbif, out_overlap)) + 
  geom_sf(alpha = 0.1, data = out_overlap) + 
  theme_bw()

ggsave(
  plot = site_selection, 
  filename = here("site_select.png"),
  device = "png", 
  units = "in", 
  height = 5, 
  width = 5
)