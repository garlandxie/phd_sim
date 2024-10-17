# function was adapted from Graham et al. 2015. Biol Conserv
# https://doi.org/10.1016/j.biocon.2015.09.002
# see also https://github.com/laurajanegraham/ifm_r/blob/master/R/model.fit.R

library(here)
library(sf)
library(ggplot2)
library(dplyr)

# prep -------------------------------------------------------------------------

# used QGIS to create a buffer around sites that had 
# relatively high sample coverage (> 80%)

buffer <- sf::read_sf(here("data", "input_data", "buffer", "buffer.shp"))
ugs_inext <- sf::read_sf(here("data", "intermediate_data", "ugs_inext.shp"))
occ_tidy <- read.csv(here("data", "intermediate_data", "occ_tidy.csv"))

# clean data -------------------------------------------------------------------

# get overlapping polygons with the candidate 2km buffer
out_overlap <- ugs_inext[st_intersects(ugs_inext, buffer, sparse = FALSE), ]

# get garlic mustard GBIF records
dsv_gbif <- occ_tidy %>%
  dplyr::filter(species == "Vincetoxicum rossicum") %>%
  dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(4326) %>%
  st_transform(crs = 32617)

dsv_sites <- out_overlap %>%
  sf::st_join(dsv_gbif) %>%
  mutate(
    occurrenceStatus = case_when(
      occurrenceStatus == "PRESENT" ~ "1", 
      is.na(occurrenceStatus) ~ "0", 
      TRUE ~ occurrenceStatus
    ) %>% as.numeric()
  ) %>%
  group_by(id) %>%
  summarize(occ_recs = sum(occurrenceStatus)) %>%
  ungroup()

# load data
#survey <- read.csv(here("data", "intermediate_data", "occ_tidy.csv"))
#survey <- survey[1:30, ]
# create a matrix to hold parameters x, y, and u
#params <- matrix(0, nrow = 10, ncol=4)

# assign variables -------------------------------------------------------------

# get calculated patch sizes
area <- survey$area_km2 

# get latitude coordinates of a given patch
x_crd <- survey$x_crds 

# get longitude coordinates of a given patch 
y_crd <- survey$y_crds

# estimate strength of the distance decay from empirical dispersal kernels
# using m
alpha <- 70

# calculate interpatch distances as the minimum edge-edge distance between 
# each patch and the nearest patch 
# gDistance using rgeos is now out of service, plus terra R package
# uses different class objects than sf, so this sf solution should work 
lines <- st_cast(dsv_sites, "MULTILINESTRING")
dist_matrix <- st_distance(lines, which = "Euclidean")

# calculate connectivity -------------------------------------------------------

# for IFM, connectivity (S) is the sum of each patch
# where the migration of each patch is the product of patch size, occupancy, 
# and interpatch distance (weighted by dispersal distance) 
edis <- as.matrix(exp(-alpha*d))
diag(edis) <- 0
edis <- sweep(edis, 2, area, "*")

# this is for a single snapshot of occupancies 
# but could include multiple surveys from GBIF?
p <- survey[,5:(no.survey+4)]
P <- rowSums(p)
S <- rowSums(edis[, p > 0])

# estimate parameters ----------------------------------------------------------

# run logistic regression 
# incidence function model can be parameterized as a linear model of 
# log-odds incidence
mod <- glm(P ~ offset(2*log(S)) + log(A), family = binomial(link = "logit"))

# estimate parameters x 
# x represents the extent to which a species’ survival is dependent on patch
# size (larger x represents weaker dependence)
beta <- coef(mod)
x <- beta[2]

# estimate parameters y and u from log(uy)
# assume that the smallest plot where the
# species was present is of the size where extinction probability E = 1
A0 <- min(A[P>0])
ey <- exp(-beta[1])
e <- A0^x
y <- sqrt(ey/e) 

# summarize parameters ---------------------------------------------------------
params <- data.frame(x = x, e = e, y = y) 