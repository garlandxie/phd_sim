# libraries --------------------------------------------------------------------
library(here)              # for creating relative file-paths
library(sf)                # for manipulating geospatial vector files
library(raster)            # for manipulating geospatial raster files
library(opendatatoronto)   # for accessing data from Open Toronto portal
library(ggplot2)           # for visualizing data 
library(tidyterra)         # for visualizing data form terra R objects

# import -----------------------------------------------------------------------

## GBIF occurrence records -----------------------------------------------------
gbif_occ_recs <- read.csv(
  here("data", "intermediate_data", "occurrence_records", 
       "occ_tidy_wgs84.csv"
       )
)

## Toronto UGS -----------------------------------------------------------------
ugs <- list_package_resources("9a284a84-b9ff-484b-9e30-82f22c1780b9") %>%
  dplyr::filter(name == "Green Spaces - 4326.zip") %>%
  get_resource()

## SDMs ------------------------------------------------------------------------

## only get the existing UGS since source strength overlap with the recorded
## presences of DSV 
sc1_sdm <- terra::rast(here(
  "data/intermediate_data/maxent_sdm/DSV.tgb/Final/CurrentEM_final_tgb.tif")
)

# clean ------------------------------------------------------------------------

# get number of GBIF records per polygon ---------------------------------------
crs_utm17n <- 26917

ugs_utm17n <- st_transform(ugs, crs_utm17n)
gbif_utm17n <- gbif_occ_recs %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = crs_utm17n)

ugs_utm17n$num_gbif_per_ugs <- lengths(sf::st_intersects(ugs_utm17n, gbif_utm17n)) 
source_ugs_sf <- dplyr::filter(ugs_utm17n, num_gbif_per_ugs > 0) %>% dplyr::select(geometry)

# create binary raster ---------------------------------------------------------
#write_sf(
#  obj = source_ugs_sf, 
#  dsn = here("data", "intermediate_data", "source_raster.shp")
#)

#v <- raster::shapefile(here("data", "intermediate_data", "source_raster.shp"))
#ext <- extent(609540, 651640, 4826360, 4857460)
#r <- raster(ext, res = 20)
#rr <- rasterize(v, r, value = 1, background = 0)
#rr2 <- calc(rr, fun= function(x) {
#  ifelse(x >0, 1, 0)
#})

# create raster with source strength within ugs --------------------------------

source_ugs_terra <- terra::vect(source_ugs_sf)
sc1_source <- terra::mask(sc1_sdm, source_ugs_terra)
sc1_source <- terra::subst(sc1_source, NA, 0)

# get habitat suitability scores
# remove zero since we want DSV patches (either sink or source) 
hsm_scores <- values(sc1_source)
hsm_scores <- hsm_scores[hsm_scores > 0]

# extract rasters for source strength based on quantiles
sc1_source30 <- terra::app(sc1_sdm, function(x) {
  ifelse(x >= stats::quantile(hsm_scores, probs = 0.7), x, 0)
})

sc1_source10 <- terra::app(sc1_sdm, function(x) {
  ifelse(x >= stats::quantile(hsm_scores, probs = 0.9), x, 0)
})

sc1_source1 <- terra::app(sc1_sdm, function(x) {
  ifelse(x >= stats::quantile(hsm_scores, probs = 0.99), x, 0)
})


# visualize --------------------------------------------------------------------
(plot_sc1_source30 <- ggplot() + 
  tidyterra::geom_spatraster(data = sc1_source30) +
  theme_bw()
)

(plot_sc1_sourc10 <- ggplot() + 
    tidyterra::geom_spatraster(data = sc1_source10) +
    theme_bw()
)

(plot_sc1_sourc1 <- ggplot() + 
    tidyterra::geom_spatraster(data = sc1_source1) +
    theme_bw()
)


# save to disk ----
terra::writeRaster(
  x = sc1_source30, 
  filename = here("data", "intermediate_data", "resist_surfaces", "source_top30.tif"),
  overwrite = TRUE
)

terra::writeRaster(
  x = sc1_source10, 
  filename = here("data", "intermediate_data", "resist_surfaces", "source_top10.tif"),
  overwrite = TRUE
)

terra::writeRaster(
  x = sc1_source1, 
  filename = here("data", "intermediate_data", "resist_surfaces", "source_top1.tif"),
  overwrite = TRUE
)

