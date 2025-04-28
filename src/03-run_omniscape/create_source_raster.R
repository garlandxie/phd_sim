# libraries ----
library(here)
library(sf)
library(raster)
library(opendatatoronto)
library(ggplot2)

# import ----
gbif_occ_recs <- read.csv(
  here("data", "intermediate_data", "occurrence_records", 
       "occ_tidy.csv"
       )
)

ugs <- list_package_resources("9a284a84-b9ff-484b-9e30-82f22c1780b9") %>%
  dplyr::filter(name == "Green Spaces - 4326.zip") %>%
  get_resource()

sc1_resist_surf <- terra::rast(here("data/intermediate_data/resist_surfaces/sc1_resist_surf.tif"))

# clean ----

# get number of GBIF records per polygon ---------------------------------------
crs_utm17n <- 26917

ugs_utm17n <- st_transform(ugs, crs_utm17n)
gbif_utm17n <- gbif_occ_recs %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = crs_utm17n)

ugs_utm17n$num_gbif_per_ugs <- lengths(sf::st_intersects(ugs_utm17n, gbif_utm17n)) 
source_ugs_sf <- dplyr::filter(ugs_utm17n, num_gbif_per_ugs > 0) %>% dplyr::select

# create binary raster ---------------------------------------------------------
write_sf(
  obj = source_ugs_sf, 
  dsn = here("data", "intermediate_data", "source_raster.shp")
)

v <- raster::shapefile(here("data", "intermediate_data", "source_raster.shp"))
ext <- extent(609540, 651640, 4826360, 4857460)
r <- raster(ext, res = 20)
rr <- rasterize(v, r, value = 1, background = 0)
rr2 <- calc(rr, fun= function(x) {
  ifelse(x >0, 1, 0)
})

# create raster with source strength within ugs --------------------------------

source_ugs_terra <- terra::vect(source_ugs_sf)
sc1_source <- terra::mask(sc1_resist_surf, source_ugs_terra)
sc1_source <- terra::subst(sc1_source, NA, 0)

sc1_source2 <- terra::app(sc1_resist_surf, function(x) {
  ifelse(x <= 63, 100 - x, 1)
})

# visualize ----
plot_source_ugs <- ggplot() + 
  geom_spatraster(data = sc1_source) +
  theme_bw() 

# save to disk ----
terra::writeRaster(
  x = sc1_source2, 
  filename = here("data", "intermediate_data", "resist_surfaces", "source_raster.tif"),
  overwrite = TRUE
)

ggsave(
  filename = here("output", "source_ugs.png"), 
  plot = plot_source_ugs, 
  device = "png", 
  width = 5, 
  height = 5
)
