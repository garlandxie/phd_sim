# libraries --------------------------------------------------------------------
library(terra)        # for manipulating rasters
library(here)         # for creating relative file-paths
library(ggplot2)      # for visualizing data
library(spatialEco)   # for inverting raster values
library(sf)           # for importing ESRI shapefiles

# import -----------------------------------------------------------------------

## urban green spaces ----------------------------------------------------------

to_ugs <- read_sf(here("data/input_data/green_spaces_4326/Green Spaces - 4326.shp"))

pl <- read_sf(here(
  "data", "intermediate_data", "parking lots",
  "pl_green_space_type.shp"))

## SDM rasters -----------------------------------------------------------------
maxent_prob_rast <- terra::rast(here(
  "syncrosim/ugs_simulations.ssim.data/Scenario-83/wisdm_OutputSpatial/maxent_prob_map.tif")
  )

tgt_back <-  terra::rast(here(
  "data/intermediate_data/target_background/trgt_prob_raster.tiff")
)


# clean ------------------------------------------------------------------------

## resistance surface ----------------------------------------------------------
resist_surf <- spatialEco::raster.invert(maxent_prob_rast)

## to ugs ----------------------------------------------------------------------
ugs_utm17n <- st_transform(to_ugs, 26917)

# visualize --------------------------------------------------------------------

(tgt_back_map <- ggplot() + 
   tidyterra::geom_spatraster(data = tgt_back) +   
   guides(fill = guide_legend(title = "Probability Surface")) + 
   scale_fill_gradientn(
     colours = terrain.colors(10),
     na.value = NA) + 
   theme_bw()
   )

(maxent_prob_map <- ggplot() + 
  tidyterra::geom_spatraster(data = maxent_prob_rast) + 
  guides(fill = guide_legend(title = "Habitat suitability of V. rossicum")) +
  scale_fill_viridis_c(
    option = "magma",
    breaks = c(0, 20, 40, 60, 80, 100),
    na.value = NA
    ) +
  theme_bw() 
)

(resist_surf_map <- ggplot() + 
    tidyterra::geom_spatraster(data = resist_surf) + 
    guides(fill = guide_legend(title = "Resistance Surface")) +
    scale_fill_viridis_c(
      option = "magma",
      na.value = NA,
      breaks = c(0, 20, 40, 60, 80, 100)
    ) 
)

new_ugs_sc <- ggplot() + 
  geom_sf(data = ugs_utm17n) + 
  geom_sf(data = pl, col = "red") +
  theme_bw()

# save to disk -----------------------------------------------------------------

ggsave(
  plot = maxent_prob_map, 
  filename = here("output", "maxent_prob_map.png"),
  device = "png", 
  units = "in", 
  width = 8, 
  height = 8
)

ggsave(
  plot = resist_surf_map, 
  filename = here("output", "resist_surface.png"),
  device = "png", 
  units = "in", 
  width = 8, 
  height = 8
)

ggsave(
  plot = tgt_back_map, 
  filename = here("output", "tgt_back_map.png"),
  device = "png", 
  units = "in", 
  width = 8, 
  height = 8
)

ggsave(
  plot = tgt_back_map, 
  filename = here("output", "tgt_back_map.png"),
  device = "png", 
  units = "in", 
  width = 8, 
  height = 8
)

ggsave(
  plot = new_ugs_sc, 
  filename = here("output", "new_ugs_scenario.png"),
  device = "png", 
  units = "in", 
  width = 8, 
  height = 8
)