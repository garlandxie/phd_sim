# libraries --------------------------------------------------------------------
library(terra)        # for doing raster calculations
library(here)         # for creating relative file-paths
library(ggplot2)      # for visualizing data
library(spatialEco)

# import -----------------------------------------------------------------------

maxent_prob_rast <- terra::rast(here(
  "syncrosim/ugs_simulations.ssim.data/Scenario-83/wisdm_OutputSpatial/maxent_prob_map.tif")
  )

# clean ------------------------------------------------------------------------

resist_surf <- spatialEco::raster.invert(maxent_prob_rast)

# visualize --------------------------------------------------------------------

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
    ) +
    theme_bw() 
)

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