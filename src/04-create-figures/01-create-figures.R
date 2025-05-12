# libraries --------------------------------------------------------------------
library(here)
library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial)

# import -----------------------------------------------------------------------

sc1_resist_surf <- terra::rast(here("data", "intermediate_data", "resist_surfaces",
                        "sc1_resist_surf.tif")
)

sc2_resist_surf <- terra::rast(here("data", "intermediate_data", "resist_surfaces",
                                    "sc2_resist_surf.tif")
)


# visualize --------------------------------------------------------------------

(plot_sc1 <- ggplot() + 
  tidyterra::geom_spatraster(data = sc1_resist_surf) +
   scale_fill_viridis_c(
     option = "turbo",
     na.value = NA,
     breaks = c(0, 25, 50, 75, 100)
   ) +
   guides(fill = guide_legend(title = "Resistance Surface")) +
   ggspatial::annotation_scale() +
  theme_bw()
)

(plot_sc2 <- ggplot() + 
    tidyterra::geom_spatraster(data = sc1_resist_surf) +
    scale_fill_viridis_c(
      option = "turbo",
      na.value = NA,
      breaks = c(0, 25, 50, 75, 100)
    ) +
    guides(fill = guide_legend(title = "Resistance Surface")) +
    ggspatial::annotation_scale() +
    theme_bw()
)
# save to disk -----------------------------------------------------------------

ggsave(
 plot = plot_sc1,
 filename = here("output", "sc1_resist_surf.png"),
 device = "png",
 units = "in",
 height = 8,
 width = 8
)