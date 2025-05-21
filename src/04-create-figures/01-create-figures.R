# libraries --------------------------------------------------------------------
library(here)
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
library(patchwork)

# import -----------------------------------------------------------------------

## ugs scenarios ---------------------------------------------------------------

ugs_existing <- read_sf(here(
  "data", "input_data", "green_spaces_4326", 
  "Green Spaces - 4326.shp")
) %>% st_transform(26917)

pl_realistic <- read_sf(here(
  "data", "intermediate_data", "parking lots",
  "pl_realistic_ugs_type.shp")
  )

pl_extreme <- read_sf(here(
  "data", "intermediate_data", "parking lots",
  "pl_extreme_ugs_type.shp")
  )

## resistance surface ----------------------------------------------------------
sc1_resist_surf <- terra::rast(here(
  "data", "intermediate_data", "resist_surfaces",
  "sc1_resist_surf.tif")
)

sc2_resist_surf <- terra::rast(here(
  "data", "intermediate_data", "resist_surfaces",
  "sc2_resist_surf.tif")
)

## omniscape impact ------------------------------------------------------------
impact_summary <- read.csv(here(
  "data","intermediate_data", "omniscape_impact", 
  "impact_summary.csv")
  )

# clean ------------------------------------------------------------------------

## omniscape impact ------------------------------------------------------------
impact_80m <- impact_summary %>%
  janitor::clean_names() %>%
  dplyr::filter(dispersal_distance_m == 80) %>%
  dplyr::select(connectivity_category, comparing_scenario, area_difference) %>%
  mutate(
    connectivity_category = factor(
      connectivity_category, 
      levels = c("Impeded", "Diffuse", "Intensified", "Channelized")), 
    
    comparing_scenario = factor(
      comparing_scenario, 
      levels = c("Adding_UGS_Realistic", "Adding_UGS_Extreme")
      )
    )

impact_1km <- impact_summary %>%
  janitor::clean_names() %>%
  dplyr::filter(dispersal_distance_m == 1000) %>%
  dplyr::select(connectivity_category, comparing_scenario, area_difference) %>%
  mutate(
    connectivity_category = factor(
      connectivity_category, 
      levels = c("Impeded", "Diffuse", "Intensified", "Channelized")), 
    
    comparing_scenario = factor(
      comparing_scenario, 
      levels = c("Adding_UGS_Realistic", "Adding_UGS_Extreme")
    )
  )

# visualize --------------------------------------------------------------------

## ugs scenarios ---------------------------------------------------------------

plot_ugs_existing <- ggplot() +
  geom_sf(data = ugs_existing) +
  theme_bw()

plot_ugs_realistic <- ggplot() +
  geom_sf(data = ugs_existing) +
  geom_sf(data = pl_realistic, col = "#FFC20A") + 
  theme_bw()

plot_ugs_extreme <- ggplot() +
  geom_sf(data = ugs_existing) +
  geom_sf(data = pl_extreme, col = "#0C7BDC") + 
  theme_bw()

plot_ugs <- plot_ugs_existing / plot_ugs_realistic / plot_ugs_extreme

## resistance surface ----------------------------------------------------------
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

## omniscape impact -------------------------------------------------

(plot_impact_80m <- impact_80m %>%
  ggplot(aes(x = connectivity_category, y = area_difference, fill = comparing_scenario)) + 
  geom_col(
    position = position_dodge2(preserve = "single")
    ) + 
   geom_hline(yintercept = 0, linewidth = 0.2) + 
   scale_fill_manual(
     name = "UGS scenario comparisons (Baseline: Existing UGS)",
     labels = c("Adding UGS (within Parkland Area of Needs)",
                "Adding UGS (maximum allocation)"),
     values=c("#FFC20A", "#0C7BDC"),
   ) + 
   scale_y_continuous(limits = c(-70, 70), breaks = seq(-70, 70, by = 10)) + 
   labs(
     x = "Connectivity Category", 
     y = "Change in landscape connectivity (as area difference)"
   ) + 
   theme_bw() + 
   theme(legend.title = element_text( size=7), legend.text=element_text(size=6))
)

(plot_impact_1km <- impact_1km %>%
    ggplot(aes(x = connectivity_category, y = area_difference, fill = comparing_scenario)) + 
    geom_col(
      position = position_dodge2(preserve = "single")
    ) + 
    geom_hline(yintercept = 0, linewidth = 0.2) + 
    scale_fill_manual(
      name = "UGS scenario comparisons (Baseline: Existing UGS)",
      labels = c("Adding UGS (within Parkland Area of Needs)",
                 "Adding UGS (maximum allocation)"),
      values=c("#FFC20A", "#0C7BDC"),
    ) + 
    scale_y_continuous(limits = c(-2000, 2000), breaks = seq(-2000, 2000, by = 500)) + 
    labs(
      x = "Connectivity Category", 
      y = "Change in landscape connectivity (as area difference)"
    ) + 
    theme_bw() + 
    theme(legend.title = element_text( size=7), legend.text=element_text(size=6))
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

ggsave(
  plot = plot_impact_80m, 
  filename = here("output", "impact_80m.png"),
  device = "png", 
  units = "in", 
  height = 5, 
  width = 9
)

ggsave(
  plot = plot_impact_1km, 
  filename = here("output", "impact_1km.png"),
  device = "png", 
  units = "in", 
  height = 5, 
  width = 9
)

ggsave(
  plot = plot_ugs,
  filename = here("output", "ugs_scenarios.png"),
  device = "png", 
  units = "in", 
  height = 5, 
  width = 9
)
