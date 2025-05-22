# libraries --------------------------------------------------------------------
library(here)
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
library(patchwork)
library(ggspatial)
library(ggsignif)

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

to_boundary <- read_sf(here(
 "data/input_data/to_boundary/citygcs_regional_mun_wgs84.shp")
 ) %>% st_transform(26917)

## resistance surface ----------------------------------------------------------
sc1_resist_surf <- terra::rast(here(
  "data", "intermediate_data", "resist_surfaces",
  "sc1_resist_surf.tif")
)

sc2_resist_surf <- terra::rast(here(
  "data", "intermediate_data", "resist_surfaces",
  "sc2_resist_surf.tif")
)

sc3_resist_surf <- terra::rast(here(
  "data", "intermediate_data", "resist_surfaces",
  "sc3_resist_surf.tif")
)

## omniscape impact ------------------------------------------------------------
impact_summary <- read.csv(here(
  "data","intermediate_data", "omniscape_impact", 
  "impact_summary.csv")
  )

# clean ------------------------------------------------------------------------

## resistance surface ----------------------------------------------------------

change_resist_sc1_sc2 <- sc2_resist_surf - sc1_resist_surf 
change_resist_sc1_sc2 <- ifel(change_resist_sc1_sc2 < 0, 1, 0)
num_change_sc1_sc2 <- sum(values(change_resist_sc1_sc2), na.rm = TRUE)

change_resist_sc1_sc3 <- sc3_resist_surf - sc1_resist_surf
change_resist_sc1_sc3 <- ifel(change_resist_sc1_sc3 < 0, 1, 0)
num_change_sc1_sc3 <- sum(values(change_resist_sc1_sc3), na.rm = TRUE)

num_change <- data.frame(
  scenario = c("Scenario 2", "Scenario 3"),
  num_pixels = c(num_change_sc1_sc2, num_change_sc1_sc3)
) 

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
  annotate("text", x = 617000, y = 4854415, label = "Scenario 1", size = 7) +
  theme_bw() + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
    )

plot_ugs_realistic <- ggplot() +
  geom_sf(data = ugs_existing) +
  geom_sf(data = pl_realistic, col = "#E66100") + 
  annotate("text", x = 617000, y = 4854415, label = "Scenario 2", size = 7) +
  theme_bw() + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

plot_ugs_extreme <- ggplot() +
  geom_sf(data = ugs_existing) +
  geom_sf(data = pl_extreme, col = "#5D3A9B") + 
  annotate("text", x = 617000, y = 4854415, label = "Scenario 3", size = 7) +
  theme_bw() + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

plot_ugs <- plot_ugs_existing / plot_ugs_realistic / plot_ugs_extreme

## resistance surface ----------------------------------------------------------
(plot_resist_surf_sc1 <- ggplot() + 
   tidyterra::geom_spatraster(data = sc1_resist_surf) +
   scale_fill_gradientn(
     name = "Resistance Values", 
     colours = terrain.colors(10),     
     na.value = NA
     ) + 
   annotate("text", x = 617000, y = 4854415, label = "Scenario 1", size = 7) +
   theme_bw() + 
   theme(
     axis.text.y = element_blank(),
     axis.ticks.y = element_blank(),
     axis.title.y = element_blank(),
     axis.text.x = element_blank(),
     axis.ticks.x = element_blank(),
     axis.title.x = element_blank()
   )
)

(plot_resist_surf_sc2 <- ggplot() + 
    tidyterra::geom_spatraster(data = sc2_resist_surf, aes(fill = DSV.tbg_EMwmeanByROC_mergedData_mergedRun_mergedAlgo)) +
    scale_fill_gradientn(
      colours = terrain.colors(10),
      na.value = NA
    ) + 
    tidyterra::geom_spatraster(data = change_resist_sc1_sc2, aes(color = change_resist_surf)) +
    scale_color_gradient(
      low = "black", high = "white"
    ) + 
    annotate("text", x = 617000, y = 4854415, label = "Scenario 2", size = 7) +
    theme_bw() + 
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
)

(plot_resist_surf_sc3 <- ggplot() + 
    tidyterra::geom_spatraster(data = sc3_resist_surf) +
    tidyterra::geom_spatraster(data = change_resist_sc1_sc3) + 
    scale_fill_gradient(
      low = "black", high = "white",
      na.value = NA
    ) + 
    annotate("text", x = 617000, y = 4854415, label = "Scenario 3", size = 7) +
    theme_bw() + 
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
)

(plot_change_resist_sc1_sc2 <- ggplot() + 
  tidyterra::geom_spatraster(data = change_resist_sc1_sc2) +
  geom_sf(data = to_boundary, fill = NA) + 
  scale_fill_gradient2(
    name = "Change in Resistance\n (relative to Scenario 1)",
    low = "#005AB5", 
    high = "#DC3220", 
    na.value = NA) + 
  annotate("text", x = 617000, y = 4854415, label = "Scenario 2", size = 7) +
  theme_bw() + 
  theme(
     axis.text.y = element_blank(),
     axis.ticks.y = element_blank(),
     axis.title.y = element_blank(),
     axis.text.x = element_blank(),
     axis.ticks.x = element_blank(),
     axis.title.x = element_blank()
   )  
)

(plot_change_resist_sc1_sc3 <- ggplot() + 
    tidyterra::geom_spatraster(data = change_resist_sc1_sc3) +
    geom_sf(data = to_boundary, fill = NA) + 
    scale_fill_gradient2(
      name = "Change in Resistance\n (relative to Scenario 1)",
      low = "#005AB5", 
      high = "#DC3220", 
      na.value = NA) + 
    annotate("text", x = 617000, y = 4854415, label = "Scenario 3", size = 7) +
    theme_bw() + 
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ) 
)

plot_resist_surf <- plot_resist_surf_sc1 + plot_change_resist

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
     values = c("#E66100", "#5D3A9B"),
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
      values = c("#E66100", "#5D3A9B"),
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
  height = 12,
  width = 9
)

ggsave(
  plot = plot_resist_surf_sc1,
  filename = here("output", "resist_surf.png"),
  device = "png", 
  units = "in", 
  height = 12,
  width = 9
)

