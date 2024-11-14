# libraries ----
library(opendatatoronto)
library(dplyr)
library(sf)
library(raster)
library(here)
library(exactextractr)
library(stringr)

# import ----

## parkland priority map (raster) ----
r <- stack(here(
  "data", 
  "input_data", 
  "parkland_priority", 
  "parkland_2022_priority_polynom1_cubic.tif")
  )

## parking lot facilities (points) ----
pl <- show_package("5c56aef2-dd4a-4e4a-9f2e-d3722622b7e6") %>%
  list_package_resources() %>%
  dplyr::filter(name == "parking-lot-facilities-q3-2016") %>%
  get_resource()

## green p parking (points) ----
gp <- show_package("b66466c3-69c8-4825-9c8b-04b270069193") %>%
  list_package_resources() %>%
  dplyr::filter(name == "green-p-parking-2019") %>%
  get_resource()

## parking lot (polygons) ----
tm <- show_package("bb408f36-6824-4158-8a12-d4efe6465959") %>%
  list_package_resources() %>%
  dplyr::filter(name == "Parking Lot WGS84") %>%
  get_resource() 

## existing green spaces ----
ugs <- show_package("9a284a84-b9ff-484b-9e30-82f22c1780b9") %>%
  list_package_resources() %>%
  dplyr::filter(name == "Green Spaces - 4326.zip") %>%
  get_resource()

## neighbourhood equity index ----

# shape-files for each of the 140 historical neighbourhoods
nbs <- show_package("neighbourhoods") %>%
  list_package_resources() %>%
  dplyr::filter(name == "Neighbourhoods - historical 140 - 4326.zip") %>%
  get_resource() 

# neighbourhood equity scores for each of the 140 neighbourhoods
nei_scores <- read.csv(here("data", "intermediate_data", "eq_index.csv"))

# clean data ----

crs_raster <- st_crs(r)

## parking lot facilities ----
pl_utm <- pl %>%
  janitor::clean_names() %>%
  dplyr::filter(!is.na(gis_coordinate)) %>%
  mutate(
    
    lat_longs = stringr::str_split(gis_coordinate, pattern = ","), 
    
    long = sapply(lat_longs, "[", 1) %>%
      stringr::str_remove(pattern = "\\(") %>%
      as.numeric(),
    
    lat = sapply(lat_longs, "[", 2) %>%
      stringr::str_remove(pattern = "\\)") %>%
      as.numeric()
    
  ) %>%
  st_as_sf(coords = c("long", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(crs_raster) %>%
  dplyr::select(parking_lot_asset_id, access, geometry)

## green p parking ----

gp_utm <- gp$carparks %>%
  st_as_sf(coords = c("lng", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(crs_raster) %>%
  dplyr::select(id, address, streetview_lat, streetview_long, geometry)

## parking polygons ----

# get city-owned parking lots 
tm_utm <- tm %>%
  st_transform(crs_raster) %>%
  dplyr::select(objectid, geometry)

#tm_gp <- st_filter(gp_utm, tm_utm, .pred = st_intersects) 
#tm_pl <- st_filter(pl_utm, tm_utm, .pred = st_intersects)
#tm_tidy <- rbind(tm_gp, tm_pl) %>% 
#  distinct(objectid, .keep_all = TRUE)

## neighbourhood equity index -----

# get the neighbourhoods numbers as separate column for each dataset
# since I think that's the easiest way to get a index 
# for joining tables 

nbs_tidy <- nbs %>%
  mutate(
    neighbourhood_num = str_split(AREA_NA7, pattern = " ") %>% 
      sapply(tail, n = 1L) %>%
      str_replace(pattern = "\\(", replacement = "") %>%
      str_replace(pattern = "\\)", replacement = "")
    ) %>%
  dplyr::select(
    neighbourhood_num, 
    neighbourhood = AREA_NA7, 
    classification = CLASSIF9, 
    geometry
  ) 

nei_tidy <- nei_scores %>%
  mutate(
    neighbourhood_num = str_split(
      neighbourhood_number_and_name, pattern = " ") %>% 
      sapply(head, n = 1L)
  ) %>%
  dplyr::select(neighbourhood_num, score) 

nei_polygons <- nbs_tidy %>%
  inner_join(nei_tidy, by = "neighbourhood_num") %>%
  dplyr::select(neighbourhood, classification, score, geometry)

## zonal statistics ----
zn <- exactextractr::exact_extract(r, tm_utm, "majority")
colnames(zn) <- c("band1_red", "band2_green", "band3_blue")

# not a very elegant solution, but it works for now
tm_zn <- cbind(tm_utm, zn) 

tm_zn_orange <- tm_zn %>%
  
  dplyr::filter(
    
    # remove grey-ish values 
    !(band1_red     > 70 &
        band2_green > 70 &
        band3_blue  > 70
    ) & 
      
    # get possible orange-ish values using a RBG color codes chart
    band1_red %in% c(51:255) &
    band2_green %in% c(25:225) &
    band3_blue %in% c(0:204) &

    # then remove green-ish values
    # range was chosen based on seeing the RGB values of 
    # randomly picked parking lot polygons that overlapping green-ish polygons
    !(band1_red %in% c(50:70) &
      band2_green %in% c(80:140) &
      band3_blue %in% c(70:130)
    )
  ) 

# manually add polygons that were not captured by the zonal statistics workflow
#st_write(obj = tm_zn, dsn = here("tm_zn.shp"), append = TRUE)
#st_write(obj = tm_zn_orange, dsn = here("tm_zn_orange.shp"), append = TRUE)

missing_polys <- read.csv(
  here("data", "input_data", "parkland_priority",
  "missing_polygons.csv")
  )

missing_polys2 <- dplyr::inner_join(
  tm_zn, 
  missing_polys, 
  by = c("objectid" = "id")
)

# join parking lot polygons datasets, where both are within
# parkland priority zones 
pl_final <- rbind(tm_zn_orange, missing_polys2) 

# eda ----

## clean data ----
# get consistent coordinate systems
nei_utm <- sf::st_transform(nei_polygons, 32617)
ugs_utm <- sf::st_transform(ugs, 32617)
pls_utm <- sf::st_transform(pl_final, 32617) 

## calculate counts ----
nei_utm$num_ex_ugs <- unlist(lapply(st_intersects(nei_utm, ugs_utm), length))
nei_utm$num_pl <- unlist(lapply(st_intersects(nei_utm, pls_utm), length))

## calculate area ----
area_pl <- pls_utm %>%
  mutate(area_m2 = st_area(geometry) %>% as.numeric()) %>%
  st_intersection(nei_utm) %>%
  group_by(neighbourhood) %>%
  summarize(
    total_area = sum(area_m2, na.rm = TRUE),
    avg_area = mean(area_m2, na.rm = TRUE)) %>%
  st_drop_geometry()

nei <- left_join(nei_utm, area_pl, by = "neighbourhood") %>%
  mutate(
    area_nb = st_area(geometry) %>% as.numeric(),
    perc_area = (total_area/area_nb)*100
    )
  
## visualize ----
(eq_vs_pl_w_zeros <- nei_utm %>%
  ggplot(aes(x = factor(classification), y = log(num_pl))) + 
 geom_boxplot() + 
 labs(
    x = "Neighbourhood equity index",
    y = "Number of existing parking lots \n (within each neighbourhood)"
    ) + 
  theme_bw()
)

(eq_vs_pl_wo_zeros <- nei_utm %>%
  dplyr::filter(num_pl > 0) %>%
  ggplot(aes(x = score, y = num_pl)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(
    x = "Neighbourhood equity index",
    y = "Number of existing parking lots \n (within each neighbourhood)"
  ) + 
  theme_bw()
)

nei %>%
  ggplot(aes(x = score, y = perc_area)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Neighbourhood equity index",
    y = "Percent area of existing parking lots \n (within each neighbourhood)"
  )

nei %>%
  ggplot(aes(x = score, y = total_area)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(
    x = "Neighbourhood equity index",
    y = "Total area of existing parking lots \n (within each neighbourhood)"
  ) + 
  theme_bw()

nei %>%
  ggplot(aes(x = score, y = avg_area)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(
    x = "Neighbourhood equity index",
    y = "Average area of existing parking lots \n (within each neighbourhood)"
  ) + 
  theme_bw()

# save to disk -----

sf::st_write(
  obj = nei_polygons,
  dsn = here("data", "intermediate_data", "nei_scores.shp")
)

sf::st_write(
  obj = pl_final,
  dsn = here("data", "intermediate_data", "parking-lots-in-parkland-priority.shp")
)

sf::st_write(
  obj = missing_polys2,
  dsn = here("data", "input_data", "parkland_priority", "missing_polygons.shp")
)


