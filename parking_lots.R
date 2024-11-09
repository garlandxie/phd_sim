# libraries ----
library(opendatatoronto)
library(dplyr)
library(sf)

# import ----

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

## address points ----
ap <- show_package("abedd8bc-e3dd-4d45-8e69-79165a76e4fa") %>%
  list_package_resources() %>%
  dplyr::filter(name == "Address Points - 4326.csv") %>%
  get_resource()

## parking lot (polygons) ----
tm <- show_package("bb408f36-6824-4158-8a12-d4efe6465959") %>%
  list_package_resources() %>%
  dplyr::filter(name == "Parking Lot WGS84") %>%
  get_resource() 

# clean data ----

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
  st_transform(32617) %>%
  dplyr::select(parking_lot_asset_id, access, geometry)

## green p parking ----

gp_utm <- gp$carparks %>%
  st_as_sf(coords = c("lng", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(32617) %>%
  dplyr::select(id, address, streetview_lat, streetview_long, geometry)

## parking polygons ----

# get city-owned parking lots 
tm_gp <- st_filter(tm_utm, gp_utm, .pred = st_intersects) 
tm_pl <- st_filter(tm_utm, pl_utm, .pred = st_intersects)
tm_tidy <- rbind(tm_gp, tm_pl) %>% 
  distinct(objectid, .keep_all = TRUE)