# libraries ----
library(opendatatoronto)
library(sf)
library(dplyr)
library(ggplot2)
library(here)

# boundary ----
to_boundary <- show_package("841fb820-46d0-46ac-8dcb-d20f27e57bcc") %>%
  list_package_resources() %>%
  get_resource() %>%
  st_transform(32617)

ugs <- show_package("9a284a84-b9ff-484b-9e30-82f22c1780b9") %>%
  list_package_resources() %>%
  dplyr::filter(format == "SHP") %>%
  dplyr::filter(name == "Green Spaces - 4326.zip") %>%
  get_resource() %>%
  st_transform(32617)

pl <- show_package("5c56aef2-dd4a-4e4a-9f2e-d3722622b7e6") %>%
  list_package_resources() %>%
  dplyr::filter(name == "parking-lot-facilities-q3-2016") %>%
  get_resource()

gp <- show_package("b66466c3-69c8-4825-9c8b-04b270069193") %>%
  list_package_resources() %>%
  dplyr::filter(name == "green-p-parking-2019") %>%
  get_resource()

# clean ----

pl_tidy <- pl %>%
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
  st_as_sf(coords = c("long", "lat")) 

pl_crs <- st_set_crs(pl_tidy, 4326)

gp_locations <- gp$carparks %>%
  st_as_sf(coords = c("lng", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(32617) %>%
  dplyr::select(id, address, streetview_lat, streetview_long, geometry)

# join -----

st_intersects()

# save to disk ----
st_write(
  obj = gp_locations, 
  dsn = here("data", "gp_locations.shp")
)
