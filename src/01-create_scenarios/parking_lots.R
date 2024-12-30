# libraries ----
library(opendatatoronto) # for extracting open data from the web
library(dplyr)           # for manipulating data
library(sf)              # for manipulating geospatial data 
library(raster)          # for manipulating raster data 
library(here)            # for creating relative file-paths
library(exactextractr)   # for calculating zonal statistics
library(stringr)         # for manipulating string characters 

# import ----

## parkland priority map (raster) ----
r <- stack(here(
  "data", 
  "input_data", 
  "parkland_priority", 
  "parkland_2022_priority_polynom1_cubic.tif")
  )

## parking lot (polygons) ----
tm <- show_package("bb408f36-6824-4158-8a12-d4efe6465959") %>%
  list_package_resources() %>%
  dplyr::filter(name == "Parking Lot WGS84") %>%
  get_resource() 

# clean data ----

crs_raster <- st_crs(r)

## parking polygons ----

# get city-owned parking lots 
tm_utm <- tm %>%
  st_transform(crs_raster) %>%
  dplyr::select(objectid, geometry)

#tm_gp <- st_filter(gp_utm, tm_utm, .pred = st_intersects) 
#tm_pl <- st_filter(pl_utm, tm_utm, .pred = st_intersects)
#tm_tidy <- rbind(tm_gp, tm_pl) %>% 
#  distinct(objectid, .keep_all = TRUE)

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

# save to disk -----

sf::st_write(
  obj = pl_final,
  dsn = here("data", "intermediate_data", "parking-lots-in-parkland-priority.shp")
)

sf::st_write(
  obj = missing_polys2,
  dsn = here("data", "input_data", "parkland_priority", "missing_polygons.shp")
)


