# libraries ----
library(here)             # for creating relative file-paths 
library(dplyr)            # for manipulating data
library(ggplot2)          # for visualizing data 
library(spatstat)         # for analyzing IDW spatial interpolation
library(opendatatoronto)  # for importing Open Toronto data
library(Metrics)          # for calculating mean square error
library(raster)           # for rasterizing image data
library(gstat)            # for analyzing Kriging spatial interpolation
library(stars)

# import ----

# get observation window (study area - boundary of TO)
to_area <- show_package("841fb820-46d0-46ac-8dcb-d20f27e57bcc") %>%
  dplyr::filter(row_number() == 1) %>%
  list_package_resources() %>%
  get_resource()

# get point sample locations for rainfall
rain_gauges <- readRDS(
  here("data", "intermediate_data", "rain_gauge_2015-2024.RDS")
  )

# clean ----

## get growing season in climatic zones of Ontario (1976-2005)
## otherwise known as frost-free season 
## end of May to end of October
## www.ontario.ca/page/climate-zones-and-planting-dates-vegetables-ontario
rg_24 <- dplyr::filter(rain_gauges, date > "2024-05-31" & date < "2024-11-31")
rg_23 <- dplyr::filter(rain_gauges, date > "2023-05-31" & date < "2023-11-31")
rg_22 <- dplyr::filter(rain_gauges, date > "2022-05-31" & date < "2022-11-31")
rg_21 <- dplyr::filter(rain_gauges, date > "2021-05-31" & date < "2021-11-31")
rg_20 <- dplyr::filter(rain_gauges, date > "2020-05-31" & date < "2020-11-31")
rg_19 <- dplyr::filter(rain_gauges, date > "2019-05-31" & date < "2019-11-31")
rg_18 <- dplyr::filter(rain_gauges, date > "2018-05-31" & date < "2018-11-31")
rg_17 <- dplyr::filter(rain_gauges, date > "2017-05-31" & date < "2017-11-31")
rg_16 <- dplyr::filter(rain_gauges, date > "2016-05-31" & date < "2016-11-31")
rg_15 <- dplyr::filter(rain_gauges, date > "2015-05-31" & date < "2015-11-31")

rg <- rg_24 %>%
  rbind(rg_23) %>%
  rbind(rg_22) %>%
  rbind(rg_21) %>%
  rbind(rg_20) %>%
  rbind(rg_19) %>%
  rbind(rg_18) %>%
  rbind(rg_17) %>%
  rbind(rg_16) %>%
  rbind(rg_15)

## get total rainfall across growing season for each location per year
rg_tidy <- rg %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year, name, longitude, latitude) %>%
  summarize(total_annual_rf = sum(rainfall, na.rm = TRUE)) %>%
  ungroup()

## calculate mean total precipitation across growing season
#  for years 2015-2024
rg_summary <- rg_tidy %>%
  mutate(
    longitude = as.character(longitude),
    latitude = as.character(latitude)
  ) %>%
  
  # double-checked coordinates via Google Maps
  # RG_002 may have been moved, but less than 2km though
  # RG_001 may have been a clerical error on Excel
  mutate(
    longitude = case_when(
      name == "RG_001" ~ "-79.4781121",
      name == "RG_002" ~ "-79.4332161",
      TRUE ~ longitude) %>% as.numeric(),
    latitude = case_when(
      name == "RG_002" ~ "43.65120",
      TRUE ~ latitude
    ) %>% as.numeric()
  ) %>%
  group_by(name, longitude, latitude) %>%
  summarize(
    mean_annual_rf = mean(total_annual_rf, na.rm = TRUE),
    sd_annual_rf = sd(total_annual_rf, na.rm = TRUE),
    sample_size = n()
    ) %>%
  
  # remove any gauge locations that have been installed and measured
  # for a single year
  dplyr::filter(sample_size > 1)

# visualize ----
(plot_rg <- rg_summary %>%
  ggplot(aes(x = longitude, y = latitude)) + 
  geom_point(aes(size = mean_annual_rf))
) 

# analysis: IDW ----

## prepare the analysis by creating a point pattern process
bbox <- sf::st_bbox(to_area)
obs_window <- owin(xrange = c(bbox[1], bbox[3]), yrange = c(bbox[2], bbox[4]))
ppp_rainfall <- ppp(
  rg_summary$longitude, rg_summary$latitude,
  window = obs_window,
  marks = rg_summary$mean_annual_rf
  )

## perform IDW
## use leave-out one cross-validation to obtain lowest error
powers <- seq(0.001, 10, 0.01)
mse_result <- NULL
for(power in powers){
  CV_idw <- idw(ppp_rainfall, power=power, at="points")
  mse_result <- c(mse_result,
                  Metrics::mse(ppp_rainfall$marks,CV_idw))
}
optimal_power <- powers[which.min(mse_result)]
plot(powers, mse_result)

## rasterize
## NOTE: may need to change CRS later on 
idw_raster <- raster(
  idw(ppp_rainfall, power=optimal_power, at="pixels"),
  crs= crs(to_area)
  )

## create prediction grid ---
## create prediction grid
bbox <- to_area %>%
  st_transform(crs = 32617) %>%
  st_bbox()

cell_size <- 0.6
x <- seq(bbox$xmin, bbox$xmax, by=cell_size)
y <- seq(bbox$ymin, bbox$ymax, by=cell_size)

grd <- expand.grid(x = x, y = y)
plot(grd$x, grd$y)

grd$mean_annual_rf <- 1
grd <- st_as_stars(grd, crs=st_crs(to_area))
st_crs(grd) <- st_crs(bbox)


## analysis: idw 2 ----
rg_idw <- gstat::idw(mean_annual_rf ~ 1, locations=rg_vc2, newdata=grd, idp = 2)
plot(rast(rg_idw["var1.pred"]))

## analysis: kriging ----

## check for stationarity
rg_vc <- rg_summary %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(32617) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

rg_vc2 <- rg_summary %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(32617) 

coordinates(rg_vc) <- ~lon + lat
rainfall_vc <- variogram(mean_annual_rf ~ 1, data = rg_vc, cloud = TRUE)
plot(rainfall_vc, ylab=bquote(gamma), xlab=c("h (separation distance in m)"))

## get sample variogram
rg_v <- gstat::variogram(log(mean_annual_rf) ~ 1, rg_vc)
plot(rg_v, ylab=bquote(gamma), xlab=c("h (separation distance in m)"))

## get theoretical variogram
rg_v_t <- vgm(psill=0.01, "Sph", range=5000, nugget=0.001)
plot(rg_v, rg_v_t, cutoff = 0.2)
rg_vfit <- fit.variogram(rg_v, rg_v_t)

# ordinary kriging
rg_ok <- gstat::krige(log(mean_annual_rf) ~ 1, rg_vc2, grd, rg_vfit)
plot(terra::rast(rg_ok['var1.var']))

## save to disk ----

write.csv(
  rg_summary,
  here("data", "intermediate_data", "rainfall.csv")
)
