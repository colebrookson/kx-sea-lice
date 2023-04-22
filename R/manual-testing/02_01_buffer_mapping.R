# mapping with polygons and distance ===========================================
library(PBSmapping)
library(dplyr)
library(magrittr)
library(sf)
library(sp)
library(raster)
library(here)

geo_data <- readRDS(here("./data/geo-spatial/gadm36_CAN_1_sp.rds"))

province = "British Columbia"
canada_prov = geo_data[geo_data$NAME_1 %in% province] # subset to just BC

clean_farm_locs <- readr::read_csv(here("./data/farm-lice/clean/clean-farm-locs.csv"))


ggplot2::ggplot() +
  geom_polygon(data = canada_prov,
               aes(x = long, y = lat, group = group),
               colour = "black",
               linewidth = 0.01,
               fill = "grey65") +
  coord_cartesian(xlim = c(-128.8, -128.1), ylim = c(52.2, 52.95)) + 
  geom_point(data = clean_farm_locs,
             aes(x = long, y = lat, fill = type, shape = type),
             size = 4) + 
  ggthemes::theme_base() +
  labs(x = "Longitude (°)", y = "Latitude (°)") 


## code for stack overflow

### version with no sf
canada <- raster::getData("GADM",country="CAN",level=1)
canada_prov = canada[canada$NAME_1 == "British Columbia"] # subset to just BC

location <- data.frame(
  name = c("one", "two"),
  lat = c(52.79883, 52.53555),
  long = c(-128.3144, -128.3593)
)

ggplot2::ggplot() +
  geom_polygon(data = canada_prov,
               aes(x = long, y = lat, group = group),
               colour = "black",
               linewidth = 0.01,
               fill = "grey65") +
  geom_point(data = location, 
             aes(x = long, y = lat), shape = 21, fill = "red", size = 3) + 
  coord_cartesian(xlim = c(-128.6, -128.2), ylim = c(52.4, 52.95)) + 
  theme_bw() + 
  labs(x = "Longitude (°)", y = "Latitude (°)") 



library(raster)
library(ggplot2)
library(sf)

canada <- raster::getData("GADM",country="CAN",level=1, download = TRUE)
canada_prov = canada[canada$NAME_1 == "British Columbia"] # subset to just BC

can_prov_sf <- st_as_sf(canada_prov)

location <- data.frame(
  name = c("one", "two"),
  lat = c(52.79883, 52.53555),
  long = c(-128.3144, -128.3593)
  ) %>% st_centroid()

ggplot2::ggplot(data = can_prov_sf) +
  geom_sf() + 
  coord_sf(xlim = c(-128.8, -128.1), ylim = c(52.2, 52.95), 
           expand = FALSE) + 
  geom_point(data = location, 
             aes(x = long, y = lat), shape = 21, fill = "red", size = 3) + 
  theme_bw() + 
    labs(x = "Longitude (°)", y = "Latitude (°)") 



world <- spData::world
unique(world$geom)
can <- world[which(world$name_long == "Canada"), ]
plot(st_geometry(can))


data(nepacLLhigh)

# look at the plot
PBSmapping::plotMap(nepacLLhigh, 
                    xlim = c(-129, -127.75), 
                    ylim = c(52, 53))


# attempt to convert data to a polygon format
poly <- nepacLLhigh %>% 
  sf::st_as_sf(coords = c("X", "Y"), 
         crs = 32611) 
plot(nepacLLhigh %>% 
       sf::st_as_sf(coords = c("X", "Y"), 
                    crs = 32611) )

sf_airports <- st_as_sf(shp_airports) 
sf_airports_polygons <- st_polygonize(sf_airports)



# data from stamen
library(ggmap)
