# mapping with polygons and distance ===========================================
library(PBSmapping)
library(dplyr)
library(magrittr)
library(sf)
library(sp)
library(raster)
library(here)
library(ggplot2)
library(tidyverse)
library(sf)
library(sfnetworks)
library(nngeo)

clean_farm_locs <- readr::read_csv(here("./data/farm-lice/clean/clean-farm-locs.csv")) %>% 
  sf::st_as_sf(., coords = c("long", "lat"))
sf::st_crs(clean_farm_locs) <- 4326
  
farms_utm <- st_transform(clean_farm_locs, 
                          crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# read the data from raster package
geo_data <- readRDS(here("./data/geo-spatial/gadm36_CAN_1_sp.rds"))
# make into sf object
geo_data_sf <- st_as_sf(geo_data)
# utm_geo_data <- st_transform(geo_data_sf, 
#                              crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
st_crs(utm_geo_data)
# filter to just BC and make a bounding box of the whole region
geo_data_sf_bc <- geo_data_sf[which(geo_data_sf$NAME_1 == "British Columbia"),]
bb <- sf::st_make_grid(sf::st_bbox(geo_data_sf_bc))
# now since we have a polygon of BC, we cant a polygon of the things that 
# are the waterways, so use the st_differnce of the bounding box and that 
# geom object and we're good 
non_land <- sf::st_difference(bb, geo_data_sf_bc)
# crop to just the study region
non_land_study <- sf::st_crop(non_land, xmin = -128.9,
                          xmax = -127.8, ymin = 52.1, 
                          ymax = 53.2)
utm_geo_data <- st_transform(non_land_study, 
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
#st_crs(utm_geo_data)
ggplot() + 
  geom_sf(data = non_land_study, color = 'blue', fill = "red") 
ggplot() + 
  geom_sf(data = utm_geo_data, color = 'blue', fill = "red") 
# sample the bounding box with regular square points, then connect each point 
# to the closest 9 points 8 should've worked, but left some diagonals out.
study_grid_sample <- sf::st_sample(sf::st_as_sfc(sf::st_bbox(utm_geo_data)), 
                            # the size is really large to make a very fine grid
                            size = 1000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 

# remove connections that are not within the water polygon
study_grid_cropped <- study_grid_sample[sf::st_within(
  study_grid_sample, utm_geo_data, sparse = F)]

# make an sfnetwork of the cropped grid
area_network <- as_sfnetwork(study_grid_cropped)

kid_bay <- farms_utm[which(farms_utm$site == "Kid Bay"),]

all_paths <- sfnetworks::st_network_paths(
  area_network,
  from = kid_bay
)

ggplot() + 
  geom_sf(data = study_grid_sample, alpha = .05) +
  geom_sf(data = study_grid_cropped, color = 'dodgerblue') + 
  geom_sf(data = utm_geo_data, color = 'blue', fill = NA) + 
  geom_sf(data = farms_utm[which(farms_utm$site == "Kid Bay"),]) + 
  geom_sf(data = area_network %>% 
            activate(edges) %>%
            slice(all_paths) %>%
            st_as_sf(),
          color = 'turquoise',
          size = 2)

# cut to just the kid bay farm and area
kid_bay_area <- sf::st_crop(non_land, xmin = -128.6,
                              xmax = -128, ymin = 52.5, 
                              ymax = 53.2)
utm_kid_bay <- st_transform(kid_bay_area, 
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
#st_crs(utm_geo_data)

ggplot() + 
  geom_sf(data = utm_kid_bay, color = 'blue', fill = "red") 
kid_grid_sample <- sf::st_sample(sf::st_as_sfc(sf::st_bbox(utm_kid_bay)), 
                                   # the size is really large to make a very fine grid
                                   size = 50000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 

# remove connections that are not within the water polygon
kid_grid_cropped <- kid_grid_sample[sf::st_within(
  kid_grid_sample, utm_kid_bay, sparse = F)]
kid_network <- as_sfnetwork(kid_grid_cropped)

ggplot() + 
  geom_sf(data = kid_grid_sample, alpha = .05) +
  geom_sf(data = kid_grid_cropped, color = 'dodgerblue') + 
  geom_sf(data = utm_kid_bay, color = 'blue', fill = NA) 













province = "British Columbia"
canada_prov = geo_data[geo_data$NAME_1 %in% province] # subset to just BC



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


# Stack overflow example =======================================================
library(tidyverse)
library(sf)
library(sfnetworks)
library(nngeo)

# set seed to make the script reproducible,
#  since there is random sampling used
set.seed(813)

# Getting your data:
x <- dget("https://raw.githubusercontent.com/BajczA475/random-data/main/Moose.lake")
# Subset to get just one lake
moose_lake <- x[5,]
boat_ramp <- dget("https://raw.githubusercontent.com/BajczA475/random-data/main/Moose.access")
sample_locations <- dget("https://raw.githubusercontent.com/BajczA475/random-data/main/Moose.ssw")
sample_bbox <- dget("https://raw.githubusercontent.com/BajczA475/random-data/main/Moose.box")

# sample the bounding box with regular square points, then connect each point to the closest 9 points
#  8 should've worked, but left some diagonals out.
sq_grid_sample <- st_sample(st_as_sfc(st_bbox(moose_lake)), 
                            size = 10000, type = 'regular') %>% st_as_sf() %>%
  st_connect(.,.,k = 9)

# remove connections that are not within the lake polygon
sq_grid_cropped <- sq_grid_sample[st_within(sq_grid_sample, moose_lake, sparse = F)]

# make an sfnetwork of the cropped grid
# make an sfnetwork of the cropped grid
lake_network <- sq_grid_cropped %>% as_sfnetwork()

# find the (approximate) distance from boat ramp to point 170 (far north)
pt170 <- st_network_paths(lake_network, 
                          from = boat_ramp,
                          to = sample_locations[170,]) %>%
  pull(edge_paths) %>%
  unlist()

lake_network %>% 
  activate(edges) %>%
  slice(pt170) %>%
  st_as_sf() %>%
  st_combine() %>%
  st_length()




ggplot() + 
  geom_sf(data = sq_grid_sample, alpha = .05) +
  geom_sf(data = sq_grid_cropped, color = 'dodgerblue') + 
  geom_sf(data = moose_lake, color = 'blue', fill = NA) +
  geom_sf(data = boat_ramp, color = 'springgreen', size = 4) + 
  geom_sf(data = sample_locations[170,], color = 'deeppink1', size = 4) +
  geom_sf(data = lake_network %>% 
            activate(edges) %>%
            slice(pt170) %>%
            st_as_sf(),
          color = 'turquoise',
          size = 2) +
  theme_void()



# new stack overflow code ======================================================
canada <- raster::getData("GADM",country="CAN",level=1)
geo_data_sf <- st_as_sf(geo_data)
geo_data_sf_bc <- geo_data_sf[which(geo_data_sf$NAME_1 == "British Columbia"),]
non_land <- sf::st_difference(geo_data_sf_bc)

