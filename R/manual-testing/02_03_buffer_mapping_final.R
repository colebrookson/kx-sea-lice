library(dplyr)
library(magrittr)
library(sf)
library(here)
library(ggplot2)
library(sfnetworks)

# define manual helper function ================================================
slice_fun <- function(net, temp_edges) {
  #' Find path length of one path
  #' 
  #' @description Taking a single path (i.e. set of edges), get the length 
  #' of that path, and remove the units on it
  #' @param net sfnetwork. A network of sfnetwork
  #' @param temp_edges vector. The edges of the path at hand 
  #' 
  #' @return numeric value (units removed) of the length of that path
  
  return(units::drop_units(net %>% 
                             activate("edges") %>% 
                             slice(temp_edges) %>% 
                             st_as_sf() %>% 
                             st_combine() %>% 
                             st_length()))
}

# load in data =================================================================

## farm data ===================================================================
clean_farm_locs <- 
  readr::read_csv(here("./data/farm-lice/clean/clean-farm-locs.csv")) %>% 
  sf::st_as_sf(., coords = c("long", "lat"))

# set the coordinates for WGS84 -- will be changed 
sf::st_crs(clean_farm_locs) <- 4326

# transform to utm 
farms_utm <- st_transform(clean_farm_locs, 
                          crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

## geo data ====================================================================

# the data is downlaoded from the raster package and can be pulled with:
# canada <- raster::getData("GADM",country="CAN",level=1)
# canada_prov = canada[canada$NAME_1 == "British Columbia"] # subset to just BC

geo_data <- readRDS(here("./data/geo-spatial/gadm36_CAN_1_sp.rds"))
rm(geo_data)
# make into sf object
geo_data_sf <- st_as_sf(geo_data)

# convert to utm
utm_geo_data <- st_transform(geo_data_sf,
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# filter to just BC 
geo_data_sf_bc <- geo_data_sf[which(geo_data_sf$NAME_1 == "British Columbia"),]

# and make a bounding box of the whole region
bb <- sf::st_make_grid(sf::st_bbox(geo_data_sf_bc))

# now since we have a polygon of BC, we cant a polygon of the things that 
# are the waterways, so use the st_differnce of the bounding box and that 
# geom object and we're good 
non_land <- sf::st_difference(bb, geo_data_sf_bc)

# crop to just the study region
non_land_study <- sf::st_crop(non_land, xmin = -128.9,
                              xmax = -127.8, ymin = 52.3, 
                              ymax = 53.1)

# make sure the projection is the same (UTM)
utm_geo_data <- st_transform(non_land_study, 
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# quick sanity check for what we're looking at 
ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey90") +
  theme_base()

# now, we can do this one farm region at a time, or all at once, I think it's 
# actually computationally faster to do all at once

## make the grid sample on the whole study region ==============================

# first make it as small as possible
grid_sample <- sf::st_sample(sf::st_as_sfc(sf::st_bbox(utm_geo_data)), 
                              # the size is really large to make very fine grid
                              size = 500000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 
saveRDS(grid_sample, here("./outputs/geo-objs/grid-sample.rds"))

# remove connections that are not within the water polygon
crop_time_start <- Sys.time()
grid_cropped <- grid_sample[sf::st_within(
  grid_sample, utm_geo_data, sparse = F)]
saveRDS(grid_cropped, here("./outputs/geo-objs/grid-sample-cropped.rds"))
crop_time <- Sys.time() - crop_time_start
print(paste0("For the cropping, elapsed time: ", round(crop_time, 2), 
             " hours"))

# now actually make the network
net_time_start <- Sys.time()
network <- as_sfnetwork(grid_cropped, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length()) 
saveRDS(network, here("./outputs/geo-objs/network.rds"))
net_time <- Sys.time() - net_time_start
print(paste0("For the network creation, elapsed time: ", round(net_time, 2), 
             " minutes"))

## get the shortest paths ======================================================
# NOTE: get the shortest paths -- this is calculating the shortest paths from 
# each of the "from" points, to all of the other points on the entire grid
# this will take a LONG time

# get the shortest paths 
paths_start <- Sys.time()
all_paths <- sfnetworks::st_network_paths(
  x = network,
  from = farms_utm, 
  weights = "weight"
)  
saveRDS(all_paths, here("./outputs/geo-objs/all-paths.rds"))
path_time <- Sys.time() - paths_start
print(paste0("To calculate the paths, elapsed time: ", round(path_time, 2), 
             " minutes"))

# now pull both the nodes and the edges 
nodes_all <- all_paths %>%
  pull(node_paths) 
edges_all <- all_paths %>%
  pull(edge_paths) 

## filter the edges ============================================================

# here, use our pre-defined function to figure out which of the edges match our 
# criteria of <30km
short_start <- Sys.time()
short_edges <- len_crit(net = network, edges = edges_all)
saveRDS(short_edges, here("./outputs/geo-objs/short-edges.rds"))
short_time <- Sys.time() - short_start
print(paste0("To calculate the paths, elapsed time: ", round(short_time, 2), 
             " minutes"))















