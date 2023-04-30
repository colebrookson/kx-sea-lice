library(dplyr)
library(magrittr)
library(sf)
library(here)
library(ggplot2)
library(sfnetworks)
library(parallel)
source(here("./R/00_functions_global.R"))

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

# make into sf object
geo_data_sf <- st_as_sf(geo_data)
rm(geo_data)
gc()

# convert to utm
# utm_geo_data_all <- st_transform(geo_data_sf,
#                              crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# filter to just BC 
geo_data_sf_bc <- geo_data_sf[which(geo_data_sf$NAME_1 == "British Columbia"),]

# and make a bounding box of the whole region
bb <- sf::st_make_grid(sf::st_bbox(geo_data_sf_bc))

# now since we have a polygon of BC, we cant a polygon of the things that 
# are the waterways, so use the st_differnce of the bounding box and that 
# geom object and we're good 
non_land <- sf::st_difference(bb, geo_data_sf_bc)

# crop to just the study region
non_land_study <- sf::st_crop(non_land, xmin = -128.85,
                              xmax = -128.12, ymin = 52.412, 
                              ymax = 53.1)

# cut to just the kid bay farm and area
upper_area <- sf::st_crop(non_land, xmin = -128.6,
                            xmax = -128.12, ymin = 52.5, 
                            ymax = 52.9)

# make sure the projection is the same (UTM)
utm_geo_data <- st_transform(non_land_study, 
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
utm_upper_area <- st_transform(upper_area, 
                            crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# quick sanity check for what we're looking at 
ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey90") +
  theme_base()
ggplot() + 
  geom_sf(data = utm_upper_area, color = 'black', fill = "grey90") +
  theme_base()

# now, we can do this one farm region at a time, or all at once, I think it's 
# actually computationally faster to do all at once

## make the grid sample on the kid-bay study region ============================

# first make it as small as possible
upper_area_grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_upper_area)),
  # the size is really large to make a fine grid
  size = 50000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 
saveRDS(upper_area_grid_sample, 
        here("./outputs/geo-objs/upper-area-grid-sample.rds"))

# remove connections that are not within the water polygon
crop_time_start <- Sys.time()

upper_area_grid_cropped <- upper_area_grid_sample[sf::st_contains(
  utm_upper_area, upper_area_grid_sample, sparse = F)]
#rm(kid_grid_sample); gc()
saveRDS(upper_area_grid_cropped, 
        here("./outputs/geo-objs/upper-area-grid-sample-cropped.rds"))

crop_time <- Sys.time() - crop_time_start
print(paste0("For the cropping, elapsed time: ", round(crop_time, 2), 
           " seconds"))

# now actually make the network
net_time_start <- Sys.time()

upper_network <- as_sfnetwork(upper_area_grid_cropped, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length()) 
saveRDS(upper_network, here("./outputs/geo-objs/upper-area-network.rds"))

net_time <- Sys.time() - net_time_start
print(paste0("For the network creation, elapsed time: ", round(net_time, 2), 
           " minutes"))

## get the shortest paths ======================================================
# NOTE: get the shortest paths -- this is calculating the shortest paths from 
# each of the "from" points, to all of the other points on the entire grid
# this will take a LONG time


# subset into the paths from each of the farms since that's how I'll need to 
# plot them
kid_bay <- farms_utm[which(farms_utm$site == "Kid Bay"),]
loch <- farms_utm[which(farms_utm$site == "Lochalsh"),]
goat <- farms_utm[which(farms_utm$site == "Goat Cove"),]
sheep <- farms_utm[which(farms_utm$site == "Sheep Passage"),]
lime <- farms_utm[which(farms_utm$site == "Lime Point"),]

### kid bay ==================================================================== 
kid_paths_start <- Sys.time()
kid_paths <- sfnetworks::st_network_paths(
  x = upper_network,
  from = kid_bay, 
  weights = "weight"
)  
saveRDS(kid_paths, here("./outputs/geo-objs/kid-bay-paths.rds"))
kid_paths_time <- Sys.time() - kid_paths_start
print(paste0("To calculate the kid paths, elapsed time: ", 
             round(kid_paths_time, 2), 
             " minutes"))
### goat cove ==================================================================
goat_paths_start <- Sys.time()
goat_paths <- sfnetworks::st_network_paths(
  x = upper_network,
  from = goat, 
  weights = "weight"
)  
saveRDS(goat_paths, here("./outputs/geo-objs/kid-bay-paths.rds"))
goat_paths_time <- Sys.time() - goat_paths_start
print(paste0("To calculate the goat paths, elapsed time: ", 
             round(goat_paths_time, 2), 
             " minutes"))

### sheep passage ==============================================================
sheep_paths_start <- Sys.time()
sheep_paths <- sfnetworks::st_network_paths(
  x = upper_network,
  from = sheep, 
  weights = "weight"
)  
saveRDS(sheep_paths, here("./outputs/geo-objs/kid-bay-paths.rds"))
sheep_paths_time <- Sys.time() - sheep_paths_start
print(paste0("To calculate the sheep paths, elapsed time: ", 
             round(sheep_paths_time, 2), 
             " minutes"))

### lime point =================================================================
lime_paths_start <- Sys.time()
lime_paths <- sfnetworks::st_network_paths(
  x = upper_network,
  from = lime, 
  weights = "weight"
)  
saveRDS(lime_paths, here("./outputs/geo-objs/kid-bay-paths.rds"))
lime_paths_time <- Sys.time() - lime_paths_start
print(paste0("To calculate the lime paths, elapsed time: ", 
             round(lime_paths_time, 2), 
             " minutes"))


## pull the nodes and the edges ================================================

# now pull both the nodes and the edges 
nodes_all_kid <- kid_paths %>%
  pull(node_paths) 
edges_all_kid <- kid_paths %>%
  pull(edge_paths) 

nodes_all_goat <- goat_paths %>%
  pull(node_paths) 
edges_all_goat <- goat_paths %>%
  pull(edge_paths) 

nodes_all_sheep <- sheep_paths %>%
  pull(node_paths) 
edges_all_sheep <- sheep_paths %>%
  pull(edge_paths) 

nodes_all_lime <- lime_paths %>%
  pull(node_paths) 
edges_all_lime <- lime_paths %>%
  pull(edge_paths) 

## filter the edges ============================================================

# here, use our pre-defined function to figure out which of the edges match our 
# criteria of <30km

### kid bay ====================================================================
kid_short_start <- Sys.time()
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_kid", "network"))

kid_short_edges <- parSapply(cl, edges_all_kid, slice_fun, net = network)
parallel::stopCluster(cl)

saveRDS(kid_short_edges, here("./outputs/geo-objs/kid-short-edges.rds"))
kid_short_time <- Sys.time() - kid_short_start
print(paste0("To calculate the paths, elapsed time: ", 
             round(kid_short_time, 2), 
             " minutes"))

### goat cove ==================================================================
goat_short_start <- Sys.time()
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_goat", "network"))

goat_short_edges <- parSapply(cl, edges_all_goat, slice_fun, net = network)
parallel::stopCluster(cl)
saveRDS(goat_short_edges, here("./outputs/geo-objs/goat-short-edges.rds"))
goat_short_time <- Sys.time() - goat_short_start
print(paste0("To calculate the paths, elapsed time: ", 
             round(goat_short_time, 2), 
             " minutes"))

### sheep passage ==============================================================
sheep_short_start <- Sys.time()
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_sheep", "network"))

sheep_short_edges <- parSapply(cl, edges_all_sheep, slice_fun, net = network)
parallel::stopCluster(cl)
saveRDS(sheep_short_edges, here("./outputs/geo-objs/sheep-short-edges.rds"))
sheep_short_time <- Sys.time() - sheep_short_start
print(paste0("To calculate the paths, elapsed time: ", 
             round(sheep_short_time, 2), 
             " minutes"))

### lime point =================================================================
lime_short_start <- Sys.time()
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_lime", "network"))

lime_short_edges <- parSapply(cl, edges_all_lime, slice_fun, net = network)
parallel::stopCluster(cl)
saveRDS(lime_short_edges, here("./outputs/geo-objs/lime-short-edges.rds"))
lime_short_time <- Sys.time() - lime_short_start
print(paste0("To calculate the paths, elapsed time: ", 
             round(lime_short_time, 2), 
             " minutes"))















