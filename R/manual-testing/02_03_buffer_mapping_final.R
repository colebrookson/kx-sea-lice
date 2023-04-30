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

# filter to just BC 
geo_data_sf_bc <- geo_data_sf[which(geo_data_sf$NAME_1 == "British Columbia"),]

# and make a bounding box of the whole region
bb <- sf::st_make_grid(sf::st_bbox(geo_data_sf_bc))

# now since we have a polygon of BC, we cant a polygon of the things that 
# are the waterways, so use the st_differnce of the bounding box and that 
# geom object and we're good 
non_land <- sf::st_difference(bb, geo_data_sf_bc)

# all analysis one study region ================================================

# crop to just the study region
non_land_study <- sf::st_crop(non_land, xmin = -128.85,
                              xmax = -128.12, ymin = 52.25, 
                              ymax = 52.95)

## NOTE: for some reason, in this data, there's an issue just north of the ymax
# which means that the cutoff here has to be this value, any further and it
# for some reason breaks. In this case study that's not a problem, but note
# that it might not be COMPLETELY accurate in the northern direction

# make sure the projection is the same (UTM)
utm_geo_data <- st_transform(non_land_study, 
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")


# quick sanity check for what we're looking at 
ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey90") +
  theme_base()

# now, we can do this one farm region at a time, or all at once, I think it's 
# actually computationally faster to do all at once
grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_geo_data)),
  # the size is really large to make a fine grid
  size = 100000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 
saveRDS(grid_sample, 
        here("./outputs/geo-objs/all-area-grid-sample.rds"))

grid_cropped <- grid_sample[sf::st_contains(
  utm_geo_data, grid_sample, sparse = F)]

network <- as_sfnetwork(grid_cropped, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())
saveRDS(grid_sample, 
        here("./outputs/geo-objs/all-area-network.rds"))

ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey90") +
  geom_sf(data = network %>% activate("edges") %>% st_as_sf()) +
  theme_base() 

# subset into the paths from each of the farms since that's how I'll need to 
# plot them
kid <- farms_utm[which(farms_utm$site == "Kid Bay"),]
loch <- farms_utm[which(farms_utm$site == "Lochalsh"),]
goat <- farms_utm[which(farms_utm$site == "Goat Cove"),]
sheep <- farms_utm[which(farms_utm$site == "Sheep Passage"),]
lime <- farms_utm[which(farms_utm$site == "Lime Point"),]
jackson <- farms_utm[which(farms_utm$site == "Jackson Pass"),]
cougar <- farms_utm[which(farms_utm$site == "Cougar Bay"),]
alex <- farms_utm[which(farms_utm$site == "Alexander Inlet"),]

## kid paths ===================================================================
kid_paths <- sfnetworks::st_network_paths(
  x = network,
  from = kid, 
  weights = "weight"
)  
nodes_all_kid <- kid_paths %>%
  pull(node_paths) 
edges_all_kid <- kid_paths %>%
  pull(edge_paths) 

kid_short_start <- Sys.time()
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_kid", "network"))

kid_short_edges <- parSapply(cl, edges_all_kid, slice_fun, 
                             net = network)
saveRDS(kid_short_edges, 
        here("./outputs/geo-objs/kid-whole-region-short-edges.rds"))
parallel::stopCluster(cl)
kid_short_time <- Sys.time() - kid_short_start

## loch paths ==================================================================
loch_paths <- sfnetworks::st_network_paths(
  x = network,
  from = loch, 
  weights = "weight"
)  
nodes_all_loch <- loch_paths %>%
  pull(node_paths) 
edges_all_loch <- loch_paths %>%
  pull(edge_paths) 

loch_short_start <- Sys.time()
cl <- parallel::makeCluster(11)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_loch", "network"))

loch_short_edges <- parSapply(cl, edges_all_loch, slice_fun, 
                             net = network)
saveRDS(loch_short_edges, 
        here("./outputs/geo-objs/loch-whole-region-short-edges.rds"))
parallel::stopCluster(cl)
loch_short_time <- Sys.time() - loch_short_start

## goat paths ==================================================================
goat_paths <- sfnetworks::st_network_paths(
  x = network,
  from = goat, 
  weights = "weight"
)  
nodes_all_goat <- goat_paths %>%
  pull(node_paths) 
edges_all_goat <- goat_paths %>%
  pull(edge_paths) 

goat_short_start <- Sys.time()
cl <- parallel::makeCluster(11)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_goat", "network"))

goat_short_edges <- parSapply(cl, edges_all_goat, slice_fun, 
                              net = network)
saveRDS(goat_short_edges, 
        here("./outputs/geo-objs/goat-whole-region-short-edges.rds"))
parallel::stopCluster(cl)
goat_short_time <- Sys.time() - goat_short_start

## sheep paths =================================================================
sheep_paths <- sfnetworks::st_network_paths(
  x = network,
  from = sheep, 
  weights = "weight"
)  
nodes_all_sheep <- sheep_paths %>%
  pull(node_paths) 
edges_all_sheep <- sheep_paths %>%
  pull(edge_paths) 

sheep_short_start <- Sys.time()
cl <- parallel::makeCluster(11)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_sheep", "network"))

sheep_short_edges <- parSapply(cl, edges_all_sheep, slice_fun, 
                              net = network)
saveRDS(sheep_short_edges, 
        here("./outputs/geo-objs/sheep-whole-region-short-edges.rds"))
parallel::stopCluster(cl)
sheep_short_time <- Sys.time() - sheep_short_start

## lime paths =================================================================
lime_paths <- sfnetworks::st_network_paths(
  x = network,
  from = lime, 
  weights = "weight"
)  
nodes_all_lime <- lime_paths %>%
  pull(node_paths) 
edges_all_lime <- lime_paths %>%
  pull(edge_paths) 

lime_short_start <- Sys.time()
cl <- parallel::makeCluster(11)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_lime", "network"))

lime_short_edges <- parSapply(cl, edges_all_lime, slice_fun, 
                               net = network)
saveRDS(lime_short_edges, 
        here("./outputs/geo-objs/lime-whole-region-short-edges.rds"))
parallel::stopCluster(cl)
lime_short_time <- Sys.time() - lime_short_start

## jackson paths ===============================================================
jackson_paths <- sfnetworks::st_network_paths(
  x = network,
  from = jackson, 
  weights = "weight"
)  
nodes_all_jackson <- jackson_paths %>%
  pull(node_paths) 
edges_all_jackson <- jackson_paths %>%
  pull(edge_paths) 

jackson_short_start <- Sys.time()
cl <- parallel::makeCluster(11)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_jackson", "network"))

jackson_short_edges <- parSapply(cl, edges_all_jackson, slice_fun, 
                              net = network)
saveRDS(jackson_short_edges, 
        here("./outputs/geo-objs/jackson-whole-region-short-edges.rds"))
parallel::stopCluster(cl)
jackson_short_time <- Sys.time() - jackson_short_start

## cougar paths ===============================================================
cougar_paths <- sfnetworks::st_network_paths(
  x = network,
  from = cougar, 
  weights = "weight"
)  
nodes_all_cougar <- cougar_paths %>%
  pull(node_paths) 
edges_all_cougar <- cougar_paths %>%
  pull(edge_paths) 

cougar_short_start <- Sys.time()
cl <- parallel::makeCluster(11)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_cougar", "network"))

cougar_short_edges <- parSapply(cl, edges_all_cougar, slice_fun, 
                                 net = network)
saveRDS(cougar_short_edges, 
        here("./outputs/geo-objs/cougar-whole-region-short-edges.rds"))
parallel::stopCluster(cl)
cougar_short_time <- Sys.time() - cougar_short_start

## alex paths ===============================================================
alex_paths <- sfnetworks::st_network_paths(
  x = network,
  from = alex, 
  weights = "weight"
)  
nodes_all_alex <- alex_paths %>%
  pull(node_paths) 
edges_all_alex <- alex_paths %>%
  pull(edge_paths) 

alex_short_start <- Sys.time()
cl <- parallel::makeCluster(11)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_alex", "network"))

alex_short_edges <- parSapply(cl, edges_all_alex, slice_fun, 
                                net = network)
saveRDS(alex_short_edges, 
        here("./outputs/geo-objs/alex-whole-region-short-edges.rds"))
parallel::stopCluster(cl)
alex_short_time <- Sys.time() - alex_short_start

## get all nodes and paths =====================================================
nodes_all_kid <- kid_short_edges %>%
  pull(node_paths) 
edges_all_kid <- kid_short_edges %>%
  pull(edge_paths) 

nodes_all_loch <- loch_short_edges %>%
  pull(node_paths) 
edges_all_loch <- loch_short_edges %>%
  pull(edge_paths) 

nodes_all_goat <- goat_short_edges %>%
  pull(node_paths) 
edges_all_goat <- goat_short_edges %>%
  pull(edge_paths) 

nodes_all_sheep <- sheep_short_edges %>%
  pull(node_paths) 
edges_all_sheep <- sheep_short_edges %>%
  pull(edge_paths) 


# upper area study region ======================================================

# cut to just the kid bay farm and area
upper_area <- sf::st_crop(non_land, xmin = -128.6,
                          xmax = -128.12, ymin = 52.5, 
                          ymax = 52.9)
utm_upper_area <- st_transform(upper_area, 
                               crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

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
kid <- farms_utm[which(farms_utm$site == "Kid Bay"),]
loch <- farms_utm[which(farms_utm$site == "Lochalsh"),]
goat <- farms_utm[which(farms_utm$site == "Goat Cove"),]
sheep <- farms_utm[which(farms_utm$site == "Sheep Passage"),]
lime <- farms_utm[which(farms_utm$site == "Lime Point"),]
jackson <- farms_utm[which(farms_utm$site == "Jackson Pass"),]
cougar <- farms_utm[which(farms_utm$site == "Cougar Bay"),]
alex <- farms_utm[which(farms_utm$site == "Alexander Inlet"),]

# do with kid paths
kid_paths <- sfnetworks::st_network_paths(
  x = upper_network,
  from = kid_bay, 
  weights = "weight"
)  

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


## pull the nodes and the edges =================================================

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
parallel::clusterExport(cl, varlist = c("edges_all_kid", "upper_network"))

kid_short_edges <- parSapply(cl, edges_all_kid, slice_fun, 
                             net = upper_network)
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
parallel::clusterExport(cl, varlist = c("edges_all_goat", "upper_network"))

goat_short_edges <- parSapply(cl, edges_all_goat, slice_fun, 
                              net = upper_network)
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
parallel::clusterExport(cl, varlist = c("edges_all_sheep", "upper_network"))

sheep_short_edges <- parSapply(cl, edges_all_sheep, slice_fun, 
                               net = upper_network)
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
parallel::clusterExport(cl, varlist = c("edges_all_lime", "upper_network"))

lime_short_edges <- parSapply(cl, edges_all_lime, slice_fun, 
                              net = upper_network)
parallel::stopCluster(cl)
saveRDS(lime_short_edges, here("./outputs/geo-objs/lime-short-edges.rds"))
lime_short_time <- Sys.time() - lime_short_start
print(paste0("To calculate the paths, elapsed time: ", 
             round(lime_short_time, 2), 
             " minutes"))


# west study region ============================================================

# cut to just the kid bay farm and area
west <- sf::st_crop(non_land, xmin = -128.85,
                    xmax = -128.36, ymin = 52.45, 
                    ymax = 53)

# make sure the projection is the same (UTM)
utm_west_area <- st_transform(west, 
                              crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# quick sanity check for what we're looking at 
ggplot() + 
  geom_sf(data = utm_west_area, color = 'black', fill = "grey90") +
  theme_base() 

west_west_grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_west_area)),
  # the size is really large to make a fine grid
  size = 200000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 
## four regions (west) =========================================================
west_west <- sf::st_crop(non_land, xmin = -128.85,
                         xmax = -128.55, ymin = 52.55, 
                         ymax = 52.8)

utm_west_west_area <- st_transform(west_west, 
                                   crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# quick sanity check for what we're looking at 
ggplot() + 
  geom_sf(data = utm_west_west_area, color = 'black', fill = "grey90") +
  theme_base() 

# first make it as small as possible
west_west_grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_west_west_area)),
  # the size is really large to make a fine grid
  size = 50000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 
saveRDS(west_west_grid_sample,
        here("./outputs/geo-objs/west-west-grid-sample.rds"))

# remove connections that are not within the water polygon
west_west_grid_cropped <- west_west_grid_sample[sf::st_contains(
  utm_west_west_area, west_west_grid_sample, sparse = F)]
saveRDS(west_west_grid_cropped,
        here("./outputs/geo-objs/west-west-grid-sample-cropped.rds"))

west_west_network <- as_sfnetwork(west_west_grid_cropped, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length()) 
saveRDS(west_west_network,
        here("./outputs/geo-objs/west-west-network.rds"))

ggplot() + 
  geom_sf(data = utm_west_west_area, color = 'black', fill = "grey90") +
  geom_sf(data = west_west_network %>% activate("edges") %>% st_as_sf()) +
  theme_base() 

## four regions (south) ========================================================
west_south <- sf::st_crop(non_land, xmin = -128.65,
                          xmax = -128.35, ymin = 52.4, 
                          ymax = 52.8)

utm_west_south_area <- st_transform(
  west_south, 
  crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# quick sanity check for what we're looking at 
ggplot() + 
  geom_sf(data = utm_west_south_area, color = 'black', fill = "grey90") +
  theme_base() 

# first make it as small as possible
west_south_grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_west_south_area)),
  # the size is really large to make a fine grid
  size = 200000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 
saveRDS(west_south_grid_sample,
        here("./outputs/geo-objs/west-south-grid-sample.rds"))

# remove connections that are not within the water polygon
west_south_grid_cropped <- west_south_grid_sample[sf::st_contains(
  utm_west_south_area, west_south_grid_sample, sparse = F)]
saveRDS(west_south_grid_cropped,
        here("./outputs/geo-objs/west-south-grid-sample-cropped.rds"))

west_south_network <- as_sfnetwork(west_south_grid_cropped, 
                                   directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length()) 
saveRDS(west_south_network,
        here("./outputs/geo-objs/west-south-network.rds"))

## four regions (north) ========================================================
west_north <- sf::st_crop(non_land, xmin = -128.65,
                          xmax = -128.3, ymin = 52.61, 
                          ymax = 52.98)

utm_west_north_area <- st_transform(
  west_north, 
  crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# quick sanity check for what we're looking at 
ggplot() + 
  geom_sf(data = utm_west_north_area, color = 'black', fill = "grey90") +
  theme_base() 

# first make it as small as possible
west_north_grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_west_north_area)),
  # the size is really large to make a fine grid
  size = 200000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 
saveRDS(west_north_grid_sample,
        here("./outputs/geo-objs/west-north-grid-sample.rds"))

# remove connections that are not within the water polygon
west_north_grid_cropped <- west_north_grid_sample[sf::st_contains(
  utm_west_north_area, west_north_grid_sample, sparse = F)]
saveRDS(west_north_grid_cropped,
        here("./outputs/geo-objs/west-north-grid-sample-cropped.rds"))

west_north_network <- as_sfnetwork(west_north_grid_cropped, 
                                   directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length()) 
saveRDS(west_north_network,
        here("./outputs/geo-objs/west-north-network.rds"))

## shortest paths ==============================================================

alex <- farms_utm[which(farms_utm$site == "Alexander Inlet"),]
cougar <- farms_utm[which(farms_utm$site == "Cougar Bay"),]

west_west_network <- readRDS(here("./outputs/geo-objs/west-west-network.rds"))
west_south_network <- readRDS(here("./outputs/geo-objs/west-south-network.rds"))
west_north_network <- readRDS(here("./outputs/geo-objs/west-north-network.rds"))

### alexander inlet ============================================================
alex_paths_west <- sfnetworks::st_network_paths(
  x = west_west_network,
  from = alex, 
  weights = "weight"
)  
alex_paths_south <- sfnetworks::st_network_paths(
  x = west_south_network,
  from = alex, 
  weights = "weight"
) 
alex_paths_north <- sfnetworks::st_network_paths(
  x = west_north_network,
  from = alex, 
  weights = "weight"
) 
saveRDS(alex_paths_west, here("./outputs/geo-objs/alex-inlet-west-paths.rds"))
saveRDS(alex_paths_south, here("./outputs/geo-objs/alex-inlet-south-paths.rds"))
saveRDS(alex_paths_north, here("./outputs/geo-objs/alex-inlet-north-paths.rds"))

### cougar bay =================================================================
cougar_paths_west <- sfnetworks::st_network_paths(
  x = west_west_network,
  from = cougar, 
  weights = "weight"
)  
cougar_paths_south <- sfnetworks::st_network_paths(
  x = west_south_network,
  from = cougar, 
  weights = "weight"
) 
cougar_paths_north <- sfnetworks::st_network_paths(
  x = west_north_network,
  from = cougar, 
  weights = "weight"
) 
saveRDS(cougar_paths_west, here("./outputs/geo-objs/cougar-bay-west-paths.rds"))
saveRDS(cougar_paths_south, 
        here("./outputs/geo-objs/cougar-bay-south-paths.rds"))
saveRDS(cougar_paths_north, 
        here("./outputs/geo-objs/cougar-bay-north-paths.rds"))

## pull nodes and edges ========================================================
nodes_alex_west <- alex_paths_west %>%
  pull(node_paths) 
edges_alex_west <- alex_paths_west %>%
  pull(edge_paths) 

nodes_alex_south <- alex_paths_south %>%
  pull(node_paths) 
edges_alex_south <- alex_paths_south %>%
  pull(edge_paths) 

nodes_alex_north <- alex_paths_north %>%
  pull(node_paths) 
edges_alex_north <- alex_paths_north %>%
  pull(edge_paths) 

nodes_cougar_west <- cougar_paths_west %>%
  pull(node_paths) 
edges_cougar_west <- cougar_paths_west %>%
  pull(edge_paths) 

nodes_cougar_south <- cougar_paths_south %>%
  pull(node_paths) 
edges_cougar_south <- cougar_paths_south %>%
  pull(edge_paths) 

nodes_cougar_north <- cougar_paths_north %>%
  pull(node_paths) 
edges_cougar_north <- cougar_paths_north %>%
  pull(edge_paths) 

## filter the edges ============================================================

# here, use our pre-defined function to figure out which of the edges match our 
# criteria of <30km

### alexander inlet ============================================================

#### west ======================================================================
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_alex_west", "west_west_network"))

alex_west_short_edges <- parSapply(cl, edges_alex_west, slice_fun, 
                             net = west_west_network)
parallel::stopCluster(cl)

saveRDS(alex_west_short_edges, 
        here("./outputs/geo-objs/alex-west-short-edges.rds"))
print("alex west")

#### south =====================================================================
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_alex_south", "west_south_network"))

alex_south_short_edges <- parSapply(cl, edges_alex_south, slice_fun, 
                              net = west_south_network)
parallel::stopCluster(cl)

saveRDS(alex_south_short_edges, 
        here("./outputs/geo-objs/alex-south-short-edges.rds"))
print("alex south")

#### north =====================================================================
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_alex_north", "west_north_network"))

alex_north_short_edges <- parSapply(cl, edges_alex_north, slice_fun, 
                                    net = west_north_network)
parallel::stopCluster(cl)

saveRDS(alex_north_short_edges, 
        here("./outputs/geo-objs/alex-north-short-edges.rds"))
print("alex north")

### cougar bay =================================================================

#### west ======================================================================
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_cougar_west", "west_west_network"))

cougar_west_short_edges <- parSapply(cl, edges_cougar_west, slice_fun, 
                                   net = west_west_network)
parallel::stopCluster(cl)

saveRDS(cougar_west_short_edges, 
        here("./outputs/geo-objs/cougar-west-short-edges.rds"))
print("cougar west")

#### south =====================================================================
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_cougar_south", "west_south_network"))

cougar_south_short_edges <- parSapply(cl, edges_cougar_south, slice_fun, 
                                    net = west_south_network)
parallel::stopCluster(cl)

saveRDS(cougar_south_short_edges, 
        here("./outputs/geo-objs/cougar-south-short-edges.rds"))
print("cougar south")

#### north =====================================================================
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_cougar_north", "west_north_network"))

cougar_north_short_edges <- parSapply(cl, edges_cougar_north, slice_fun, 
                                    net = west_north_network)
parallel::stopCluster(cl)

saveRDS(cougar_north_short_edges, 
        here("./outputs/geo-objs/cougar-north-short-edges.rds"))
print("cougar north")


