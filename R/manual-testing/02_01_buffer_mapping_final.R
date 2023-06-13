library(dplyr)
library(magrittr)
library(sf)
library(here)
library(ggplot2)
library(sfnetworks)
library(parallel)
library(sp)
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
# make into sf object
geo_data_bc <- geo_data[which(geo_data$NAME_1 == "British Columbia"),]
geo_data_sf_bc <- st_as_sf(geo_data_bc)

bc_utm <- st_transform(geo_data_sf_bc, 
                       crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
bc_cropped <- sf::st_crop(bc_utm, xmin = 450000,
                                      xmax = 600000, ymin = 5750000, 
                                      ymax = 6000000)

saveRDS(bc_cropped, 
        here("./outputs/geo-objs/fresh/large-land-for-plotting.rds"))

ggplot() + 
  geom_sf(data = bc_cropped) +
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

# and make a bounding box of the whole region
# and make a bounding box of the whole region
bb <- sf::st_make_grid(sf::st_bbox(bc_cropped), n = 1)

# now since we have a polygon of BC, we cant a polygon of the things that 
# are the waterways, so use the st_differnce of the bounding box and that 
# geom object and we're good 
non_land <- sf::st_difference(bb, bc_cropped)

# crop to just the study region
non_land_study <- sf::st_crop(
  non_land,
  ymin = 5788000, ymax = 5890000, xmin = 510000, xmax = 580000
)
utm_geo_data <- st_transform(non_land_study, 
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

saveRDS(utm_geo_data, here("./outputs/geo-objs/fresh/utm-geo-data.rds"))


ggplot() + 
  geom_sf(data = utm_geo_data) +
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

# make grids ===================================================================

## first, the main network =====================================================

# make the largest version of the grid
grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_geo_data)),
  # the size is really large to make a fine grid
  size = 75000, type = 'regular') %>% 
  sf::st_as_sf() 

# subset grid
# grid_cropped <- grid_sample[sf::st_contains(
#   utm_geo_data, grid_sample, sparse = F)]

ggplot() + 
  geom_sf(data = grid_cropped) + # looks like 100,000 is enough? 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")


## sample quite highly the small areas =========================================

### west area ==================================================================

geo_data_w <- st_crop(utm_geo_data, 
                  xmin = 515000,
                  xmax = 535000,
                  ymin = 5825000,
                  ymax = 5840000)
ggplot() + 
  geom_sf(data = geo_data_w) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(515000, 535000), ylim = c(5825000, 5840000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") 

#### north-south ===============================================================

geo_data_w_ns <- st_crop(utm_geo_data, 
                        xmin = 516000,
                        xmax = 516800,
                        ymin = 5829000,
                        ymax = 5834500)

ggplot() + 
  geom_sf(data = geo_data_w_ns) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(516000, 516800), ylim = c(5829000, 5834000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") 

# north version
grid_w_ns_ns <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_w_ns)),
  # the size is really large to make a fine grid
  size = 500, type = 'regular') %>% 
  sf::st_as_sf() 
# %>%
#   nngeo::st_connect(.,.,k = 9)
# grid_w_ns_ns_cropped <- grid_w_ns_ns[sf::st_contains(
#   geo_data_w_ns, grid_w_ns_ns, sparse = F)]

ggplot() + 
  geom_sf(data = geo_data_w) + 
  geom_sf(data = grid_w_ns_ns_cropped) + 
 #geom_sf(data = connect_to_ns_n, colour = "red") + 
 #geom_sf(data = connect_to_ns_s, colour = "red") + 
 #geom_sf(data = connect_ns_n_to_s, colour = "red") + 
 #geom_sf(data = grid_w_ns_n_cropped) + 
 #geom_sf(data = grid_w_ns_s_cropped) + 
  coord_sf(xlim = c(516000, 516800), ylim = c(5829000, 5834000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### east-west =================================================================

geo_data_w_eastwest <- st_crop(utm_geo_data, 
                            xmin = 525000,
                            xmax = 527300,
                            ymin = 5828000,
                            ymax = 5829400)
ggplot() + 
  geom_sf(data = geo_data_w_eastwest) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(523500, 528500), ylim = c(5828000, 5830000),
    datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

# west version
grid_w_ew_s <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_w_eastwest)),
  # the size is really large to make a fine grid
  size = 500, type = 'regular') %>% 
  sf::st_as_sf() 
#%>%
 # nngeo::st_connect(.,.,k = 9)
# grid_w_ew_s_cropped <- grid_w_ew_s[sf::st_contains(
#   geo_data_w_eastwest, grid_w_ew_s, sparse = F)]

ggplot() + 
  geom_sf(data = geo_data_w_eastwest) + 
  geom_sf(data = grid_cropped) + 
  #geom_sf(data = connect_to_ew_w, colour = "red") +
  #geom_sf(data = connect_to_ew_e, colour = "red") +
  #geom_sf(data = connect_to_ew_e_w, colour = "red") +
  geom_sf(data = grid_w_ew_s_cropped, colour = "red") +
  coord_sf(xlim = c(524500, 527500), ylim = c(5828000, 5830000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### south-east-northwest ======================================================

geo_data_w_farm <- st_crop(utm_geo_data, 
                               xmin = 521400,
                               xmax = 525600,
                               ymin = 5831000,
                               ymax = 5833000)
ggplot() + 
  geom_sf(data = geo_data_w_farm) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(521400, 525600), ylim = c(5831000, 5833000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

# west version
grid_w_ew_farm <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_w_farm)),
  # the size is really large to make a fine grid
  size = 500, type = 'regular') %>% 
  sf::st_as_sf() #%>%
#   nngeo::st_connect(.,.,k = 9)
# grid_w_ew_farm_cropped <- grid_w_ew_farm[sf::st_contains(
#   geo_data_w_farm, grid_w_ew_farm, sparse = F)]

ggplot() + 
  geom_sf(data = geo_data_w_farm) + 
 # geom_sf(data = grid_cropped) + 
  #geom_sf(data = connect_to_ew_w, colour = "red") +
  #geom_sf(data = connect_to_ew_e, colour = "red") +
  #geom_sf(data = connect_to_ew_e_w, colour = "red") +
  geom_sf(data = grid_w_ew_farm_cropped, colour = "red") +
  coord_sf(xlim = c(521400, 525600), ylim = c(5831000, 5833000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

### central area ===============================================================

geo_data_c <- st_crop(utm_geo_data, 
                      xmin = 537500,
                      xmax = 549000,
                      ymin = 5816000,
                      ymax = 5823000)
ggplot() + 
  geom_sf(data = geo_data_c) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(537500, 549000), ylim = c(5816000, 5823000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### central-south =============================================================

geo_data_c_s <- st_crop(utm_geo_data, 
                      xmin = 539000,
                      xmax = 541000,
                      ymin = 5817500,
                      ymax = 5818800)
ggplot() + 
  geom_sf(data = geo_data_c_s) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(539000, 541000), ylim = c(5817500, 5818800),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

# west area
grid_c_s_w <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_c_s)),
  size = 500, type = 'regular') %>% 
  sf::st_as_sf() #%>%
#   nngeo::st_connect(.,.,k = 9)
# grid_c_s_w_cropped <- grid_c_s_w[sf::st_contains(
#   geo_data_c_s, grid_c_s_w, sparse = F)]

ggplot() + 
  geom_sf(data = geo_data_c_s) + 
  #geom_sf(data = grid_cropped) +
  # geom_sf(data = connect_to_cs_w, colour = "red") +
  # geom_sf(data = connect_to_cs_e, colour = "red") +
  # geom_sf(data = connect_to_cs_w_e, colour = "red") +
  # geom_sf(data = grid_c_s_w_cropped, colour = "red") +
  geom_sf(data = grid_c_s_w_cropped, colour = "red") +
  coord_sf(xlim = c(539000, 541000), ylim = c(5817500, 5818800),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### central-north =============================================================

geo_data_c_n <- st_crop(utm_geo_data, 
                        xmin = 546500,
                        xmax = 548000,
                        ymin = 5819300,
                        ymax = 5820000)
ggplot() + 
  geom_sf(data = geo_data_c_n) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(546000, 548000), ylim = c(5819000, 5820000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

# west area
grid_c_n_w <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_c_n)),
  size = 100, type = 'regular') %>% 
  sf::st_as_sf() #%>%
#   nngeo::st_connect(.,.,k = 9)
# grid_c_n_w_cropped <- grid_c_n_w[sf::st_contains(
#   geo_data_c_n, grid_c_n_w, sparse = F)]

ggplot() + 
  geom_sf(data = geo_data_c) + 
  # geom_sf(data = grid_cropped) +
  # geom_sf(data = connect_to_cn_w, colour = "red") +
  # geom_sf(data = connect_to_cn_e, colour = "red") +
  # geom_sf(data = connect_to_cn_w_e, colour = "red") +
  # geom_sf(data = grid_c_n_w_cropped, colour = "red") +
  geom_sf(data = grid_c_n_w_cropped, colour = "red") +
  coord_sf(xlim = c(546000, 548000), ylim = c(5819000, 5820000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

### north area =================================================================

geo_data_n <- st_crop(utm_geo_data, 
                      xmin = 557000,
                      xmax = 558000,
                      ymin = 5854000,
                      ymax = 5856000)
ggplot() + 
  geom_sf(data = geo_data_n) + 
  geom_sf(data = all_grids_cropped) + 
  coord_sf(xlim = c(555000, 560000), ylim = c(5845000, 5860000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")



grid_north <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_n)),
  size = 100, type = 'regular') %>% 
  sf::st_as_sf() #%>%
#   nngeo::st_connect(.,.,k = 9)
# grid_north_cropped <- grid_north[sf::st_contains(
#   geo_data_n, grid_north, sparse = F)]

ggplot() + 
  geom_sf(data = geo_data_n) + 
  # geom_sf(data = grid_cropped) +
  # geom_sf(data = connect_to_cn_w, colour = "red") +
  # geom_sf(data = connect_to_cn_e, colour = "red") +
  # geom_sf(data = connect_to_cn_w_e, colour = "red") +
  # geom_sf(data = grid_c_n_w_cropped, colour = "red") +
  geom_sf(data = grid_north_cropped, colour = "red") +
  coord_sf(xlim = c(555000, 560000), ylim = c(5845000, 5860000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

## now look at the whole thing again ===========================================

# double checking the northern region
ggplot() + 
  geom_sf(data = utm_geo_data) + 
  geom_sf(data = grid_cropped) +
  coord_sf(xlim = c(555000, 560000), ylim = c(5845000, 5860000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")
# it's all good, so save the focal objects (the geo_data and the grid cropped)

## join the grids ==============================================================

all_grids <- rbind(grid_sample, grid_w_ns_ns, grid_w_ew_s, 
                   grid_w_ew_farm, grid_c_s_w, grid_c_n_w, grid_north)
all_grids_conn <- all_grids %>%
  nngeo::st_connect(.,.,k = 9)
all_grids_cropped <- all_grids_conn[sf::st_contains(
  utm_geo_data, all_grids_conn, sparse = F)]

### plot key areas =============================================================

# make some points that we'll use for a test via the shortest path thing
west_area_points <- st_as_sf(data.frame(
  east = c(5832000, 5835000),
  north = c(522500, 516500)
  ), coords = c("north","east"), 
  remove = FALSE, 
  crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m")
ggplot() + 
  geom_sf(data = geo_data_w) + 
  geom_sf(data = all_grids_cropped) + 
  geom_sf(data = west_area_points, colour = "red") +
  coord_sf(xlim = c(515000, 535000), ylim = c(5825000, 5840000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") 

central_area_points <- st_as_sf(
  data.frame(
    east = c(5817000, 5820000),
    north = c(544000, 548900)
    ), coords = c("north","east"), 
  remove = FALSE, 
  crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m")
ggplot() + 
  geom_sf(data = geo_data_c) + 
  geom_sf(data = all_grids_cropped) +
  geom_sf(data = central_area_points, colour = "red") +
  coord_sf(xlim = c(537500, 549000), ylim = c(5816000, 5823000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

north_area_points <- st_as_sf(
  data.frame(
    east = c(5852000, 5854000),
    north = c(558000, 555000)
  ), coords = c("north","east"), 
  remove = FALSE, 
  crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m")
ggplot() + 
  geom_sf(data = utm_geo_data) + 
  geom_sf(data = all_grids_cropped) +
  geom_sf(data = north_area_points) +
  coord_sf(xlim = c(555000, 560000), ylim = c(5845000, 5860000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")


### save key objects ===========================================================
qs::qsave(utm_geo_data, here("./outputs/geo-objs/fresh/utm-geo-data.qs"))
qs::qsave(all_grids_cropped, here("./outputs/geo-objs/fresh/grid-cropped.qs"))

# make network =================================================================

network <- as_sfnetwork(all_grids_cropped, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())
qs::qsave(network, 
        here("./outputs/geo-objs/fresh/network.qs"))

# check the network is connected in the important ways =========================

# west area shortest path
west_path = st_network_paths(network, 
                        from = west_area_points[1,],
                        to = west_area_points[2,],
                        weights = "weight") %>%
  pull(edge_paths) %>%
  unlist()
central_path = st_network_paths(network, 
                             from = central_area_points[1,],
                             to = central_area_points[2,],
                             weights = "weight") %>%
  pull(edge_paths) %>%
  unlist()
northern_path = st_network_paths(network, 
                                from = north_area_points[1,],
                                to = north_area_points[2,],
                                weights = "weight") %>%
  pull(edge_paths) %>%
  unlist()

# plot the shortest paths to confirm
test_plot <- ggplot() + 
  geom_sf(data = utm_geo_data) + 
  geom_sf(data = north_area_points, shape = 21, fill = "pink", 
          colour = "black", size = 3) +  
  geom_sf(data = central_area_points, shape = 21, fill = "pink", 
          colour = "black", size = 3) +  
  geom_sf(data = west_area_points, shape = 21, fill = "pink", 
          colour = "black", size = 3) +  
  geom_sf(data = network %>% 
            activate("edges") %>% 
            slice(west_path, northern_path, central_path) %>% 
            st_as_sf(), colour = "green", size = 5) +
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") +
  theme_base()
ggsave(
  here("./figs/maps/test-path-areas.png"),
  test_plot
)
# buffer for each farm =========================================================

## bring in the farms ==========================================================
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
cl <- parallel::makeCluster(18)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_kid", "network"))

kid_short_edges <- parSapply(cl, edges_all_kid, slice_fun, 
                             net = network)
parallel::stopCluster(cl)
kid_short_time <- Sys.time() - kid_short_start

# now subset the indices of the edges that fulfill our criteria (i.e. < 30km)
indices_keep_kid <- which(kid_short_edges < 30000)
keep_edges_kid <- unique(edges_all_kid[indices_keep_kid] %>% unlist())

# we can get the nodes if we want too
nodes_to_keep_kid <- nodes_all_kid[indices_keep_kid]
# this is keeping just the last node at the end of a path - we'll use this 
# to create a different coloured border on the buffer area 
nodes_to_keep_kid <- unlist(
  lapply(nodes_to_keep_kid, tail, n = 1L) %>% unlist()
)

edges_nodes_keep_kid <- list(
  nodes = nodes_to_keep_kid,
  edges = keep_edges_kid
)
saveRDS(edges_nodes_keep_kid,
        here("./outputs/geo-objs/fresh/edges-nodes-to-keep-kid.rds"))

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
cl <- parallel::makeCluster(18)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_loch", "network"))

loch_short_edges <- parSapply(cl, edges_all_loch, slice_fun, 
                              net = network)
parallel::stopCluster(cl)
loch_short_time <- Sys.time() - loch_short_start

indices_keep_loch <- which(loch_short_edges < 30000)
keep_edges_loch <- unique(edges_all_loch[indices_keep_loch] %>% unlist())

nodes_to_keep_loch <- nodes_all_loch[indices_keep_loch]
nodes_to_keep_loch <- unlist(
  lapply(nodes_to_keep_loch, tail, n = 1L) %>% unlist()
)

edges_nodes_keep_loch <- list(
  nodes = nodes_to_keep_loch,
  edges = keep_edges_loch
)
saveRDS(edges_nodes_keep_loch,
        here("./outputs/geo-objs/fresh/edges-nodes-to-keep-loch.rds"))

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
cl <- parallel::makeCluster(18)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_goat", "network"))

goat_short_edges <- parSapply(cl, edges_all_goat, slice_fun, 
                              net = network)
parallel::stopCluster(cl)
goat_short_time <- Sys.time() - goat_short_start

indices_keep_goat <- which(goat_short_edges < 30000)
keep_edges_goat <- unique(edges_all_goat[indices_keep_goat] %>% unlist())

nodes_to_keep_goat <- nodes_all_goat[indices_keep_goat]
nodes_to_keep_goat <- unlist(
  lapply(nodes_to_keep_goat, tail, n = 1L) %>% unlist()
)

edges_nodes_keep_goat <- list(
  nodes = nodes_to_keep_goat,
  edges = keep_edges_goat
)
saveRDS(edges_nodes_keep_goat,
        here("./outputs/geo-objs/fresh/edges-nodes-to-keep-goat.rds"))

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
cl <- parallel::makeCluster(18)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_sheep", "network"))

sheep_short_edges <- parSapply(cl, edges_all_sheep, slice_fun, 
                               net = network)
parallel::stopCluster(cl)
sheep_short_time <- Sys.time() - sheep_short_start

indices_keep_sheep <- which(sheep_short_edges < 30000)
keep_edges_sheep <- unique(edges_all_sheep[indices_keep_sheep] %>% unlist())

nodes_to_keep_sheep <- nodes_all_sheep[indices_keep_sheep]
nodes_to_keep_sheep <- unlist(
  lapply(nodes_to_keep_sheep, tail, n = 1L) %>% unlist()
)

edges_nodes_keep_sheep <- list(
  nodes = nodes_to_keep_sheep,
  edges = keep_edges_sheep
)
saveRDS(edges_nodes_keep_sheep,
        here("./outputs/geo-objs/fresh/edges-nodes-to-keep-sheep.rds"))

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
cl <- parallel::makeCluster(18)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_lime", "network"))

lime_short_edges <- parSapply(cl, edges_all_lime, slice_fun, 
                              net = network)
parallel::stopCluster(cl)
lime_short_time <- Sys.time() - lime_short_start

indices_keep_lime <- which(lime_short_edges < 30000)
keep_edges_lime <- unique(edges_all_lime[indices_keep_lime] %>% unlist())

nodes_to_keep_lime <- nodes_all_lime[indices_keep_lime]
nodes_to_keep_lime <- unlist(
  lapply(nodes_to_keep_lime, tail, n = 1L) %>% unlist()
)

edges_nodes_keep_lime <- list(
  nodes = nodes_to_keep_lime,
  edges = keep_edges_lime
)
saveRDS(edges_nodes_keep_lime,
        here("./outputs/geo-objs/fresh/edges-nodes-to-keep-lime.rds"))

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
cl <- parallel::makeCluster(18)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_jackson", "network"))

jackson_short_edges <- parSapply(cl, edges_all_jackson, slice_fun, 
                                 net = network)
parallel::stopCluster(cl)
jackson_short_time <- Sys.time() - jackson_short_start

indices_keep_jackson <- which(jackson_short_edges < 30000)
keep_edges_jackson <- unique(edges_all_jackson[indices_keep_jackson] 
                             %>% unlist())

nodes_to_keep_jackson <- nodes_all_jackson[indices_keep_jackson]
nodes_to_keep_jackson <- unlist(
  lapply(nodes_to_keep_jackson, tail, n = 1L) %>% unlist()
)

edges_nodes_keep_jackson <- list(
  nodes = nodes_to_keep_jackson,
  edges = keep_edges_jackson
)
saveRDS(edges_nodes_keep_jackson,
        here("./outputs/geo-objs/fresh/edges-nodes-to-keep-jackson.rds"))

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
cl <- parallel::makeCluster(9)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_cougar", "network"))

cougar_short_edges <- parSapply(cl, edges_all_cougar, slice_fun, 
                                net = network)
parallel::stopCluster(cl)
cougar_short_time <- Sys.time() - cougar_short_start

indices_keep_cougar <- which(cougar_short_edges < 30000)
keep_edges_cougar <- unique(edges_all_cougar[indices_keep_cougar] %>% unlist())

nodes_to_keep_cougar <- nodes_all_cougar[indices_keep_cougar]
nodes_to_keep_cougar <- unlist(
  lapply(nodes_to_keep_cougar, tail, n = 1L) %>% unlist()
)

edges_nodes_keep_cougar <- list(
  nodes = nodes_to_keep_cougar,
  edges = keep_edges_cougar
)
saveRDS(edges_nodes_keep_cougar,
        here("./outputs/geo-objs/fresh/edges-nodes-to-keep-cougar.rds"))

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
cl <- parallel::makeCluster(9)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_alex", "network"))

alex_short_edges <- parSapply(cl, edges_all_alex, slice_fun, 
                              net = network)
parallel::stopCluster(cl)
alex_short_time <- Sys.time() - alex_short_start

indices_keep_alex <- which(alex_short_edges < 30000)
keep_edges_alex <- unique(edges_all_alex[indices_keep_alex] %>% unlist())

nodes_to_keep_alex <- nodes_all_alex[indices_keep_alex] 
nodes_to_keep_alex <- unlist(
  lapply(nodes_to_keep_alex, tail, n = 1L) %>% unlist()
)

edges_nodes_keep_alex <- list(
  nodes = nodes_to_keep_alex,
  edges = keep_edges_alex
)
saveRDS(edges_nodes_keep_alex,
        here("./outputs/geo-objs/fresh/edges-nodes-to-keep-alex.rds"))

all_nodes_edges_to_keep <- list(
  "Lime Point" = edges_nodes_keep_lime,
  "Sheep Passage" = edges_nodes_keep_sheep,
  "Kid Bay" = edges_nodes_keep_kid,
  "Goat Cove" = edges_nodes_keep_goat,
  "Lochalsh" = edges_nodes_keep_loch,
  "Jackson Pass" = edges_nodes_keep_jackson,
  "Alexander Inlet" = edges_nodes_keep_alex,
  "Cougar Bay" = edges_nodes_keep_cougar
)

saveRDS(all_nodes_edges_to_keep, 
        here("./outputs/geo-objs/fresh/all-edges-nodes-to-keep.rds"))

# make test plots ==============================================================
ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99") + 
  geom_sf(data = network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_cougar) %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  #geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = alex,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() + 
  labs(title = "Alexander Inlet")

# test to see if it's connected across that network 
ggplot() + 
  #geom_sf(data = geo_data_w_eastwest) + 
  geom_sf(data = network %>%
            activate("nodes") %>% 
            activate("edges") %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  geom_sf(data = network %>% 
            activate("edges") %>% 
            slice(path) %>% 
            st_as_sf(), colour = "red") +
  geom_sf(data = points, size = 2) +
  coord_sf(xlim = c(523500, 528500), ylim = c(5828000, 5830000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")


points = data.frame(
  east=c(5828750, 5828750),
  north=c(524000, 528000)
)

points = st_as_sf(points, coords = c("north","east"), remove = FALSE, 
                  crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m")
path = st_network_paths(network, 
                 from = points[1,],
                 to = points[2,],
                 weights = "weight") %>%
  pull(edge_paths) %>%
  unlist()

library(sf)
library(nngeo)

nc = st_read(system.file("shape/nc.shp", package="sf"))
nc_utm = st_transform(nc, crs="+proj=utm +zone=18 +datum=NAD83 +unit=m") # using UTM

# crop the area to two smaller areas for sampling the grids
area_1 <- st_crop(nc_utm, xmin = 0, xmax = 20000, 
                  ymin = 3950000, ymax = 4000000)
area_2 <- st_crop(nc_utm, xmin = 20100, xmax = 25000, 
                  ymin = 3960000, ymax = 3980000)
area_3 <- st_crop(nc_utm, xmax = 0, xmin = -10000,
                  ymin = 3960000, ymax = 3970000)

# sample the grids and connect each
grid_1 <- sf::st_sample(
  area_1, size = 25, type = 'regular') %>% 
  sf::st_as_sf() 

grid_2 <- sf::st_sample(
  area_2, size = 25, type = 'regular') %>% 
  sf::st_as_sf() 

grid_3 <- sf::st_sample(
  area_3, size = 25, type = 'regular') %>% 
  sf::st_as_sf() 

grid_connect <- nngeo::st_connect(st_combine(grid_1), st_combine(grid_2))

grid_connect2 <- nngeo::st_connect(st_combine(grid_1), st_combine(grid_3))

all_grids <- c(grid_1, grid_2, grid_3, grid_connect, grid_connect2)

all_points <- rbind(grid_1, grid_2, grid_3)
all_grids <- all_points %>%
  nngeo::st_connect(.,.,k = 9, maxdist = 10000)

ggplot() + 
  geom_sf(data = nc_utm) +
  geom_sf(data = area_1, fill = 'blue', alpha = 0.3) + 
  geom_sf(data = area_2, fill = "red", alpha = 0.3) + 
  geom_sf(data = area_3, fill = "yellow", alpha = 0.3) + 
  geom_sf(data = grid_1, colour = "blue") + 
  geom_sf(data = grid_2, colour = "red") +
  geom_sf(data = grid_3, colour = "yellow") +
  geom_sf(data = all_grids, colour = "purple", size = 2, alpha = 0.2) +
  geom_sf(data = points, colour = "black", size = 4) +
  geom_sf(data = net %>% activate("edges") %>% slice(path) %>% st_as_sf(),
          colour = "green", size = 3) +
  coord_sf(datum = "+proj=utm +zone=18 +datum=NAD83 +unit=m",
           xlim = c(-10000, 30000), ylim = c(3940000, 4001000))

points <- data.frame(
  east = c(3960447,3960606),
  north = c(-8684.72, 24512.78)
)

points = st_as_sf(points, coords = c("north","east"), remove = FALSE, 
                  crs = "+proj=utm +zone=18 +datum=NAD83 +unit=m")


net <- as_sfnetwork(all_grids, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())
path <- st_network_paths(
 x = net, 
 from = points[1,],
 to = points[2,],
 weights = "weight") %>%
  pull(edge_paths) %>%
  unlist()
