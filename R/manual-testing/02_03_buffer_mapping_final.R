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
geo_data_bc <- geo_data_sf[which(geo_data$NAME_1 == "British Columbia"),]

# make into sf object
geo_data_sf_bc <- st_as_sf(geo_data_bc)

# filter to just BC 
bc_wgs <- sf::st_crop(geo_data_sf, xmin = -129.5,
                      xmax = -127.75, ymin = 52, 
                      ymax = 54)

bc_utm <- st_transform(geo_data_sf_bc, 
                       crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
geo_data_sf_bc_cropped <- sf::st_crop(bc_utm, xmin = 450000,
                                      xmax = 600000, ymin = 5750000, 
                                      ymax = 6000000)

ggplot() + 
  geom_sf(data = geo_data_sf_bc_cropped) +
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")


# 54 = 581934.93
# -127.5 = 5984244.79
# 52 = 465674.83
# -129.5 = 5761156.24

# and make a bounding box of the whole region
bb <- sf::st_make_grid(sf::st_bbox(geo_data_sf_bc_cropped), n = 1)

# now since we have a polygon of BC, we cant a polygon of the things that 
# are the waterways, so use the st_differnce of the bounding box and that 
# geom object and we're good 
non_land <- sf::st_difference(bb, geo_data_sf_bc_cropped)

non_land_for_plot <- non_land %>% 
  st_cast("MULTIPOLYGON")

# all analysis one study region ================================================

# crop to just the study region
non_land_study <- sf::st_crop(non_land, xmin = -128.85,
                              xmax = -128.12, ymin = 52.25, 
                              ymax = 52.95)

# get the land here
bb_non_land <- sf::st_make_grid(sf::st_bbox(non_land_study))
bb_non_land_utm <- st_transform(bb_non_land, 
                                crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

## NOTE: for some reason, in this data, there's an issue just north of the ymax
# which means that the cutoff here has to be this value, any further and it
# for some reason breaks. In this case study that's not a problem, but note
# that it might not be COMPLETELY accurate in the northern direction

# make sure the projection is the same (UTM)
utm_geo_data <- st_transform(non_land_study, 
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
saveRDS(utm_geo_data, here("./outputs/geo-objs/utm-geo-data.rds"))

# crop the land so we can plot that separately
land_study <- sf::st_difference(bb_non_land_utm, utm_geo_data) %>% 
  st_cast("MULTIPOLYGON")
utm_land_data <- st_transform(land_study, 
                              crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
saveRDS(utm_land_data, here("./outputs/geo-objs/utm-land-data.rds"))

ggplot() + 
  geom_sf(non_land) + 
  geom_sf(bb_non_land)


## NOTE: also, I'm going to make a much bigger one of this for the year-by-year
# plotting which needs a bigger area to show the populations of salmon too
non_land_larger <- sf::st_crop(non_land, xmin = 450000,
                               xmax = 600000, ymin = 5780000, 
                               ymax = 6000000)
utm_non_geo_land_larger <- st_transform(non_land_larger, 
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
# to deal with the weird lat/long thing for now, we'll just make four land 
# areas and plot all four


non_land_larger_se <- sf::st_crop(
  non_land, 
  xmin = -129, xmax = -127.75, ymin = 52,  ymax = 53)
utm_non_land_larger_se <- st_transform(non_land_larger_se, 
                                    crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

bb_non_land_larger_se <- st_transform(
  sf::st_make_grid(sf::st_bbox(
    non_land_larger_se
  )),
  crs="+proj=utm +zone=9 +datum=NAD83 +unit=m"
)
bb_non_land_larger_sw <- sf::st_transform(
  sf::st_make_grid(sf::st_bbox(
    sf::st_crop(non_land_larger, 
                xmin = -129.5, xmax = -129, ymin = 52,  ymax = 53)
  )),
  st_transform
)
bb_non_land_larger_ne <- sf::st_transform(
  sf::st_make_grid(sf::st_bbox(
  sf::st_crop(non_land_larger, 
              xmin = -129, xmax = -127.75, ymin = 53,  ymax = 54)
  )),
  crs="+proj=utm +zone=9 +datum=NAD83 +unit=m"
)
bb_non_land_larger_nw <- sf::st_transform(
  sf::st_make_grid(sf::st_bbox(
    sf::st_crop(non_land_larger, 
                xmin = -129.5, xmax = -129, ymin = 53,  ymax = 54)
  )),
  crs="+proj=utm +zone=9 +datum=NAD83 +unit=m"
)

saveRDS(utm_geo_data_large, here("./outputs/geo-objs/utm-geo-data-large.rds"))

land_larger_se <- sf::st_difference(
  bb_non_land_larger_se, utm_non_land_larger_se) %>% 
  st_cast("MULTIPOLYGON")
land_larger_sw <- sf::st_difference(
  bb_non_land_larger_sw, utm_geo_data_large) %>% 
  st_cast("MULTIPOLYGON")
land_larger_ne <- sf::st_difference(
  bb_non_land_larger_ne, utm_geo_data_large) %>% 
  st_cast("MULTIPOLYGON")
land_larger_ne <- sf::st_difference(
  bb_non_land_larger_ne, utm_geo_data_large) %>% 
  st_cast("MULTIPOLYGON")







utm_land_data_larger <- st_transform(land_larger, 
                              crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
saveRDS(utm_land_data_larger, 
        here("./outputs/geo-objs/utm-land-larger-data.rds"))

# quick sanity check for what we're looking at 
ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey90") +
  theme_base()

# now, we can do this one farm region at a time, or all at once, I think it's 
# actually computationally faster to do all at once
grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_geo_data)),
  # the size is really large to make a fine grid
  size = 205000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 
saveRDS(grid_sample, 
        here("./outputs/geo-objs/all-area-grid-sample.rds"))

grid_cropped <- grid_sample[sf::st_contains(
  utm_geo_data, grid_sample, sparse = F)]

network <- as_sfnetwork(grid_cropped, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())
saveRDS(network, 
        here("./outputs/geo-objs/all-area-network.rds"))

ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey90") +
  geom_sf(data = network %>% activate("edges") %>% st_as_sf()) +
  theme_base() 

## need to do a separate one for the west ======================================
west <- sf::st_crop(non_land, xmin = -128.85,
                    xmax = -128.3, ymin = 52.42, 
                    ymax = 52.85)

# make sure the projection is the same (UTM)
utm_west_area <- st_transform(west, 
                              crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# quick sanity check for what we're looking at 
# ggplot() + 
#   geom_sf(data = utm_west_area, color = 'black', fill = "grey90") +
#   geom_sf(data = west_network %>% 
#             activate("edges") %>% 
#             st_as_sf()) +
#   theme_base() 

west_grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_west_area)),
  # the size is really large to make a fine grid
  size = 200000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 

west_grid_cropped <- west_grid_sample[sf::st_contains(
  utm_west_area, west_grid_sample, sparse = F)]

west_network <- as_sfnetwork(west_grid_cropped, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())
saveRDS(west_network, 
        here("./outputs/geo-objs/west-area-network.rds"))
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
cl <- parallel::makeCluster(18)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all_kid", "network"))

kid_short_edges <- parSapply(cl, edges_all_kid, slice_fun, 
                             net = network)
saveRDS(kid_short_edges, 
        here("./outputs/geo-objs/kid-whole-region-short-edges.rds"))
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
        here("./outputs/geo-objs/edges-nodes-to-keep-kid.rds"))

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
saveRDS(loch_short_edges, 
        here("./outputs/geo-objs/loch-whole-region-short-edges.rds"))
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
        here("./outputs/geo-objs/edges-nodes-to-keep-loch.rds"))

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
saveRDS(goat_short_edges, 
        here("./outputs/geo-objs/goat-whole-region-short-edges.rds"))
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
        here("./outputs/geo-objs/edges-nodes-to-keep-goat.rds"))

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
saveRDS(sheep_short_edges, 
        here("./outputs/geo-objs/sheep-whole-region-short-edges.rds"))
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
        here("./outputs/geo-objs/edges-nodes-to-keep-sheep.rds"))

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
saveRDS(lime_short_edges, 
        here("./outputs/geo-objs/lime-whole-region-short-edges.rds"))
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
        here("./outputs/geo-objs/edges-nodes-to-keep-lime.rds"))

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
saveRDS(jackson_short_edges, 
        here("./outputs/geo-objs/jackson-whole-region-short-edges.rds"))
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
        here("./outputs/geo-objs/edges-nodes-to-keep-jackson.rds"))

## cougar paths ===============================================================
cougar_paths <- sfnetworks::st_network_paths(
  x = west_network,
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
saveRDS(cougar_short_edges, 
        here("./outputs/geo-objs/cougar-whole-region-short-edges.rds"))
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
        here("./outputs/geo-objs/edges-nodes-to-keep-cougar.rds"))

## alex paths ===============================================================
alex_paths <- sfnetworks::st_network_paths(
  x = west_network,
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
saveRDS(alex_short_edges, 
        here("./outputs/geo-objs/alex-whole-region-short-edges.rds"))
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
        here("./outputs/geo-objs/edges-nodes-to-keep-alex.rds"))

edges_nodes_to_keep_lime <- readRDS(here::here("./outputs/geo-objs/edges-nodes-to-keep-lime.rds"))
edges_nodes_to_keep_sheep <- readRDS(here::here("./outputs/geo-objs/edges-nodes-to-keep-sheep.rds"))
edges_nodes_to_keep_kid <- readRDS(here::here("./outputs/geo-objs/edges-nodes-to-keep-kid.rds"))
edges_nodes_to_keep_goat <- readRDS(here::here("./outputs/geo-objs/edges-nodes-to-keep-goat.rds"))
edges_nodes_to_keep_loch <- readRDS(here::here("./outputs/geo-objs/edges-nodes-to-keep-loch.rds"))
edges_nodes_to_keep_jackson <- readRDS(here::here("./outputs/geo-objs/edges-nodes-to-keep-jackson.rds"))
edges_nodes_to_keep_alex <- readRDS(here::here("./outputs/geo-objs/edges-nodes-to-keep-alex.rds"))
edges_nodes_to_keep_cougar <- readRDS(here::here("./outputs/geo-objs/edges-nodes-to-keep-cougar.rds"))

all_nodes_edges_to_keep <- list(
  "Lime Point" = edges_nodes_to_keep_lime,
  "Sheep Passage" = edges_nodes_to_keep_sheep,
  "Kid Bay" = edges_nodes_to_keep_kid,
  "Goat Cove" = edges_nodes_to_keep_goat,
  "Lochalsh" = edges_nodes_to_keep_loch,
  "Jackson Pass" = edges_nodes_to_keep_jackson,
  "Alexander Inlet" = edges_nodes_to_keep_alex,
  "Cougar Bay" = edges_nodes_to_keep_cougar
)
saveRDS(all_nodes_edges_to_keep, here("./outputs/geo-objs/all-edges-nodes-to-keep.rds"))

# plot from just the one study region ==========================================
alex_buffer <- ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99") + 
  geom_sf(data = west_network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_alex) %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = alex,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() + 
  labs(title = "Alexander Inlet")
ggsave(
  here("./figs/maps/temp/alex-buffer.png"),
  alex_buffer
)

cougar_buffer <- ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99") + 
  geom_sf(data = west_network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_cougar) %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = cougar,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() + 
  labs(title = "Cougar Bay")
ggsave(
  here("./figs/maps/temp/cougar-buffer.png"),
  cougar_buffer
)

jackson_buffer <- ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99") + 
  geom_sf(data = network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_jackson) %>% 
            st_as_sf(),fill = "lightpink", colour = "lightpink") +
  geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = jackson,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() +
  labs(title = "Jackson Pass")
ggsave(
  here("./figs/maps/temp/jackson-buffer.png"),
  jackson_buffer
)

lime_buffer <- ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99") + 
  geom_sf(data = network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_lime) %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = lime,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() +
  labs(title = "Lime Point")
ggsave(
  here("./figs/maps/temp/lime-buffer.png"),
  lime_buffer
)

sheep_buffer <- ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99") + 
  geom_sf(data = network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_sheep) %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = sheep,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() +
  labs(title = "Sheep Passage")
ggsave(
  here("./figs/maps/temp/sheep-buffer.png"),
  sheep_buffer
)

goat_buffer <- ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99") + 
  geom_sf(data = network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_goat) %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = goat,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() +
  labs(title = "Goat Cove")
ggsave(
  here("./figs/maps/temp/goat-buffer.png"),
  goat_buffer
)

loch_buffer <- ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99") + 
  geom_sf(data = network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_loch) %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = loch,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() +
  labs(title = "Lochalsh")
ggsave(
  here("./figs/maps/temp/loch-buffer.png"),
  loch_buffer
)

kid_buffer <- ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99", alpha = 1) + 
  geom_sf(data = network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_kid) %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = kid,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() +
  labs(title = "Kid Bay")

ggsave(
  here("./figs/maps/temp/kid-buffer.png"),
  kid_buffer
)

## attempting to plot with boundary not the points =============================
jack_polygon <- network %>% 
  activate("nodes") %>% 
  slice(nodes_to_keep_jackson) %>% 
  st_as_sf() %>% 
  st_cast(to = "MULTIPOINT") %>% 
  summarize(geometry = st_combine(x)) %>% 
  st_cast("POLYGON") 

test <- st_filter(jack_polygon, utm_geo_data)

ggplot() +   
  theme_base() +
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99", alpha = 1) + 
  geom_sf(data = jack_polygon, fill = "lightpink", colour = "lightpink") + 
  geom_sf(data = utm_land_data, fill = "grey50") + 
  theme(panel.grid.major = element_line(colour = "transparent"))+ 
  # geom_sf(data = network %>%
  #           activate("nodes") %>%
  #           slice(nodes_to_keep_jackson) %>% 
  #           st_as_sf(), colour = "purple4", fill = "grey80", 
  #         shape = 21, alpha = 0.1) +
  geom_sf(data = jackson,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() +
  labs(title = "Jackson Pass")
ggsave(
  here("./figs/maps/temp/jackson-buffer.png"),
  jackson_buffer
)

# set up testing for incorporation into targets=================================
library(targets)
sr_pop_data = tar_read(clean_pink_spawner_recruit_data)
sr_pop_sites = get_data_csv(tar_read(sr_pop_sites))
geo_data = readRDS(tar_read(geo_spatial))
farm_data = tar_read(clean_farm_lice_data)
farm_locs = tar_read(clean_farm_locs)
fig_output = here::here("./figs/maps/yearly-pop-maps/")
data_output = here::here("./data/spawner-recruit/clean/")
















