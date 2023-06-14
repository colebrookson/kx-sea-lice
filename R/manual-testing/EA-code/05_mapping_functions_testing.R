library(dplyr)
library(magrittr)
library(sf)
library(here)
library(ggplot2)
library(sfnetworks)
library(parallel)
library(sp)
library(targets)
library(qs)

source(here("./R/00_functions_global.R"))
source(here("./R/04_functions_mapping.R"))

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


sr_pop_data = tar_read(clean_pink_spawner_recruit_data)
sr_pop_sites = get_data_csv(tar_read(sr_pop_sites))
large_land = readRDS(tar_read(large_land))
farm_data = tar_read(clean_farm_lice_data)
farm_locs = tar_read(clean_farm_locs)
network = qread(tar_read(network))
all_edges_nodes = readRDS(tar_read(all_nodes_edges_to_keep))




# random testing ===============================================================

network <- qread(here("./outputs/geo-objs/fresh/network.qs"))
utm_geo_data <- qread(here("./outputs/geo-objs/fresh/utm-geo-data.qs"))
large_land <- readRDS(here("./outputs/geo-objs/fresh/large-land-for-plotting.rds"))

all_grids_cropped <- qread(here("./outputs/geo-objs/fresh/grid-cropped.qs"))

edges_nodes_to_keep_lime <- readRDS(here::here("./outputs/geo-objs/fresh/edges-nodes-to-keep-lime.rds"))
edges_nodes_to_keep_sheep <- readRDS(here::here("./outputs/geo-objs/fresh/edges-nodes-to-keep-sheep.rds"))
edges_nodes_to_keep_kid <- readRDS(here::here("./outputs/geo-objs/fresh/edges-nodes-to-keep-kid.rds"))
edges_nodes_to_keep_goat <- readRDS(here::here("./outputs/geo-objs/fresh/edges-nodes-to-keep-goat.rds"))
edges_nodes_to_keep_loch <- readRDS(here::here("./outputs/geo-objs/fresh/edges-nodes-to-keep-loch.rds"))
edges_nodes_to_keep_jackson <- readRDS(here::here("./outputs/geo-objs/fresh/edges-nodes-to-keep-jackson.rds"))
edges_nodes_to_keep_alex <- readRDS(here::here("./outputs/geo-objs/fresh/edges-nodes-to-keep-alex.rds"))
edges_nodes_to_keep_cougar <- readRDS(here::here("./outputs/geo-objs/fresh/edges-nodes-to-keep-cougar.rds"))
#utm_geo_data <- readRDS(here("./outputs/geo-objs/fresh/utm-geo-data.rds"))

nodes_to_keep_lime <- edges_nodes_to_keep_lime$nodes
nodes_to_keep_sheep <- edges_nodes_to_keep_sheep$nodes
nodes_to_keep_kid <- edges_nodes_to_keep_kid$nodes
nodes_to_keep_goat <- edges_nodes_to_keep_goat$nodes
nodes_to_keep_loch <- edges_nodes_to_keep_loch$nodes
nodes_to_keep_jackson <- edges_nodes_to_keep_jackson$nodes
nodes_to_keep_alex <- edges_nodes_to_keep_alex$nodes
nodes_to_keep_cougar <- edges_nodes_to_keep_cougar$nodes

all_edges_nodes <- list(
  "Lime Point" = nodes_to_keep_lime,
  "Sheep Passage" = nodes_to_keep_sheep,
  "Kid Bay" = nodes_to_keep_kid,
  "Goat Cove" = nodes_to_keep_goat,
  "Lochalsh" = nodes_to_keep_loch,
  "Jackson Pass" = nodes_to_keep_jackson,
  "Alexander Inlet" = nodes_to_keep_alex,
  "Cougar Bay" = nodes_to_keep_cougar
)
saveRDS(all_edges_nodes, here("./outputs/geo-objs/fresh/all-edges-nodes-to-keep.rds"))
all_edges_nodes = all_nodes_edges_to_keep

ggplot() + 
  geom_sf(data = utm_geo_data, color = 'black', fill = "grey99") + 
  geom_sf(data = network %>%
            activate("nodes") %>%
            slice(nodes_to_keep_goat) %>% 
            st_as_sf(), fill = "lightpink", colour = "lightpink") +
  #geom_sf(data = utm_land_data, fill = "grey50") +
  geom_sf(data = goat,
          shape = 21, fill = "purple1", colour = "black", size = 2.5) +
  theme_base() +
  labs(title = "Goat Cove")

kid <- farms_utm[which(farms_utm$site == "Kid Bay"),]
loch <- farms_utm[which(farms_utm$site == "Lochalsh"),]
goat <- farms_utm[which(farms_utm$site == "Goat Cove"),]
sheep <- farms_utm[which(farms_utm$site == "Sheep Passage"),]
lime <- farms_utm[which(farms_utm$site == "Lime Point"),]
jackson <- farms_utm[which(farms_utm$site == "Jackson Pass"),]
cougar <- farms_utm[which(farms_utm$site == "Cougar Bay"),]
alex <- farms_utm[which(farms_utm$site == "Alexander Inlet"),]


