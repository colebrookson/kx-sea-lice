library(dplyr)
library(magrittr)
library(sf)
library(here)
library(ggplot2)
library(sfnetworks)
library(parallel)
library(sp)
library(targets)
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
utm_geo_data = readRDS(tar_read(utm_geo_data))
utm_land_data = readRDS(tar_read(utm_land_data))
farm_data = tar_read(clean_farm_lice_data)
farm_locs = tar_read(clean_farm_locs)
network = readRDS(tar_read(all_region_network))
all_edges_nodes = readRDS(tar_read(all_nodes_edges_to_keep))

network <- readRDS(here("./outputs/geo-objs/test/network.rds"))
west_network <- readRDS(here("./outputs/geo-objs/test/west-area-network.rds"))


edges_nodes_to_keep_lime <- readRDS(here::here("./outputs/geo-objs/test/edges-nodes-to-keep-lime.rds"))
edges_nodes_to_keep_sheep <- readRDS(here::here("./outputs/geo-objs/test/edges-nodes-to-keep-sheep.rds"))
edges_nodes_to_keep_kid <- readRDS(here::here("./outputs/geo-objs/test/edges-nodes-to-keep-kid.rds"))
edges_nodes_to_keep_goat <- readRDS(here::here("./outputs/geo-objs/test/edges-nodes-to-keep-goat.rds"))
edges_nodes_to_keep_loch <- readRDS(here::here("./outputs/geo-objs/test/edges-nodes-to-keep-loch.rds"))
edges_nodes_to_keep_jackson <- readRDS(here::here("./outputs/geo-objs/test/edges-nodes-to-keep-jackson.rds"))
edges_nodes_to_keep_alex <- readRDS(here::here("./outputs/geo-objs/test/edges-nodes-to-keep-alex.rds"))
edges_nodes_to_keep_cougar <- readRDS(here::here("./outputs/geo-objs/test/edges-nodes-to-keep-cougar.rds"))

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
all_edges_nodes = all_nodes_edges_to_keep
