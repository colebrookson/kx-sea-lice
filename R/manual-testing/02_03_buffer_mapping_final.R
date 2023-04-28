library(dplyr)
library(magrittr)
library(sf)
library(here)
library(ggplot2)
library(sfnetworks)

# define manual helper function ================================================
len_crit <- function(net, edges, nodes = NULL) {
  #' Look at whether or not each of the individual paths calcualted actually
  #' pass the required test
  #' 
  #' @description  Look at whether or not each of the individual paths 
  #' calculated actually pass the required test
  #' @param net sfnetwork. A network of sfnetwork
  #' @param slice_val integer. The value to slice the edges into 
  #' @param edges list. The list of the edges in the shortest path
  
  # initialize empty vector
  all_edges <- as.numeric()
  # initialize empty vector for nodes if applicable 
  if(!is.null(nodes)) {
    all_nodes <- as.numeric()
  }
  
  # get the length of the edges so we know a progress measure
  no_slices <- length(edges)
  # get 0.1 increments:
  edge_incs <- c(round(0.1*no_slices), round(0.2*no_slices), 
                 round(0.3*no_slices), round(0.4*no_slices),
                 round(0.5*no_slices), round(0.6*no_slices),
                 round(0.7*no_slices), round(0.8*no_slices),
                 round(0.9*no_slices), round(0.99*no_slices))
  edge_msgs <- c("10% ", "20% ", "30% ", "40% ", "50% ", "60% ", "70% ","80% ",
                 "90% ", "99% ")
  start_time <- Sys.time()
  # go through each of the slices (aka each of the paths)
  for(slice in 1:length(edges)) {
    
    # check what the temporary path length is
    temp_len <- net %>% 
      activate("edges") %>% 
      slice(edges[[slice]]) %>% 
      st_as_sf() %>% 
      st_combine() %>% 
      st_length()
    
    # if the temporary length is long enough, add the edges of that path to 
    # the total edges
    if(temp_len < units::set_units(30000, m)) {
      # if the length of the current path is long enough, add it to all_edges
      all_edges <- c(all_edges, edges[[slice]])
      
      # if we also want to plot the nodes, we can do so
      if(!is.null(nodes)) {
        # if the length is long enough, keep the LAST node in that set
        all_nodes <- c(all_nodes, nodes[[slice]][[length(nodes[[slice]])]]) 
      }
    }
    if(slice %in% edge_incs) {
      pos <- match(slice, edge_incs)
      curr_time <- Sys.time() - start_time
      print(paste0(edge_msgs[pos], "elapsed time: ", round(curr_time, 2), 
                   " minutes"))
    }
  }
  
  # if the nodes are selected return both that and the edges
  if(!is.null(nodes)) {
    # keep only the unique ones
    unique_nodes <- unique(all_nodes)
    # keep only the unique ones
    unique_edges <- unique(all_edges)
    # list up both
    nodes_edges <- list(
      nodes = unique_nodes,
      edges = unique_edges 
    )
    # return both
    return(nodes_edges)
  }
  
  # keep only the unique ones
  unique_edges <- unique(all_edges)
  
  return(unique_edges)
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
                              xmax = -127.8, ymin = 52.1, 
                              ymax = 53.2)

# make sure the projection is the same (UTM)
utm_geo_data <- st_transform(non_land_study, 
                             crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")

# quick sanity check for what we're looking at 
ggplot() + 
  geom_sf(data = utm_geo_data, color = 'blue', fill = "red") 

# now, we can do this one farm region at a time, or all at once, I think it's 
# actually computationally faster to do all at once

## make the grid sample on the whole study region ==============================

# first make it as small as possible
smallest_study_area <- sf::st_crop(non_land, xmin = -128.6,
                            xmax = -128.12, ymin = 52.5, 
                            ymax = 52.9)
grid_sample <- sf::st_sample(sf::st_as_sfc(sf::st_bbox(utm_kid_bay)), 
                                 # the size is really large to make a very fine grid
                                 size = 50000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 

























