orig_func <- function(net, edges, nodes = NULL) {
  # initialize empty vector
  all_edges <- as.numeric()
  
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

microbenchmark::microbenchmark(
  len = {
    # check what the temporary path length is
    temp_len <- net %>% 
      activate("edges") %>% 
      slice(edges_all[[11]]) %>% 
      st_as_sf() %>% 
      st_combine() %>% 
      st_length()
  },
  return_part = {
    if(temp_len < units::set_units(30000, m)) {
      # if the length of the current path is long enough, add it to all_edges
      all_edges <- c(all_edges, edges_all[[11]])
    }
  },
  times = 1000
)

## faster version 
fast_func <- function(net, edges, nodes = NULL) {
  
  # initialize empty vector
  all_edges <- as.numeric()
  # initialize empty vector for nodes if applicable 
  if(!is.null(nodes)) {
    all_nodes <- as.numeric()
  }
  
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
}

slice_fun <- function(net, edges) {
  # check what the temporary path length is
  temp_len <- net %>% 
    activate("edges") %>% 
    slice(edges) %>% 
    st_as_sf() %>% 
    st_combine() %>% 
    st_length()
  
  # if the temporary length is long enough, add the edges of that path to 
  # the total edges
  if(temp_len < units::set_units(1000, m)) {
    # if the length of the current path is long enough, add it to all_edges
    return(edges)
  } else {
    return(NA)
  }
}
# use the roxel example ========================================================
library(sfnetworks)
library(microbenchmark)

# Load the roxel dataset
# Calculate shortest paths from a starting node to multiple end nodes
net = as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

paths = st_network_paths(net, from = 495, weights = "weight")
paths

nodes_all <- paths %>%
  pull(node_paths) 

edges_all <- paths %>%
  pull(edge_paths) 

net %>% 
  activate("edges") %>% 
  slice(edges_all[[701]]) %>% 
  st_as_sf() %>% 
  st_combine() %>% 
  st_length()

test_edges <- edges_all[c(1:10, 701)]

edges <- lapply(test_edges, slice_fun, net = net)


microbenchmark::microbenchmark(
  apply = {
    lapply(test_edges, slice_fun, net = net)
  },
  loop = {
    orig_func(net, test_edges)
  },
  times = 100
)

## put the apply within the function and use snow ==============================

library(sfnetworks)
library(parallel)
library(tidyverse)

# Load the roxel dataset
# Calculate shortest paths from a starting node to multiple end nodes
net = as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

paths = st_network_paths(net, from = 495, weights = "weight")

nodes_all <- paths %>%
  pull(node_paths) 

edges_all <- paths %>%
  pull(edge_paths) 

test_edges <- edges_all[c(1:50)]

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
slice_fun(net, edges_all[[1]])
sapply(test_edges, slice_fun, net = net)

# start the cluster

start_time <- Sys.time()
cl <- parallel::makeCluster(8)

parallel::clusterEvalQ(cl, {library(dplyr); library(sfnetworks); 
  library(magrittr); library(sf)})
parallel::clusterExport(cl, varlist = c("edges_all", "net"))

result <- parSapply(cl, edges_all, slice_fun, net = net)

parallel::stopCluster(cl)
end_time <- Sys.time()
end_time - start_time

indices_keep <- which(result > 1000)
keep_edges <- unique(edges_all[indices_keep] %>% unlist())

# we can get the nodes if we want too
nodes_to_keep_all <- nodes_all[indices_keep]
nodes_to_keep <- unlist(lapply(nodes_to_keep_all, tail, n = 1L) %>% unlist())

ggplot() + 
  geom_sf(data = net %>%
            activate("edges") %>%  
            st_as_sf(), colour = "grey90") +
  geom_sf(data = net %>%
            activate("nodes") %>%  
            st_as_sf(), colour = "grey90")  + 
  geom_sf(data = net %>%
            activate("edges") %>%
            slice(keep_edges) %>% 
            st_as_sf()
  ) +
  geom_sf(data = net %>%
            activate("nodes") %>%  
            slice(495) %>% 
            st_as_sf(), size = 3.5, fill = "orange", colour = "black", shape = 21) +
  geom_sf(data = net %>%
            activate("nodes") %>%
            slice(nodes_to_keep) %>%
            st_as_sf(), size = 2, fill = "red", colour = "black", shape = 21,
          alpha = 0.4) +
  theme_void()

