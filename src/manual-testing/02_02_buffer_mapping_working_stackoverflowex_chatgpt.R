# answer from ChatGPT ==========================================================
library(sfnetworks)

# Load the roxel dataset
roxel <- as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035) %>%
  activate("edges") %>%
  mutate(weight = edge_length()) 

# Calculate shortest paths from a starting node to all other nodes
start_node <- roxel %>% activate("nodes") %>% filter(label == "a") %>% pull(id)
shortest_paths <- roxel %>% activate("edges") %>% shortest_paths(from = start_node, output = "sfnetwork")

# Get all edges and nodes involved in the shortest paths
all_shortest_path_edges <- shortest_paths$path_data$edge_id
all_shortest_path_nodes <- unique(c(shortest_paths$path_data$from_id, shortest_paths$path_data$to_id))

# Create subgraph
my_subgraph <- subset(roxel, edges %in% all_shortest_path_edges, nodes %in% all_shortest_path_nodes)

# Plot subgraph
plot(my_subgraph)

roxel_sfn <- as_sfnetwork(roxel, directed = TRUE)

# Define starting node and end nodes for shortest paths
start_node <- "a"
end_nodes <- c("d", "f")

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

get_length <- function(all_edges, )

net %>% 
  activate("edges") %>% 
  slice(edges[[1]]) %>% 
  st_as_sf() %>% 
  st_combine() %>% 
  st_length()

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
    if(temp_len > units::set_units(30000, m)) {
      # if the length of the current path is long enough, add it to all_edges
      all_edges <- c(all_edges, edges[[slice]])
      
      # if we also want to plot the nodes, we can do so
      if(!is.null(nodes)) {
        # if the length is long enough, keep the LAST node in that set
        all_nodes <- c(all_nodes, nodes[[slice]][[length(nodes[[slice]])]]) 
      }
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

short_edges <- len_crit(net = net, edges = edges_all, nodes = nodes_all)

plot_path = function(edges) {
  ggplot() + 
    geom_sf(data = net %>%
              activate("edges") %>%  
              st_as_sf(), colour = "grey90") +
    geom_sf(data = net %>%
              activate("nodes") %>%  
              st_as_sf(), colour = "grey90")  + 
    geom_sf(data = net %>%
                 activate("edges") %>%
                 slice(short_edges$edges) %>% 
                 st_as_sf()
                 ) +
    geom_sf(data = net %>%
              activate("nodes") %>%  
              slice(495) %>% 
              st_as_sf(), size = 3.5, fill = "orange", colour = "black", shape = 21) +
    geom_sf(data = net %>%
              activate("nodes") %>%  
              slice(short_edges$nodes) %>% 
              st_as_sf(), size = 2, fill = "red", colour = "black", shape = 21,
            alpha = 0.4) +
    theme_void()
}

colors = sf.colors(3, categorical = TRUE)

plot(net, col = "grey")
paths %>%
  pull(edge_paths) %>%
  purrr::walk(plot_path)
net %>%
  activate("nodes") %>%
  st_as_sf() %>%
  slice(c(495, 121, 458)) %>%
  plot(col = colors, pch = 8, cex = 2, lwd = 2, add = TRUE)