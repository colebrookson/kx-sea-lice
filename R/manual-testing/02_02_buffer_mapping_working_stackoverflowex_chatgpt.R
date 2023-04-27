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

nodes <- paths %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()

edges <- paths %>%
  slice(c(1, 701)) %>%
  pull(edge_paths) %>%
  unlist()

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
                 slice(edges) %>% 
                 st_as_sf()
                 ) +
    geom_sf(data = net %>%
              activate("nodes") %>%  
              slice(495) %>% 
              st_as_sf(), size = 3.5, fill = "orange", colour = "black", shape = 21) +
    geom_sf(data = net %>%
              activate("nodes") %>%  
              slice(c(1,701)) %>% 
              st_as_sf(), size = 3.5, fill = "red", colour = "black", shape = 21) +
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