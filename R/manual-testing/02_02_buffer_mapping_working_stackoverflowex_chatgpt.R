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

start_node <- 495
short_paths_sf <- st_network_paths(
  net %>% activate(nodes), 
  from = 495, 
  to = c(1:701), 
  weights = "weight") 
shortest_paths <- shortest_paths(
  net, from = 495, to = c(1:701), output = "both")

# Extract the edges and nodes from the shortest paths
shortest_path_edges <- unique(unlist(lapply(short_paths_sf, `[[`, "vpath")))
shortest_path_nodes <- unique(unlist(lapply(shortest_paths, `[[`, "node")))

# Create the subgraph
subgraph <- subset(mat, nodes = shortest_path_nodes, edges = shortest_path_edges)

# Plot the subgraph
plot(subgraph)
