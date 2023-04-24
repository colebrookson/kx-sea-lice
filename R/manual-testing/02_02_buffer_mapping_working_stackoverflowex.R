# Stack overflow example =======================================================
library(tidyverse)
library(sf)
library(sfnetworks)
library(nngeo)

# set seed to make the script reproducible,
#  since there is random sampling used
set.seed(813)

# Getting your data:
x <- dget("https://raw.githubusercontent.com/BajczA475/random-data/main/Moose.lake")
# Subset to get just one lake
moose_lake <- x[5,]
boat_ramp <- dget("https://raw.githubusercontent.com/BajczA475/random-data/main/Moose.access")
sample_locations <- dget("https://raw.githubusercontent.com/BajczA475/random-data/main/Moose.ssw")
sample_bbox <- dget("https://raw.githubusercontent.com/BajczA475/random-data/main/Moose.box")

# sample the bounding box with regular square points, then connect each point to the closest 9 points
#  8 should've worked, but left some diagonals out.
sq_grid_sample <- st_sample(st_as_sfc(st_bbox(moose_lake)), 
                            size = 10000, type = 'regular') %>% st_as_sf() %>%
  st_connect(.,.,k = 9)

# remove connections that are not within the lake polygon
sq_grid_cropped <- sq_grid_sample[st_within(sq_grid_sample, moose_lake, sparse = F)]

# make an sfnetwork of the cropped grid
# make an sfnetwork of the cropped grid
lake_network <- sq_grid_cropped %>% as_sfnetwork()

# find the (approximate) distance from boat ramp to point 170 (far north)
pt170 <- st_network_paths(lake_network, 
                          from = boat_ramp,
                          to = sample_locations[170,]) %>%
  pull(edge_paths) %>%
  unlist()

lake_network %>% 
  activate(edges) %>%
  slice(pt170) %>%
  st_as_sf() %>%
  st_combine() %>%
  st_length()




ggplot() + 
  geom_sf(data = sq_grid_sample, alpha = .05) +
  geom_sf(data = sq_grid_cropped, color = 'dodgerblue') + 
  geom_sf(data = moose_lake, color = 'blue', fill = NA) +
  geom_sf(data = boat_ramp, color = 'springgreen', size = 4) + 
  geom_sf(data = sample_locations[170,], color = 'deeppink1', size = 4) +
  geom_sf(data = lake_network %>% 
            activate(edges) %>%
            slice(pt170) %>%
            st_as_sf(),
          color = 'turquoise',
          size = 2) +
  theme_void()

