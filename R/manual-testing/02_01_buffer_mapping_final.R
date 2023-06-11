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

# do the network analysis ======================================================

## first, the main network =====================================================

# make the largest version of the grid
grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(utm_geo_data)),
  # the size is really large to make a fine grid
  size = 100000, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 

# subset grid
grid_cropped <- grid_sample[sf::st_contains(
  utm_geo_data, grid_sample, sparse = F)]

ggplot() + 
  geom_sf(data = grid_cropped) + # looks like 20,000 is enough? 
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

# so looking at this plot, we need two joiners, one for the east-west corridor
# and one for the north south corridor

#### north-south ===============================================================

geo_data_w_ns <- st_crop(utm_geo_data, 
                        xmin = 516000,
                        xmax = 516800,
                        ymin = 5830000,
                        ymax = 5831000)

ggplot() + 
  geom_sf(data = geo_data_w) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(516000, 516800), ylim = c(5830000, 5831000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") 

# make two grids, one north one south, connect the major network to them, then
# connect to each other
geo_data_w_ns_n <- st_crop(utm_geo_data, 
                         xmin = 516200,
                         xmax = 516500,
                         ymin = 5830400,
                         ymax = 5830600)
geo_data_w_ns_s <- st_crop(utm_geo_data, 
                           xmin = 516200,
                           xmax = 516400,
                           ymin = 5830200,
                           ymax = 5830400)

# north version
grid_w_ns_n <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_w_ns_n)),
  # the size is really large to make a fine grid
  size = 10, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)
grid_w_ns_n_cropped <- grid_w_ns_n[sf::st_contains(
  geo_data_w_ns_n, grid_w_ns_n, sparse = F)]

# south version
grid_w_ns_s <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_w_ns_s)),
  # the size is really large to make a fine grid
  size = 10, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)
grid_w_ns_s_cropped <- grid_w_ns_s[sf::st_contains(
  geo_data_w_ns_s, grid_w_ns_s, sparse = F)]

# connect the larger grids to the smaller grids and the smaller to each other
connect_to_ns_n <- nngeo::st_connect(st_combine(grid_cropped), 
                                     st_combine(grid_w_ns_n_cropped))
connect_to_ns_s <- nngeo::st_connect(st_combine(grid_cropped), 
                                     st_combine(grid_w_ns_s_cropped))
connect_ns_n_to_s <- nngeo::st_connect(st_combine(grid_w_ns_n_cropped), 
                                    st_combine(grid_w_ns_s_cropped))
# re-assign the grid cropped with all three grids, and the connectors
grid_cropped <- c(grid_cropped, grid_w_ns_n_cropped, grid_w_ns_s_cropped,
                  connect_to_ns_n, connect_to_ns_s, connect_ns_n_s)

ggplot() + 
  geom_sf(data = geo_data_w) + 
  geom_sf(data = grid_cropped) + 
  #geom_sf(data = grid_w_ns_n_cropped, colour = "red") +
  #geom_sf(data = grid_w_ns_s_cropped, colour = "blue") +
  #geom_sf(data = connect_to_ns_n, colour = "yellow") +
  #geom_sf(data = connect_to_ns_s, colour = "yellow") +
  #geom_sf(data = connect_ns_n_to_s, colour = "yellow") +
  coord_sf(xlim = c(516000, 516800), ylim = c(5830000, 5831000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### east-west =================================================================

geo_data_w_eastwest <- st_crop(utm_geo_data, 
                            xmin = 524500,
                            xmax = 527500,
                            ymin = 5828000,
                            ymax = 5830000)
ggplot() + 
  geom_sf(data = geo_data_w_eastwest) + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

grid_w_eastwest <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_w_eastwest)),
  # the size is really large to make a fine grid
  size = 500, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)

grid_w_eastwest_cropped <- grid_w_eastwest[sf::st_contains(
  geo_data_w_eastwest, grid_w_eastwest, sparse = F)]

ggplot() + 
  geom_sf(data = grid_w_eastwest_cropped)

ggplot() + 
  geom_sf(data = grid_w_cropped) + 
  geom_sf(data = geo_data_w_eastwest) +
  geom_sf(data = grid_w_eastwest_cropped, colour = "red") + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### west north-south area =====================================================

geo_data_w_northsouth <- st_crop(utm_geo_data, 
                            xmin = 515600,
                            xmax = 518000,
                            ymin = 5830000,
                            ymax = 5834500)
ggplot() + 
  geom_sf(data = geo_data_w_northsouth) + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

grid_w_northsouth <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_w_northsouth)),
  # the size is really large to make a fine grid
  size = 500, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)

grid_w_northsouth_cropped <- grid_w_northsouth[sf::st_contains(
  geo_data_w_northsouth, grid_w_northsouth, sparse = F)]

ggplot() + 
  geom_sf(data = grid_w_cropped) + 
  geom_sf(data = geo_data_w_northsouth) +
  geom_sf(data = grid_w_northsouth_cropped, colour = "red") + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

### north area =================================================================

geo_data_n <- st_crop(utm_geo_data, 
                      xmin = 540000,
                      xmax = 560000,
                      ymin = 5840000,
                      ymax = 5860000)
ggplot() + 
  geom_sf(data = geo_data_n) + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

grid_n <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_n)),
  # the size is really large to make a fine grid
  size = 1500, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)

grid_n_cropped <- grid_n[sf::st_contains(
  geo_data_n, grid_n, sparse = F)]

ggplot() + 
  #geom_sf(data = grid_cropped) +
  geom_sf(data = geo_data_n) +
  geom_sf(data = grid_n_cropped, colour = "red") + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### west north-east area ======================================================

geo_data_n_northeast <- st_crop(utm_geo_data, 
                               xmin = 557000,
                               xmax = 558000,
                               ymin = 5854000,
                               ymax = 5856000)
ggplot() + 
  geom_sf(data = geo_data_n_northeast) + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

grid_n_northeast <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_n_northeast)),
  # the size is really large to make a fine grid
  size = 100, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)

grid_n_northeast_cropped <- grid_n_northeast[sf::st_contains(
  geo_data_n_northeast, grid_n_northeast, sparse = F)]

ggplot() + 
  geom_sf(data = grid_n_cropped) + 
  geom_sf(data = geo_data_n_northeast) +
  geom_sf(data = grid_n_northeast_cropped, colour = "red") + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

### central area ===============================================================

geo_data_c <- st_crop(utm_geo_data, 
                      xmin = 537500,
                      xmax = 549000,
                      ymin = 5816000,
                      ymax = 5823000)
ggplot() + 
  geom_sf(data = geo_data_c) + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

grid_c <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_c)),
  # the size is really large to make a fine grid
  size = 2500, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)

grid_c_cropped <- grid_c[sf::st_contains(
  geo_data_c, grid_c, sparse = F)]

ggplot() + 
  #geom_sf(data = grid_cropped) +
  geom_sf(data = geo_data_c) +
  geom_sf(data = grid_c_cropped, colour = "red") + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")


#### central small area ========================================================
geo_data_c <- st_crop(utm_geo_data, 
                      xmin = 546000,
                      xmax = 548000,
                      ymin = 5819000,
                      ymax = 5820000)
ggplot() + 
  geom_sf(data = geo_data_c) + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

grid_c <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_c)),
  # the size is really large to make a fine grid
  size = 500, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)

grid_c_cropped <- grid_c[sf::st_contains(
  geo_data_c, grid_c, sparse = F)]

ggplot() + 
  geom_sf(data = grid_cropped) +
  geom_sf(data = geo_data_c) +
  geom_sf(data = grid_c_cropped, colour = "red") + 
  coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

## join all the networks together from west to east ============================

### west area joins ============================================================

ggplot() + 
  geom_sf(data = grid_cropped) + 
  geom_sf(data = grid_w_cropped, colour = "red") + 
  geom_sf(data = grid_w_eastwest_cropped, colour = "purple") + 
  geom_sf(data = grid_w_northsouth_cropped, colour = "blue") 

# so looking at this we can tell we need to join the main grid to the west 
# grid from the 

