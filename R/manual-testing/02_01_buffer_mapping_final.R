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

# make grids ===================================================================

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
  geom_sf(data = grid_cropped) + # looks like 100,000 is enough? 
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
                        ymin = 5829000,
                        ymax = 5834500)

ggplot() + 
  geom_sf(data = geo_data_w) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(516000, 516800), ylim = c(5829000, 5834000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") 

# make two grids, one north one south, connect the major network to them, then
# connect to each other
geo_data_w_ns_n <- st_crop(utm_geo_data, 
                         xmin = 516500,
                         xmax = 516700,
                         ymin = 5833500,
                         ymax = 5834100)
geo_data_w_ns_s <- st_crop(utm_geo_data, 
                           xmin = 516200,
                           xmax = 516400,
                           ymin = 5829500,
                           ymax = 5830000)

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
                  connect_to_ns_n, connect_to_ns_s, connect_ns_n_to_s)

ggplot() + 
  geom_sf(data = geo_data_w) + 
  geom_sf(data = grid_cropped) + 
 #geom_sf(data = connect_to_ns_n, colour = "red") + 
 #geom_sf(data = connect_to_ns_s, colour = "red") + 
 #geom_sf(data = connect_ns_n_to_s, colour = "red") + 
 #geom_sf(data = grid_w_ns_n_cropped) + 
 #geom_sf(data = grid_w_ns_s_cropped) + 
  coord_sf(xlim = c(516000, 516800), ylim = c(5829000, 5834000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### east-west =================================================================

geo_data_w_eastwest <- st_crop(utm_geo_data, 
                            xmin = 524500,
                            xmax = 527500,
                            ymin = 5828000,
                            ymax = 5830000)
ggplot() + 
  geom_sf(data = geo_data_w_eastwest) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(524500, 527500), ylim = c(5828000, 5830000),
    datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

# make one grid in the west, one in the east
geo_data_w_ew_w <- st_crop(utm_geo_data, 
                           xmin = 525000,
                           xmax = 525500,
                           ymin = 5828500,
                           ymax = 5829000)
geo_data_w_ew_e <- st_crop(utm_geo_data, 
                           xmin = 526500,
                           xmax = 527000,
                           ymin = 5828200,
                           ymax = 5828500)

# west version
grid_w_ew_w <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_w_ew_w)),
  # the size is really large to make a fine grid
  size = 15, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)
grid_w_ew_w_cropped <- grid_w_ew_w[sf::st_contains(
  geo_data_w_ew_w, grid_w_ew_w, sparse = F)]

# east version
grid_w_ew_e <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_w_ew_e)),
  # the size is really large to make a fine grid
  size = 20, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)
grid_w_ew_e_cropped <- grid_w_ew_e[sf::st_contains(
  geo_data_w_ew_e, grid_w_ew_e, sparse = F)]

# connect the larger grids to the smaller grids and the smaller to each other
connect_to_ew_w <- nngeo::st_connect(st_combine(grid_cropped), 
                                     st_combine(grid_w_ew_w_cropped))
connect_to_ew_e <- nngeo::st_connect(st_combine(grid_cropped), 
                                     st_combine(grid_w_ew_e_cropped))
connect_to_ew_e_w <- nngeo::st_connect(st_combine(grid_w_ew_w_cropped), 
                                       st_combine(grid_w_ew_e_cropped))

# make all the grids one
grid_cropped <- c(grid_cropped, grid_w_ew_e_cropped, grid_w_ew_w_cropped,
                  connect_to_ew_e, connect_to_ew_w, connect_to_ew_e_w)

ggplot() + 
  geom_sf(data = geo_data_w_eastwest) + 
  geom_sf(data = grid_cropped) + 
  #geom_sf(data = connect_to_ew_w, colour = "red") +
  #geom_sf(data = connect_to_ew_e, colour = "red") +
  #geom_sf(data = connect_to_ew_e_w, colour = "red") +
  geom_sf(data = grid_w_ew_e_cropped, colour = "red") +
  geom_sf(data = grid_w_ew_w_cropped, colour = "red") +
  coord_sf(xlim = c(524500, 527500), ylim = c(5828000, 5830000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

### central area ===============================================================

geo_data_c <- st_crop(utm_geo_data, 
                      xmin = 537500,
                      xmax = 549000,
                      ymin = 5816000,
                      ymax = 5823000)
ggplot() + 
  geom_sf(data = geo_data_c) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(537500, 549000), ylim = c(5816000, 5823000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### central-south =============================================================

geo_data_c_s <- st_crop(utm_geo_data, 
                      xmin = 539500,
                      xmax = 541000,
                      ymin = 5817500,
                      ymax = 5818200)
ggplot() + 
  geom_sf(data = geo_data_c_s) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(539500, 541000), ylim = c(5817500, 5818200),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

geo_data_c_s_w <- st_crop(utm_geo_data, 
                           xmin = 539800,
                           xmax = 540000,
                           ymin = 5817900,
                           ymax = 5818000)
geo_data_c_s_e <- st_crop(utm_geo_data, 
                           xmin = 540200,
                           xmax = 540400,
                           ymin = 5817700,
                           ymax = 5817800)

# west area
grid_c_s_w <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_c_s_w)),
  size = 15, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)
grid_c_s_w_cropped <- grid_c_s_w[sf::st_contains(
  geo_data_c_s_w, grid_c_s_w, sparse = F)]

# east area 
grid_c_s_e <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_c_s_e)),
  size = 15, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)
grid_c_s_e_cropped <- grid_c_s_e[sf::st_contains(
  geo_data_c_s_e, grid_c_s_e, sparse = F)]

# connect grids
connect_to_cs_w <- nngeo::st_connect(st_combine(grid_cropped), 
                                     st_combine(grid_c_s_w_cropped))
connect_to_cs_e <- nngeo::st_connect(st_combine(grid_cropped), 
                                     st_combine(grid_c_s_e_cropped))
connect_to_cs_w_e <- nngeo::st_connect(st_combine(grid_c_s_w_cropped), 
                                     st_combine(grid_c_s_e_cropped))
grid_cropped <- c(grid_cropped, grid_c_s_w_cropped, grid_c_s_e_cropped,
                  connect_to_cs_w, connect_to_cs_e, connect_to_cs_w_e)

ggplot() + 
  geom_sf(data = geo_data_c) + 
  geom_sf(data = grid_cropped) +
  geom_sf(data = connect_to_cs_w, colour = "red") +
  geom_sf(data = connect_to_cs_e, colour = "red") +
  geom_sf(data = connect_to_cs_w_e, colour = "red") +
  geom_sf(data = grid_c_s_w_cropped, colour = "red") +
  geom_sf(data = grid_c_s_e_cropped, colour = "red") +
  coord_sf(xlim = c(539500, 541000), ylim = c(5817500, 5818200),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

#### central-north =============================================================

geo_data_c_n <- st_crop(utm_geo_data, 
                        xmin = 546000,
                        xmax = 548000,
                        ymin = 5819000,
                        ymax = 5820000)
ggplot() + 
  geom_sf(data = geo_data_c_n) + 
  geom_sf(data = grid_cropped) + 
  coord_sf(xlim = c(546000, 548000), ylim = c(5819000, 5820000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

geo_data_c_n_w <- st_crop(utm_geo_data, 
                          xmin = 547000,
                          xmax = 547200,
                          ymin = 5819400,
                          ymax = 5819600)
geo_data_c_n_e <- st_crop(utm_geo_data, 
                          xmin = 547500,
                          xmax = 547600,
                          ymin = 5819400,
                          ymax = 5819600)

# west area
grid_c_n_w <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_c_n_w)),
  size = 15, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)
grid_c_n_w_cropped <- grid_c_n_w[sf::st_contains(
  geo_data_c_n_w, grid_c_n_w, sparse = F)]

# east area 
grid_c_n_e <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(geo_data_c_n_e)),
  size = 15, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)
grid_c_n_e_cropped <- grid_c_n_e[sf::st_contains(
  geo_data_c_n_e, grid_c_n_e, sparse = F)]

# connect grids
connect_to_cn_w <- nngeo::st_connect(st_combine(grid_cropped), 
                                     st_combine(grid_c_n_w_cropped))
connect_to_cn_e <- nngeo::st_connect(st_combine(grid_cropped), 
                                     st_combine(grid_c_n_e_cropped))
connect_to_cn_w_e <- nngeo::st_connect(st_combine(grid_c_n_w_cropped), 
                                       st_combine(grid_c_n_e_cropped))
grid_cropped <- c(grid_cropped, grid_c_n_w_cropped, grid_c_n_e_cropped,
                  connect_to_cn_w, connect_to_cn_e, connect_to_cn_w_e)

ggplot() + 
  geom_sf(data = geo_data_c) + 
  geom_sf(data = grid_cropped) +
  geom_sf(data = connect_to_cn_w, colour = "red") +
  geom_sf(data = connect_to_cn_e, colour = "red") +
  geom_sf(data = connect_to_cn_w_e, colour = "red") +
  geom_sf(data = grid_c_n_w_cropped, colour = "red") +
  geom_sf(data = grid_c_n_e_cropped, colour = "red") +
  coord_sf(xlim = c(546000, 548000), ylim = c(5819000, 5820000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")

## now look at the whole thing again ===========================================

# double checking the northern region
ggplot() + 
  geom_sf(data = utm_geo_data) + 
  geom_sf(data = grid_cropped) +
  coord_sf(xlim = c(555000, 560000), ylim = c(5845000, 5860000),
           datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m")
# it's all good, so save the focal objects (the geo_data and the grid cropped)

### save key objects ===========================================================
qs::qsave(utm_geo_data, here("./outputs/geo-objs/fresh/utm-geo-data.qs"))
qs::qsave(grid_cropped, here("./outputs/geo-objs/fresh/grid-cropped.qs"))
