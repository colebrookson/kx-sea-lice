library(sf)
library(sfnetworks)
library(nngeo)

geo_data <- readRDS(here("./data/geo-spatial/gadm36_CAN_1_sp.rds"))
# make into sf object
# make into sf object
geo_data_bc <- geo_data[which(geo_data$NAME_1 == "British Columbia"),]
geo_data_sf_bc <- st_as_sf(geo_data_bc)

bc_utm <- st_transform(geo_data_sf_bc, 
                       crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
cropped <- sf::st_crop(bc_utm, xmin = 542000,
                                      xmax = 550000, ymin = 5818000, 
                                      ymax = 5822000)

bb <- sf::st_make_grid(sf::st_bbox(cropped), n = 1)

ggplot() + 
  geom_sf(data = geo_data_sf_bc_cropped)

# now since we have a polygon of BC, we cant a polygon of the things that 
# are the waterways, so use the st_differnce of the bounding box and that 
# geom object and we're good 
study_area <- sf::st_difference(bb, cropped)
ggplot() + 
  geom_sf(data = study_area) + 
  coord_sf(datum="+proj=utm +zone=9 +datum=NAD83 +unit=m")+
  theme_bw()

grid_sample <- sf::st_sample(
  sf::st_as_sfc(sf::st_bbox(study_area)),
  # the size is really large to make a fine grid
  size = 500, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 

grid_cropped <- grid_sample[sf::st_contains(
  study_area, grid_sample, sparse = F)]

ggplot() + 
  geom_sf(data = grid_cropped)

network <- as_sfnetwork(grid_cropped, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())

ggplot() + 
  geom_sf(data = study_area) + 
  geom_sf(data = network %>% 
            activate("nodes") %>% 
            activate("edges")  %>%
            st_as_sf()) +
  coord_sf(datum="+proj=utm +zone=9 +datum=NAD83 +unit=m")+
  theme_bw()

# add in points

focal_area <- sf::st_crop(study_area, xmin = 546900, xmax = 548000,
                          ymin = 5819400, ymax = 5820000)
ggplot() + 
  geom_sf(data = study_area) +
  geom_sf(data = focal_area, fill = "grey20") +
  coord_sf(datum="+proj=utm +zone=9 +datum=NAD83 +unit=m")+
  theme_bw()


focal_grid_sample <- sf::st_sample(
  focal_area, size = 25, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9) 

focal_grid_cropped <- focal_grid_sample[sf::st_contains(
  focal_area, focal_grid_sample, sparse = F)]

# ad the two grids together
all_grid <- st_sfc(list(grid_cropped, focal_grid_cropped))

trying = grid_cropped %>% 
  st_geometry() %>% 
  st_nearest_points(st_geometry(focal_grid_cropped)) %>% 
  st_as_sf()

 
  

ggplot() + 
  #geom_sf(data = study_area) +
  geom_sf(data = focal_area, fill = "grey20") +
  geom_sf(data = focal_grid_cropped, colour = "red") + 
  coord_sf(datum="+proj=utm +zone=9 +datum=NAD83 +unit=m")+
  theme_bw()



# make an sfc_LINESTRING beside one another

nc = st_read(system.file("shape/nc.shp", package="sf"))
nc_utm = st_transform(nc, crs="+proj=utm +zone=18 +datum=NAD83 +unit=m")
ggplot() + 
  geom_sf(data = nc_utm) +
  coord_sf(datum = "+proj=utm +zone=18 +datum=NAD83 +unit=m")

area_1 <- st_crop(nc_utm, xmin = 0, xmax = 20000, 
                  ymin = 3950000, ymax = 4000000)
area_2 <- st_crop(nc_utm, xmin = 20100, xmax = 25000, 
                  ymin = 3960000, ymax = 3980000)
area_3 <- st_crop(nc_utm, xmin = -10000, xmax = -500, ymin = 3960000, 
                  ymax = 3970000)
ggplot() + 
  geom_sf(data = nc_utm) +
  geom_sf(data = area_1, fill = 'blue', alpha = 0.3) + 
  geom_sf(data = area_2, fill = "red", alpha = 0.3) + 
  geom_sf(data = area_3, fill = "yellow", alpha = 0.3) + 
  coord_sf(datum = "+proj=utm +zone=18 +datum=NAD83 +unit=m",
           xlim = c(-10000, 30000), ylim = c(3940000, 4001000))

grid_1 <- sf::st_sample(
  area_1, size = 25, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)

grid_2 <- sf::st_sample(
  area_2, size = 25, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)

grid_3 <- sf::st_sample(
  area_3, size = 25, type = 'regular') %>% 
  sf::st_as_sf() %>%
  nngeo::st_connect(.,.,k = 9)

grid_connect <- nngeo::st_connect(st_combine(grid_1), st_combine(grid_2))

grid_connect2 <- nngeo::st_connect(st_combine(grid_1), st_combine(grid_3))

all_grids <- c(grid_1, grid_2, grid_3, grid_connect, grid_connect2)

network <- as_sfnetwork(grid_cropped, directed = FALSE) %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())


ggplot() + 
  geom_sf(data = nc_utm) +
  geom_sf(data = area_1, fill = 'blue', alpha = 0.3) + 
  geom_sf(data = area_2, fill = "red", alpha = 0.3) + 
  geom_sf(data = area_3, fill = "yellow", alpha = 0.3) + 
  geom_sf(data = grid_1, colour = "blue") + 
  geom_sf(data = grid_2, colour = "red") +
  geom_sf(data = all_grids, colour = "purple", size = 2) +
  coord_sf(datum = "+proj=utm +zone=18 +datum=NAD83 +unit=m",
           xlim = c(-10000, 30000), ylim = c(3940000, 4001000))
