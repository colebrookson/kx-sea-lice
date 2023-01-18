library(dplyr)
library(here)
library(readr)
library(ggplot2)

farm_locs <- readr::read_csv(
  here("./data/farm-lice/raw/farm_location_metadata.csv")
)

farm_locs <- farm_locs %>% 
  dplyr::filter(
    site %in% c("Lochalsh", "Jackson Pass", "Kid Bay", "Alexander Inlet",
                "Sheep Passage", "Goat Cove", "Lime Point")
  ) %>% 
  dplyr::select(site, latitude, longitude, area) 

# manually put in cougar bay
cougar_add <- data.frame(
  site = "Cougar Bay",
  latitude = 52.743511,
  longitude = -128.587263,
  area = 7
)

all_farm_locs <- rbind(farm_locs, cougar_add) %>% 
  dplyr::rename(
    lat = latitude,
    long = longitude
  ) %>% 
  dplyr::mutate(type = "farm")

# get the sampling area info 
kx_sampling <- read_csv(here("./data/wild-lice/raw/kitasoo_sampling_sites.csv"))

kx_sampling <- kx_sampling %>% 
  dplyr::rename(
    site = name,
    long = lon
  ) %>% 
  dplyr::select(site, lat, long) %>% 
  dplyr::mutate(
    area = 7,
    type = "sampling"
  ) 

all_locations <- rbind(all_farm_locs, kx_sampling)

# geospatial stuff
geo_data = readRDS(here("./data/geo-spatial/gadm36_CAN_1_sp.rds"))
province = "British Columbia"
canada_prov = geo_data[geo_data$NAME_1 %in% province] # subset to just BC


ggplot() +
  geom_polygon(data = canada_prov,
               aes(x = long, y = lat, group = group),
               colour = "black",
               size = 0.01,
               fill = "grey65") +
  coord_cartesian(xlim = c(-128.5, -130.3), ylim = c(52.87, 53.0)) + 
  geom_point(data = all_locations,
             aes(x = long, y = lat, fill = type),
             size = 4) 

