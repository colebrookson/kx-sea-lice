library(dplyr)
library(here)
library(readr)
library(ggplot2)
library(ggrepel)

farm_locs <- readr::read_csv(
  here("./data/farm-lice/raw/farm_location_metadata.csv")
)
pop_ns <- readr::read_csv(
  here::here("./data/spawner-recruit/raw/conservation_unit_system_site.csv")
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

all_locations <- rbind(all_farm_locs, kx_sampling) %>% 
  dplyr::mutate(
    type = as.factor(type)
  ) %>% 
  dplyr::mutate(
    ff = ifelse(type == "farm", "bold", "plain")
  )

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
  coord_cartesian(xlim = c(-128.8, -128.1), ylim = c(52.2, 52.95)) + 
  geom_point(data = all_locations,
             aes(x = long, y = lat, fill = type, shape = type),
             size = 4) + 
  ggthemes::theme_base() +
  labs(x = "Longitude (°)", y = "Latitude (°)") +
  scale_shape_manual("Location", values = c(21, 22)) + 
  scale_fill_manual("Location", values = c("purple", "gold2")) +
  ggrepel::geom_text_repel(data = all_locations,
                  aes(x = long, y = lat, 
                      label = site, fontface = ff),
                  size = 3,
                  max.overlaps = 20) 
  


# sr population stuff ==========================================================
sr_pop_data <- read_csv(here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))
sr_pop_sites <- read_csv(here::here("./data/spawner-recruit/raw/conservation_unit_system_site.csv"))
farm_data <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))

# filter the sites to just the ones in our data
sr_pop_data_area7 <- sr_pop_data %>% 
  dplyr::filter(area == 7) %>% 
  dplyr::mutate(
    # NOTE - there are some of these that I do have location data for, they're 
    # just under a different name, so take care of those here. 
    river = dplyr::case_when(
      river == "COOPER INLET-FANNIE COVE CREEKS" ~ "COOPER INLET CREEKS",
      river == "KITASOO CREEK" ~ "KITASU CREEK",
      river == "KILDIDT LAGOON CREEK #2" ~ "KILDIDT LAGOON #2 CREEK",
      river == "COOPER INLET-FANNIE COVE LH CREEK" ~ "COOPER INLET CREEKS",
      TRUE ~ as.character(river)
    )
  )
length(unique(sr_pop_data_area7$river))

sr_pop_sites_filter <- sr_pop_sites %>% 
  standardize_names(.) %>% 
  dplyr::filter(system_site %in% unique(sr_pop_data_area7$river)) %>% 
  dplyr::filter(species_qualified %in% c("PKE", "PKO"))

length(unique(sr_pop_sites_filter$system_site))


sites_not_in_nuseds <- unique(sr_pop_data_area7$river[
  which(sr_pop_data_area7$river %notin% sr_pop_sites_filter$system_site)])

for(yr in 2005:2020) {
  # get the farms in that time period
  farms_temp <- (farm_data %>% 
    dplyr::filter(year == yr) %>% 
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::group_by(farm) %>% 
    dplyr::summarize(inventory = mean(mean_inventory, na.rm = TRUE)))$farm
  
  
}

  
  
  