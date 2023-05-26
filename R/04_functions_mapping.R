##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-01-18
#'
#' This file contains the functions that map the locations and sites we're 
#' interested in for this project 
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# clean_farm_locations =========================================================
clean_farm_locations <- function(farm_locations, output_path){
  #' Take data on the farm locations and clean and prepare them to be used
  #' 
  #' @description Since the data on the farm locations isn't perfect (there's
  #' one farm missing), do that cleaning manually
  #' 
  #' @param farm_locations file. Information on all the farms in the region
  #' @param output_path character. Location to write out the locations of 
  #' the farms 
  #'  
  #' @usage clean_farm_locations(farm_locations)
  #' @return clean data frame 
  #'
  
  # filter to just the farms we're concerned with in this analysis
  farm_locs <- farm_locations %>% 
    dplyr::filter(
      site %in% c("Lochalsh", "Jackson Pass", "Kid Bay", "Alexander Inlet",
                  "Sheep Passage", "Goat Cove", "Lime Point")
    ) %>% 
    dplyr::select(site, latitude, longitude, area) 
  
  # manually put in cougar bay since it's not in the dataset
  cougar_add <- data.frame(
    site = "Cougar Bay",
    latitude = 52.743511,
    longitude = -128.587263,
    area = 7
  )
  
  # rename for ease
  all_farm_locs <- rbind(farm_locs, cougar_add) %>% 
    dplyr::rename(
      lat = latitude,
      long = longitude
    ) %>% 
    dplyr::mutate(type = "farm")
  
  readr::write_csv(all_farm_locs, paste0(output_path, "clean-farm-locs.csv"))
  
  return(all_farm_locs)
}

# clean_kitasoo_sampling =======================================================
clean_kitasoo_sampling <- function(kx_sampling, all_farm_locs) {
  #' Bring in the data on where the sampling happended and clean it
  #' 
  #' @description KXSA sampling data needs to be cleaned and renamed so it can
  #' get stitched to the farm location data. Also join to the farm data
  #' 
  #' @param kx_sampling file. Information on all the sampling locations in the
  #' sampling region
  #' @param all_farm_locs. dataframe. Information on all the farm locations from
  #' being cleaned earlier. 
  #'  
  #' @usage clean_kitasoo_sampling(kx_sampling)
  #' @return clean data frame 
  #'
  
  # clean data
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
  
  # put the different location data together
  all_locations <- rbind(all_farm_locs, kx_sampling) %>% 
    dplyr::mutate(
      type = as.factor(type)
    ) %>% 
    dplyr::mutate(
      # use this so in the plot I can make the labels different fonts
      ff = ifelse(type == "farm", "bold", "plain")
    )
  
  return(all_locations)
}

# make_sampling_map ============================================================
make_sampling_map <- function(farm_locations, kx_sampling, geo_data, 
                              output_path, farm_path) {
  #' Take the all_locations data and make the plot with the geospatial data 
  #' 
  #' @description The geospatial data has been pre-downloaded, so take that 
  #' data along with the cleaned location data and 
  #' 
  #' @param kx_sampling file. Information on all the sampling locations in the
  #' sampling region
  #' @param farm_locations file. Information on all the farms in the region
  #' @param geo_data file. The geo-spatial data rds file
  #' @param output_path character. Where to save the plot
  #' @param farm_path character. Where to save the data of the farm locations
  #'  
  #' @usage clean_kitasoo_sampling(kx_sampling)
  #' @return clean data frame 
  #'
  
  # make the data that we need
  all_locations <- clean_kitasoo_sampling(
    # this data can be just used
    kx_sampling, 
    # use the cleaning function for this one
    clean_farm_locations(farm_locations, farm_path))
  
  # geospatial stuff
  province = "British Columbia"
  canada_prov = geo_data[geo_data$NAME_1 %in% province] # subset to just BC
  
  ggplot2::ggsave(
    
    # output part
    paste0(output_path, "study-region-map.png"),
    
    # make and save the actual plot
    ggplot2::ggplot() +
      geom_polygon(data = canada_prov,
                   aes(x = long, y = lat, group = group),
                   colour = "black",
                   linewidth = 0.01,
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
                               max.overlaps = 20),
    
    # make the size 
    height = 8, width = 9
  )
  
}

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

list_reassign <- function(l, nodes_edges) {
  #' Just keep nodes or edges
  #' 
  #' @description Small simple function to take in a list of lists, and only 
  #' keep the list of name "edges" or "nodes
  #' @param l list. A list of length 2, with each sub-item being a list
  #' @param nodes_edges character. Which of the two we want to keep 
  #' 
  #' @return list of length 1
  
  return(l[nodes_edges])
  
}

# make_yearly_popn_maps ========================================================
make_yearly_popn_maps <- function(sr_pop_data, sr_pop_sites, utm_geo_data, 
                                  utm_land_data, farm_data, farm_locs, network, 
                                  all_edges_nodes, fig_output, 
                                  data_output) {
  #' Maps of each year's population and farm co-occurrence
  #' 
  #' @description To determine which farms are high, medium, or low risk, we
  #' need yearly maps to show which populations might pass farms
  #' 
  #' @param sr_pop_data dataframe. The cleaned pink SR data
  #' @param sr_pop_sites file. Information on all the populations locations
  #' @param utm_geo_data file. The geo-spatial data rds file
  #' @param farm_data dataframe. Data of the info for the different farms
  #' @param farm_locs dataframe. The cleaned data of the different 
  #' locations of the farms
  #' @param network sfnetwork tibble. The network for the entire region
  #' @param west_network sfnetwork tibble. The network for the western region
  #' @param all_edges_nodes list. The list of all the lists of edges and nodes
  #' to be kept to map onto the region
  #' @param fig_output character. Where to save the figure files
  #' @param data_output character. Where to save the data files 
  #'  
  #' @usage make_yearly_popn_maps(sr_pop_data, sr_pop_sites, geo_data,
  #' farm_data, clean_farm_locs, output_path)
  #' @return NA
  #'
  
  # filter the sites to just the ones in our data
  sr_pop_data_area67 <- sr_pop_data %>% 
    dplyr::filter(area %in% c(6, 7))
  
  # clean the farm data names so they match up between the dataframe
  farm_data <- farm_data %>% 
    dplyr::mutate(
      farm = dplyr::case_when(
        farm == "Goat" ~ "Goat Cove",
        farm == "Sheep Pass" ~ "Sheep Passage",
        farm == "Lime" ~ "Lime Point",
        farm == "Alexander" ~ "Alexander Inlet",
        TRUE ~ as.character(farm)
      )
    )
  
  # this is an empty dataframe to fill in 
  site_data_by_year <- data.frame(
    site_name = as.character(),
    brood_year = as.numeric(),
    lat = as.numeric(),
    long = as.numeric(),
    site_num = as.numeric()
  )
  
  ## set up the farms that we'll need to plot but with sf functions ============
  farms_sf <- sf::st_as_sf(farm_locs, coords = c("long", "lat"))
  
  # set the coordinates for WGS84
  sf::st_crs(farms_sf) <- 4326 
  # transform to utm 
  farms_utm <- sf::st_transform(farms_sf,  
                            crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
  
  for(yr in 2005:2020) {
    
    # get the farms in that time period
    farms_temp <- (farm_data %>% 
                     dplyr::filter(year == yr) %>% 
                     dplyr::filter(month %in% c(3, 4)) %>% 
                     dplyr::group_by(farm) %>% 
                     dplyr::summarize(inventory = 
                                        mean(mean_inventory, 
                                             na.rm = TRUE)))$farm
    
    farm_locs_temp <- farm_locs %>% 
      dplyr::filter(site %in% farms_temp) %>% 
      dplyr::select(site, lat, long)
    
    sr_pop_sites_filter <- sr_pop_sites %>% 
      standardize_names(.) %>% 
      dplyr::filter(system_site %in% unique(sr_pop_data_area67$river)) 
    
    # get the populations in that year
    sr_pop_temp <- sr_pop_data_area67 %>% 
      # brood year of yr will pass fish farms in year + 1
      dplyr::filter(brood_year == (yr - 1))
    # subset to just the locations that were shown to be present in that year
    site_year_temp <- sr_pop_sites_filter %>% 
      dplyr::filter(system_site %in% unique(sr_pop_temp$river)) %>% 
      dplyr::select(system_site, y_lat, x_longt) %>% 
      unique() %>% 
      dplyr::rename(site_name = system_site, lat = y_lat, long = x_longt) %>% 
      dplyr::mutate(site_num = seq_len(nrow(.)),
                    brood_year = unique(sr_pop_temp$brood_year)) %>% 
      dplyr::select(site_name, brood_year, lat, long, site_num)
    # keep these data in the larger dataframe to refer back to
    site_data_by_year <- rbind(
      site_data_by_year,
      site_year_temp
    )
    
    # put the farm locations and the population locations together
    locs_temp <- rbind(
      (farm_locs_temp %>% 
         dplyr::mutate(type = "farm")),
      (site_year_temp %>% 
         dplyr::mutate(type = "population") %>% 
         dplyr::mutate(type = as.factor(type),
                       site = as.character(site_num)) %>% 
         dplyr::select(site, lat, long, type))
    ) %>%
      # this is being done to the whole resulting df
      dplyr::mutate(
        ff = ifelse(type == "farm", "bold", "plain"),
        fsize = ifelse(type == "farm", 1.8, 1.5)
      ) %>% 
      sf::st_as_sf(., coords = c("long", "lat"))
    
    # need to temp make it WGS84
    sf::st_crs(locs_temp) <- 4326
    
    # transform to utm 
    locs_temp_utm <- sf::st_transform(locs_temp, 
                              crs="+proj=utm +zone=9 +datum=NAD83 +unit=m") %>% 
      dplyr::mutate(
        X = data.frame(sf::st_coordinates(.))$X,
        Y = data.frame(sf::st_coordinates(.))$Y
      ) %>% 
      dplyr::mutate(
        type = as.factor(type),
        ff = as.factor(ff),
        fsize = as.factor(fsize)
      )
    
    ## figure out what nodes need to be kept for this year =====================
    curr_edges_nodes <- all_edges_nodes[
      which(names(all_edges_nodes) %in% farm_locs_temp$site)]
    curr_nodes <- sapply(curr_edges_nodes, list_reassign, 
                         nodes_edges="nodes") %>% 
      unlist()
      
    ggplot2::ggplot() +
      # geom_sf(data = utm_geo_data, fill = "white") +
      geom_sf(data = network %>%
                activate("nodes") %>%
                slice(curr_nodes) %>% 
                st_as_sf(), fill = "lightpink", colour = "lightpink") +
      geom_sf(data = utm_land_data, fill = "grey70") +
      geom_sf(data = locs_temp_utm,
              aes(fill = type, shape = type),
              colour = "black")  + 
      coord_sf(xlim = c(465674.8, 600000), ylim = c(5761156, 5983932), 
               expand = FALSE) + 
      scale_shape_manual("Location", values = c(21, 22)) + 
      scale_fill_manual("Location", values = c("purple", "gold2")) +
      ggrepel::geom_text_repel(data = locs_temp_utm,
                               aes(x = X, y = Y, 
                                   label = site, fontface = ff, size = fsize),
                               max.overlaps = 20) + 
      scale_size_manual("", values = c(1.9, 2.5)) + 
      guides(
        size = "none",
        fill = guide_legend(
          override.aes = list(
            size = 3
          )
        )
      ) + 
      theme_base() +
      theme(
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90)
      ) + 
      labs(
        x = "Longitude", y = "Latitude"
      )
    # make and save the dataframe
    ggplot2::ggsave( 
      
      # output path
      paste0(fig_output, "map-by-year-", yr, ".png"),
    
      # make the plot
      ggplot2::ggplot() +
        geom_polygon(data = canada_prov,
                     aes(x = long, y = lat, group = group),
                     colour = "black",
                     linewidth = 0.01,
                     fill = "grey65") +
        coord_cartesian(xlim = c(-129.5, -127.75), ylim = c(52, 54)) + 
        geom_point(data = locs_temp,
                   aes(x = long, y = lat, fill = type, shape = type),
                   size = 2) + 
        theme_base() +
        labs(x = "Longitude (°)", y = "Latitude (°)",
             title = paste0(yr, ", brood year ", (yr-1))) +
        scale_shape_manual("Location", values = c(21, 22)) + 
        scale_fill_manual("Location", values = c("purple", "gold2")) +
        ggrepel::geom_text_repel(data = locs_temp,
                                 aes(x = long, y = lat, 
                                     label = site, fontface = ff),
                                 size = 3,
                                 max.overlaps = 20),
      
      # make the size 
      height = 8, width = 9
    )
    
  }
  
  # write out the data with the corresponding names/numbers for each year
  readr::write_csv(
    site_data_by_year,
    paste0(data_output, "site-name-combos-for-exposed-populations.csv")
  )
  
  
}
