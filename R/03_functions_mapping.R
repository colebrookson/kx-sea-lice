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
    height = 8, width = 7
  )
  
}

# clean_population_locations ===================================================
# clean_population_locations <- function(sr_pop_sites)