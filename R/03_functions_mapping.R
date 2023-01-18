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
clean_farm_locations <- function(farm_locations){
  #' Take data on the farm locations and clean and prepare them to be used
  #' 
  #' @description Since the data on the farm locations isn't perfect (there's
  #' one farm missing), do that cleaning manually
  #' 
  #' @param farm_locations file. Information on all the farms in the region
  #'  
  #' @usage clean_farm_locations(farm_locations)
  #' @return clean data frame 
  #'
  
  # filter to just the farms we're concerned with in this analysis
  farm_locs <- farm_locs %>% 
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
}

