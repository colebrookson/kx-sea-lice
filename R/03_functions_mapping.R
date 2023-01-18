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
clean_kitasoo_sampling <- function() {
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
}