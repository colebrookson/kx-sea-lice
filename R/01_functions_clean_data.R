##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This file contains all functions to load, and clean data for this project
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# clean_wild_lice ==============================================================
clean_wild_lice <- function(file, output_path) {
  #' Takes in object, and sorts thru the file to fix any errors that are present
  #' 
  #' @description First deals with the incorrect types that aren't expected 
  #' 
  #' @param file data frame. The file at hand
  #'  
  #' @usage multiplesheets(here("./data/wild-lice/klemtu_wild_lice_data.xlsx"))
  #' @return Dataframe of all lice data 
  #' 
  
  # get rid of the problems 
  wild_lice <- wild_lice %>% 
    mutate(
      `Length (mm)` = ifelse(
        `Length (mm)` == "too decomposed", 
        NA,
        ifelse(
          `Length (mm)` == "1.9.", 
          1.9,
          `Length (mm)`
        )
      )
    )
  
  # standardize names 
  wild_lice <- standardize_names(wild_lice)
  
  readr::write_csv(
    paste0(
      output_path, "wild_lice_df.csv"
    )
  )
  
  return(wild_lice)
  
}