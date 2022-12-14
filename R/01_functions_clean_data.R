##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This targets file contains all functions to load, and clean data for this 
#' project
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

library(readr)
library(here)
library(magrittr)
library(dplyr)

wild_lice <- readr::read_csv(
  here::here("./data/wild-lice/raw/klemtu_wild_lice_data_CB.csv")) 



clean_wild_lice <- function(file) {
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
  
  
  
}