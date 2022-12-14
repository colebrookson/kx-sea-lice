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

library(readxl)


multiplesheets <- function(fname) {
  #' Pulls in multiple excel sheets and collapses into a single object
  #' 
  #' @description 
  #' 
  #' @param n_spp integer. The number of species at hand.
  #' @param richness_shape character. The shape of the network. Can take on 
  #' values of "top-heavy", "bottom-heavy" or "uniform"
  #' @param rates_df data.frame. Dataframe from pre-analysis body sizes
  #' with the information on rates from each trophic level
  #'  
  #' @usage define_species_list(20, "top-heavy")
  #' @return List of the species for the present network
  #' 
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}