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
library(here)

fname <- here("./data/wild-lice/klemtu_wild_lice_data.xlsx")

multiplesheets <- function(fname) {
  #' Pulls in multiple excel sheets and collapses into a single object
  #' 
  #' @description Takes in the path to the file, reads it in, goes through 
  #' each sheet to make it into a list, then puts those list items into one 
  #' dataframe since the names are technically similar
  #' 
  #' @param fname character. The file name for the .xls(x) file
  #'  
  #' @usage multiplesheets(here("./data/wild-lice/klemtu_wild_lice_data.xlsx"))
  #' @return Dataframe of all lice data 
  #' 
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  list_of_dfs <- lapply(tibble, as.data.frame)
  
  # name the different df's by the names of the sheets
  names(list_of_dfs) <- sheets
  
  # start at 2 because the first one is the summary sheet, and we don't want 
  # that one, just the actual data 
  
  
  return()
}