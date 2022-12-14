##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This targets file contains all targets to run this analysis. If you are not
#' familiar with the targets package, see the documentation here:
#' https://books.ropensci.org/targets/
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

library(targets)
library(tarchetypes)
library(here)

source(here::here("./R/00_functions_global.R"))

list(
  ## files =====================================================================
  tar_target(raw_wild_lice_data,
             here::here(
               "./data/wild-lice/raw/klemtu_wild_lice_data_CB.csv"),
             format = "file")
  
)
