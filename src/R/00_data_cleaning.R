##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2024-08-19
#'
#' Code to clean all the data using pre-written functions
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

library(magrittr)
library(ggplot2)
source(here::here("./src/R/functions/01_functions_clean_data.R"))

# wild lice data ===============================================================

# read in raw files
raw_wild_lice <- readr::read_csv(
    here::here("./data/wild-lice/raw/klemtu_wild_lice_data_CB.csv")
)
dates_to_join <- readr::read_csv(
    here::here("./data/wild-lice/raw/unique_dates_manually_edited.csv")
)

#' Cleaning Lice Data:
#'
#' 1. keep only pink & chum and make sure the names are consistent
#' 2. join the seine dates, this is done (paritially) by hand if need be, see
#' function for more info
#' 3. get stage sums for leps
#' 4. make a plot of the observations per-year
wild_lice <- clean_wild_lice(
    raw_wild_lice = raw_wild_lice,
    dates_to_join = dates_to_join,
    raw_output_path = here::here("./data/wild-lice/raw/"),
    clean_output_path = here::here("./data/wild-lice/clean/"),
    fig_output_path = here::here("./figs/wild-lice/")
)

# plot the wild lice data
plot_wild_lice_data(
    wild_lice = wild_lice,
    output_path = here::here("./figs/wild-lice/")
)

# clean pink stock-recruit data ================================================

# read in raw file
sr_data <- readr::read_csv(
    here::here(paste0(
      "./data/spawner-recruit/raw/river-level-sr/",
      "NCC_streams_river-level_SR_data_2023-04-19.csv"
    )
)

#' Cleaning the sr-data 
#'
#' 1. do renaming so everything is consistent 
#' 2. get rid of the years where recruitment wasn't measured 
#' 3. figure out how many observations there are of each population
#' 4. exclude the populations taht don't have enough observations 

