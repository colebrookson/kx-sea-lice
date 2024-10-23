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
source(here::here("./src/R/functions/00_functions_global.R"))
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
names(wild_lice)
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
    ))
)

#' Cleaning the sr-data
#'
#' 1. do renaming so everything is consistent
#' 2. get rid of the years where recruitment wasn't measured
#' 3. figure out how many observations there are of each population
#' 4. exclude the populations that don't have enough observations
all_pinks_rivers <- clean_pk_sr_data(
    sr_data = sr_data,
    output_path = here::here("./data/spawner-recruit/clean/"),
    n_obs = 4
)
all_chums_rivers <- clean_chum_sr_data(
    sr_data = sr_data,
    output_path = here::here("./data/spawner-recruit/clean/"),
    n_obs = 4
)

# clean farm lice ==============================================================

# read raw files
old_lice <- readxl::read_excel(
    here::here("./data/farm-lice/raw/klemtu_farm_lice_data_old.xls"),
    sheet = 9
)
new_lice <- readxl::read_excel(
    here::here("./data/farm-lice/raw/klemtu_farm_lice_data_new.xlsx"),
    sheet = 5
)

#' Cleaning Farm Lice Data
#'
#' The most harrowing task of all
#' 1. fix all the dates and get rid of observations with no dates
#' 2. standardize names
#' 3. make sure inventory measurements are correct and being carried along
#' 4. join the old and the new versions of the farm lice
#' 5. plot the inventory and lice numbers through time
farm_lice <- clean_farm_lice(
    old_lice = old_lice,
    new_lice = new_lice,
    data_output_path = here::here("./data/farm-lice/clean/"),
    fig_output_path = here::here("./figs/farm-lice//")
)

# clean population locations ===================================================

# read in file
sr_pop_sites <- readr::read_csv(
    here::here("./data/spawner-recruit/raw/conservation_unit_system_site.csv")
)

#' Clean the locations
#'
#' 1. These are essentially clean, but we need to make sure the gfe_ids and the
#' unique identifiers map up, and that everything is unique
sr_sites_clean <- clean_pop_sites(
    sr_pop_sites = sr_pop_sites,
    output_path = here::here("./data/spawner-recruit/clean/")
)
