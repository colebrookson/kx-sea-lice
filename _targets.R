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
source(here::here("./R/01_functions_clean_data.R"))
source(here::here("./R/02_functions_lice_regression.R"))

tar_option_set(packages = c("here", "readr", "magrittr", "dplyr", "ggplot2", 
                            "ggthemes", "wesanderson", "lubridate", "janitor",
                            "tibble"))
options(dplyr.summarise.inform = FALSE)

list(
  ## files =====================================================================
  tar_target(
    raw_wild_lice_data,
    here::here(
      "./data/wild-lice/raw/klemtu_wild_lice_data_CB.csv"
    ),
    format = "file"
  ),
  tar_target(
    dates_to_join,
    here::here(
      "./data/wild-lice/raw/unique_dates_manually_edited.csv"
    ),
    format = "file"
  ),
  tar_target(
    sr_data,
    here::here(
      "./data/spawner-recruit/raw/river-level-sr/NCC_streams_SR_data.csv"
    ),
    format = "file"
  ),
  tar_target(
    farm_locations,
    here::here(
      "./data/farm-lice/raw/farm_location_metadata.csv"
    ),
    format = "file"
  ),
  tar_target(
    kx_sampling,
    here::here(
      "./data/wild-lice/raw/kitasoo_sampling_sites.csv"
    ),
    format = "file"
  ),
  tar_target(
    old_farm_lice,
    here::here(
      "./data/farm-lice/raw/klemtu_farm_lice_data_old.xls"
    ),
    format = "file"
  ),
  tar_target(
    new_farm_lice,
    here::here(
      "./data/farm-lice/raw/klemtu_farm_lice_data_new.xlsx"
    )
  ),
  ## data cleaning =============================================================
  tar_target(
    clean_wild_lice_data,
    clean_wild_lice(
      raw_wild_lice = get_data_csv(raw_wild_lice_data),
      dates_to_join = get_data_csv(dates_to_join),
      raw_output_path = here::here("./data/wild-lice/raw/"),
      clean_output_path = here::here("./data/wild-lice/clean/"),
      fig_output_path = here::here("./figs/wild-lice/")
    )
  ),
  tar_target(
    clean_farm_lice_data,
    clean_farm_lice(
      old_lice = readxl::read_excel(old_farm_lice,
                                    sheet = 9),
      new_lice = readxl::read_excel(new_farm_lice,
                                    sheet = 5),
      data_output_path = here::here("./data/farm-lice/clean/"),
      fig_output_path = here::here("./figs/farm-lice/")
    )
  ),
  tar_target(
    clean_pink_spawner_recruit_data,
    clean_pk_sr_data(
      get_data_csv(sr_data),
      here::here(
        "./data/spawner-recruit/clean/"
      )
    )
  ),
  ## lice regression ===========================================================
  tar_target(
    wild_farm_lice_regression,
    lice_regression(
      wild_lice = clean_wild_lice_data,
      farm_lice = clean_farm_lice_data,
      mod_output_path = here::here("./outputs/lice-regression/"),
      plot_output_path = here::here("./figs/lice/regression/")
    )
  ),
  ## useful plots/extra content ================================================
  tar_target(wild_lice_per_fish_plot,
             plot_wild_lice_data(
               wild_lice = clean_wild_lice_data,
               output_path = here::here("./figs/wild-lice/")
             )
  )
)
