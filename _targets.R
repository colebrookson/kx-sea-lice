##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This targets file contains all targets to run this analysis. If you are not
#' familiar with the targets package, see the documentation here:
#' https://books.ropensci.org/targets/
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

library(targets)
library(tarchetypes)
library(here)

source(here::here("./src/R/00_functions_global.R"))
source(here::here("./src/R/01_functions_clean_data.R"))
source(here::here("./src/R/02_functions_lice_v_lice_regression.R"))
source(here::here("./src/R/03_functions_wild_lice_regression.R"))
source(here::here("./src/R/04_functions_mapping.R"))
source(here::here("./src/R/05_functions_power_analysis.R"))

tar_option_set(packages = c(
  "here", "readr", "magrittr", "dplyr", "ggplot2",
  "ggthemes", "wesanderson", "lubridate", "janitor",
  "tibble", "ggrepel", "sp", "glmmTMB", "sf",
  "sfnetworks", "qs", "tidygraph", "patchwork"
))

options(dplyr.summarise.inform = FALSE)

file_targets <- list(
  ## files =====================================================================
  tar_target(raw_wild_lice_data,
    here::here("./data/wild-lice/raw/klemtu_wild_lice_data_CB.csv"),
    format = "file"
  ),
  tar_target(dates_to_join,
    here::here("./data/wild-lice/raw/unique_dates_manually_edited.csv"),
    format = "file"
  ),
  tar_target(sr_data,
    here::here(paste0(
      "./data/spawner-recruit/raw/river-level-sr/",
      "NCC_streams_river-level_SR_data_2023-04-19.csv"
    )),
    format = "file"
  ),
  tar_target(farm_locations,
    here::here("./data/farm-lice/raw/farm_location_metadata.csv"),
    format = "file"
  ),
  tar_target(kx_sampling,
    here::here("./data/wild-lice/raw/kitasoo_sampling_sites.csv"),
    format = "file"
  ),
  tar_target(old_farm_lice,
    here::here("./data/farm-lice/raw/klemtu_farm_lice_data_old.xls"),
    format = "file"
  ),
  tar_target(new_farm_lice,
    here::here("./data/farm-lice/raw/klemtu_farm_lice_data_new.xlsx"),
    format = "file"
  ),
  tar_target(sr_pop_sites_raw,
    here::here("./data/spawner-recruit/raw/conservation_unit_system_site.csv"),
    format = "file"
  ),
  tar_target(pink_exposure_df,
    here::here(
      "./data/spawner-recruit/clean/pink-exposure-categorization-df.csv"
    ),
    format = "file"
  ),
  tar_target(chum_exposure_df,
    here::here(
      "./data/spawner-recruit/clean/chum-exposure-categorization-df.csv"
    ),
    format = "file"
  ),
  ## geo-data files ============================================================
  ### nodes to keep ============================================================
  tar_target(
    all_edges_nodes,
    here::here("./outputs/geo-objs/fresh/all-edges-nodes-to-keep.rds")
  ),
  ### networks =================================================================
  tar_target(network,
    here::here("./outputs/geo-objs/fresh/network.qs"),
    format = "file"
  ),
  ### background area ==========================================================
  # tar_target(utm_geo_data,
  #            here::here("./outputs/geo-objs/utm-geo-data.rds")),
  tar_target(
    large_land,
    here::here("./outputs/geo-objs/fresh/large-land-for-plotting.rds")
  ),
  tar_target(
    larger_land,
    here::here(
      "./outputs/geo-objs/fresh/even-large-land-for-plotting.rds"
    )
  )
)

## data cleaning ===============================================================
data_options <- tibble::tibble(
  include_2005 = c(TRUE, FALSE),
  include_2015 = c(TRUE, FALSE)
)
## wild lice data ============================================================

data_cleaning_targets <- list(
  tar_target(
    clean_wild_lice_data_2005,
    # version of the data that includes 2005
    clean_wild_lice(
      raw_wild_lice = get_data_csv(raw_wild_lice_data),
      dates_to_join = get_data_csv(dates_to_join),
      include_2005 = TRUE,
      raw_output_path = here::here("./data/wild-lice/raw//"),
      clean_output_path = here::here("./data/wild-lice/clean//"),
      fig_output_path = here::here("./figs/wild-lice//")
    )
  ),
  tar_target(
    clean_wild_lice_data_2006,
    # the version of the data that EXcludes 2005
    clean_wild_lice(
      raw_wild_lice = get_data_csv(raw_wild_lice_data),
      dates_to_join = get_data_csv(dates_to_join),
      include_2005 = FALSE,
      raw_output_path = here::here("./data/wild-lice/raw//"),
      clean_output_path = here::here("./data/wild-lice/clean//"),
      fig_output_path = here::here("./figs/wild-lice//")
    )
  ),
  tar_target(
    clean_farm_lice_data,
    clean_farm_lice(
      old_lice = readxl::read_excel(old_farm_lice,
        sheet = 9
      ),
      new_lice = readxl::read_excel(new_farm_lice,
        sheet = 5
      ),
      data_output_path = here::here("./data/farm-lice/clean//"),
      fig_output_path = here::here("./figs/farm-lice//")
    )
  ),
  tar_target(
    clean_pink_spawner_recruit_data,
    clean_pk_sr_data(
      get_data_csv(sr_data),
      here::here(
        "./data/spawner-recruit/clean//"
      )
    )
  ),
  tar_target(
    clean_wild_pop_location_data,
    clean_pop_sites(
      sr_pop_sites = readr::read_csv(sr_pop_sites_raw),
      output_path = here::here("./data/spawner-recruit/clean//")
    )
  ),
  tar_target(
    clean_chum_spawner_recruit_data,
    clean_chum_sr_data(
      get_data_csv(sr_data),
      here::here(
        "./data/spawner-recruit/clean//"
      )
    )
  ),
  tar_target(
    clean_coho_spawner_recruit_data,
    clean_coho_sr_data(
      get_data_csv(sr_data),
      here::here(
        "./data/spawner-recruit/clean//"
      )
    )
  ),
  tar_target(
    clean_farm_locs,
    clean_farm_locations(
      farm_locations = get_data_csv(farm_locations),
      output_path = here::here("./data/farm-lice/clean//")
    )
  )
)
## lice regression ===========================================================
lice_regression_targets <- list(
  tar_target(
    wild_farm_lice_regression,
    lice_regression(
      wild_lice = clean_wild_lice_data,
      farm_lice = clean_farm_lice_data,
      mod_output_path = here::here("./outputs/lice-regression//"),
      plot_output_path = here::here("./figs/regression//")
    )
  )
)
## useful plots/extra content ================================================
mapping_targets <- list(
  tar_target(
    yearly_nonexposure_maps_pink,
    make_nonexposure_yearly_maps(
      sr_pop_data = clean_pink_spawner_recruit_data,
      sr_pop_sites = clean_wild_pop_location_data,
      large_land = readRDS(large_land),
      farm_data = clean_farm_lice_data,
      farm_locs = clean_farm_locs,
      network = qs::qread(network),
      all_edges_nodes = readRDS(all_edges_nodes),
      species = "Pink",
      fig_output = here::here("./figs/maps/yearly-pop-maps/pink/no-exposure//"),
      data_output = here::here("./data/spawner-recruit/clean//")
    )
  ),
  tar_target(
    yearly_nonexposure_maps_chum,
    make_nonexposure_yearly_maps(
      sr_pop_data = clean_chum_spawner_recruit_data,
      sr_pop_sites = clean_wild_pop_location_data,
      large_land = readRDS(large_land),
      farm_data = clean_farm_lice_data,
      farm_locs = clean_farm_locs,
      network = qs::qread(network),
      all_edges_nodes = readRDS(all_edges_nodes),
      species = "Chum",
      fig_output = here::here("./figs/maps/yearly-pop-maps/chum/no-exposure//"),
      data_output = here::here("./data/spawner-recruit/clean//")
    )
  ),
  tar_target(
    yearly_popn_exposure_maps_pink,
    make_yearly_popn_maps(
      sr_pop_data = clean_pink_spawner_recruit_data,
      sr_pop_sites = clean_wild_pop_location_data,
      large_land = readRDS(large_land),
      farm_data = clean_farm_lice_data,
      farm_locs = clean_farm_locs,
      network = qs::qread(network),
      exposure_df = read_csv(pink_exposure_df),
      all_edges_nodes = readRDS(all_edges_nodes),
      species = "Pink",
      fig_output = here::here("./figs/maps/yearly-pop-maps/pink//"),
      data_output = here::here("./data/spawner-recruit/clean//")
    )
  ),
  tar_target(
    yearly_popn_exposure_maps_chum,
    make_yearly_popn_maps(
      sr_pop_data = clean_chum_spawner_recruit_data,
      sr_pop_sites = clean_wild_pop_location_data,
      large_land = readRDS(large_land),
      farm_data = clean_farm_lice_data,
      farm_locs = clean_farm_locs,
      network = qs::qread(network),
      exposure_df = read_csv(chum_exposure_df),
      all_edges_nodes = readRDS(all_edges_nodes),
      species = "Chum",
      fig_output = here::here("./figs/maps/yearly-pop-maps/chum//")
    )
  ),
  tar_target(
    yearly_popn_exposure_large_maps_pink,
    make_yearly_popn_maps(
      sr_pop_data = clean_pink_spawner_recruit_data,
      sr_pop_sites = clean_wild_pop_location_data,
      large_land = readRDS(larger_land),
      farm_data = clean_farm_lice_data,
      farm_locs = clean_farm_locs,
      network = qs::qread(network),
      exposure_df = read_csv(pink_exposure_df),
      all_edges_nodes = readRDS(all_edges_nodes),
      species = "Pink",
      fig_output = here::here("./figs/maps/yearly-pop-maps/pink/larger//"),
      data_output = here::here("./data/spawner-recruit/clean//"),
      size = "large"
    )
  ),
  tar_target(
    yearly_popn_exposure_large_maps_chum,
    make_yearly_popn_maps(
      sr_pop_data = clean_chum_spawner_recruit_data,
      sr_pop_sites = clean_wild_pop_location_data,
      large_land = readRDS(larger_land),
      farm_data = clean_farm_lice_data,
      farm_locs = clean_farm_locs,
      network = qs::qread(network),
      exposure_df = read_csv(chum_exposure_df),
      all_edges_nodes = readRDS(all_edges_nodes),
      species = "Chum",
      fig_output = here::here("./figs/maps/yearly-pop-maps/chum/larger//"),
      # data_output = here::here("./data/spawner-recruit/clean//"),
      size = "large"
    )
  )
)
# tar_target(
#   wild_lice_per_fish_plot,
#   plot_wild_lice_data(
#     wild_lice = clean_wild_lice_data,
#     output_path = here::here("./figs/wild-lice//")
#   )
# ),
# NOTE TO SELF - THE STUDY MAP USED THE OLD LOCATION DATA, IT NEEDS TO BE CHANGED
# tar_target(
#   study_map,
#   make_sampling_map(
#     farm_locations = get_data_csv(farm_locations),
#     kx_sampling = get_data_csv(kx_sampling),
#     geo_data = readRDS(geo_spatial),
#     output_path = here::here("./figs/maps//"),
#     farm_path = here::here("./data/farm-lice/clean//")
#   )
# ),
# tar_target(
#   yearly_nonexposure_maps_chum,
#   make_nonexposure_yearly_maps(
#     sr_pop_data = clean_chum_spawner_recruit_data,
#     sr_pop_sites = clean_wild_pop_location_data,
#     large_land = readRDS(large_land),
#     farm_data = clean_farm_lice_data,
#     farm_locs = clean_farm_locs,
#     network = qs::qread(network),
#     all_edges_nodes = readRDS(all_edges_nodes),
#     species = "Chum",
#     fig_output = here::here("./figs/maps/yearly-pop-maps/chum/no-exposure//"),
#     data_output = here::here("./data/spawner-recruit/clean//")
#   )
# ),
# tar_target(
#   yearly_popn_exposure_maps_pink,
#   make_yearly_popn_maps(
#     sr_pop_data = clean_pink_spawner_recruit_data,
#     sr_pop_sites = clean_wild_pop_location_data,
#     large_land = readRDS(large_land),
#     farm_data = clean_farm_lice_data,
#     farm_locs = clean_farm_locs,
#     network = qs::qread(network),
#     exposure_df = read_csv(pink_exposure_df),
#     all_edges_nodes = readRDS(all_edges_nodes),
#     species = "Pink",
#     fig_output = here::here("./figs/maps/yearly-pop-maps/pink//"),
#     data_output = here::here("./data/spawner-recruit/clean//")
#   )
# ),
# tar_target(
#   yearly_popn_exposure_maps_chum,
#   make_yearly_popn_maps(
#     sr_pop_data = clean_chum_spawner_recruit_data,
#     sr_pop_sites = clean_wild_pop_location_data,
#     large_land = readRDS(large_land),
#     farm_data = clean_farm_lice_data,
#     farm_locs = clean_farm_locs,
#     network = qs::qread(network),
#     exposure_df = read_csv(chum_exposure_df),
#     all_edges_nodes = readRDS(all_edges_nodes),
#     species = "Chum",
#     fig_output = here::here("./figs/maps/yearly-pop-maps/chum//")
#   )
# ),
# tar_target(
#   yearly_popn_exposure_large_maps_pink,
#   make_yearly_popn_maps(
#     sr_pop_data = clean_pink_spawner_recruit_data,
#     sr_pop_sites = clean_wild_pop_location_data,
#     large_land = readRDS(larger_land),
#     farm_data = clean_farm_lice_data,
#     farm_locs = clean_farm_locs,
#     network = qs::qread(network),
#     exposure_df = read_csv(pink_exposure_df),
#     all_edges_nodes = readRDS(all_edges_nodes),
#     species = "Pink",
#     fig_output = here::here("./figs/maps/yearly-pop-maps/pink/larger//"),
#     data_output = here::here("./data/spawner-recruit/clean//"),
#     size = "large"
#   )
# ),
# tar_target(
#   yearly_popn_exposure_large_maps_chum,
#   make_yearly_popn_maps(
#     sr_pop_data = clean_chum_spawner_recruit_data,
#     sr_pop_sites = clean_wild_pop_location_data,
#     large_land = readRDS(larger_land),
#     farm_data = clean_farm_lice_data,
#     farm_locs = clean_farm_locs,
#     network = qs::qread(network),
#     exposure_df = read_csv(chum_exposure_df),
#     all_edges_nodes = readRDS(all_edges_nodes),
#     species = "Chum",
#     fig_output = here::here("./figs/maps/yearly-pop-maps/chum/larger//"),
#     #data_output = here::here("./data/spawner-recruit/clean//"),
#     size = "large"
#   )
# )
## model fitting =============================================================
#' NOTE:
#' Here are represented only the final models for each species. Each sub-model
#' was also fit, but is not included in it's pipeline since they're
#' computationally intensive and run in parallel over a long period of time.
#' The code for these model fitting procedures can be found in the
#' R/manual-testing folder

list(
  file_targets,
  data_cleaning_targets,
  mapping_targets
)
