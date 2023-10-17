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
source(here::here("./R/02_functions_lice_v_lice_regression.R"))
source(here::here("./R/03_functions_wild_lice_regression.R"))
source(here::here("./R/04_functions_mapping.R"))
source(here::here("./R/05_functions_power_analysis.R"))

tar_option_set(packages = c("here", "readr", "magrittr", "dplyr", "ggplot2", 
                            "ggthemes", "wesanderson", "lubridate", "janitor",
                            "tibble", "ggrepel", "sp", "glmmTMB", "sf", 
                            "sfnetworks", "qs", "tidygraph"))
options(dplyr.summarise.inform = FALSE)

list(
  ## files =====================================================================
  tar_target(
    raw_wild_lice_data,
    here::here("./data/wild-lice/raw/klemtu_wild_lice_data_CB.csv"),
    format = "file"
  ),
  tar_target(
    dates_to_join,
    here::here("./data/wild-lice/raw/unique_dates_manually_edited.csv"),
    format = "file"
  ),
  tar_target(
    sr_data,
    here::here(
      paste0(
        "./data/spawner-recruit/raw/river-level-sr/",
        "NCC_streams_river-level_SR_data_2023-04-19.csv"
      )
      ),
    format = "file"
  ),
  tar_target(
    farm_locations,
    here::here("./data/farm-lice/raw/farm_location_metadata.csv"),
    format = "file"
  ),
  tar_target(
    kx_sampling,
    here::here("./data/wild-lice/raw/kitasoo_sampling_sites.csv"),
    format = "file"
  ),
  tar_target(
    geo_spatial,
    here::here("./data/geo-spatial/gadm36_CAN_1_sp.rds"),
    format = "file"
  ),
  tar_target(
    old_farm_lice,
    here::here("./data/farm-lice/raw/klemtu_farm_lice_data_old.xls"),
    format = "file"
  ),
  tar_target(
    new_farm_lice,
    here::here("./data/farm-lice/raw/klemtu_farm_lice_data_new.xlsx"),
    format = "file"
  ),
  tar_target(
    sr_pop_sites_raw,
    here::here("./data/spawner-recruit/raw/conservation_unit_system_site.csv"),
    format = "file"
  ),
  tar_target(
    all_power_sims,
    here::here("./outputs/power-analysis/pink-all-power-analysis-runs.csv"),
    format = "file"
  ),
  tar_target(
    pink_exposure_df,
    here::here(
      "./data/spawner-recruit/clean/pink-exposure-categorization-df.csv"),
    format = "file"
  ),
  ## geo-data files ============================================================
  ### nodes to keep ============================================================
  tar_target(all_edges_nodes, 
            here::here("./outputs/geo-objs/fresh/all-edges-nodes-to-keep.rds")),
  ### networks =================================================================
  tar_target(network,
             here::here("./outputs/geo-objs/fresh/network.qs"),
             format = "file"),
  ### background area ==========================================================
  # tar_target(utm_geo_data, 
  #            here::here("./outputs/geo-objs/utm-geo-data.rds")),
  tar_target(large_land,
             here::here("./outputs/geo-objs/fresh/large-land-for-plotting.rds")),
  ## data cleaning =============================================================
  tar_target(
    clean_wild_lice_data,
    clean_wild_lice(
      raw_wild_lice = get_data_csv(raw_wild_lice_data),
      dates_to_join = get_data_csv(dates_to_join),
      raw_output_path = here::here("./data/wild-lice/raw//"),
      clean_output_path = here::here("./data/wild-lice/clean//"),
      fig_output_path = here::here("./figs/wild-lice//")
    )
  ),
  tar_target(
    clean_farm_lice_data,
    clean_farm_lice(
      old_lice = readxl::read_excel(old_farm_lice,
                                    sheet = 9),
      new_lice = readxl::read_excel(new_farm_lice,
                                    sheet = 5),
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
  ),
  ## lice regression ===========================================================
  tar_target(
    wild_farm_lice_regression,
    lice_regression(
      wild_lice = clean_wild_lice_data,
      farm_lice = clean_farm_lice_data,
      mod_output_path = here::here("./outputs/lice-regression//"),
      plot_output_path = here::here("./figs/regression//")
    )
  ),
  ## power analysis ============================================================
  #' NOTE:
  #' Nearly all of the power analysis proper is not included in this targets 
  #' pipeline. That is because the simulations required were set up to run on 
  #' Compute Canada Alliance servers, separate from this desktop-appropriate
  #' analysis. The files used to actually run that analysis can be found
  #' in the folder: `./R/CC/` and the outputs from that set of simulations are
  #' located in the folder `./outputs/power-analysis/saved-runs/` but all other
  #' analysis of those files, namely the summarizing and plotting of those data
  #' from the outputs are done in this target below
  tar_target(
    prep_pink_data_power_analysis,
    power_prep_pink(
      wild_lice = clean_wild_lice_data
    )
  ),
  tar_target(
    power_analysis_prep_pink,
    power_pink_mod(
      pred_yearly = prep_pink_data_power_analysis,
      pink_sr_df = clean_pink_spawner_recruit_data,
      output_path = here::here("./outputs/power-analysis//")
    )
  ),
  tar_target(
    power_analysis,
    plot_power(
      all_power_sims = get_data_csv(all_power_sims),
      output_path = here::here("./figs/power-analysis//")
    )
  ),
  ## useful plots/extra content ================================================
  tar_target(
    wild_lice_per_fish_plot,
    plot_wild_lice_data(
      wild_lice = clean_wild_lice_data,
      output_path = here::here("./figs/wild-lice//")
    )
  ),
  tar_target(
    study_map,
    make_sampling_map(
      farm_locations = get_data_csv(farm_locations),
      kx_sampling = get_data_csv(kx_sampling),
      geo_data = readRDS(geo_spatial),
      output_path = here::here("./figs/maps//"),
      farm_path = here::here("./data/farm-lice/clean//")
    )
  ),
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
      fig_output = here::here("./figs/maps/yearly-pop-maps/pink/no-exposure//")
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
      fig_output = here::here("./figs/maps/yearly-pop-maps/chum/no-exposure//")
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
      exposure_df = read_csv(tar_read(pink_exposure_df)),
      all_edges_nodes = readRDS(all_edges_nodes),
      species = "Pink",
      fig_output = here::here("./figs/maps/yearly-pop-maps/pink//"),
      data_output = here::here("./data/spawner-recruit/clean//")
    )
  )
  # tar_target(
  #   yearly_popn_exposure_maps_chum,
  #   make_yearly_popn_maps(
  #     sr_pop_data = clean_chum_spawner_recruit_data,
  #     sr_pop_sites = clean_wild_pop_location_data,
  #     large_land = readRDS(large_land),
  #     farm_data = clean_farm_lice_data,
  #     farm_locs = clean_farm_locs,
  #     network = qs::qread(network),
  #     exposure_df = read_csv(tar_read(exposure_df)),
  #     all_edges_nodes = readRDS(all_edges_nodes),
  #     fig_output = here::here("./figs/maps/yearly-pop-maps/chum/"),
  #     data_output = here::here("./data/spawner-recruit/clean/")
  #   )
  # )
  
  # tar_target(
  #   yearly_popn_exposure_maps,
  #   make_yearly_popn_maps(
  #     sr_pop_data = clean_pink_spawner_recruit_data,
  #     sr_pop_sites = get_data_csv(sr_pop_sites),
  #     utm_geo_data = readRDS(utm_geo_data),
  #     utm_land_data = readRDS(utm_land_data),
  #     farm_data = clean_farm_lice_data,
  #     farm_locs = clean_farm_locs,
  #     network = all_region_network,
  #     west_network = west_region_network,
  #     fig_output = here::here("./figs/maps/yearly-pop-maps/"),
  #     data_output = here::here("./data/spawner-recruit/clean/")
  #   )
  # )
)