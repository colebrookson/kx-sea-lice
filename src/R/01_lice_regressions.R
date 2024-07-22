##' All regressions for wild lice vs farm lice
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
source(here::here("./src/R/functions/02_functions_lice_v_lice_regression.R"))

# normal data frame ============================================================




## exclude upper laredo ======================================================
reg_wild_lice_up_lar <- wild_lice %>%
    dplyr::filter(site != "Upper Laredo") %>%
    dplyr::mutate(
        year = as.factor(year)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(wild_lice = mean(lep_total, na.rm = TRUE))

reg_data_up_lar <- data.frame(cbind(
    reg_wild_lice_up_lar,
    farm_lice = reg_farm_lice$farm_lice
))

## exclude both laredo sites =================================================
reg_wild_lice_both_lar <- wild_lice %>%
    dplyr::filter(site %notin% c("Lower Laredo", "Upper Laredo")) %>%
    dplyr::mutate(
        year = as.factor(year)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(wild_lice = mean(lep_total, na.rm = TRUE))

reg_data_both_lar <- data.frame(cbind(
    reg_wild_lice_both_lar,
    farm_lice = reg_farm_lice$farm_lice
))
