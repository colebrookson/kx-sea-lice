##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2024-08-19
#'
#' Central location for all the code (in order)
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

# Clean data ===================================================================

#' There are a handful of major data components over a few different sources
#' that are cleaned here:
#'      1. data on sea lice on wild fish -- `wild_lice`
#'      2. data on sea lice on farmed fish -- `farm_lice`
#'      3. stock-recruit data for wild chum and pink salmon
#'             `all_pinks_rivers`
#'             `all_chums_rivers`
#'      4. geospatial data on stock locations -- `sr_pop_sites`

source(here::here("./src/R/00_data_cleaning.R"))
