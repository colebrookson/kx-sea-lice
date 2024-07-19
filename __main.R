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
#'      1. data on sea lice on wild fish
#'      2. data on sea lice on farmed fish
#'      3. stock-recruit data for wild chum and pink salmon
#'      4. geospatial data on where the various fish stocks are

source(here::here("./src/R/00_data_cleaning.R"))
