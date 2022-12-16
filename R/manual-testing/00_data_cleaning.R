##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This  file contains all manual testing for data cleaning and organization
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

library(readr)
library(here)
library(magrittr)
library(dplyr)
library(ggplot2)

# wild lice ==================================================================== 
wild_lice <- readr::read_csv(
  here::here("./data/wild-lice/raw/klemtu_wild_lice_data_CB.csv"),
  warnings=FALSE)

wild_lice <- wild_lice %>% 
  mutate(
    `Length (mm)` = ifelse(
      `Length (mm)` == "too decomposed", 
      NA,
      ifelse(
        `Length (mm)` == "1.9.", 
        1.9,
        `Length (mm)`
      )
    )
  )