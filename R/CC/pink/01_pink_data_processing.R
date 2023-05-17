##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-03-02
#'
#' This file contains the code to amalgamate and write out the data object that 
#' contains all of the simulation information. While this file itself is not run
#' on the compute canada servers, it is in this part of the repository since 
#' there are no targets associated with it, since it can't be 
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#' 

library(here)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)

# get the files from the directory
files <- list.files(here("./outputs/power-analysis/saved-runs/pink/"))

# set up the dataframe
df <- data.frame(
  c = as.numeric(),
  null_like = as.numeric(),
  alt_like = as.numeric(),
  p = as.numeric(),
  i = as.integer()
)

# loop through and append the files
for(f in files) {
  # read in file
  df_temp <- readr::read_csv(paste0(
    here("./outputs/power-analysis/saved-runs/pink/"), 
    f),
    show_col_types = FALSE) %>% 
    # make column of the i 
    dplyr::mutate(
      i = as.integer(
        # extract the i from the file name
        stringr::str_split(stringr::str_split(f, "c-matrix-")[[1]][2], 
                           ".csv")[[1]][1]
        )
    )
  # bind to df 
  df <- rbind(df, df_temp)
}

# write out file
readr::write_csv(
  df, 
  here("./outputs/power-analysis/pink-all-power-analysis-runs.csv")
)
