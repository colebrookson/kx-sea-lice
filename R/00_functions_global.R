##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This targets file contains all functions that don't relate to a specific 
#' part of the analysis, but are required to perform general tasks
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# %notin% ======================================================================
`%notin%` = Negate(`%in%`)

# get_data =====================================================================
get_data_csv = function(file) {
  #' Takes in the file and reads it for use
  #' 
  #' @description Generic function to interface with the file targets, to read 
  #' them in and make them available for analysis
  #' 
  #' @param file character. The file path in the target
  #'  
  #' @usage get_data_csv(here("./data/wild-lice/file.csv"))
  #' @return Dataframe 
  #' 
  
  readr::read_csv(file, show_col_types = FALSE) 
}

# standardize_names ============================================================
standardize_names = function(df) {
  #' Standardizes column names in a dataframe
  #' 
  #' @description Generic function to take in some set of names and make them 
  #' consistent and readable
  #' 
  #' @param df dataFrame. The data at hand
  #'  
  #' @usage standardize_names(df)
  #' @return df, but with new names 
  #'
  # get current set of names
  current_names = names(df)
  
  # loop through, pull the name out, change " " to "_"
  for(name in seq_len(length(current_names))) {
    current_names[name] = gsub("\\ ", "_", current_names[name])
  }
  
  # check for any upper case letters and make those lower_case
  current_names = tolower(current_names)
  
  # remove brackets
  for(name in seq_len(length(current_names))) {
    current_names[name] = gsub("\\(", "", current_names[name])
    current_names[name] = gsub("\\)", "", current_names[name])
  }
  
  # rename the dataframe
  names(df) = current_names
  
  #return dataframe renamed 
  return(df)
}

# std_err ======================================================================
std_err <- function(x) {
  #' Calculate standard error
  #' 
  #' @description Easy add on function to calculate standard error 
  #' 
  #' @param x vector of values
  #'  
  #' @usage std_err(df, na.rm = TRUE)
  #' @return numeric value
  #'
  
  return(sd(x, na.rm = TRUE) / sqrt(length(x)))
}
