##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This file contains all functions to load, and clean data for this project
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# clean_wild_lice ==============================================================
clean_wild_lice <- function(wild_lice, output_path) {
  #' Takes in object, and sorts thru the file to fix any errors that are present
  #' 
  #' @description First deals with the incorrect types that aren't expected 
  #' 
  #' @param wild_lice data frame. The file at hand
  #'  
  #' @usage 
  #' @return Dataframe of all lice data 
  #' 
  
  # get rid of the problems 
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
  
  # standardize names 
  wild_lice <- standardize_names(wild_lice)
  
  readr::write_csv(
    wild_lice,
    paste0(
      output_path, "wild_lice_df.csv"
    )
  )
  
  return(wild_lice)
  
}

# plot_wild_lice ===============================================================
plot_wild_lice_data <- function(wild_lice, output_path) {
  #' Preliminary plot of the wild lice data 
  #' 
  #' @description Prior to fitting models, make plots of the wild lice data, 
  #' across years 
  #' 
  #' @param wild_lice data frame. Cleaned data frame of the wild lice data
  #' @param output_path character. Path to where to save the plot
  #'  
  #' @usage plot_wild_lice_data(wild_lice, output_path)
  #' @return None
  #' 
  
  # make variables necessary into factors 
  wild_lice_grouped <- wild_lice %>% 
    # get rid of the unknown year lice 
    dplyr::filter(!is.na(year)) %>% 
    dplyr::mutate(
      year_fac = as.factor(year)
    ) %>% 
    dplyr::group_by(year_fac) %>% 
    dplyr::summarize(
      mean_lice = mean(lep_total, na.rm = TRUE),
      lice_se = std_err(lep_total), 
      n_obs = n()
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      up_ci = mean_lice + 1.96*lice_se,
      lo_ci = mean_lice - 1.96*lice_se
    )
  
  # make the plot and save
  ggplot2::ggsave(
    # output path
    paste0(output_path, "wild-lice-per-fish-per-year.png"),
    # plot object
    ggplot2::ggplot(data = wild_lice_grouped) + 
    geom_errorbar(aes(x = year_fac, ymax = up_ci, ymin = lo_ci),
                  width = 0, size = 1) +
    geom_point(aes(x = year_fac, y = mean_lice, fill = n_obs), 
               colour = "black", shape = 21, size = 4) + 
    ggthemes::theme_base() + 
    theme(
      panel.border = element_rect(colour = "black", size = 1.1),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = c(0.2, 0.7)
    ) + 
    scale_fill_gradientn("No. of Obs", 
                         colours = wesanderson::wes_palette("Zissou1", 100, 
                                               type = "continuous")) + 
    labs(x = "Year", y = "Average Lice per Fish"),
    # size of the output
    height = 5, width = 5.8
  )
}

# clean_psf_data ===============================================================