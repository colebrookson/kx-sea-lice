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
  #' @param output_path character. Where to save the actual work 
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

# clean_pk_sr_data ================================================================
clean_pk_sr_data <- function(sr_data, output_path) {
  #' Take in the raw spawner-recruit data and clean and write out the clean 
  #' version
  #' 
  #' @description Data needs to be renamed, cleaned up a bit, do this with this 
  #' one function 
  #' 
  #' @param sr_data file. the raw SR data
  #' @param output_path character. Path to where to save the plot
  #'  
  #' @usage clean_sr_data(sr_data, output_path)
  #' @return the clean sr data
  #' 
  
  # basic cleaning (renaming etc)
  all_pinks <- sr_data %>% 
    dplyr::rename(
      gfe_id = GFE_ID,
      brood_year = BroodYear,
      river = River,
      species = Species,
      indicator = Indicator,
      long = xLONG,
      lat = yLAT,
      area = StatArea,
      con_unit = CU,
      con_unit_2 = CU_2,
      spawners = Spawners,
      returns = Returns,
      recruits = Recruits
    ) %>% 
    dplyr::filter(
      species %in% c("PKO", "PKE")
    ) %>% 
    # get rid of NA's
    dplyr::filter_at(
      vars(spawners, returns), all_vars(!is.na(.))
    )
  
  # figure out how many observations per population there is
  pinks_even <- all_pinks %>% 
    dplyr::filter(species %in% c("PKE"))
  pinks_odd <- all_pinks %>% 
    dplyr::filter(species %in% c("PKO")) 
  
  all_pinks_obs_per_stream <- all_pinks %>% 
    dplyr::mutate(river = as.factor(river)) %>% 
    dplyr::group_by(river) %>% 
    dplyr::summarize(n = n()) 
  
  pke_obs_per_stream <- pinks_even %>% 
    dplyr::mutate(river = as.factor(river)) %>% 
    dplyr::group_by(river) %>% 
    dplyr::summarize(n = n())
  
  pko_obs_per_stream <- pinks_odd %>% 
    dplyr::mutate(river = as.factor(river)) %>% 
    dplyr::group_by(river) %>% 
    dplyr::summarize(n = n())
  
  min_obs <- min(summary(pke_obs_per_stream$n)[[2]], # [[2]] is 1st quartile
                 summary(pko_obs_per_stream$n)[[2]])
  
  # find the streams to exclude
  pke_streams_to_exclude <- pke_obs_per_stream %>% 
    dplyr::filter(
      n < min_obs
    ) 
  pko_streams_to_exclude <- pko_obs_per_stream %>% 
    dplyr::filter(
      n < min_obs
    ) 
  
  # note that this could be condensed but it's handy to have a more easily 
  # accessible list of all the streams that we're keeping and the 
  # number of observations at each stream 
  streams_to_keep <- all_pinks_obs_per_stream %>% 
    dplyr::filter(
      river %notin% c(pke_streams_to_exclude$river, 
                      pko_streams_to_exclude$river)
    )
  readr::write_csv(
    streams_to_keep,
    paste0(output_path, "pink-obs-per-stream.csv")
  )
  
  # make the dataframe to move on with 
  all_pinks_rivers <- all_pinks %>% 
    dplyr::filter(
      river %in% streams_to_keep$river
    ) %>% 
    dplyr::arrange(
      brood_year
    )
  readr::write_csv(
    all_pinks_rivers,
    paste0(output_path, "pink-sr-data-clean.csv")
  )
  
  return(all_pinks_rivers)
}
