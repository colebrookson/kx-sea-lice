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
clean_wild_lice <- function(raw_wild_lice, dates_to_join, raw_output_path, 
                            clean_output_path, fig_output_path) {
  #' Clean up wild lice data from the manually edited .csv file
  #' 
  #' @description Take the csv file that was manually edited, make some cleaning
  #' actions, make a plot of the number of observations in a given year, and
  #' save both the new dataframe and also the plot 
  #' 
  #' @param raw_wild_lice file. The raw data file
  #' @param dates_to_join file. The previously (manually) edited file with all
  #' the info of the seine_date cleaning stuff
  #' @param raw_output_path character. Path to where to save the dataframe with 
  #' the unique values to do the manual cleaning 
  #' @param clean_output_path character. Where to save the clean dataframe
  #' @param fig_output_path character. Path to where to save the figure
  #'  
  #' @usage clean_wild_lice(raw_wild_lice, output_path, fig_output_path)
  #' @return clean df of the wild lice info
  #'
  
  wild_lice_clean <- raw_wild_lice %>% 
    dplyr::mutate(seine_date = as.factor(seine_date)) %>% 
    dplyr::select(
      year, seine_date, site, zone, fish_spp, lep_co, lep_c1, lep_c2, lep_c3, 
      lep_c4, lep_pam, lep_paf, lep_am, lep_af,lep_total, cal_total, lat, long
    ) 
  
  # figure out the seine_date situation by year 
  wild_lice_dates <- wild_lice_clean %>% 
    dplyr::select(year, seine_date) %>% 
    unique()
  readr::write_csv(
    wild_lice_dates,
    paste0(raw_output_path, "unique_dates_for_manual_edit.csv")
  )
  
  # important check to make sure no additional manual cleaning needs to happen
  dates_to_join <- dates_to_join %>% 
    dplyr::mutate(seine_date = as.factor(seine_date)) %>% 
    dplyr::filter(
      seine_date %notin% c("no date", "NO INFO", "no paper", "none", "Unknown",
                           NA, "?")
    )
  wild_lice_to_join <- wild_lice_clean %>% 
    dplyr::select(-year) %>% 
    dplyr::filter(
      seine_date %notin% c("no date", "NO INFO", "no paper", "none", "Unknown",
                           NA, "?")
    )
  if(all(sort(unique(dates_to_join$seine_date)) != 
     sort(unique(wild_lice_to_join$seine_date)))) {
    stop("ERROR - seine dates don't match up, manual cleaning process likely
         needed for new seine dates")
  }
  
  # perform a join to get columns for year, month, day
  wild_lice_clean_dates_fixed <- dplyr::left_join(
      wild_lice_to_join,
      dates_to_join,
      by = "seine_date"
    ) 
  
  wild_lice_to_save = wild_lice_clean_dates_fixed%>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      lep_motiles = sum(lep_pam, lep_paf, lep_am, lep_af, na.rm = TRUE),
      lep_chal = sum(lep_c1, lep_c2, lep_c3, lep_c4, na.rm = TRUE)
    ) %>% 
    dplyr::select(-c(lep_pam, lep_paf, lep_am, lep_af,
                     lep_c1, lep_c2, lep_c3, lep_c4))
  
  # make dataframe to plot
  obs_per_year <- wild_lice_clean_dates_fixed %>% 
    dplyr::mutate(year = as.factor(year)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(n = dplyr::n())
  
  # sampling per year plot 
  ggplot2::ggsave(
    # output path 
    paste0(fig_output_path, "number-of-obs-per-year.png"),
    # plot
    ggplot(data = obs_per_year) + 
      geom_col(aes(x = year, y = n, fill = n),
               colour = "black") + 
      scale_x_discrete() +
      scale_fill_gradientn("No. of Obs", 
                           colours = 
                             wesanderson::wes_palette("Zissou1", 100,
                                                      type = "continuous"))+
      ggthemes::theme_base() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
      ) + 
      labs(x = "Year", y = "Number of Individual Fish Observed")
  )
  
  # sampling per month 
  per_month_sampling <- wild_lice_clean_dates_fixed %>% 
    dplyr::group_by(month) %>% 
    dplyr::summarize(n = dplyr::n()) %>% 
    dplyr::mutate(
      month_char = factor(dplyr::case_when(
        month == 4 ~ "April", 
        month == 5 ~   "May", 
        month == 6 ~  "June",
        month == 7 ~   "July"
      ), levels = c("April", "May", "June", "July"))
    )
  
  ggplot2::ggsave(
    # output path 
    paste0(fig_output_path, "number-of-obs-per-month.png"),
    # plot
    ggplot(data = per_month_sampling) + 
      geom_col(aes(x = month_char, y = n, fill = n),
               colour = "black") + 
      ggthemes::theme_base() +
      scale_fill_gradientn("No. of Obs", 
                           colours = wesanderson::wes_palette(
                             "Zissou1", 8, type = "continuous")) + 
      labs(x = "Month", y = "Total Observations Across Years") + 
      theme(
        legend.position = "none"
      )
  )

  
  # write out clean data
  readr::write_csv(
    wild_lice_to_save,
    paste0(
      clean_output_path, "clean-wild-lice-df.csv"
    )
  )
  
  # return the clean dataframe of wild lice
  return(wild_lice_to_save)
}

# plot_wild_lice ===============================================================
plot_wild_lice_data <- function(wild_lice, output_path) {
  #' Preliminary plot of the wild lice data 
  #' 
  #' @description Prior to fitting models, make plots of the wild lice data, 
  #' across years. Look at this for all of the possible groupings. Possible 
  #' groupings are 1) All lice, 2) All leps, 3) All motile leps
  #' 
  #' @param wild_lice data frame. Cleaned data frame of the wild lice data
  #' @param output_path character. Path to where to save the plot
  #'  
  #' @usage plot_wild_lice_data(wild_lice, output_path)
  #' @return None
  #' 
  
  # all leps scenario 
  wild_lice_grouped <- wild_lice %>% 
    dplyr::filter(!is.na(year)) %>% # get rid of the unknown year lice 
    dplyr::mutate(
      year_fac = as.factor(year)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      all_lice = sum(lep_total + cal_total, na.rm = TRUE)
    ) %>% 
    dplyr::select(year_fac, all_lice, lep_motiles, lep_total) %>% 
    tidyr::pivot_longer(
      cols = !year_fac,
      names_to = "lice_group",
      values_to = "count"
    ) %>% 
    dplyr::group_by(year_fac, lice_group) %>% 
    dplyr::summarize(
      mean = mean(count, na.rm = TRUE),
      se = std_err(count)
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      up_ci = mean + (1.96 * se),
      lo_ci = mean - (1.96 * se),
    )
  
  # make the plot and save
  ggplot2::ggsave(
    # output path
    paste0(output_path, "wild-lice-per-fish-per-year.png"),

    # plot object
    ggplot2::ggplot(data = wild_lice_grouped) +
    geom_errorbar(aes(x = year_fac, ymin = lo_ci, ymax = up_ci,
                      group = lice_group),
                  width = 0, size = 1, colour = "black",
                  position = position_dodge(width = 0.8)) +
    geom_point(aes(x = year_fac, y = mean, fill = lice_group),
               colour = "black", shape = 21, size = 4,
               position = position_dodge(width = 0.8)) +
    ggthemes::theme_base() +
    labs(
      x = "Year", y = "Average Lice per Fish (with 95% CI)"
    ) +
    scale_fill_manual("Lice Groupings",
                      values = c("#469990", "#000075", "#42d4f4"),
                      labels = c("All Lice", "Motile Leps", "All Leps")) +
    theme(
      panel.border = element_rect(colour = "black", size = 1.1),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = c(0.2, 0.7)
    ),

    # size arguments
    width = 8,
    height = 5.5
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

# clean_farm_loc_data ==========================================================
clean_farm_loc_data <- function(farm_loc_data) {
  #' Take in the raw spawner-recruit data and clean and write out the clean 
  #' version
  #' 
  #' @description Data needs to be renamed, cleaned up a bit, do this with this 
  #' one function 
  #' 
  #' @param farm_loc_data file. Data on the locations of the different farms
  #'  
  #' @usage clean_farm_loc_data(farm_loc_data)
  #' @return the clean sr data
  #' 
}

# clean_farm_lice ==============================================================
clean_farm_lice <- function(old_lice, new_lice, data_output_path, 
                            fig_output_path) {
  #' Clean the data on farm lice from two sources
  #' 
  #' @description The lice on farmed fish data are in two separate files, so 
  #' this function, reads them in and cleans each of them in turn, binds them
  #' together, and makes a figure of lice and inventory through time  
  #' 
  #' @param old_lice file. Lice data on farms from 2005 to part of 2019
  #' @param new_lice file. Lice data on farms from the last part of 2019 through
  #' 2022 summer
  #' @param data_output_path character. Where to save the cleaned data file
  #' @param fig_output_path character. Where to save the figure of inventory and 
  #' lice data 
  #'  
  #' @usage clean_farm_lice(old_lice, new_lice, data_output_path, 
  #' fig_output_path)
  #' @return the clean farmed lice data
  #' 
  
  # old lice ===================================================================
  
  # remove obs with NAs in the date column 
  old_lice <- old_lice %>% 
    dplyr::filter(!is.na(DATE))
  
  # since it's an excel sheet it read it in oddly, so now we need to transfer  
  # data back to a readable structure 
  old_lice$DATE <- janitor::excel_numeric_to_date(
    as.numeric(
      as.character(old_lice$DATE)), 
    date_system = "modern") 
  
  # get rid of NA's and add in the appropriate columns 
  old_lice <- old_lice %>% 
    dplyr::filter(
      !is.na(DATE)
    ) %>% 
    dplyr::mutate(
      day = lubridate::day(DATE),
      month = lubridate::month(DATE),
      year = lubridate::year(DATE)
    )
  
  # standardize names
  old_lice <- standardize_names(old_lice) %>% 
    # needs some custom renames too
    dplyr::rename(
      adult_fem_no_egg = `adult_female_w/o_eggs`,
      adult_fem_w_egg = `adult_female____with_eggs`,
      num_fish_sampled = `#_of_fish_sampled`
    )
  
  # there's one specific problem, a space in the character vector of the 
  # inventories, so I'll change that first
  old_lice[which(old_lice$site_inventory == 
                   "846 012"), "site_inventory"] <- "846012"
  
  old_lice_trim <- old_lice %>% 
    dplyr::select(year, month, farm, adult_fem_no_egg, 
                  adult_fem_w_egg, site_inventory) %>% 
    dplyr::mutate(
      year = as.factor(year), 
      month = as.factor(month),
      farm = as.factor(farm),
      inventory = as.numeric(as.integer(site_inventory))
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      lice = sum(adult_fem_no_egg, 
                 adult_fem_w_egg, na.rm = TRUE)
    ) %>% 
    dplyr::group_by(year, month, farm) %>% 
    dplyr::summarize(
      mean_inventory = mean(inventory, na.rm = TRUE), 
      lice = mean(lice, na.rm = TRUE)
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      total_lice = lice * mean_inventory
    ) %>% 
    dplyr::mutate(
      day = 01,
      year = as.integer(as.character(year)),
      month = as.integer(as.character(month))
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      date = lubridate::make_date(year = as.integer(year), 
                                  month = as.integer(month),
                                  day = day)
    ) %>% 
    dplyr::arrange(date)
  
  # new lice ===================================================================
  
  new_lice <- new_lice %>% 
    dplyr::mutate(
      day = lubridate::day(DATE),
      month = lubridate::month(DATE),
      year = lubridate::year(DATE)
    ) %>% 
    standardize_names(.) %>% 
    # needs some custom renames too
    dplyr::rename(
      adult_fem_no_egg = `adult_female_w/o_eggs`,
      adult_fem_w_egg = `adult_female____with_eggs`,
      num_fish_sampled = `#_of_fish_sampled`
    )
  
  new_lice_trim <- new_lice %>% 
    dplyr::select(year, month, farm, adult_fem_no_egg, 
                  adult_fem_w_egg, site_inventory) %>% 
    dplyr::mutate(
      year = as.factor(year), 
      month = as.factor(month),
      farm = as.factor(farm),
      inventory = as.numeric(as.integer(site_inventory))
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      lice = sum(adult_fem_no_egg, 
                 adult_fem_w_egg, na.rm = TRUE)
    )   %>% 
    dplyr::group_by(year, month, farm) %>% 
    dplyr::summarize(
      mean_inventory = mean(inventory, na.rm = TRUE), 
      lice = mean(lice, na.rm = TRUE)
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      total_lice = lice * mean_inventory
    ) %>% 
    dplyr::mutate(
      day = 01,
      year = as.integer(as.character(year)),
      month = as.integer(as.character(month))
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      date = lubridate::make_date(year = as.integer(year), 
                                  month = as.integer(month),
                                  day = day)
    ) %>% 
    dplyr::arrange(date)
  
  # join lice ==================================================================
  
  all_lice <- rbind(old_lice_trim, new_lice_trim) 
  
  all_lice_by_date <- all_lice %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarize(
      Inventory = mean(mean_inventory, na.rm = TRUE),
      Lice = mean(total_lice, na.rm = TRUE)
    ) %>% 
    dplyr::filter(!is.na(Inventory))
  
  all_lice_both_measures <- all_lice_by_date %>% 
    tidyr::pivot_longer(
      cols = !date, 
      values_to = "vals"
    ) %>% 
    dplyr::mutate(
      Measurement = as.factor(name)
    )
  
  readr::write_csv(
    all_lice,
    paste0(data_output_path, "clean-farm-lice-df.csv")
  )
  
  # figure =====================================================================
  ggplot2::ggsave(
    
    # output path
    paste0(fig_output_path, "inventory-lice-plot.png"),
    
    # make the plot
    ggplot(data = all_lice_both_measures) + 
      geom_line(aes(x = date, y = vals, colour = Measurement, 
                    linetype = Measurement)) +
      geom_point(aes(x = date, y = vals, fill = Measurement), colour = "black",
                 shape = 21) +
      ggthemes::theme_base() + 
      scale_fill_manual(values = c("goldenrod1", "purple2")) + 
      scale_colour_manual(values = c("goldenrod1", "purple2")) + 
      scale_x_date(date_breaks = "1 years") +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5)
      ) + 
      labs(x = "Date",  y = "Values")
  )
  
  return(data.frame(all_lice))
}






