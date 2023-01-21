##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-12-12
#'
#' This file contains the functions that run regressions on the wild and farmed
#' lice data 
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# lice_regression ==============================================================
lice_regression <- function(wild_lice, farm_lice, mod_output_path, 
                            plot_output_path) {
  #' Perform regression on wild and farm lice
  #' 
  #' @description Perform regression against the mean values of lice on farmed
  #' fish vs on wild lice with the data being at the level of year
  #' 
  #' @param wild_lice dataframe. Cleaned data of lice on wild fish
  #' @param farm_lice dataframe. Cleaned data of lice on farmed fish
  #' @param mod_output_path character. Where to save the model object and 
  #' associated output from the model.
  #' @param plot_output_path character. Where to save the figures created from 
  #' the models
  #'  
  #' @usage lice_regression(wild_lice, farm_lice, mod_output_path)
  #' @return model object
  #'
  
  # prep data ==================================================================
  
  ## all sites included ========================================================
  
  reg_wild_lice <- wild_lice %>%
    dplyr::mutate(
      year = as.factor(year)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(wild_lice = mean(lep_total, na.rm = TRUE))
  
  reg_farm_lice <- data.frame(farm_lice) %>%
    dplyr::mutate(
      year = as.numeric(year)
    ) %>% 
    dplyr::filter(year >= 2005) %>%
    dplyr::mutate(
      year = as.factor(year)
    ) %>% 
    dplyr::group_by(year) %>%
    dplyr::summarize(farm_lice = mean(total_lice, na.rm = TRUE))

  reg_data <- data.frame(cbind(
    reg_wild_lice, farm_lice = reg_farm_lice$farm_lice
  ))
  
  #' We want to try excluding a couple of sites and seeing if it changes the 
  #' results of the regression. We'll exclude the site "Upper Laredo", and then
  #' we'll exclude both Laredo sites. 
  
  ## exclude upper laredo ======================================================
  reg_wild_lice_up_lar <- wild_lice %>%
    dplyr::filter(site != "Upper Laredo") %>% 
    dplyr::mutate(
      year = as.factor(year)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(wild_lice = mean(lep_total, na.rm = TRUE))
  
  reg_data_up_lar <- data.frame(cbind(
    reg_wild_lice_up_lar, farm_lice = reg_farm_lice$farm_lice
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
    reg_wild_lice_both_lar, farm_lice = reg_farm_lice$farm_lice
  ))

  # regression =================================================================

  ## all sites included ========================================================
  model <- stats::lm(
    wild_lice ~ farm_lice,
    data = reg_data
  )
  
  # extract vals of use
  coefs = broom::tidy(model)
  fitted_vals = broom::augment(model)
  model_vals = broom::glance(model)
  
  saveRDS(model, paste0(mod_output_path, "regression-ob.rds"))
  readr::write_csv(coefs,
                   paste0(mod_output_path, "coeffs-regression.rds"))
  readr::write_csv(fitted_vals,
                   paste0(mod_output_path, "fitted-vals-regression.rds"))
  readr::write_csv(model_vals,
                   paste0(mod_output_path, "model-vals-regression.rds"))
  
  ggplot2::ggsave(
    
    # output path
    paste0(plot_output_path, "regression-plot.png"),
    
    # make the actual plot
    ggplot(data = reg_data, aes(x = farm_lice, y = wild_lice)) +
      geom_point(
        # use the shape and colour to denote which are which
        fill = "30D5C8", shape = 21,
        colour = "black", size = 5) +
      stat_smooth(method = stats::lm, formula = y ~ x,
                  colour = "black", alpha = 0.2) +
      ggthemes::theme_base()  +
      labs(x = "Lice on Farmed Fish (millions)",
           y = "Mean Lice per Wild Fish (All Sites)") +
      scale_x_continuous(breaks = c(0, 500000, 1000000, 1500000, 2000000),
                         labels = c("0", "0.5", "1.0", "1.5", "2.0")) +
      annotate(geom = "text", 
               x = min(reg_data$farm_lice, na.rm = TRUE), 
               y = max(reg_data$wild_lice, na.rm = TRUE), 
               label = paste("R^2 ==", round(model_vals$r.squared, 2)),
               parse = TRUE,
               size = 7,
               hjust = -0.5,
               vjust = 2)
  )
  
  ## upper laredo excluded =====================================================
  model_up_lar <- stats::lm(
    wild_lice ~ farm_lice,
    data = reg_data_up_lar
  )
  
  # extract vals of use
  coefs_up_lar = broom::tidy(model_up_lar)
  fitted_vals_up_lar = broom::augment(model_up_lar)
  model_vals_up_lar = broom::glance(model_up_lar)
  
  saveRDS(model_up_lar, paste0(mod_output_path, 
                               "regression-ob-upper-laredo-excl.rds"))
  readr::write_csv(coefs_up_lar,
                   paste0(mod_output_path, 
                          "coeffs-regression-upper-laredo-excl.rds"))
  readr::write_csv(fitted_vals_up_lar,
                   paste0(mod_output_path, 
                          "fitted-vals-regression-upper-laredo-excl.rds"))
  readr::write_csv(model_vals_up_lar,
                   paste0(mod_output_path, 
                          "model-vals-regression-upper-laredo-excl.rds"))
  
  ggplot2::ggsave(
    
    # output path
    paste0(plot_output_path, "regression-plot-upper-laredo-excl.png"),
    
    # make the actual plot
    ggplot(data = reg_data_up_lar, aes(x = farm_lice, y = wild_lice)) +
      geom_point(
        # use the shape and colour to denote which are which
        fill = "30D5C8", shape = 21,
        colour = "black", size = 5) +
      stat_smooth(method = stats::lm, formula = y ~ x,
                  colour = "black", alpha = 0.2) +
      ggthemes::theme_base()  +
      labs(x = "Lice on Farmed Fish (millions)",
           y = "Mean Lice per Wild Fish (Upper Laredo Excluded)") +
      scale_x_continuous(breaks = c(0, 500000, 1000000, 1500000, 2000000),
                         labels = c("0", "0.5", "1.0", "1.5", "2.0")) +
      annotate(geom = "text", 
               x = min(reg_data$farm_lice, na.rm = TRUE), 
               y = max(reg_data$wild_lice, na.rm = TRUE), 
               label = paste("R^2 ==", round(model_vals_up_lar$r.squared, 2)),
               parse = TRUE,
               size = 7,
               hjust = -0.5,
               vjust = 2)
  )
  

  ## both laredos excluded =====================================================
  model_both_lar <- stats::lm(
    wild_lice ~ farm_lice,
    data = reg_data_up_lar
  )
  
  # extract vals of use
  coefs_both_lar = broom::tidy(model_both_lar)
  fitted_vals_both_lar = broom::augment(model_both_lar)
  model_vals_both_lar = broom::glance(model_both_lar)
  
  saveRDS(model_both_lar, paste0(mod_output_path, 
                               "regression-ob-both-laredos-excl.rds"))
  readr::write_csv(coefs_both_lar,
                   paste0(mod_output_path, 
                          "coeffs-regression-both-laredos-excl.rds"))
  readr::write_csv(fitted_vals_both_lar,
                   paste0(mod_output_path, 
                          "fitted-vals-regression-both-laredos-excl.rds"))
  readr::write_csv(model_vals_both_lar,
                   paste0(mod_output_path, 
                          "model-vals-regression-both-laredos-excl.rds"))
  
  ggplot2::ggsave(
    
    # output path
    paste0(plot_output_path, "regression-plot-both-laredo-excl.png"),
    
    # make the actual plot
    ggplot(data = reg_data_both_lar, aes(x = farm_lice, y = wild_lice)) +
      geom_point(
        # use the shape and colour to denote which are which
        fill = "30D5C8", shape = 21,
        colour = "black", size = 5) +
      stat_smooth(method = stats::lm, formula = y ~ x,
                  colour = "black", alpha = 0.2) +
      ggthemes::theme_base()  +
      labs(x = "Lice on Farmed Fish (millions)",
           y = "Mean Lice per Wild Fish (Both Laredos Excluded)") +
      scale_x_continuous(breaks = c(0, 500000, 1000000, 1500000, 2000000),
                         labels = c("0", "0.5", "1.0", "1.5", "2.0")) +
      annotate(geom = "text", 
               x = min(reg_data$farm_lice, na.rm = TRUE), 
               y = max(reg_data$wild_lice, na.rm = TRUE), 
               label = paste("R^2 ==", round(model_vals_both_lar$r.squared, 2)),
               parse = TRUE,
               size = 7,
               hjust = -0.5,
               vjust = 2)
  )

  # log regression =============================================================

  model_log <- stats::lm(
    log10(wild_lice) ~ log10(farm_lice),
    data = reg_data
  )

  # extract vals of use
  log_coefs = broom::tidy(model_log)
  log_fitted_vals = broom::augment(model_log)
  log_model_vals = broom::glance(model_log)

  saveRDS(model_log, paste0(mod_output_path, "log-regression-ob.rds"))
  readr::write_csv(log_coefs,
                   paste0(mod_output_path, "log-coeffs-regression.rds"))
  readr::write_csv(log_fitted_vals,
                   paste0(mod_output_path, "log-fitted-vals-regression.rds"))
  readr::write_csv(log_model_vals,
                   paste0(mod_output_path, "log-model-vals-regression.rds"))

  ggplot2::ggsave(

    # output path
    paste0(plot_output_path, "log-regression-plot.png"),

    # make the plot
    ggplot(data = reg_data, aes(x = log10(farm_lice), y = log10(wild_lice))) +
      geom_point(
        # use the shape and colour to denote which are which
        fill = "30D5C8", shape = 21,
        colour = "black", size = 5) +
      stat_smooth(method = stats::lm, formula = y ~ x,
                  colour = "black", alpha = 0.2) +
      ggthemes::theme_base() +
      labs(x = "Lice on Farmed Fish (Log 10)",
           y = "Mean Number of Lice per Wild Juvenile Fish (Log 10)") +
      annotate(geom = "text", 
               x = min(log10(reg_data$farm_lice), na.rm = TRUE), 
               y = max(log10(reg_data$wild_lice), na.rm = TRUE), 
               label = paste("R^2 ==", round(log_model_vals$r.squared, 2)),
               parse = TRUE,
               size = 7,
               hjust = -0.5,
               vjust = 2)

  )
}

