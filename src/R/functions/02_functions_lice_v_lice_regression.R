##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-12-12
#'
#' This file contains the functions that run regressions on the wild and farmed
#' lice data
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

# lice_regression ==============================================================
lice_regression <- function(wild_lice, farm_lice, mod_output_path, name,
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
  #' @param name character. The name (indicating the subset data etc) that
  #' is being used for this model
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
    reg_wild_lice,
    farm_lice = reg_farm_lice$farm_lice
  ))

  #' We want to try excluding a couple of sites and seeing if it changes the
  #' results of the regression. We'll exclude the site "Upper Laredo", and then
  #' we'll exclude both Laredo sites.

  # regression =================================================================

  model <- stats::lm(
    wild_lice ~ farm_lice,
    data = reg_data
  )

  # extract vals of use
  coefs <- broom::tidy(model)
  fitted_vals <- broom::augment(model)
  model_vals <- broom::glance(model)

  saveRDS(model, paste0(mod_output_path, name, "-ob.rds"))
  readr::write_csv(
    coefs,
    paste0(mod_output_path, name, "-coeffs.rds")
  )
  readr::write_csv(
    fitted_vals,
    paste0(mod_output_path, name, "-fitted-vals.rds")
  )
  readr::write_csv(
    model_vals,
    paste0(mod_output_path, name, "-model-vals.rds")
  )

  ggplot2::ggsave(

    # output path
    paste0(plot_output_path, name, "-plot.png"),

    # make the actual plot
    ggplot(data = reg_data, aes(x = farm_lice, y = wild_lice)) +
      geom_point(
        # use the shape and colour to denote which are which
        fill = "30D5C8", shape = 21,
        colour = "black", size = 5
      ) +
      stat_smooth(
        method = stats::lm, formula = y ~ x,
        colour = "black", alpha = 0.2
      ) +
      ggthemes::theme_base() +
      labs(
        x = "Lice on Farmed Fish (millions)",
        y = "Mean Lice per Wild Fish (All Sites)"
      ) +
      scale_x_continuous(
        breaks = c(0, 500000, 1000000, 1500000, 2000000),
        labels = c("0", "0.5", "1.0", "1.5", "2.0")
      ) +
      annotate(
        geom = "text",
        x = min(reg_data$farm_lice, na.rm = TRUE),
        y = max(reg_data$wild_lice, na.rm = TRUE),
        label = paste("R^2 ==", round(model_vals$r.squared, 2)),
        parse = TRUE,
        size = 7,
        hjust = -0.5,
        vjust = 2
      )
  )

  # log regression =============================================================

  model_log <- stats::lm(
    log10(wild_lice) ~ log10(farm_lice),
    data = reg_data
  )

  # extract vals of use
  log_coefs <- broom::tidy(model_log)
  log_fitted_vals <- broom::augment(model_log)
  log_model_vals <- broom::glance(model_log)

  saveRDS(model_log, paste0(mod_output_path, name, "-log-ob.rds"))
  readr::write_csv(
    log_coefs,
    paste0(mod_output_path, name, "-log-coeffs.rds")
  )
  readr::write_csv(
    log_fitted_vals,
    paste0(mod_output_path, name, "-log-fitted-vals.rds")
  )
  readr::write_csv(
    log_model_vals,
    paste0(mod_output_path, name, "-log-model-vals.rds")
  )

  results_list <- list(
    "regular" = list(model, coefs, fitted_vals, model_vals),
    "log" = list(model_log, log_coefs, log_fitted_vals, log_model_vals)
  )

  ggplot2::ggsave(

    # output path
    paste0(plot_output_path, name, "-log-plot.png"),

    # make the plot
    ggplot(data = reg_data, aes(x = log10(farm_lice), y = log10(wild_lice))) +
      geom_point(
        # use the shape and colour to denote which are which
        fill = "30D5C8", shape = 21,
        colour = "black", size = 5
      ) +
      stat_smooth(
        method = stats::lm, formula = y ~ x,
        colour = "black", alpha = 0.2
      ) +
      ggthemes::theme_base() +
      labs(
        x = "Lice on Farmed Fish (Log 10)",
        y = "Mean Number of Lice per Wild Juvenile Fish (Log 10)"
      ) +
      annotate(
        geom = "text",
        x = min(log10(reg_data$farm_lice), na.rm = TRUE),
        y = max(log10(reg_data$wild_lice), na.rm = TRUE),
        label = paste("R^2 ==", round(log_model_vals$r.squared, 2)),
        parse = TRUE,
        size = 7,
        hjust = -0.5,
        vjust = 2
      )
  )
  return(results_list)
}
