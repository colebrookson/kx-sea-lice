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

#' Extract Summary Statistics and Bayesian R² from a stan_glm Model
#'
#' @description This function extracts important summary statistics including
#' posterior means, medians, credible intervals, R-hat values, effective sample
#' sizes, and Bayesian R² from a stan_glm model object.
#'
#' @param model A stan_glm model object fitted using the rstanarm package.
#' @param model_name A character string specifying the name of the model.
#'
#' @return A data frame containing the summary statistics for the model.
#'
#' @examples
#' \dontrun{
#' # Fit a stan_glm model
#' model <- stan_glm(y ~ x, data = df)
#'
#' # Extract summary statistics
#' summary_df <- extract_model_summary(model, "Example Model")
#' }
extract_model_summary <- function(model, model_name) {
  # Extract coefficients and their statistics
  # Extract posterior summary statistics
  # Use broom.mixed to tidy the model output
  # Use broom.mixed to tidy the model output
  coef_summary <- broom.mixed::tidy(model,
    conf.level = 0.9,
    conf.int = TRUE,
    conf.method = c("quantile")
  )

  # Extract Rhat and n_eff using the posterior package
  rhat_values <- bayesplot::rhat(model)
  neff_values <- bayesplot::neff_ratio(model)
  # Extract Bayesian R-squared if desired
  bayes_r2 <- median(rstanarm::bayes_R2(model))
  summary_df <- coef_summary %>%
    dplyr::mutate(
      rhat = rhat_values[match(term, names(rhat_values))],
      n_eff = neff_values[match(term, names(neff_values))],
      model = model_name,
      bayes_r2 = bayes_r2
    ) %>%
    dplyr::select(
      model, term, estimate, conf.low, conf.high, bayes_r2,
      rhat, n_eff
    )
}

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

  model <- rstanarm::stan_glm(
    wild_lice ~ farm_lice,
    data = reg_data,
    iter = 10000,
    warmup = 2500
  )
  sims <- as.matrix(model)
  model_pp_check <- bayesplot::pp_check(model, "stat") +
    theme_base()
  ggsave(
    # output path
    paste0(plot_output_path, name, "-pp-check.png"),
    model_pp_check
  )

  # make results plot
  fits <- model %>%
    dplyr::as_tibble() %>%
    dplyr::rename(
      intercept = `(Intercept)`,
      # farm_lice = `log10(farm_lice)`
    ) %>%
    dplyr::select(-sigma)

  rsq <- rstanarm::bayes_R2(model)
  # print(median(rsq))
  # make the plot
  ggplot2::ggsave(

    # output path
    paste0(plot_output_path, name, "-plot.png"),
    ggplot(reg_data) +
      aes(x = farm_lice, y = wild_lice) +
      geom_abline(
        aes(intercept = intercept, slope = farm_lice),
        data = dplyr::sample_n(fits, 1000),
        color = "grey60",
        alpha = 0.15,
        linetype = "dashed"
      ) +
      theme_base() +
      # Plot the mean values of our parameters in blue
      # this corresponds to the coefficients returned by our
      # model summary
      geom_abline(
        intercept = mean(fits$intercept),
        slope = mean(fits$farm_lice),
        linewidth = 1,
        color = "#8222be"
      ) +
      geom_point(
        fill = "#c88eed",
        colour = "black",
        shape = 21,
        size = 3
      ) +
      # set the axis labels and plot title
      labs(
        x = "Lice on Farmed Fish",
        y = "Lice on Wild Fish",
      ) +
      annotate(
        geom = "text",
        x = min(reg_data$farm_lice, na.rm = TRUE),
        y = max(reg_data$wild_lice, na.rm = TRUE),
        label = paste("R^2 ==", round(median(rsq), 2)),
        parse = TRUE,
        size = 7,
        hjust = -0.5,
        vjust = 2
      ),
    height = 6, width = 8
  )

  # log regression =============================================================

  log_reg_data <- reg_data %>%
    dplyr::mutate(
      wild_lice = log10(wild_lice),
      farm_lice = log10(farm_lice)
    )
  model_log <- rstanarm::stan_glm(
    wild_lice ~ farm_lice,
    data = log_reg_data,
    iter = 10000,
    warmup = 2500
  )
  sims_log <- as.matrix(model_log)
  model_log_pp_check <- bayesplot::pp_check(model_log, "stat") +
    theme_base()
  ggsave(
    # output path
    paste0(plot_output_path, name, "-log10-pp-check.png"),
    model_log_pp_check
  )

  # make results plot
  fits_log <- model_log %>%
    dplyr::as_tibble() %>%
    dplyr::rename(
      intercept = `(Intercept)`,
      # farm_lice = `log10(farm_lice)`
    ) %>%
    dplyr::select(-sigma)

  rsq_log <- rstanarm::bayes_R2(model_log)
  # print(median(rsq))
  # make the plot
  ggplot2::ggsave(

    # output path
    paste0(plot_output_path, name, "-log10-plot.png"),
    ggplot(log_reg_data) +
      aes(x = farm_lice, y = wild_lice) +
      geom_abline(
        aes(intercept = intercept, slope = farm_lice),
        data = dplyr::sample_n(fits_log, 1000),
        color = "grey60",
        alpha = 0.15,
        linetype = "dashed"
      ) +
      theme_base() +
      # Plot the mean values of our parameters in blue
      # this corresponds to the coefficients returned by our
      # model summary
      geom_abline(
        intercept = mean(fits_log$intercept),
        slope = mean(fits_log$farm_lice),
        linewidth = 1,
        color = "#19999b"
      ) +
      geom_point(
        fill = "#5ce6e9",
        colour = "black",
        shape = 21,
        size = 3
      ) +
      # set the axis labels and plot title
      labs(
        x = "Lice on Farmed Fish (Log10)",
        y = "Lice on Wild Fish (Log10)",
      ) +
      annotate(
        geom = "text",
        x = min(log_reg_data$farm_lice, na.rm = TRUE),
        y = max(log_reg_data$wild_lice, na.rm = TRUE),
        label = paste("R^2 ==", round(median(rsq_log), 2)),
        parse = TRUE,
        size = 7,
        hjust = -0.5,
        vjust = 2
      ),
    height = 6, width = 8
  )

  results_list <- list(model, model_log)

  return(results_list)
}
