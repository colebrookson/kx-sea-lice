##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-04-22
#'
#' This file contains the functions that run regressions to figure out how many
#' lice per year there are on wild fish
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

#' Create Diagnostic Plots for a List of Models
#'
#' @description This function takes a named list of `rstanarm` model objects
#' and generates diagnostic plots (density plot, posterior plot for fixed
#' effects, and trace plot for each model. The plots are saved as PNG files in
#' a specified directory.
#'
#' @param model_list A named list of `rstanarm` model objects.
#' @param n_coefs A numeric value of now many coefficients to include. This
#' will correspond to the number of years (the only fixed effect) were used
#' in the model
#'
#' @return None. The function saves the generated plots as PNG files in the
#' `./figs/lice-per-year-regression/` directory.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   x = runif(100, 1, 10),
#'   y = 2 * log10(runif(100, 1, 10)) + rnorm(100)
#' )
#'
#' # Fit models
#' model1 <- rstanarm::stan_glm(y ~ x, data = df)
#' model2 <- rstanarm::stan_glm(y ~ log10(x), data = df)
#'
#' # Create a named list of models
#' model_list <- list(
#'   "model1" = model1,
#'   "model2" = model2
#' )
#'
#' # Call the function to create diagnostic plots
#' create_diagnostic_plots(model_list)
#' }
#' @importFrom bayesplot ppc_dens_overlay mcmc_areas mcmc_trace color_scheme_set
#' facet_text
#' @importFrom ggplot2 ggsave ggtitle
#' @importFrom here here
#' @import rstanarm
#' @import dplyr
#' @import bayesplot
#' @import ggplot2
#' @import here
create_diagnostic_plots <- function(model_list, n_coefs) {
  # Loop through each model in the list
  for (model_name in names(model_list)) {
    model <- model_list[[model_name]]

    # Density plot
    # ppc_dens_plot <- bayesplot::pp_check(
    #   model,
    #   n = 100
    #   # yrep = rstanarm::posterior_predict(model, draws = 100)
    # ) + theme_base() + xlim(0, 100)
    # ggplot2::ggsave(
    #   here::here(paste0(
    #     "./figs/lice-per-year-regression/diagnostics/",
    #     model_name, "-ppc-dens.png"
    #   )),
    #   ppc_dens_plot
    # )

    # Posterior plot for the fixed effects
    model_posterior <- as.array(model)
    plot_title <- ggplot2::ggtitle(
      "Posterior distributions",
      "with medians and 90% intervals"
    )
    posterior_year <- bayesplot::mcmc_areas(model_posterior,
      pars = names(model$coefficients)[1:n_coefs],
      prob = 0.9
    ) + plot_title +
      theme_base()
    ggplot2::ggsave(
      here::here(paste0(
        "./figs/lice-per-year-regression/diagnostics/",
        model_name, "-posterior.png"
      )),
      posterior_year
    )

    # Trace plot
    bayesplot::color_scheme_set("mix-blue-pink")
    trace_plot <- bayesplot::mcmc_trace(
      model_posterior,
      pars = names(model$coefficients)[1:n_coefs], n_warmup = 2500,
      facet_args = list(nrow = 3)
    ) + bayesplot::facet_text(size = 15) +
      theme_base()
    ggplot2::ggsave(
      here::here(paste0(
        "./figs/lice-per-year-regression/diagnostics/",
        model_name, "-trace.png"
      )),
      trace_plot,
      height = 10, width = 20
    )
  }
}

#' Fit the actual models we'll need for estimating yearly lice amounts
#'
#' @description Fit the model and save the objects for pink salmon to be used
#' in the power analysis
#'
#' @param wild_lice dataframe. The lice on wild fish data
#' @param output_path character. Where to save the model objects
#' @param run_or_read_models character. Denote whether to re-run the models
#' entirely or read the model objects from a previous, saved version
#' @param run_or_read_predictions character. Denote whether to re-run the model
#' predictions entirely or just read the model predictions from a previous,
#' saved version
#'
#' @usage power_prep_pink(all_power_sims, output_path)
#' @return a set of predicted dataframes
lice_per_year_regression <- function(
    wild_lice, output_path, run_or_read_models,
    run_or_read_predictions) {
  # wild_lice <- targets::tar_read(clean_wild_lice_data_2005)

  ## first regression for the wild lice ========================================
  wild_lice <- wild_lice %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      date = lubridate::make_date(year, month, day),
      site = as.factor(site),
      year = as.factor(year),
      all_lice = lep_total + cal_total
    ) %>%
    dplyr::mutate(
      week = as.factor(lubridate::week(date))
    )
  # separate species to double check between 2009 and 2017
  chum_2009_17 <- wild_lice %>%
    dplyr::filter(year %in% c(2009:2017) &
      fish_spp == "Chum")
  pink_2009_17 <- wild_lice %>%
    dplyr::filter(year %in% c(2009:2017) &
      fish_spp == "Pink")

  if (run_or_read_models == "run") {
    leps_all_mod <- rstanarm::stan_glmer(
      lep_total ~ year + (1 | week) + (1 | site),
      data = wild_lice,
      family = rstanarm::neg_binomial_2(link = "log"),
      chains = 4,
      cores = 4,
      iter = 10000,
      warmup = 2500
    )

    all_NA_mod <- rstanarm::stan_glmer(
      all_lice ~ year + (1 | week) + (1 | site),
      data = wild_lice,
      family = rstanarm::neg_binomial_2(link = "log"),
      chains = 4,
      cores = 4,
      iter = 10000,
      warmup = 2500
    )

    leps_co_mod <- rstanarm::stan_glmer(
      lep_co ~ year + (1 | week) + (1 | site),
      data = wild_lice,
      family = rstanarm::neg_binomial_2(link = "log"),
      chains = 4,
      cores = 4,
      iter = 10000,
      warmup = 2500
    )

    leps_mot_mod <- rstanarm::stan_glmer(
      lep_motiles ~ year + (1 | week) + (1 | site),
      data = wild_lice,
      family = rstanarm::neg_binomial_2(link = "log"),
      chains = 4,
      cores = 4,
      iter = 10000,
      warmup = 2500
    )

    leps_chal_mod <- rstanarm::stan_glmer(
      lep_chal ~ year + (1 | week) + (1 | site),
      data = wild_lice,
      family = rstanarm::neg_binomial_2(link = "log"),
      chains = 4,
      cores = 4,
      iter = 10000,
      warmup = 2500
    )
    chum_mod <- rstanarm::stan_glmer(
      lep_total ~ year + (1 | week) + (1 | site),
      data = chum_2009_17,
      family = rstanarm::neg_binomial_2(link = "log"),
      chains = 4,
      cores = 4,
      iter = 10000,
      warmup = 2500
    )
    chum_all_lice_mod <- rstanarm::stan_glmer(
      all_lice ~ year + (1 | week) + (1 | site),
      data = chum_2009_17,
      family = rstanarm::neg_binomial_2(link = "log"),
      chains = 4,
      cores = 4,
      iter = 10000,
      warmup = 2500
    )

    pink_mod <- rstanarm::stan_glmer(
      lep_total ~ year + (1 | week) + (1 | site),
      data = pink_2009_17,
      family = rstanarm::neg_binomial_2(link = "log"),
      chains = 4,
      cores = 4,
      iter = 10000,
      warmup = 2500
    )

    pink_all_lice_mod <- rstanarm::stan_glmer(
      all_lice ~ year + (1 | week) + (1 | site),
      data = pink_2009_17,
      family = rstanarm::neg_binomial_2(link = "log"),
      chains = 4,
      cores = 4,
      iter = 10000,
      warmup = 2500
    )
    all_stage_models <- list(
      "leps-all" = leps_all_mod,
      "all-lice" = all_NA_mod,
      "leps-co" = leps_co_mod,
      "leps-mot" = leps_mot_mod,
      "leps-chal" = leps_chal_mod
    )
    qs::qsave(all_stage_models, paste0(output_path, "all-stage-model-fits.qs"))
    spp_models <- list(
      "chum-leps" = chum_mod,
      "pink-leps" = pink_mod,
      "chum-lice" = chum_all_lice_mod,
      "pink-lice" = pink_all_lice_mod
    )
    qs::qsave(spp_models, paste0(output_path, "all-species-model-fits.qs"))
  } else {
    all_stage_models <- qs::qread(paste0(
      output_path,
      "all-stage-model-fits.qs"
    ))
    spp_models <- qs::qread(paste0(output_path, "all-species-model-fits.qs"))
  }

  ## model diagnostics =========================================================
  create_diagnostic_plots(all_stage_models, n_coefs = 18)
  create_diagnostic_plots(spp_models, n_coefs = 9)

  ## model predictions =========================================================
  ### stage level predictions ==================================================
  #' we need a few things to be different for the different models. The years
  #' that each model uses are slightly different, the copepodite model uses
  #' only a subset of the years, so we need to be filtering for the appropriate
  #' number of years when we make the within-sample prediction. Additionally
  #' we want the number of farms

  if (run_or_read_predictions == "run") {
    predicted_stage_dfs <- data.frame()
    years_list <- list(
      "leps-all" = c(2005:2022), "all-lice" = c(2005:2022),
      "leps-co" = c(2009:2013, 2015:2022), "leps-mot" = c(2005:2022),
      "leps-chal" = c(2005:2022)
    )
    predict_farm_nums <- c(2, 4, 3, 4, 5, 5, 6, 4, 4, 5, 4, 5, 3, 3, 3, 3)
    predict_co_nums <- c(5, 5, 6, 4, 4, 4, 5, 3, 3, 3, 3)
    predict_farms <- list(
      "leps-all" = predict_farm_nums, "all-lice" = predict_farm_nums,
      "leps-co" = predict_co_nums, "leps-mot" = predict_farm_nums,
      "leps-chal" = predict_farm_nums
    )
    for (model_name in names(all_stage_models)) {
      model <- all_stage_models[[model_name]]
      years <- years_list[[model_name]]

      # Generate predictions
      x <- wild_lice %>%
        dplyr::filter(!is.na(site), !is.na(week)) %>%
        dplyr::mutate(year = as.numeric(as.character(year))) %>%
        dplyr::filter(year %in% years) %>%
        dplyr::mutate(
          year = as.factor(year)
        ) %>%
        modelr::data_grid(year, site, week) %>%
        tidybayes::add_epred_draws(model, re_formula = NA)

      x_subset <- x[, c("year", ".epred")] %>%
        dplyr::filter(year %notin% c(2021, 2022)) %>%
        dplyr::mutate(year = as.factor(year)) %>%
        dplyr::group_by(year) %>%
        dplyr::arrange(`.epred`) %>%
        dplyr::reframe(
          lo = quantile(.epred, prob = 0.05),
          median = quantile(.epred, prob = 0.5),
          hi = quantile(.epred, prob = 0.95)
        ) %>%
        dplyr::mutate(
          stage = model_name,
          farms = predict_farms[[model_name]]
        )
      # bind
      predicted_stage_dfs <- rbind(predicted_stage_dfs, x_subset)
    }
    qs::qsave(predicted_stage_dfs, paste0(
      output_path,
      "all-stages-model-predictions.qs"
    ))

    predicted_spp_dfs <- data.frame()
    for (model_name in names(spp_models)) {
      model <- spp_models[[model_name]]
      if (model_name %in% c("chum-leps", "chum-lice")) {
        data <- chum_2009_17
      } else {
        data <- pink_2009_17
      }

      # Generate predictions
      x <- data %>%
        dplyr::filter(!is.na(site), !is.na(week)) %>%
        dplyr::mutate(year = as.numeric(as.character(year))) %>%
        dplyr::filter(year %in% c(2009:2017)) %>%
        dplyr::mutate(
          year = as.factor(year)
        ) %>%
        modelr::data_grid(year, site, week) %>%
        tidybayes::add_epred_draws(model, re_formula = NA)

      x_subset <- x[, c("year", ".epred")] %>%
        dplyr::mutate(year = as.factor(year)) %>%
        dplyr::group_by(year) %>%
        dplyr::arrange(`.epred`) %>%
        dplyr::reframe(
          lo = quantile(.epred, prob = 0.05),
          median = quantile(.epred, prob = 0.5),
          hi = quantile(.epred, prob = 0.95)
        ) %>%
        dplyr::mutate(
          species = stringr::str_split(model_name, "-")[[1]][1],
          lice = stringr::str_split(model_name, "-")[[1]][2]
        )
      predicted_spp_dfs <- rbind(predicted_spp_dfs, x_subset)
    }
    qs::qsave(predicted_spp_dfs, paste0(
      output_path,
      "all-species-model-predictions.qs"
    ))

    model_predictions <- list(predicted_stage_dfs, predicted_spp_dfs)
  } else {
    predicted_stage_dfs <- qs::qread(paste0(
      output_path,
      "all-species-model-predictions.qs"
    ))
    predicted_spp_dfs <- qs::qread(paste0(
      output_path,
      "all-species-model-predictions.qs"
    ))
    model_predictions <- list(predicted_stage_dfs, predicted_spp_dfs)
  }

  return(model_predictions)
}
