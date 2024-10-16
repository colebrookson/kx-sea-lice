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
create_diagnostic_plots <- function(model_list, n_coefs_list) {
  # Loop through each model in the list
  for (model_name in names(model_list)) {
    model <- model_list[[model_name]]
    n_coefs <- n_coefs_list[[model_name]]

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
#' @return a set of predicted dataframes and all model objects
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
      "all-NA" = all_NA_mod,
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
    ## model diagnostics =======================================================
    stage_model_ncoefs <- list(
      "leps-all" = 18,
      "all-NA" = 18,
      "leps-co" = 11,
      "leps-mot" = 18,
      "leps-chal" = 18
    )
    spp_models_ncoefs <- list(
      "chum-leps" = 9,
      "pink-leps" = 9,
      "chum-lice" = 9,
      "pink-lice" = 9
    )
    create_diagnostic_plots(all_stage_models, stage_model_ncoefs)
    create_diagnostic_plots(spp_models, spp_models_ncoefs)
  } else {
    all_stage_models <- qs::qread(paste0(
      output_path,
      "all-stage-model-fits.qs"
    ))
    spp_models <- qs::qread(paste0(output_path, "all-species-model-fits.qs"))
  }

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
      "leps-all" = c(2005:2022), "all-NA" = c(2005:2022),
      "leps-co" = c(2009:2013, 2015:2022), "leps-mot" = c(2005:2022),
      "leps-chal" = c(2005:2022)
    )
    predict_farm_nums <- c(2, 4, 3, 4, 5, 5, 6, 4, 4, 5, 4, 5, 3, 3, 3, 3)
    predict_co_nums <- c(5, 5, 6, 4, 4, 4, 5, 3, 3, 3, 3)
    predict_farms <- list(
      "leps-all" = predict_farm_nums, "all-NA" = predict_farm_nums,
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
          farms = predict_farms[[model_name]],
          lice = stringr::str_split(model_name, "-")[[1]][1],
          stage = stringr::str_split(model_name, "-")[[1]][2]
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
      "all-stages-model-predictions.qs"
    ))
    predicted_spp_dfs <- qs::qread(paste0(
      output_path,
      "all-species-model-predictions.qs"
    ))
    model_predictions <- list(predicted_stage_dfs, predicted_spp_dfs)
  }
  results_ob <- list(model_predictions, all_stage_models, spp_models)

  return(results_ob)
}

#' Extract Fixed Effects
#'
#' @param model A fitted Bayesian model object.
#' @param model_name A string representing the name of the model.
#' @return A data frame of fixed effects with 90% credible intervals.
extract_fixed_effects <- function(model, model_name) {
  # Extract coefficients and their statistics
  coef_summary <- broom.mixed::tidy(model,
    conf.level = 0.9,
    conf.int = TRUE,
    conf.method = c("quantile")
  )
  coef_summary$model <- model_name
  coef_summary <- coef_summary[
    ,
    c("model", "term", "estimate", "conf.low", "conf.high")
  ]

  # # Extract random effects using the ranef function
  random_effects_list <- rstanarm::ranef(model)

  # Combine the random effects into a single data frame
  random_effects_df <- do.call(rbind, lapply(
    names(random_effects_list),
    function(group) {
      df <- as.data.frame(random_effects_list[[group]])
      df$group <- group
      df$term <- rownames(df)
      return(df)
    }
  ))

  # Reset row names
  rownames(random_effects_df) <- NULL

  return(coef_summary)
}

#' Extract Model Diagnostics
#'
#' @param model A fitted Bayesian model object.
#' @param model_name A string representing the name of the model.
#' @return A data frame of model performance metrics including ELPD, Rhat,
#'   and n_eff.
extract_diagnostics <- function(model, model_name) {
  # Extract diagnostics

  # things I want:
  #'  1. the Posterior Predictive Checks (PPC) (in a plot)
  #'  2. Root Mean Square Error (RMSE)
  #'  3. Effective Sample Size (ESS)
  #'  4. Rhat (Potential Scale Reduction Factor)
  #'  5. LOO Predictive Density (ELPD)
  #'  6.
  #'

  # Get the ppc checks in a plot
  y_rep <- rstanarm::posterior_predict(model, draws = 500)
  log_y_rep <- log10(y_rep)
  y <- stats::model.frame(model)[, all.vars(model$formula)[1]]
  log_y <- log10(y)
  intervals <- bayesplot::ppc_intervals(
    y = y,
    yrep = y_rep,
    x = as.numeric(as.character(stats::model.frame(model)[, "year"])),
    prob = 0.5
  ) +
    theme_base() +
    scale_x_continuous(labels = c(2005:2022), breaks = c(2005:2022)) +
    labs(x = "Years model was fit")

  sqrt_y <- sqrt(y)
  sqrt_yrep <- sqrt(y_rep)
  # three different types of ppc plots
  dens_psuedo_log_plot <- bayesplot::ppc_dens_overlay(y, y_rep) +
    ggplot2::scale_x_continuous(trans = "pseudo_log") +
    theme_base() +
    labs(x = "Psudeo-logged values")
  sqrt_dens_plot <- bayesplot::ppc_dens_overlay(sqrt_y, sqrt_yrep) +
    theme_base() +
    xlim(c(0, 8)) +
    labs(x = "Square-root values")
  rootogram_plot <- bayesplot::ppc_rootogram(
    y = y, yrep = y_rep
  ) +
    theme_base()

  diagnostics_plots <- patchwork::wrap_plots(
    intervals, dens_psuedo_log_plot, sqrt_dens_plot, rootogram_plot,
    nrow = 2, ncol = 2
  )
  ggsave(
    here::here(paste0(
      "./figs/lice-per-year-regression/diagnostics/",
      model_name, "-int-ppc-diags.png"
    )),
    diagnostics_plots,
    width = 12, height = 10
  )

  # get the root mean squared error
  rmse <- sqrt(mean((y - rowMeans(y_rep))^2))

  # get the effective sample size
  ess <- rstan::monitor(model$stanfit)
  ess$vars <- rownames(ess)
  ess_fixed <- ess %>% dplyr::filter(vars %in% names(rstanarm::fixef(model)))
  loo_results <- loo::loo(
    model
  )
  # max(loo_results$diagnostics$pareto_k)
  # str(loo::pareto_k_table(loo_results))
  loo_estimates <- loo_results$estimates
  elpd <- loo_estimates["elpd_loo", "Estimate"]
  se_elpd <- loo_estimates["elpd_loo", "SE"]
  effective_params <- loo_estimates["p_loo", "Estimate"]
  se_effective_params <- loo_estimates["p_loo", "SE"]
  rhat <- bayesplot::rhat(model)
  neff <- round(mean(bayesplot::neff_ratio(model)[
    1:length(rstanarm::fixef(model))
  ]), 3)

  # Combine into a summary data frame
  diagnostics <- data.frame(
    model = model_name,
    rmse = rmse,
    elpd = elpd,
    mean_bulk_fixed_ess = mean(ess_fixed$Bulk_ESS),
    se_elpd = se_elpd,
    effective_params = effective_params,
    se_effective_params = se_effective_params,
    rhat = max(rhat),
    n_eff_ratio = min(neff)
  )

  return(diagnostics)
}

#' Extract Random Effects
#'
#' @param model A fitted Bayesian model object.
#' @param model_name A string representing the name of the model.
#' @return A data frame of random effects.
extract_random_effects <- function(model, model_name) {
  # Extract random effects
  rand_summary <- broom.mixed::tidy(model,
    conf.level = 0.9,
    conf.int = TRUE,
    conf.method = "quantile",
    effects =
    )
  coef_summary$model <- model_name
  coef_summary <- coef_summary[
    ,
    c("model", "term", "estimate", "conf.low", "conf.high")
  ]

  return(coef_summary)
}
#' Generate LaTeX Tables for Main Model
#'
#' @param model A fitted Bayesian model object.
#' @param model_name A string representing the name of the model.
#' @param fixed_effects_file_path A string representing the path to the .tex file
#'   for fixed effects and diagnostics.
#' @param random_effects_file_path A string representing the path to the .tex file
#'   for random effects.
generate_main_model_tables <- function(
    model, model_name,
    fixed_effects_file_path,
    random_effects_file_path) {
  # testing
  # model <- main_model
  # model_name <- "leps-all"

  # Extract fixed effects and diagnostics
  fixed_effects <- extract_fixed_effects(model, model_name)
  fixed_effects$model <- "Main model"

  diagnostics <- extract_diagnostics(model, model_name)
  diagnostics$model <- "Main model"
  # breaking this into two tables so they're both easier to read
  diagnostics_1 <- diagnostics[, c(
    "model", "rmse", "elpd",
    "mean_bulk_fixed_ess", "se_elpd"
  )]
  diagnostics_2 <- diagnostics[, c(
    "model", "effective_params", "se_effective_params", "rhat", "n_eff_ratio"
  )]

  # Create LaTeX tables for fixed effects and diagnostics
  fixed_effects_table <- knitr::kable(
    fixed_effects,
    format = "latex",
    col.names = c(
      "Model", "Term", "Estimate", "10\\%", "90\\%"
    ),
    booktabs = TRUE,
    escape = FALSE,
    caption = "Fixed Effects with 90 \\% Credible Intervals for Bayesian
    generalized linear model estimating the number of lice on fish per year.
    This model uses all years of the timeseries and the response
    variable is \\textit{L. salmonis} lice of all stages"
  ) %>% kableExtra::kable_classic()

  diagnostics_1_table <- knitr::kable(
    diagnostics_1,
    format = "latex",
    col.names = c(
      "Model", "RMSE", "ELPD", "Mean Bulk ESS", "SE of ELPD"
    ),
    caption = "Model Diagnostics with measures of fit.",
    booktabs = TRUE,
    escape = FALSE
  ) %>% kableExtra::kable_classic()
  diagnostics_2_table <- knitr::kable(
    diagnostics_2,
    format = "latex",
    col.names = c(
      "Model", "EPs", "SE of EP",
      "$\\hat{R}$", "Mean FE $n_{eff}$ ratio"
    ),
    caption = "Model Diagnostics with  measures of convergence of the model.
    EP = 'Effective Parameters', FE = 'Fixed Effects'",
    booktabs = TRUE,
    escape = FALSE
  ) %>% kableExtra::kable_classic()

  # Combine fixed effects and diagnostics tables
  combined_tables <- paste(fixed_effects_table, diagnostics_1_table,
    diagnostics_2_table,
    sep = "\n\n"
  )

  # Convert the LaTeX table to a character vector
  latex_table_lines <- strsplit(combined_tables, "\n")[[1]]

  # Find the position to insert the label (before \end{table})
  insert_pos_fixef <- (which(grepl(
    "\\\\end\\{tabular\\}",
    latex_table_lines
  )) + 1)[1]
  insert_pos_diags_1 <- (which(grepl(
    "\\\\end\\{tabular\\}",
    latex_table_lines
  )) + 1)[2]
  insert_pos_diags_2 <- (which(grepl(
    "\\\\end\\{tabular\\}",
    latex_table_lines
  )) + 1)[3]

  # Insert the label
  latex_table_lines <- append(latex_table_lines,
    paste0("\\label{", "SI-", model_name, "-fixef", "}"),
    after = insert_pos_fixef - 1
  )
  latex_table_lines <- append(latex_table_lines,
    paste0("\\label{SI-", model_name, "-diags-1", "}"),
    after = insert_pos_diags_1 - 1
  )
  latex_table_lines <- append(latex_table_lines,
    paste0("\\label{SI-", model_name, "-diags-2", "}"),
    after = insert_pos_diags_2
  )

  # Write fixed effects and diagnostics tables to .tex file
  writeLines(latex_table_lines, paste0(
    fixed_effects_file_path,
    "main-model-fixef-diags.tex"
  ))

  # Extract random effects
  # random_effects <- extract_random_effects(model, model_name)

  # # Create LaTeX table for random effects
  # random_effects_table <- knitr::kable(
  #   random_effects,
  #   format = "latex",
  #   col.names = c("Model", "Term", "Estimate", "10\\%", "90\\%"),
  #   caption = "Random Effects"
  # )
  # ranef_table_lines <- strsplit(random_effects_table, "\n")[[1]]

  # insert_pos_ranef <- (which(grepl(
  #   "\\\\end\\{tabular\\}",
  #   ranef_table_lines
  # )) + 1)
  # ranef_table_lines <- append(ranef_table_lines,
  #   paste0("\\label\\{SI-", model_name, "-ranefs-", "\\}"),
  #   after = insert_pos_ranef - 1
  # )

  # # Write random effects table to .tex file
  # writeLines(ranef_table_lines, paste0(
  #   random_effects_file_path,
  #   "main-model-randef.tex"
  # ))
}

#' Generate LaTeX Tables for Models in Groups
#'
#' @param models A list of fitted Bayesian model objects.
#' @param model_names A vector of strings representing model names.
#' @param group_name A string representing the group name.
#' @param file_path A string representing the path to the .tex file to save.
generate_group_tables <- function(models, model_names, group_name, file_path) {
  # Extract fixed effects, diagnostics, and random effects
  fixed_effects_list <- lapply(seq_along(models), function(i) {
    extract_fixed_effects(models[[i]], model_names[i])
  })

  diagnostics_list <- lapply(seq_along(models), function(i) {
    extract_diagnostics(models[[i]], model_names[i])
  })

  # random_effects_list <- lapply(seq_along(models), function(i) {
  #   extract_random_effects(models[[i]], model_names[i])
  # })

  # Combine data frames
  fixed_effects_combined <- do.call(rbind, fixed_effects_list)
  diagnostics_combined <- do.call(rbind, diagnostics_list)
  # random_effects_combined <- do.call(rbind, random_effects_list)

  # Create LaTeX tables
  fixed_effects_table <- knitr::kable(
    fixed_effects_combined,
    format = "latex",
    col.names = c(
      "Model", "Term", "Estimate", "10\\%", "90\\%", "Model"
    ),
    caption = paste("Fixed Effects for Group:", group_name),
    booktabs = TRUE,
    escape = FALSE
  ) %>% kableExtra::kable_classic()

  diagnostics_table <- knitr::kable(
    diagnostics_combined,
    format = "latex",
    col.names = c(
      "Model", "ELPD", "SE of ELPD", "Effective Parameters",
      "SE of Effective Parameters", "$\\hat{R}$", "$n_{eff}$"
    ),
    caption = paste("Performance Metrics for Group:", group_name),
    booktabs = TRUE,
    escape = FALSE
  ) %>% kableExtra::kable_classic()

  # random_effects_table <- knitr::kable(
  #   random_effects_combined,
  #   format = "latex",
  #   col.names = c("Model", "Term", "Estimate"),
  #   caption = paste("Random Effects for Group:", group_name)
  # )

  # Combine tables
  combined_tables <- paste(
    fixed_effects_table,
    diagnostics_table,
    # random_effects_table,
    sep = "\n\n"
  )

  latex_table_lines <- strsplit(combined_tables, "\n")[[1]]

  # Find the position to insert the label (before \end{table})
  insert_pos_fixef <- (which(grepl(
    "\\\\end\\{tabular\\}",
    latex_table_lines
  )) + 1)[1]
  insert_pos_diags <- (which(grepl(
    "\\\\end\\{tabular\\}",
    latex_table_lines
  )) + 1)[2]

  # Insert the label
  latex_table_lines <- append(latex_table_lines,
    paste0("\\label\\{SI-", group_name, "-fixef-", "\\}"),
    after = insert_pos_fixef - 1
  )
  latex_table_lines <- append(latex_table_lines,
    paste0("\\label\\{SI-", group_name, "-diags-", "\\}"),
    after = insert_pos_diags - 1
  )

  # Write to .tex file
  writeLines(latex_table_lines, paste0(
    file_path, group_name,
    "-fixef-diags.tex"
  ))
}

#' Generate and Write All LaTeX Tables
#'
#' @param main_model The main model object.
#' @param main_model_name The name of the main model.
#' @param model_list A list of all model objects.
#' @param model_names A vector of all model names.
generate_all_tables <- function(
    main_model, main_model_name,
    model_list, model_names) {
  # Generate tables for the main model
  generate_main_model_tables(
    main_model = main_model, main_model_name = main_model_name,
    fixed_effects_file_path = fixed_effects_file_path,
    random_effects_file_path = random_effects_file_path
  )

  # Define model groups
  group_1 <- c("all-NA", "leps-co", "leps-mot", "leps-chal")
  group_2 <- c("chum-leps", "pink-leps", "chum-lice", "pink-lice")

  # Split models into groups
  models_group_1 <- model_list[model_names %in% group_1]
  names_group_1 <- model_names[model_names %in% group_1]

  models_group_2 <- model_list[model_names %in% group_2]
  names_group_2 <- model_names[model_names %in% group_2]

  # Generate tables for each group
  generate_group_tables(
    models = models_group_1,
    model_names = names_group_1,
    file_path = here::here("./outputs/model-outputs/lice-per-year/tables/"),
    group_name = "Stage models"
  )
  generate_group_tables(
    models = models_group_2,
    model_names = names_group_2,
    file_path = here::here("./outputs/model-outputs/lice-per-year/tables/"),
    group_name = "Chum vs. pink models"
  )
}
