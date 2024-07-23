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

bayesian_wild_fish_regression <- function(wild_lice) {
  #' Fit the actual models we'll need for the power analysis
  #'
  #' @description Fit the model and save the objects for pink salmon to be used
  #' in the power analysis
  #'
  #' @param wild_lice dataframe. The lice on wild fish data
  #'
  #' @usage power_prep_pink(all_power_sims, output_path)
  #' @return NA
  #'

  ## bayesian formulation of the model =========================================
  #' The associated Bayesian generalized linear mixed-effects model (GLMM)
  #' included fixed effects for year and louse stage and random effects for w
  #' eek-of-year (to account for spatially coherent seasonal variation in
  #' louse counts) and location-year combination (to account for infestation
  #' variation among locations due to farm activity or environmental factors).
  #' The model employed a negative binomial error distribution to account for
  #' parasite clustering, a log link function, and uniform priors.

  wild_lice <- wild_lice %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      date = lubridate::make_date(year, month, day),
      site = as.factor(site),
      year = as.factor(year)
    ) %>%
    dplyr::mutate(
      week = as.factor(lubridate::week(date))
    ) %>%
    dplyr::filter(fish_spp %in% c("Pink", "Chum", "P", "CM"))

  all_spp_all_stages <- rstanarm::stan_glmer(
    lep_total ~ year + (1 | week) + (1 | site),
    data = wild_lice,
    family = rstanarm::neg_binomial_2(link = "log"),
    chains = 6,
    cores = 6,
    iter = 20000,
    warmup = 8000
  )
  qs::qsave(
    all_spp_all_stages,
    paste0(here::here(
      "./outputs/model-outputs/regression-diagnostics",
      "/all-spp-all-stages.qs"
    ))
  )
  all_spp_all_stages <- qs::qread(
    paste0(here::here(
      "./outputs/model-outputs/regression-diagnostics",
      "/all-spp-all-stages.qs"
    ))
  )

  ## density plot - not useful but keep anyways
  all_spp_all_stages_ppc_dens <- bayesplot::ppc_dens_overlay(
    y = all_spp_all_stages$y,
    yrep = rstanarm::posterior_predict(all_spp_all_stages, draws = 100)
  ) +
    theme_base()
  ggplot2::ggsave(
    here::here(paste0(
      "./figs/lice-per-year-regression/",
      "all-spp-all-stages-ppc-dens.png"
    )),
    all_spp_all_stages_ppc_dens
  )

  # posterior plot for the fixed effects
  all_spp_all_stages_posterior <- as.array(all_spp_all_stages)
  plot_title <- ggplot2::ggtitle(
    "Posterior distributions",
    "with medians and 90% intervals"
  )
  posterior_year <- bayesplot::mcmc_areas(all_spp_all_stages_posterior,
    pars = names(all_spp_all_stages$coefficients)[1:18],
    prob = 0.9
  ) + plot_title +
    theme_base()
  ggsave(
    here::here("./figs/lice-per-year-regression/posterior.png"),
    posterior_year
  )

  # trace plot
  bayesplot::color_scheme_set("mix-blue-pink")
  all_spp_all_stages_trace <- bayesplot::mcmc_trace(
    all_spp_all_stages_posterior,
    pars = names(all_spp_all_stages$coefficients)[1:18], n_warmup = 8000,
    facet_args = list(nrow = 3)
  ) + bayesplot::facet_text(size = 15) +
    theme_base()
  ggplot2::ggsave(
    here::here("./figs/lice-per-year-regression/trace.png"),
    all_spp_all_stages_trace,
    height = 10, width = 20
  )

  ### do the prediction ========================================================

  x <- wild_lice %>%
    dplyr::filter(!is.na(site), !is.na(week)) %>%
    modelr::data_grid(year, site, week) %>%
    tidybayes::add_epred_draws(all_spp_all_stages, re_formula = NA)
  x_subset <- x[, c("year", ".epred")] %>%
    dplyr::mutate(year = as.factor(year)) %>%
    dplyr::group_by(year) %>%
    dplyr::arrange(`.epred`) %>%
    dplyr::reframe(
      lo = quantile(.epred, prob = 0.05),
      median = quantile(.epred, prob = 0.5),
      hi = quantile(.epred, prob = 0.95)
    )

  xfig <- ggplot2::ggplot(data = x_subset) +
    ggplot2::geom_errorbar(aes(x = year, ymin = lo, ymax = hi, width = 0)) +
    ggplot2::geom_point(aes(x = year, y = median)) +
    theme_base()

  ggsave(
    here::here("./EXAMPLE.png"),
    xfig
  )
}

#' Fit the actual models we'll need for the power analysis
#'
#' @description Fit the model and save the objects for pink salmon to be used
#' in the power analysis
#'
#' @param wild_lice dataframe. The lice on wild fish data
#' @param output_path character. Where to save the model objects
#'
#' @usage power_prep_pink(all_power_sims, output_path)
#' @return a set of predicted dataframes
lice_per_year_regression <- function(wild_lice, output_path) {
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

  # separate species to double check between 2009 and 2017
  chum_2009_17 <- wild_lice %>%
    dplyr::filter(year %in% c(2009:2017) &
      fish_spp == "Chum")
  pink_2009_17 <- wild_lice %>%
    dplyr::filter(year %in% c(2009:2017) &
      fish_spp == "Pink")

  chum_glmm_nb <- glmmTMB::glmmTMB(
    lep_total ~ year + (1 | week) + (1 | site),
    family = nbinom2,
    data = chum_2009_17
  )
  chum_all_lice_glmm_nb <- glmmTMB::glmmTMB(
    all_lice ~ year + (1 | week) + (1 | site),
    family = nbinom2,
    data = chum_2009_17
  )
  pink_glmm_nb <- glmmTMB::glmmTMB(
    lep_total ~ year + (1 | week) + (1 | site),
    family = nbinom2,
    data = pink_2009_17
  )
  pink_all_lice_glmm_nb <- glmmTMB::glmmTMB(
    all_lice ~ year + (1 | week) + (1 | site),
    family = nbinom2,
    data = pink_2009_17
  )

  all_models <- list(
    leps_all_glmm_nb, all_NA_glmm_nb, leps_co_glmm_nb, leps_mot_glmm_nb,
    leps_chal_glmm_nb, chum_glmm_nb, pink_glmm_nb, chum_all_lice_glmm_nb,
    pink_all_lice_glmm_nb
  )
  saveRDS(all_models, paste0(output_path, "all-model-fits.rds"))

  all_stage_models <- list(
    leps_all_mod, all_NA_mod, leps_co_mod, leps_mot_mod, leps_chal_mod
  )
  saveRDS(all_stage_models, paste0(output_path, "all-stage-model-fits.rds"))
  spp_models <- list(
    chum_glmm_nb, pink_glmm_nb, chum_all_lice_glmm_nb, pink_all_lice_glmm_nb
  )

  ## model predictions =========================================================
  # Prediction data
  predict_data <- data.frame(
    year = as.character(c(2005:2022)),
    week = NA,
    site = NA
  )
  predict_data_co <- data.frame(
    year = as.character(c(2009:2013, 2015:2022)),
    week = NA,
    site = NA
  )
  years <- list(
    (2005:2022), (2005:2022), c(2009:2013, 2015:2022),
    (2005:2022), (2005:2022)
  )
  species <- c("Leps", "All", rep("Leps", 3))
  stage <- c("All", "NA", "Copepodites", "Motiles", "Chalimus")

  predict_farm_nums <- c(2, 4, 3, 4, 5, 5, 6, 4, 4, 5, 4, 5, 3, 3, 3, 3)
  predict_co_nums <- c(5, 5, 6, 4, 4, 4, 5, 3, 3, 3, 3)
  predict_farms <- list(
    predict_farm_nums, predict_farm_nums, predict_co_nums,
    predict_farm_nums, predict_farm_nums
  )

  # Initialize list to store prediction dataframes
  predicted_dfs <- data.frame()
  predict_dfs <- list(
    predict_data, predict_data, predict_data_co,
    predict_data, predict_data
  )

  # Iterate through each model and make predictions
  for (i in seq_along(all_stage_models)) {
    model <- all_stage_models[[i]]

    predicted_df <- data.frame(
      year = as.character(years[[i]]),
      predict(
        object = model,
        newdata = predict_dfs[[i]],
        re.form = ~0,
        se.fit = TRUE,
        type = "response"
      )
    )

    predicted_df$area <- 7
    predicted_df <- predicted_df[predicted_df$year < 2021, ]
    predicted_df$farms <- predict_farms[[i]]
    predicted_df$year <- as.numeric(predicted_df$year)

    # add species and stage
    predicted_df$stage <- stage[i]
    predicted_df$species <- species[i]

    # Store the predicted dataframe in the list
    predicted_dfs <- rbind(predicted_dfs, predicted_df)
  }

  ### predictions for within each of the species ===============================
  species_predict_data <- data.frame(
    year = as.character(c(2009:2017)),
    week = NA,
    site = NA
  )
  # Initialize list to store prediction dataframes
  predicted_spp_dfs <- data.frame()

  # Iterate through each model and make predictions
  for (i in seq_along(spp_models)) {
    model <- spp_models[[i]]

    predicted_df <- data.frame(
      year = as.character(c(2009:2017)),
      predict(
        object = model,
        newdata = species_predict_data,
        re.form = ~0,
        se.fit = TRUE,
        type = "response"
      )
    )
    predicted_df$area <- 7
    predicted_df$farms <- c(5, 5, 6, 4, 4, 5, 4, 5, 3)
    predicted_df$year <- as.numeric(predicted_df$year)
    predicted_df$species <- ifelse(i %in% c(1, 3), "Chum", "Pink")
    predicted_df$louse_species <- ifelse(i %in% c(1, 2), "Leps", "All")
    # Store the predicted dataframe in the list
    predicted_spp_dfs <- rbind(predicted_spp_dfs, predicted_df)
  }

  predicted_dfs <- predicted_dfs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      up = fit + (1.96 * se.fit),
      lo = fit - (1.96 * se.fit)
    )
  predicted_spp_dfs <- predicted_spp_dfs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      up = fit + (1.96 * se.fit),
      lo = fit - (1.96 * se.fit)
    )

  return(list(leps_all_glmm_nb, predicted_dfs, predicted_spp_dfs))
}
