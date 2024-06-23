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

# power_prep_pink ==============================================================
power_prep_pink <- function(wild_lice) {
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

  # wild_lice <- targets::tar_read(clean_wild_lice_data_2005)

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
    )

  all_spp_all_stages <- rstanarm::stan_glmer(
    lep_total ~ year + (1 | week) + (1 | site),
    data = wild_lice,
    family = rstanarm::neg_binomial_2(link = "log"),
    chains = 6,
    cores = 6,
    iter = 8000,
    warmup = 2000,
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
    pars = names(all_spp_all_stages$coefficients)[1:18], n_warmup = 2000,
    facet_args = list(nrow = 3)
  ) + bayesplot::facet_text(size = 15) +
    theme_base()
  ggplot2::ggsave(
    here::here("./figs/lice-per-year-regression/trace.png"),
    all_spp_all_stages_trace,
    height = 10, width = 20
  )

  ### do the prediction ========================================================
  predict_data <- data.frame(expand.grid(
    year = as.character(c(2005:2022)),
    week = as.factor(levels((wild_lice$week))[1]),
    site = as.factor(levels(unique(wild_lice$site))[1])
  ))
  prediction <- tidybayes::epred_draws(
    object = all_spp_all_stages,
    newdata = predict_data,
    re_formula = NULL
  )
  ggplot(
    prediction,
    aes(
      x = .epred, y = year, fill = year
    )
  ) +
    tidybayes::stat_halfeye(.width = 0.95) +
    scale_fill_manual(values = c(rep("lightpink", 18))) +
    labs(
      x = "Count", y = "Year",
      subtitle = "Posterior predictions"
    ) +
    theme(legend.position = "bottom") +
    theme_bw()





  ## first regression for the wild lice ========================================
  wild_lice <- wild_lice %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      date = lubridate::make_date(year, month, day),
      site = as.factor(site),
      year = as.factor(year)
    ) %>%
    dplyr::mutate(
      week = as.factor(lubridate::week(date))
    )

  # do first with negative binomial
  wild_lice_glmm_nb <- glmmTMB::glmmTMB(
    lep_total ~ year + (1 | week) + (1 | site),
    family = nbinom2,
    data = wild_lice
  )
  # now with poisson
  wild_lice_glmm_poi <- glmmTMB::glmmTMB(
    lep_total ~ year + (1 | week) + (1 | site),
    family = poisson(link = "log"),
    data = wild_lice
  )

  ## do a check to see which model is better ===================================
  if (AIC(wild_lice_glmm_nb) < (AIC(wild_lice_glmm_poi) + 2)) {
    # the plus two is to make sure that the nb is at least 2 delta AIC better
    best_model <- wild_lice_glmm_nb
  } else if (AIC(wild_lice_glmm_poi) < (AIC(wild_lice_glmm_nb) + 2)) {
    best_model <- wild_lice_glmm_poi
  } else {
    stop("Models not different enough via AIC - requires manual decision")
  }

  ## model predictions =========================================================
  predict_data <- data.frame(
    year = as.character(c(2006:2022)),
    week = NA,
    site = NA
  )

  predicted_yearly_lice <- data.frame(
    year = as.character(c(2006:2022)),
    stats::predict(
      object = best_model,
      newdata = predict_data,
      re.form = ~0,
      se.fit = TRUE,
      type = "response"
    )
  ) %>%
    dplyr::mutate(
      area = 7
    )

  # get yearly predicted
  pred_yearly <- predicted_yearly_lice %>%
    dplyr::filter(year < 2021) %>%
    dplyr::mutate(farms = c(4, 3, 4, 5, 6, 4, 4, 5, 4, 5, 5, 3, 4, 3, 4)) %>%
    dplyr::mutate(year = as.numeric(year))

  return(pred_yearly)
}

# power_pink_mod ===============================================================
power_pink_mod <- function(pred_yearly, pink_sr_df, output_path) {
  #' Fit the models for pink salmon that we need for the power analysis
  #'
  #' @description Fit the model and save the objects for pink salmon to be used
  #' in the power analysis
  #'
  #' @param pred_yearly dataframe. Predicted amounts of lice per year
  #' @param pink_sr_df dataframe. The clean sr data for pinks alone
  #' @param output_path character. Where to save the fit objects
  #'
  #' @usage power_pink_mod(pred_yearly, pink_sr_df, output_path)
  #' @return NA
  #'

  ## calculate survival ========================================================
  pink_sr <- pink_sr_df %>%
    dplyr::select(
      brood_year, river, species, area,
      spawners, returns, recruits
    ) %>%
    # get the survival
    dplyr::rowwise() %>%
    dplyr::mutate(
      survival = log(recruits / spawners),
      lice = as.numeric(NA),
      return_year = brood_year + 2,
      lice_year = brood_year + 1
    )

  ## insert NA's where we need them ============================================
  # make the 10 years before data equal to NA
  pink_sr[which(pink_sr$area == "7" &
    pink_sr$lice_year %in% c(1994:2005)), "lice"] <- NA
  # make effect in all other areas 0
  pink_sr[which(pink_sr$area != "7"), "lice"] <- 0
  # make effect in area 7 before 1994 into 0
  pink_sr[which(pink_sr$area == "7" &
    pink_sr$lice_year < 1994), "lice"] <- 0

  pred_yearly <- pred_yearly %>% dplyr::mutate(year = as.numeric(year))
  # str(pred_yearly); str(pink_sr)
  for (year in c(2006:max(pink_sr$lice_year))) {
    pink_sr[which(pink_sr$area == "7" &
      pink_sr$lice_year == year), "lice"] <-
      pred_yearly[which(
        pred_yearly$year == year
      ), "fit"]
  }

  # to double check the loop worked properly, there should only be the number of
  # NA's equal to the number of observations between years 1994 & 2004 that are
  # in area 7
  if (nrow(pink_sr[which(pink_sr$area == "7" &
    pink_sr$lice_year %in% c(1994:2005)), ]) !=
    nrow(pink_sr[which(is.na(pink_sr$lice)), ])) {
    stop("ERROR - something wrong in the NA ascribes")
  }

  ## fit stock recruit model for pre-lice years ================================

  # fit model for pre-affect years
  pink_sr_pre_2005 <- pink_sr %>%
    dplyr::filter(brood_year < 2005) %>%
    dplyr::mutate(
      river = as.factor(river),
      area = as.factor(area),
      brood_year = as.factor(brood_year)
    )

  # find out the ones with too few observations
  too_few <- data.frame(table(pink_sr_pre_2005$river))
  too_few <- too_few[which(too_few$Freq < 4), "Var1"]

  # take the too-few ones out
  pink_sr_pre_2005 <- pink_sr_pre_2005 %>%
    dplyr::filter(river %notin% too_few)

  # fit null model pre-power analysis
  null_model <- lme4::lmer(survival ~ spawners:river + (1 | brood_year / area),
    data = pink_sr_pre_2005
  )
  summary(null_model)

  # get the fixed effects values here to use in the loops ======================
  r <- lme4::fixef(null_model)[[1]]
  b_i_vals <- lme4::fixef(null_model)

  # So there's this "missing" level and it's because Carpenter Bay doesn't
  # actually fit a value since it has no data. We need to replace this with NA
  all_rivers <- unique(pink_sr_pre_2005$river)
  missing_level <- all_rivers[which(all_rivers %notin%
    stringr::str_remove(
      names(b_i_vals),
      "spawners:river"
    ))]
  # adding in the name here so the whole loop below works properly
  names(b_i_vals) <- c(
    paste0("spawners:river", missing_level),
    names(b_i_vals)[2:length(b_i_vals)]
  )
  b_i_vals[1] <- as.numeric(NA)
  b_i_df <- data.frame(b_i_vals)
  b_i_df$popn <- rownames(b_i_df)
  rownames(b_i_df) <- NULL
  # clean up the river names
  b_i_df$popn <- stringr::str_remove(b_i_df$popn, "spawners:river")

  # get the estimated variance for the three types of random effects
  re_sd <- lme4::VarCorr(null_model)
  sd_area_year <- attr(re_sd$`area:brood_year`, "stddev")
  sd_year <- attr(re_sd$brood_year, "stddev")
  resid_sd <- sigma(null_model)

  df_fit_items <- data.frame(
    r = r,
    resid_sd = resid_sd,
    sd_area_year = sd_area_year[[1]],
    sd_year = sd_year[[1]]
  )

  # save objects
  readr::write_csv(
    x = df_fit_items,
    path = paste0(output_path, "pink-fit-null-model-objects.csv")
  )
  readr::write_csv(
    x = pink_sr,
    path = paste0(output_path, "pink-sr-data-ready-for-sims.csv")
  )
  readr::write_csv(
    x = b_i_df,
    path = paste0(output_path, "pink-b-i-df.csv")
  )

  list_obs <- list(
    df_fit_items = df_fit_items,
    pink_sr = pink_sr,
    b_i_df = b_i_df
  )

  return(list_obs)
}

# power_prep_chum ==============================================================
power_prep_chum <- function(wild_lice) {
  #' Fit the actual models we'll need for the power analysis
  #'
  #' @description Fit the model and save the objects for chum salmon to be used
  #' in the power analysis
  #'
  #' @param wild_lice dataframe. The lice on wild fish data
  #'
  #' @usage power_prep_chum(all_power_sims, output_path)
  #' @return NA
  #'

  ## first regression for the wild lice ========================================
  wild_lice <- wild_lice %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      date = lubridate::make_date(year, month, day),
      site = as.factor(site),
      year = as.factor(year)
    ) %>%
    dplyr::mutate(
      week = as.factor(lubridate::week(date))
    )

  # do first with negative binomial
  wild_lice_glmm_nb <- glmmTMB::glmmTMB(
    lep_total ~ year + (1 | week) + (1 | site),
    family = nbinom2,
    data = wild_lice
  )
  # now with poisson
  wild_lice_glmm_poi <- glmmTMB::glmmTMB(
    lep_total ~ year + (1 | week) + (1 | site),
    family = poisson(link = "log"),
    data = wild_lice
  )

  ## do a check to see which model is better ===================================
  if (AIC(wild_lice_glmm_nb) < (AIC(wild_lice_glmm_poi) + 2)) {
    # the plus two is to make sure that the nb is at least 2 delta AIC better
    best_model <- wild_lice_glmm_nb
  } else if (AIC(wild_lice_glmm_poi) < (AIC(wild_lice_glmm_nb) + 2)) {
    best_model <- wild_lice_glmm_poi
  } else {
    stop("Models not different enough via AIC - requires manual decision")
  }

  ## model predictions =========================================================
  predict_data <- data.frame(
    year = as.character(c(2006:2022)),
    week = NA,
    site = NA
  )

  predicted_yearly_lice <- data.frame(
    year = as.character(c(2006:2022)),
    stats::predict(
      object = best_model,
      newdata = predict_data,
      re.form = ~0,
      se.fit = TRUE,
      type = "response"
    )
  ) %>%
    dplyr::mutate(
      area = 7
    )

  # get yearly predicted
  pred_yearly <- predicted_yearly_lice %>%
    dplyr::filter(year < 2018) %>% # this is when our chum data end
    dplyr::mutate(farms = c(4, 3, 4, 5, 6, 4, 4, 5, 4, 5, 5, 3)) %>%
    dplyr::mutate(year = as.numeric(year))

  return(pred_yearly)
}

# power_chum_mod ===============================================================
power_chum_mod <- function(pred_yearly, chum_sr_df, output_path) {
  #' Fit the models for pink salmon that we need for the power analysis
  #'
  #' @description Fit the model and save the objects for pink salmon to be used
  #' in the power analysis
  #'
  #' @param pred_yearly dataframe. Predicted amounts of lice per year
  #' @param chum_sr_df dataframe. The clean sr data for pinks alone
  #' @param output_path character. Where to save the fit objects
  #'
  #' @usage power_pink_mod(pred_yearly, chum_sr_df, output_path)
  #' @return NA
  #'

  ## calculate survival ========================================================
  chum_sr <- chum_sr_df %>%
    dplyr::select(
      brood_year, river, species, area,
      spawners, returns, recruits
    ) %>%
    # get the survival
    dplyr::rowwise() %>%
    dplyr::mutate(
      survival = log(recruits / spawners),
      lice = as.numeric(NA),
      return_year = brood_year + 2,
      lice_year = brood_year + 1
    )

  ## insert NA's where we need them ============================================
  # make the 10 years before data equal to NA
  chum_sr[which(chum_sr$area == "7" &
    chum_sr$lice_year %in% c(1994:2005)), "lice"] <- NA
  # make effect in all other areas 0
  chum_sr[which(chum_sr$area != "7"), "lice"] <- 0
  # make effect in area 7 before 1994 into 0
  chum_sr[which(chum_sr$area == "7" &
    chum_sr$lice_year < 1994), "lice"] <- 0

  pred_yearly <- pred_yearly %>% dplyr::mutate(year = as.numeric(year))
  # str(pred_yearly); str(chum_sr)
  for (year in c(2006:max(chum_sr$lice_year))) {
    chum_sr[which(chum_sr$area == "7" &
      chum_sr$lice_year == year), "lice"] <-
      pred_yearly[which(
        pred_yearly$year == year
      ), "fit"]
  }

  # to double check the loop worked properly, there should only be the number of
  # NA's equal to the number of observations between years 1994 & 2004 that are
  # in area 7
  if (nrow(chum_sr[which(chum_sr$area == "7" &
    chum_sr$lice_year %in% c(1994:2005)), ]) !=
    nrow(chum_sr[which(is.na(chum_sr$lice)), ])) {
    stop("ERROR - something wrong in the NA ascribes")
  }

  ## fit stock recruit model for pre-lice years ================================

  # fit model for pre-affect years
  chum_sr_pre_2005 <- chum_sr %>%
    dplyr::filter(brood_year < 2005) %>%
    dplyr::mutate(
      river = as.factor(river),
      area = as.factor(area),
      brood_year = as.factor(brood_year)
    )

  # find out the ones with too few observations
  too_few <- data.frame(table(chum_sr_pre_2005$river))
  too_few <- too_few[which(too_few$Freq < 4), "Var1"]

  # take the too-few ones out
  chum_sr_pre_2005 <- chum_sr_pre_2005 %>%
    dplyr::filter(river %notin% too_few)

  # fit null model pre-power analysis
  null_model <- lme4::lmer(survival ~ spawners:river + (1 | brood_year / area),
    data = chum_sr_pre_2005
  )
  summary(null_model)

  # get the fixed effects values here to use in the loops ======================
  r <- lme4::fixef(null_model)[[1]]
  b_i_vals <- lme4::fixef(null_model)

  # So there's this "missing" level and it's because Carpenter Bay doesn't
  # actually fit a value since it has no data. We need to replace this with NA
  all_rivers <- unique(chum_sr_pre_2005$river)
  missing_level <- all_rivers[which(all_rivers %notin%
    stringr::str_remove(
      names(b_i_vals),
      "spawners:river"
    ))]
  # adding in the name here so the whole loop below works properly
  names(b_i_vals) <- c(
    paste0("spawners:river", missing_level),
    names(b_i_vals)[2:length(b_i_vals)]
  )
  b_i_vals[1] <- as.numeric(NA)
  b_i_df <- data.frame(b_i_vals)
  b_i_df$popn <- rownames(b_i_df)
  rownames(b_i_df) <- NULL
  # clean up the river names
  b_i_df$popn <- stringr::str_remove(b_i_df$popn, "spawners:river")

  # get the estimated variance for the three types of random effects
  re_sd <- lme4::VarCorr(null_model)
  sd_area_year <- attr(re_sd$`area:brood_year`, "stddev")
  sd_year <- attr(re_sd$brood_year, "stddev")
  resid_sd <- sigma(null_model)

  df_fit_items <- data.frame(
    r = r,
    resid_sd = resid_sd,
    sd_area_year = sd_area_year[[1]],
    sd_year = sd_year[[1]]
  )

  # save objects
  readr::write_csv(
    x = df_fit_items,
    path = paste0(output_path, "chum-fit-null-model-objects.csv")
  )
  readr::write_csv(
    x = chum_sr,
    path = paste0(output_path, "chum-sr-data-ready-for-sims.csv")
  )
  readr::write_csv(
    x = b_i_df,
    path = paste0(output_path, "chum-b-i-df.csv")
  )

  list_obs <- list(
    df_fit_items = df_fit_items,
    chum_sr = chum_sr,
    b_i_df = b_i_df
  )

  return(list_obs)
}
