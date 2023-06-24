library(readr)
library(here)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(lubridate)
library(glmmTMB)
library(stringr)
library(rstanarm)


farm_lice <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))
pink_sr_df <- read_csv(here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))
wild_lice <- read_csv(here("./data/wild-lice/clean/clean-wild-lice-df.csv"))
exposure_df <- read_csv(here("./data/spawner-recruit/clean/exposure-categorization-df.csv"))

source(here("./R/00_functions_global.R"))

# wild lice regression =========================================================

# first regression for the wild lice
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

wild_lice_glmm_nb <- glmmTMB::glmmTMB(
  lep_total ~ year + (1 | week) + (1 | site),
  family = nbinom2,
  data = wild_lice
)

summary(wild_lice_glmm_nb)

# wild_lice_glmm_poi <- glmmTMB::glmmTMB(
#   lep_total ~ year + (1 | week) + (1 | site),
#   family = poisson(link = "log"),
#   data = wild_lice
# )
# AIC(wild_lice_glmm_nb,wild_lice_glmm_poi) # nb is better 

predict_data <- data.frame(
  year = as.character(c(2005:2022)),
  week = NA,
  site = NA
)

predicted_yearly_lice <- data.frame(
  year = as.character(c(2005:2022)),
  stats::predict(
    object = wild_lice_glmm_nb,
    newdata = predict_data,
    re.from = ~0,
    se.fit = TRUE,
    type = "response"
  )
) %>% 
  dplyr::mutate(
    area = 7,
    up_ci = fit + 1.96*se.fit,
    lo_ci = fit - 1.96*se.fit
  )

pred_yearly <- predicted_yearly_lice %>% 
  dplyr::filter(year < 2021) %>% 
  dplyr::mutate(farms = c(2, 4, 3, 4, 5, 6, 4, 4, 5, 4, 5, 5, 3, 4, 3, 4))
ggplot(data = pred_yearly) + 
  geom_errorbar(aes(x = year, ymin = lo_ci, ymax = up_ci),
             colour = "black", width = 0) + 
  geom_point(aes(x = year, y = fit), colour = "purple", size = 2) + 
  theme_base() + 
  #ylim(c(0, 0.8)) + 
  theme(
    axis.text.x = element_text(angle = 90)
  ) + 
  labs(x = "year", y = "Estimated Number of Lice per Wild Fish")
  
# set up lice data in the stock recruit data ===================================

# keep only the relevant info for pinks
pink_sr <- pink_sr_df %>% 
  dplyr::select(gfe_id, brood_year, con_unit, river, species, area, 
                spawners, returns, recruits) %>% 
  # get the survival
  dplyr::rowwise() %>% 
  dplyr::mutate(
    survival = log(recruits / spawners),
    lice = as.numeric(NA),
    return_year = brood_year + 2,
    lice_year = brood_year + 1
  )

## BEGIN NOTE ==================================================================
#' There are three different categorizations we're doing here: 
#' 1 - assuming the marginally exposed fish are fully exposed
#' 2 - assuming they're not exposed at all
#' 3 - adding a parameter to account for that level
#' Then we take these three groupings and compare them with each other to see
#' if they happen to fit better or not
#' 

pink_sr_2005_onward <- pink_sr %>% 
  dplyr::filter(lice_year >= 2005) %>% 
  dplyr::left_join(
    ., 
    y = exposure_df,
    by = c("gfe_id" = "sites", "lice_year" = "year")
  ) %>%
  dplyr::left_join(
    .,
    y = pred_yearly %>% dplyr::mutate(year = as.integer(year)) %>% 
      dplyr::select(year, fit),
    by = c("lice_year" = "year")
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    lice_1 = dplyr::case_when(
      exposure == "yes"   ~ fit,
      exposure == "maybe" ~ fit,
      exposure == "no"    ~ 0,
      TRUE                ~ 0
    ), 
    lice_2 = dplyr::case_when(
      exposure == "yes"   ~ fit,
      exposure == "maybe" ~ 0,
      exposure == "no"    ~ 0,
      TRUE                ~ 0
    ), 
    lice_3 = dplyr::case_when(
      exposure == "yes"   ~ fit,
      exposure == "maybe" ~ fit,
      exposure == "no"    ~ 0,
      TRUE                ~ 0
    ),
    certainty = as.factor(dplyr::case_when(
      exposure == "yes"   ~ "certain",
      exposure == "maybe" ~ "uncertain",
      exposure == "no"    ~ "certain",
      TRUE                ~ "certain"
    ))
  ) %>% 
  dplyr::select(-c(fit, exposure)) %>% 
  dplyr::mutate(
    area = as.factor(area),
    certainty = as.factor(certainty)
  )

pink_sr_pre_2005 <- pink_sr %>% 
  dplyr::filter(lice_year < 2005) %>% 
  dplyr::mutate(
    lice_1 = NA,
    lice_2 = NA,
    lice_3 = NA,
    certainty = "certain"
  )

# make the 10 years before data equal to NA
pink_sr_pre_2005[which(pink_sr_pre_2005$area %in% c("6","7") & 
                         pink_sr_pre_2005$lice_year %in% c(1994:2004)), 
                 c("lice_1", "lice_2", "lice_3")] <- NA
# make effect in all other areas 0
pink_sr_pre_2005[which(pink_sr_pre_2005$area %notin% c("6","7")), 
                 c("lice_1", "lice_2", "lice_3")] <- 0
# make effect in area 7 before 1994 into 0
pink_sr_pre_2005[which(pink_sr_pre_2005$area %in% c("6","7") & 
                         pink_sr_pre_2005$lice_year < 1994), 
                 c("lice_1", "lice_2", "lice_3")] <- 0

pink_sr <- rbind(pink_sr_pre_2005, pink_sr_2005_onward) 
# get rid of the two observations with recruits = 0
pink_sr <- pink_sr[which(pink_sr$recruits != 0),]

# frequentist approach =========================================================

# see if the data can be sub-set
# pink_sr_river_counts = pink_sr %>% 
#   group_by(river, brood_year, area) %>% 
#   summarize(n = n())
# pink_sr <- pink_sr %>% 
#   dplyr::left_join(
#     ., 
#     y = pink_sr_river_counts,
#     by = "river"
#   )
# 
# ## model fits ==================================================================
# 
# freq_null_model <- lme4::lmer(survival ~ spawners:river + (1|brood_year/area),
#                          data = pink_sr)
# freq_alt_mod_1 <- lme4::lmer(survival ~ spawners:river + lice_1 +
#                         (1|brood_year/area),
#                       data = pink_sr)
# freq_alt_mod_2 <- lme4::lmer(survival ~ spawners:river + lice_2 +
#                           (1|brood_year/area),
#                         data = pink_sr)
# freq_alt_mod_3 <- lme4::lmer(survival ~ spawners:river + lice_3:certainty +
#                           (1|brood_year/area),
#                         data = pink_sr)
# 
# freq_alt_mod_4 <- lme4::lmer(survival ~ spawners:river + lice_1 +
#                                       (1|brood_year/area) + (1|river),
#                                     data = pink_sr)
# 
# all_fit <- allFit(freq_alt_mod_4, maxfun = 1e05)
# 
# ## process model results =======================================================
# fixef(alt_mod_3)
# ranef(alt_mod_3)
# 
# AIC(null_model, alt_mod_1, alt_mod_2, alt_mod_3, alt_mod_4)
# summary(alt_mod_1)
# summary(alt_mod_2)
# summary(alt_mod_3)
# summary(alt_mod_4)
# -0.354 == -3.540e-01
# 
# coefs_1 <- broom.mixed::tidy(alt_mod_1)
# coefs_2 <- broom.mixed::tidy(alt_mod_2)
# coefs_3 <- broom.mixed::tidy(alt_mod_3)
# coefs_1 %>% 
#   dplyr::filter(term == "lice_1")
# coefs_2 %>% 
#   dplyr::filter(term == "lice_2")
# coefs_3 %>% 
#   dplyr::filter(term %in% c("lice_3:certaintycertain", 
#                             "lice_3:certaintyuncertain"))
# 
# c_df <- data.frame(
#   c = c(-0.354,-0.354,-0.495,-0.5078),
#   std_err = c(0.147, 0.155, 0.155, 0.155),
#   model = c("model 1", "model 2", "model 3 - exp.", "model 3 - pot.")
# ) %>% 
#   dplyr::mutate(
#     up = c + (1.96*std_err),
#     lo = c - (1.96*std_err)
#   )
# 
# ggplot(data = c_df) + 
#   geom_errorbar(aes(x = model, ymin = lo, ymax = up),
#                 width = 0) + 
#   geom_point(aes(x = model, y = c, fill = model), 
#              colour = "black", shape = 21, size = 3) + 
#   geom_hline(aes(yintercept = 0), colour = "grey80", linetype = "dashed") +
#   theme_base()
# 
# ## plot observations in each category ==========================================
# ggplot(data = exposure_df) + 
#   geom_histogram(aes(x = exposure), stat = "count",
#                  colour = "black", fill = c("yellow3", "green4", "red3")) + 
#   theme_base() + 
#   labs(y = "No. of Obs in Each Category")

# bayesian approach ============================================================

## model fitting ===============================================================
# 
bayes_null_model <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area) +
    (1|con_unit/brood_year) + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  cores = 8
)
qs::qsave(bayes_null_model,
          here("./outputs/model-outputs/bayes-null-model-ob.qs"))

bayes_alt_model_1 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_1 + (1|brood_year/area) +
    (1|con_unit/brood_year) + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  cores = round(0.8 * parallel::detectCores())
)
qs::qsave(bayes_alt_model_1,
          here("./outputs/model-outputs/bayes-alt-model-1-ob.qs"))

bayes_alt_model_2 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_2 + (1|brood_year/area) +
    (1|con_unit/brood_year) + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  cores = round(0.8 * parallel::detectCores())
)
qs::qsave(bayes_alt_model_2,
          here("./outputs/model-outputs/bayes-alt-model-2-ob.qs"))

bayes_alt_model_3 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty + (1|brood_year/area) +
    (1|con_unit/brood_year) + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  cores = round(0.8 * parallel::detectCores())
)
qs::qsave(bayes_alt_model_3,
          here("./outputs/model-outputs/bayes-alt-model-3-ob.qs"))

## Model selection =============================================================

bayes_null_model <- qs::qread(
  here("./outputs/model-outputs/bayes-null-model-ob.qs"))
bayes_alt_model_1 <- qs::qread(
  here("./outputs/model-outputs/bayes-alt-model-1-ob.qs"))
bayes_alt_model_2 <- qs::qread(
  here("./outputs/model-outputs/bayes-alt-model-2-ob.qs"))
bayes_alt_model_3 <- qs::qread(
  here("./outputs/model-outputs/bayes-alt-model-3-ob.qs"))

#launch_shinystan(bayes_null_model, ppd = FALSE)

null_shiny <- shinystan::as.shinystan(bayes_null_model)
alt1_shiny <- shinystan::as.shinystan(bayes_alt_model_1)
alt2_shiny <- shinystan::as.shinystan(bayes_alt_model_2)
alt3_shiny <- shinystan::as.shinystan(bayes_alt_model_3)

# calculate the waic for each of the models, then save the objects again
bayes_null_model$waic <- waic(bayes_null_model,
                cores = 12)
bayes_alt_model_1$waic <- waic(bayes_alt_model_1,
                cores = 17)
bayes_alt_model_2$waic <- waic(bayes_alt_model_2,
                cores = 16)
bayes_alt_model_3$waic <- waic(bayes_alt_model_3,
                cores = 16)

qs::qsave(bayes_null_model,
          here("./outputs/model-outputs/bayes-null-model-ob.qs"))
qs::qsave(bayes_alt_model_1,
          here("./outputs/model-outputs/bayes-alt-model-1-ob.qs"))
qs::qsave(bayes_alt_model_2,
          here("./outputs/model-outputs/bayes-alt-model-2-ob.qs"))
qs::qsave(bayes_alt_model_3,
          here("./outputs/model-outputs/bayes-alt-model-3-ob.qs"))

# compare the models 


