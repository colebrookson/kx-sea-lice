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
library(bayesplot)


farm_lice <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))
chum_sr_df <- read_csv(here("./data/spawner-recruit/clean/chum-sr-data-clean.csv"))
wild_lice <- read_csv(here("./data/wild-lice/clean/clean-wild-lice-df.csv"))
exposure_df <- read_csv(here("./data/spawner-recruit/clean/chum-exposure-categorization-df.csv"))

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
chum_sr <- chum_sr_df %>% 
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


chum_sr_2005_onward <- chum_sr %>% 
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

chum_sr_pre_2005 <- chum_sr %>% 
  dplyr::filter(lice_year < 2005) %>% 
  dplyr::mutate(
    lice_1 = NA,
    lice_2 = NA,
    lice_3 = NA,
    certainty = "certain"
  )

# make the 10 years before data equal to NA
chum_sr_pre_2005[which(chum_sr_pre_2005$area %in% c("6","7") & 
                         chum_sr_pre_2005$lice_year %in% c(1994:2004)), 
                 c("lice_1", "lice_2", "lice_3")] <- NA
# make effect in all other areas 0
chum_sr_pre_2005[which(chum_sr_pre_2005$area %notin% c("6","7")), 
                 c("lice_1", "lice_2", "lice_3")] <- 0
# make effect in area 7 before 1994 into 0
chum_sr_pre_2005[which(chum_sr_pre_2005$area %in% c("6","7") & 
                         chum_sr_pre_2005$lice_year < 1994), 
                 c("lice_1", "lice_2", "lice_3")] <- 0

chum_sr <- rbind(chum_sr_pre_2005, chum_sr_2005_onward) 
# get rid of the two observations with recruits = 0
chum_sr <- chum_sr[which(chum_sr$recruits != 0),]

# make a separate factor that accounts for brood year and area 
chum_sr$area_year <- as.factor(paste(chum_sr$brood_year,
                                     chum_sr$area, sep = "_"))

# make sure all RE's are factors
chum_sr$brood_year <- as.factor(chum_sr$brood_year)
chum_sr$area <- as.factor(chum_sr$area)
chum_sr$con_unit <- as.factor(chum_sr$con_unit)
chum_sr$river <- as.factor(chum_sr$river)


# bayesian approach ============================================================

## just brood year and area ====================================================
start_time <- Sys.time()
by_area_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area),
  data = chum_sr,
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_null,
          here("./outputs/model-outputs/chum/by-area-null.qs"))
by_area_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty + (1|brood_year/area),
  data = chum_sr,
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_alt,
          here("./outputs/model-outputs/chum/by-area-alt.qs"))
end_time <- Sys.time()
end_time - start_time
## conservation unit and brood year ============================================
start_time <- Sys.time() 
con_by_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year) +
    (-1+con_unit|brood_year),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(con_by_null,
          here("./outputs/model-outputs/chum/con-by-null.qs"))
con_by_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  + (1|brood_year) +
    (-1+con_unit|brood_year),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(con_by_alt,
          here("./outputs/model-outputs/chum/con-by-alt.qs"))
end_time <- Sys.time() 
end_time - start_time

## river and brood year ========================================================
start_time <- Sys.time()
river_by_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|river) + (1|brood_year),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(river_by_null,
          here("./outputs/model-outputs/chum/river-by-null.qs"))
river_by_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  +(1|river) + (1|brood_year),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(river_by_alt,
          here("./outputs/model-outputs/chum/river-by-alt.qs"))

## con unit brood year area ====================================================
by_area_con_by_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area) +
    (-1+con_unit|brood_year),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_con_by_null,
          here("./outputs/model-outputs/chum/by-area-con-by-null.qs"))

by_area_con_by_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  + (1|brood_year/area) +
    (-1+con_unit|brood_year),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_con_by_alt,
          here("./outputs/model-outputs/chum/by-area-con-by-alt.qs"))

## river brood year area =======================================================
by_area_river_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area) + (1|river),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_river_null,
          here("./outputs/model-outputs/chum/by-area-river-null.qs"))

by_area_river_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  + (1|brood_year/area) + 
    (1|river),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_river_alt,
          here("./outputs/model-outputs/chum/by-area-river-alt.qs"))
end_time <- Sys.time() 
end_time - start_time
## all re's ====================================================================
start_time <- Sys.time()
all_re_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area) + 
    (-1+con_unit|brood_year) + (1|river),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(all_re_null,
          here("./outputs/model-outputs/chum/all-re-null.qs"))

all_re_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  + (1|area_year) + (1|area) + 
    (-1+con_unit|brood_year)  + (1|river),
  data = chum_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4,
  iter = 8000
)
qs::qsave(all_re_alt,
          here("./outputs/model-outputs/chum/all-re-alt.qs"))

bayesplot::color_scheme_set("purple")
pairs(all_re_alt, 
      pars = c("(Intercept)", "log-posterior", "lice_3:certaintycertain", 
               "sigma", "lice_3:certaintyuncertain"))

posterior <- as.matrix(all_re_alt)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 90% intervals")
bayesplot::mcmc_areas(
  posterior,
  pars = c("(Intercept)", "lice_3:certaintycertain", 
           "lice_3:certaintyuncertain"),
  prob = 0.9
) + 
  plot_title + 
  scale_y_discrete(labels = c("Growth Rate", "Certain Lice", "Uncertain Lice"))

