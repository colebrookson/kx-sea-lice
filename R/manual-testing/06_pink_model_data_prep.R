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
pink_sr_df <- read_csv(here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))
wild_lice <- read_csv(here("./data/wild-lice/clean/clean-wild-lice-df.csv"))
exposure_df <- read_csv(here("./data/spawner-recruit/clean/pink-exposure-categorization-df.csv"))

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
    re.form = ~0,
    se.fit = TRUE,
    type = "response"
  )
) %>% 
  dplyr::mutate(
    # area = 7,
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

## BEGIN NOTE 
#' There are three different categorizations we're doing here: 
#' 1 - assuming the marginally exposed fish are fully exposed
#' 2 - assuming they're not exposed at all
#' 3 - adding a parameter to account for that level
#' Then we take these three groupings and compare them with each other to see
#' if they happen to fit better or not
#' 

# 5 data structures ============================================================

#' 1) All maybes are no's
#' 2) All maybes are yes's
#' 3) All maybes are maybes
#' 4) Northern maybes are maybes, southern maybes are no's
#' 5) Southern maybes are maybes, northern maybes are no's

# format exposure df's for each one -- for the first three, we can use existing
# data format
exposure_df_all_scen <- exposure_df %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    exposure_4 = dplyr::case_when(
      maybes == "south" ~ "no",
      TRUE              ~ exposure
    ),
    exposure_5 = dplyr::case_when(
      maybes == "north" ~ "no",
      TRUE              ~ exposure
    )
  ) %>% 
  dplyr::select(-maybes)

pink_sr_2005_onward <- pink_sr %>% 
  dplyr::filter(lice_year >= 2005) %>% 
  dplyr::left_join(
    ., 
    y = exposure_df_all_scen,
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
    lice_4 = dplyr::case_when(
      exposure == "yes"   ~ fit,
      exposure == "maybe" ~ fit,
      exposure == "no"    ~ 0,
      TRUE                ~ 0
    ),
    lice_5 = dplyr::case_when(
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
    )),
    certainty_4 = as.factor(dplyr::case_when(
      exposure_4 == "yes"   ~ "certain",
      exposure_4 == "maybe" ~ "uncertain",
      exposure_4 == "no"    ~ "certain",
      TRUE                  ~ "certain"
    )),
    certainty_5 = as.factor(dplyr::case_when(
      exposure_5 == "yes"   ~ "certain",
      exposure_5 == "maybe" ~ "uncertain",
      exposure_5 == "no"    ~ "certain",
      TRUE                  ~ "certain"
    )),
  ) %>% 
  dplyr::select(-c(fit, exposure)) %>% 
  dplyr::mutate(
    area = as.factor(area),
    certainty = as.factor(certainty)
  )

pink_sr_pre_2005 <- pink_sr %>% 
  dplyr::filter(lice_year < 2005) %>% 
  dplyr::mutate(
    exposure_4 = NA,
    exposure_5 = NA,
    lice_1 = NA,
    lice_2 = NA,
    lice_3 = NA,
    lice_4 = NA,
    lice_5 = NA,
    certainty = "certain",
    certainty_4 = "certain",
    certainty_5 = "certain"
  )

# make the 10 years before data equal to NA, but for each pop'n that has an 
# exposure
any_exposed_pop <- pink_sr_2005_onward %>% 
  dplyr::filter(!is.na(lice_1)) %>% 
  distinct(gfe_id)

pink_sr_pre_2005[which(pink_sr_pre_2005$gfe_id %in% any_exposed_pop$gfe_id & 
                         pink_sr_pre_2005$lice_year %in% c(1994:2004)), 
                 c("lice_1", "lice_2", "lice_3", "lice_4", "lice_5")] <- NA
# make effect in all other areas 0
pink_sr_pre_2005[which(pink_sr_pre_2005$area %notin% any_exposed_pop$gfe_id), 
                 c("lice_1", "lice_2", "lice_3", "lice_4", "lice_5")] <- 0
# make effect in area 7 before 1994 into 0
pink_sr_pre_2005[which(pink_sr_pre_2005$area %in% any_exposed_pop$gfe_id & 
                         pink_sr_pre_2005$lice_year < 1994), 
                 c("lice_1", "lice_2", "lice_3", "lice_4", "lice_5")] <- 0

pink_sr <- rbind(pink_sr_pre_2005, pink_sr_2005_onward) 
# get rid of the two observations with recruits = 0
pink_sr <- pink_sr[which(pink_sr$recruits != 0),]

# make a separate factor that accounts for brood year and area 
pink_sr$area_year <- as.factor(paste(pink_sr$brood_year,
                                     pink_sr$area, sep = "_"))

# make sure all RE's are factors
pink_sr$brood_year <- as.factor(pink_sr$brood_year)
pink_sr$area <- as.factor(pink_sr$area)
pink_sr$con_unit <- as.factor(pink_sr$con_unit)
pink_sr$river <- as.factor(pink_sr$river)

write_csv(pink_sr, 
          here::here("./data/spawner-recruit/clean/pink-sr-for-model.csv"))