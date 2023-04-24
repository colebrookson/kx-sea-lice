##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-04-22
#'
#' This file contains the functions that run regressions to figure out how many
#' lice per year there are on wild fish 
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# power_prep_pink ==============================================================
power_prep_pink <- function(farm_lice, pink_sr_df, wild_lice) {
  #' Fit the actual models we'll need for the power analysis
  #' 
  #' @description Fit the model and save the objects for pink salmon to be used
  #' in the power analysis 
  #' 
  #' @param farm_lice dataframe. The clean farm lice dataframe
  #' @param pink_sr_df dataframe. The clean sr data for pinks alone
  #' @param wild_lice dataframe. The lice on wild fish data
  #'  
  #' @usage power_prep_pink(all_power_sims, output_path)
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
  if(AIC(wild_lice_glmm_nb) < (AIC(wild_lice_glmm_poi) + 2)) {
    # the plus two is to make sure that the nb is at least 2 delta AIC better
    best_model <- wild_lice_glmm_nb
  } else if(AIC(wild_lice_glmm_poi) < (AIC(wild_lice_glmm_nb) + 2)) {
    best_model <- wild_lice_glmm_poi
  } else {
    stop("Models not different enough via AIC - requires manual decision")
  }
  
  ## model predictions =========================================================
  predict_data <- data.frame(
    year = as.character(c(2005:2022)),
    week = NA,
    site = NA
  )
  
  predicted_yearly_lice <- data.frame(
    year = as.character(c(2005:2022)),
    stats::predict(
      object = best_model,
      newdata = predict_data,
      re.from = ~0,
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
    dplyr::mutate(farms = c(2, 4, 3, 4, 5, 6, 4, 4, 5, 4, 5, 5, 3, 4, 3, 4))
  ggplot(data = pred_yearly) + 
    geom_point(aes(x = farms, y = fit), colour = "purple", size = 2) + 
    ggthemes::theme_base() + 
    ylim(c(0, 0.8))
}

farm_lice <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))
pink_sr_df <- read_csv(here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))
wild_lice <- read_csv(here("./data/wild-lice/clean/clean-wild-lice-df.csv"))

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
    area = 7
  )

pred_yearly <- predicted_yearly_lice %>% 
  dplyr::filter(year < 2021) %>% 
  dplyr::mutate(farms = c(2, 4, 3, 4, 5, 6, 4, 4, 5, 4, 5, 5, 3, 4, 3, 4))
ggplot(data = pred_yearly) + 
  geom_point(aes(x = farms, y = fit), colour = "purple", size = 2) + 
  ggthemes::theme_base() + 
  ylim(c(0, 0.8))


# set up lice data in the stock recruit data ===================================

# keep only the relevant info for pinks
pink_sr <- pink_sr_df %>% 
  dplyr::select(brood_year, river, species, area, 
                spawners, returns, recruits) %>% 
  # get the survival
  dplyr::rowwise() %>% 
  dplyr::mutate(
    survival = log(recruits / spawners),
    lice = as.numeric(NA),
    return_year = brood_year + 2,
    lice_year = brood_year + 1
  )

# make the 10 years before data equal to NA
pink_sr[which(pink_sr$area == "7" & 
                pink_sr$lice_year %in% c(1994:2004)), "lice"] <- NA
# make effect in all other areas 0
pink_sr[which(pink_sr$area != "7"), "lice"] <- 0
# make effect in area 7 before 1994 into 0
pink_sr[which(pink_sr$area == "7" & 
                pink_sr$lice_year < 1994), "lice"] <- 0

for(year in c(2005:max(pink_sr$lice_year))) {
  pink_sr[which(pink_sr$area == "7" & 
                  pink_sr$lice_year == year), "lice"] <-
    predicted_yearly_lice[which(
      predicted_yearly_lice$year == year), "fit"]
}

# to double check the loop worked properly, there should only be the number of 
# NA's equal to the number of observations between years 1994 and 2004 that are
# in area 7
nrow(pink_sr[which(pink_sr$area == "7" & 
                     pink_sr$lice_year %in% c(1994:2004)), ])
nrow(pink_sr[which(is.na(pink_sr$lice)), ])

# fit stock recruit model for pre-lice years ===================================

# fit model for pre-affect years
pink_sr_pre_2005 <- pink_sr %>% 
  dplyr::filter(brood_year < 2005) %>% 
  dplyr::mutate(
    river = as.factor(river),
    area = as.factor(area),
    brood_year = as.factor(brood_year)
  )

# fit null model pre-power analysis
null_model <- lme4::lmer(survival ~ spawners:river + (1|brood_year/area),
                         data = pink_sr_pre_2005)
summary(null_model)

# get the fixed effects values here to use in the loops
r <- lme4::fixef(null_model)[[1]]
b_i_vals <- lme4::fixef(null_model)

# So there's this "missing" level and it's because Carpenter Bay doesn't
# actually fit a value since it has no data. We need to replace this with an NA
all_rivers <- unique(pink_sr_pre_2005$river)
missing_level <- all_rivers[which(all_rivers %notin% 
                                    stringr::str_remove(names(b_i_vals), 
                                                        "spawners:river"))]
# adding in the name here so the whole loop below works properly
names(b_i_vals) <- c(paste0("spawners:river", missing_level), 
                     names(b_i_vals)[2:length(b_i_vals)])
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

readr::write_csv(
  x = df_fit_items, 
  path = here::here("./outputs/power-analysis/fit-null-model-objects.csv"))
readr::write_csv(
  x = pink_sr,
  path = here::here("./outputs/power-analysis/pink-sr-data-ready-for-sims.csv")
)
readr::write_csv(
  x = b_i_df,
  path = here::here("./outputs/power-analysis/b-i-df.csv")
)