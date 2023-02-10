library(readr)
library(here)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(lubridate)
library(glmmTMB)

farm_lice <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))
pink_sr_df <- read_csv(here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))
wild_lice <- read_csv(here("./data/wild-lice/clean/clean-wild-lice-df.csv"))

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

wild_lice_glmm_poi <- glmmTMB::glmmTMB(
  lep_total ~ year + (1 | week) + (1 | site),
  family = poisson,
  data = wild_lice
)
AIC(wild_lice_glmm_nb,wild_lice_glmm_poi) # nb is better 

predict_data <- data.frame(
  year = as.character(c(2005:2022))
)

predicted_yearly_lice <- data.frame(
  stats::predict(
    wild_lice_glmm_nb,
    new_data = predict_data,
    type = "response",
    re.form = NA,
    se.fit = TRUE
  )
)
predict(wild_lice_glmm_nb,
        new_data = predict_data,
        type = "response")

# keep only the relevant info for pinks
pink_sr <- pink_sr_df %>% 
  dplyr::select(brood_year, river, species, area, 
                spawners, returns, recruits) %>% 
  # get the survival
  dplyr::rowwise() %>% 
  dplyr::mutate(survival = log(recruits / spawners))

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

re_effects <- lme4::ranef(null_model)
re_area_year <- re_effects$`area:brood_year`
re_year <- re_effects$brood_year

re_sd <- lme4::VarCorr(null_model)
sd_area_year <- attr(re_sd$`area:brood_year`, "stddev")
sd_year <- attr(re_sd$brood_year, "stddev")
resid_sd <- sigma(null_model)

mean(re_effects$`area:brood_year`)

for(year in 2005:2015) {
  year_re <- rnorm(0, sd_year)
  for(area in )
}


summary(null_model)

