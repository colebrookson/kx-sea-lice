farm_lice <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))
pink_sr_df <- read_csv(here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))
chum_sr_df <- read_csv(here("./data/spawner-recruit/clean/chum-sr-data-clean.csv"))
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