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
  family = poisson(link = "log"),
  data = wild_lice
)
AIC(wild_lice_glmm_nb,wild_lice_glmm_poi) # nb is better 

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
                pink_sr$lice_year < 2005), "lice"] <- 0

for(year in c(min(pink_sr$lice_year):max(pink_sr$lice_year))) {
  for(area in unique(pink_sr$area)) {
    if(area == "7") {
      if(year > 2004) {
        pink_sr[which(pink_sr$area == "7" & 
                        pink_sr$lice_year == year), "lice"] <-
          predicted_yearly_lice[which(
            predicted_yearly_lice$year == year), "fit"]
      } else if(year %in% c(1994:2004)) {
        pink_sr[which(pink_sr$area == "7" & 
                        pink_sr$lice_year == year), "lice"] <- NA
      } else {
        pink_sr[which(pink_sr$area == area & 
                        pink_sr$lice_year == year), "lice"] <- 0
      }
    } else {
      pink_sr[which(pink_sr$area == area & 
                      pink_sr$lice_year == year), "lice"] <- 0
    }
  }
}

# to double check the loop worked properly, there should only be the number of 
# NA's equal to the number of observations between years 1994 and 2004 that are
# in area 7
nrow(pink_sr[which(pink_sr$area == "7" & 
                     pink_sr$brood_year %in% c(1994:2004)), ])
nrow(pink_sr[which(is.na(pink_sr$lice)), ])

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

# get the fixed effects values here to use in the loops
r <- lme4::fixef(null_model)[[1]]
b_i_vals <- lme4::fixef(null_model)

# get the estimated variance for the three types of random effects
re_sd <- lme4::VarCorr(null_model)
sd_area_year <- attr(re_sd$`area:brood_year`, "stddev")
sd_year <- attr(re_sd$brood_year, "stddev")
resid_sd <- sigma(null_model)

# set areas here
all_areas <- unique(pink_sr[which(pink_sr$brood_year > 2004), "area"])

# number of times this happens
for(i in 1:1000) {
  
  # setting the hypothetical value of c
  for(c in seq(0, 1, 0.01)) {
    
    # years we have data for
    for(year in 2005:2015) {
      year_re <- rnorm(0, sd_year) # set the year random effect value
      
      for(area in all_areas) {
        area_re <- rnorm(0, sd_area_year) # set the area w/in year random effect value
        
        # get temporary subset of data 
        temp_df <- pink_sr[which(pink_sr$brood_year == as.character(year) & 
                                   pink_sr$area == area), ]
        if(nrow(temp_df)) {
          next
        }
        temp_pops <- unique(temp_df$river)
        
        for(popn in temp_pops) {
        
          b_i <- b_i_vals[[paste0("spawners:river", popn)]]
          survival <- r - b_i * temp[which(river == popn), "recruits"] -
            # the c term and lice goes here
            
        }
      }
    }
  }
}



summary(null_model)







data(sleepstudy,package="lme4")
g0 <- glmmTMB(Reaction~Days+(Days|Subject),sleepstudy)

nd <- data.frame(
  Days = unique(sleepstudy$Days),
  Subject = NA
)

predict(g0, sleepstudy, type = "response")
## Predict new Subject
nd <- sleepstudy[1,]
nd$Subject <- "new"
predict(g0, newdata=nd, type = "response")
## population-level prediction
nd_pop <- data.frame(Days=unique(sleepstudy$Days),
                     Subject=NA)
predict(g0, newdata=nd_pop)