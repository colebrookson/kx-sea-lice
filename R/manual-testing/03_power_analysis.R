library(readr)
library(here)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(lubridate)
library(glmmTMB)
library(stringr)

farm_lice <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))
pink_sr_df <- read_csv(here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))
wild_lice <- read_csv(here("./data/wild-lice/clean/clean-wild-lice-df.csv"))

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

# get the fixed effects values here to use in the loops
r <- lme4::fixef(null_model)[[1]]
b_i_vals <- lme4::fixef(null_model)
# because the first value (r) is essentially the reference value for the pop'ns
# that has to also act as the value for the "missing" level of the fixed effect
all_rivers <- unique(pink_sr$river)
missing_level <- all_rivers[which(all_rivers %notin% 
                                    stringr::str_remove(names(b_i_vals), 
                                                        "spawners:river"))]
# adding in the name here so the whole loop below works properly
names(b_i_vals) <- c(paste0("spawners:river", missing_level), 
                     names(b_i_vals)[2:length(b_i_vals)])

# get the estimated variance for the three types of random effects
re_sd <- lme4::VarCorr(null_model)
sd_area_year <- attr(re_sd$`area:brood_year`, "stddev")
sd_year <- attr(re_sd$brood_year, "stddev")
resid_sd <- sigma(null_model)

# set up and run loops for power analysis ======================================

# look for any populations that have more than one count in a year 
# x = pink_sr %>% 
#   group_by(area, river, brood_year) %>% 
#   summarize(n = n())

# set areas here
all_areas <- unique(pink_sr$area)

# setting the hypothetical value of c
# set up list for storage
c_list <- vector(mode = "list", length = 11)
c_counter <- 1
for(c in seq(0, 1, 0.1)) {
  
  # number of times this happens
  # set sub-c list 
  c_i_list <- vector(mode = "list", length = 10)
  for(i in 1:10) {
    
    # make dataframe to fill
    c_level_df <- pink_sr
    c_level_df$survival <- as.numeric(NA)
    
    # years we have data for
    for(curr_year in ##################### year loop ###########################
        c(min(pink_sr$brood_year):max(pink_sr$brood_year))) {
      # set the year random effect value
      year_re <- rnorm(n = 1, mean = 0, sd = sd_year) 
      
      for(curr_area in all_areas) { ############### area loop ##################
        # set the area w/in year random effect value
        area_re <- rnorm(n = 1, mean = 0, sd = sd_area_year) 
        
        # get temporary subset of data 
        temp_df <- c_level_df[which(c_level_df$brood_year == 
                                      as.character(curr_year) & 
                                      c_level_df$area == curr_area), ]
        if(nrow(temp_df) == 0) {
          next
        }
        
        for(row in seq_len(nrow(temp_df))) { ####### population loop ###########
          # draw pop & year level variance
          epsilon <- rnorm(1, mean = 0, sd = resid_sd)
          
          # get the population on hand
          popn <- temp_df[[row, "river"]]
          
          # set population level density dependence
          b_i <- b_i_vals[[paste0("spawners:river", popn)]]
          
          # calculate survival
          survival <- 
            # r, and b * N(t-2)
            r - (b_i * temp_df[[row, "spawners"]]) -
            # the c term and lice goes here
            (c * temp_df[[row, "lice"]]) +
            # yearly variation
            year_re +
            # area within year variation
            area_re + 
            # residual variation 
            epsilon
          
          # put survival value in temporary dataframe
          temp_df[row, "survival"] <- survival
        }
        # put the survival values in the larger dataframe 
        c_level_df[which(c_level_df$brood_year == as.character(curr_year) & 
                           c_level_df$area == curr_area), "survival"] <- 
          # this was just filled in the previous loop
          temp_df[which(temp_df$brood_year == 
                          as.character(curr_year) & 
                          temp_df$area == curr_area), "survival"]
      }
    }
    
    c_i_list[[i]] <- c_level_df
    print(paste0(i, " out of 110"))
  }
  c_list[[c_counter]]  <- c_i_list
}
saveRDS(c_list, here("./outputs/power-analysis/list-of-dfs.rds"))


# now loop through the data and fit the models =================================




# empty lists for outer objects
outer_liklihoods_c <- vector(mode = "list", length = length(c_list))

for(i in seq_len(length(c_list))) {
  
  # empty lists for inner objects 
  inner_liklihood_null <- vector(mode = "list", length = length(c_list[[1]]))
  inner_liklihood_alt <- vector(mode = "list", length = length(c_list[[1]]))
  inner_lrt <- vector(mode = "list", length = length(c_list))
  
  for(j in seq_len(length(c_list[[i]]))) {
    df <- c_list[[i]][[j]]
    
    null_mod <- lme4::lmer(survival ~ spawners:river + (1|brood_year/area),
                           data = df)
    
    alt_mod <- 
  }
}



for(c in seq(0, 1, 0.01)) {
  
  # set up list for storage
  c_list <- vector(mode = "list", length = 1000)
  
  # number of times this happens
  for(i in 1:1000) {
    
    start_time <- Sys.time()
    # make dataframe to fill
    c_level_df <- pink_sr
    c_level_df$survival <- as.numeric(NA)
    
    # years we have data for
    for(curr_year in ##################### year loop ###########################
        c(min(pink_sr$brood_year):max(pink_sr$brood_year))) {
      # set the year random effect value
      year_re <- rnorm(n = 1, mean = 0, sd = sd_year) 
      
      for(curr_area in all_areas) { ############### area loop ##################
        # set the area w/in year random effect value
        area_re <- rnorm(n = 1, mean = 0, sd = sd_area_year) 
        
        # get temporary subset of data 
        # temp_df <- c_level_df[which(c_level_df$brood_year == 
        #                               as.character(curr_year) & 
        #                               c_level_df$area == curr_area), ]
        
        # this is looping through what is commented out above as being assigned
        # to temp_df -- this is just faster
        for(row in seq_len(
          nrow(
            c_level_df[which(c_level_df$brood_year == 
                             as.character(curr_year) & 
                             c_level_df$area == curr_area), ])
            )
          ) { ####### population loop ###########
          
          
          # calculate survival
          
          # temp_df[row, "survival"] <- 
          
          # so instead of doing it how it's commented out above, I'm assinging
          # it straight to the c_level_df, by using the subset as is put above
          # plus another subset here with "row" the iterator and the "survival"
          # column, again, it's just faster
          c_level_df[which(c_level_df$brood_year == as.character(curr_year) & 
                    c_level_df$area == curr_area), ][[row, "survival"]] <-
            # r, and b * N(t-2)
            r - (
              # set population level density dependence
              b_i_vals[[paste0("spawners:river", 
                               # get the population on hand
                               c_level_df[which(c_level_df$brood_year == 
                                                  as.character(curr_year) & 
                                                  c_level_df$area == curr_area), 
                                          ][[row, "river"]])]]
              # number of spawners
              * c_level_df[which(c_level_df$brood_year == 
                                   as.character(curr_year) & 
                                   c_level_df$area == curr_area), 
                           ][[row, "spawners"]]) -
            # the c term and lice goes here
            (c * c_level_df[which(c_level_df$brood_year == 
                                    as.character(curr_year) & 
                                    c_level_df$area == curr_area), 
                            ][[row, "lice"]]) +
            # yearly variation
            year_re +
            # area within year variation
            area_re + 
            # residual variation 
            resid_sd
        }
        
        ## the below only needs to be here if we use temp_df which we've shown 
        ## we don't need to 
        
        # # put the survival values in the larger dataframe 
        # c_level_df[which(c_level_df$brood_year == as.character(curr_year) & 
        #                    c_level_df$area == curr_area), "survival"] <- 
        #   # this was just filled in the previous loop
        #   temp_df[which(temp_df$brood_year == 
        #                   as.character(curr_year) & 
        #                   temp_df$area == curr_area), "survival"]
      }
    }
    
    c_list[[i]] <- c_level_df
    print()
  }
}