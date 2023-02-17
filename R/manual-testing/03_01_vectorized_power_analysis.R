# try to replicate with the real version 

# set the i value 
i <- args[1]

# pull in the data with the info needed
fit_items <- readr::read_csv(
  here::here("./outputs/power-analysis/fit-null-model-objects.csv")
)
pink_sr <- readr::read_csv(
  here::here("./outputs/power-analysis/pink-sr-data-ready-for-sims.csv")
)
b_i_df <- readr::read_csv(
  here::here("./outputs/power-analysis/b-i-df.csv")
) 
b_i_df$popn <- as.factor(b_i_df$popn)
# make the other dataframes that need to be pulled together 

# year random effects
year_df <- data.frame(
  year = as.factor(unique(pink_sr$brood_year)),
  year_re = rnorm(n = length(unique(pink_sr$brood_year)),
                  mean = 0,
                  sd = fit_items$sd_year)
)
# area and year level random effect
area_year_df <- data.frame(
  # make a unique combination of each brood year/ area combo 
  expand.grid(
    "year" = as.factor(unique(pink_sr$brood_year)), 
    "area" = as.factor(unique(pink_sr$area))  
  )
)
# draw the actual random effect
area_year_df$area_re <- rnorm(
  n = nrow(area_year_df),
  mean = 0,
  sd = fit_items$sd_area_year
)
# add in the b_i values 
b_i_df$epsilon <- rnorm(n = nrow(b_i_df), mean = 0, sd = fit_items$resid_sd)

# now join all dataframes together
pink_sr$year <- as.factor(pink_sr$brood_year) # to have a factor to join with
pink_sr$popn <- as.factor(pink_sr$river) # to have a factor to join with

joined_df <- dplyr::left_join(pink_sr, year_df, by = "year") %>% 
  dplyr::left_join(., area_year_df, by = c("year", "area"))


