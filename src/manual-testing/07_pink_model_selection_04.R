library(readr)
library(here)
library(rstanarm)
library(bayesplot)

pink_sr <- read_csv(here("./data/spawner-recruit/clean/pink-sr-for-model.csv"))

# make sure all RE's are factors
pink_sr$brood_year <- as.factor(pink_sr$brood_year)
pink_sr$area <- as.factor(pink_sr$area)
pink_sr$con_unit <- as.factor(pink_sr$con_unit)
pink_sr$river <- as.factor(pink_sr$river)

# bayesian version =============================================================
start_time <- Sys.time()
alt_3 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  + (1|area_year) + (1|area) + 
    (-1+con_unit|brood_year)  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 6,
  adapt_delta = 0.9999,
  control = list(max_treedepth = 25),
  cores = 6,
  iter = 8000
)
alt_3$waic <- waic(alt_3, cores = 6)
qs::qsave(alt_3,
          here("./outputs/model-outputs/TEMP-MANUAL/alt-3.qs"))

end_time <- Sys.time() 
end_time - start_time

start_time <- Sys.time()
alt_4 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_4:certainty_4  + (1|area_year) + (1|area) + 
    (-1+con_unit|brood_year)  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 6,
  adapt_delta = 0.9999,
  control = list(max_treedepth = 25),
  cores = 6,
  iter = 8000
)
alt_4$waic <- waic(alt_4, cores = 6)
qs::qsave(alt_4,
          here("./outputs/model-outputs/TEMP-MANUAL/alt-4.qs"))

end_time <- Sys.time() 
end_time - start_time

start_time <- Sys.time()
alt_5 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_5:certainty_5  + (1|area_year) + (1|area) + 
    (-1+con_unit|brood_year)  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 6,
  adapt_delta = 0.9999,
  control = list(max_treedepth = 25),
  cores = 6,
  iter = 8000
)
alt_5$waic <- waic(alt_5, cores = 6)
qs::qsave(alt_5,
          here("./outputs/model-outputs/TEMP-MANUAL/alt-5.qs"))

end_time <- Sys.time() 
end_time - start_time

# read in and look at WAIC
# null_model <- qs::qread(here("./outputs/model-outputs/TEMP-MANUAL/null-model.qs"))
# alt_1 <- qs::qread(here("./outputs/model-outputs/TEMP-MANUAL/alt-1.qs"))
# alt_2 <- qs::qread(here("./outputs/model-outputs/TEMP-MANUAL/alt-2.qs"))
# alt_1$waic; alt_2$waic; alt_3$waic; alt_4$waic; alt_5$waic; null_model$waic

# alt 1 - 25999.0
# alt 2 - 26007.6
# alt 3 - 26000.4
# alt 4 - 25998.0
# alt 5 - 26000.0
