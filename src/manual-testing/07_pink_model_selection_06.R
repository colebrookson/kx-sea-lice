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
alt_5 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_5:certainty_5  + (1|area_year) + (1|area) + 
    (-1+con_unit|brood_year)  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 3,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 3,
  iter = 2000
)
alt_5$waic <- waic(alt_5, cores = 4)
qs::qsave(alt_5,
          here("./outputs/model-outputs/TEMP-MANUAL/alt-5.qs"))

end_time <- Sys.time() 
end_time - start_time