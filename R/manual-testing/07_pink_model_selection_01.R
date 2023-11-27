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
null_model <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area) + 
    (-1+con_unit|brood_year) + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.999,
  control = list(max_treedepth = 25),
  cores = 4, 
  iter = 8000
)
null_model$waic <- waic(null_model, cores = 4)
qs::qsave(null_model,
          here("./outputs/model-outputs/TEMP-MANUAL/null-model.qs"))

end_time <- Sys.time() 
end_time - start_time
