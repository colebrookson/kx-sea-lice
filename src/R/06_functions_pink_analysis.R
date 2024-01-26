

# bayesian approach ============================================================

## just brood year and area ====================================================
start_time <- Sys.time()
by_area_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area),
  data = pink_sr,
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_null,
          here("./outputs/model-outputs/pink/by-area-null.qs"))
by_area_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty + (1|brood_year/area),
  data = pink_sr,
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_alt,
          here("./outputs/model-outputs/pink/by-area-alt.qs"))
end_time <- Sys.time()
end_time - start_time
## conservation unit and brood year ============================================
start_time <- Sys.time() 
con_by_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year) +
    (-1+con_unit|brood_year),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(con_by_null,
          here("./outputs/model-outputs/pink/con-by-null.qs"))
con_by_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  + (1|brood_year) +
    (-1+con_unit|brood_year),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(con_by_alt,
          here("./outputs/model-outputs/pink/con-by-alt.qs"))
end_time <- Sys.time() 
end_time - start_time

## river and brood year ========================================================
start_time <- Sys.time()
river_by_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|river) + (1|brood_year),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(river_by_null,
          here("./outputs/model-outputs/pink/river-by-null.qs"))
river_by_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  +(1|river) + (1|brood_year),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(river_by_alt,
          here("./outputs/model-outputs/pink/river-by-alt.qs"))

## con unit brood year area ====================================================
by_area_con_by_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area) +
    (-1+con_unit|brood_year),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_con_by_null,
          here("./outputs/model-outputs/pink/by-area-con-by-null.qs"))

by_area_con_by_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  + (1|brood_year/area) +
    (-1+con_unit|brood_year),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_con_by_alt,
          here("./outputs/model-outputs/pink/by-area-con-by-alt.qs"))

## river brood year area =======================================================
by_area_river_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area) + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_river_null,
          here("./outputs/model-outputs/pink/by-area-river-null.qs"))
          
by_area_river_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  + (1|brood_year/area) + 
    (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(by_area_river_alt,
          here("./outputs/model-outputs/pink/by-area-river-alt.qs"))
end_time <- Sys.time() 
end_time - start_time
## all re's ====================================================================
start_time <- Sys.time()
all_re_null <- rstanarm::stan_lmer(
  survival ~ spawners:river + (1|brood_year/area) + 
    (-1+con_unit|brood_year) + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4, 
  iter = 8000
)
qs::qsave(all_re_null,
          here("./outputs/model-outputs/pink/all-re-null.qs"))

all_re_alt <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  + (1|area_year) + (1|area) + 
    (-1+con_unit|brood_year)  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.99,
  control = list(max_treedepth = 15),
  cores = 4
)
qs::qsave(all_re_alt,
          here("./outputs/model-outputs/pink/all-re-alt.qs"))

end_time <- Sys.time() 
end_time - start_time

fitting_stan_model <- function(formula, data, chains, adapt_delta, 
                               max_treedepth, cores, output_path, name) {
  
  
  # fit the model
  mod <- rstanarm::stan_lmer(
    formula = formula,
    data = data,
    chains = chains,
    adapt_delta = adapt_delta,
    control = list(max_treedepth = max_treedepth),
    cores = cores
  )
  
  # save model
  qs::qsave(
    mod, 
    file = paste0(here("./model-outputs/"), name)
  )
}

## trouble shooting ============================================================
bayes_alt_model_old <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  
  + (1|brood_year/area),
  #+ (0+con_unit|brood_year) 
  #+ (1|brood_year) 
  #+ (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  #adapt_delta = 0.999,
  #control = list(max_treedepth = 25),
  cores = 4,
  iter = 2000
)
qs::qsave(bayes_alt_model_old,
          here("./outputs/model-outputs/bayes-alt-model-old-ob.qs"))

bayes_alt_model_river <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  
  + (1|brood_year/area),
  #+ (0+con_unit|brood_year) 
  #+ (1|brood_year) 
  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.999,
  control = list(max_treedepth = 25),
  cores = 4,
  iter = 500
)
qs::qsave(bayes_alt_model_river,
          here("./outputs/model-outputs/bayes-alt-model-river-ob.qs"))

bayes_alt_model_con_unit_no_area <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  
  + (1|brood_year)
  + (1+con_unit||brood_year) 
  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.999,
  control = list(max_treedepth = 35),
  cores = 4,
  iter = 6000
)
qs::qsave(bayes_alt_model_con_unit_no_area,
          here("./outputs/model-outputs/bayes-alt-model-con-unit-no-area-ob.qs"))


bayes_alt_model_all_re <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  
  + (1|area_year)
  + (0+con_unit|brood_year) 
  + (1|brood_year) 
  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 10,
  adapt_delta = 0.99999,
  control = list(max_treedepth = 65),
  cores = 10,
  iter = 10000
)
qs::qsave(bayes_alt_model_all_re,
          here("./outputs/model-outputs/bayes-alt-model-all-re-ob.qs"))


## model fitting ===============================================================
# 
bayes_null_model <- rstanarm::stan_lmer(
  survival ~ spawners:river 
  + (0+con_unit|brood_year) 
  + (1|brood_year) 
  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.999,
  control = list(max_treedepth = 25),
  cores = round(0.8 * parallel::detectCores())
)
qs::qsave(bayes_null_model,
          here("./outputs/model-outputs/bayes-null-model-ob.qs"))

bayes_alt_model_1 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_1 
  + (0+con_unit|brood_year) 
  + (1|brood_year) 
  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.999,
  control = list(max_treedepth = 25),
  cores = round(0.8 * parallel::detectCores())
)
qs::qsave(bayes_alt_model_1,
          here("./outputs/model-outputs/bayes-alt-model-1-ob.qs"))

bayes_alt_model_2 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_2  
  + (0+con_unit|brood_year) 
  + (1|brood_year) 
  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.999,
  control = list(max_treedepth = 25),
  cores = round(0.8 * parallel::detectCores())
)
qs::qsave(bayes_alt_model_2,
          here("./outputs/model-outputs/bayes-alt-model-2-ob.qs"))

bayes_alt_model_3 <- rstanarm::stan_lmer(
  survival ~ spawners:river + lice_3:certainty  
  #+ (1|area_year)
  + (0+con_unit|brood_year) 
  + (1|brood_year) 
  + (1|river),
  data = pink_sr,
  #family = gaussian(link = "identity"),
  #prior = normal(0, 5),
  chains = 4,
  adapt_delta = 0.999,
  control = list(max_treedepth = 25),
  cores = round(0.8 * parallel::detectCores())
)
qs::qsave(bayes_alt_model_3,
          here("./outputs/model-outputs/bayes-alt-model-3-ob.qs"))

## Model selection =============================================================

bayes_null_model <- qs::qread(
  here("./outputs/model-outputs/bayes-null-model-ob.qs"))
bayes_alt_model_1 <- qs::qread(
  here("./outputs/model-outputs/bayes-alt-model-1-ob.qs"))
bayes_alt_model_2 <- qs::qread(
  here("./outputs/model-outputs/bayes-alt-model-2-ob.qs"))
bayes_alt_model_3 <- qs::qread(
  here("./outputs/model-outputs/bayes-alt-model-3-ob.qs"))

#launch_shinystan(bayes_null_model, ppd = FALSE)
launch_shinystan(bayes_alt_model_3, ppd = FALSE)

null_shiny <- shinystan::as.shinystan(bayes_null_model)
alt1_shiny <- shinystan::as.shinystan(bayes_alt_model_1)
alt2_shiny <- shinystan::as.shinystan(bayes_alt_model_2)
alt3_shiny <- shinystan::as.shinystan(bayes_alt_model_3)

# calculate the waic for each of the models, then save the objects again
bayes_null_model$waic <- waic(bayes_null_model,
                cores = 12)
bayes_alt_model_1$waic <- waic(bayes_alt_model_1,
                cores = 17)
bayes_alt_model_2$waic <- waic(bayes_alt_model_2,
                cores = 16)
bayes_alt_model_3$waic <- waic(bayes_alt_model_3,
                cores = 16)

qs::qsave(bayes_null_model,
          here("./outputs/model-outputs/bayes-null-model-ob.qs"))
qs::qsave(bayes_alt_model_1,
          here("./outputs/model-outputs/bayes-alt-model-1-ob.qs"))
qs::qsave(bayes_alt_model_2,
          here("./outputs/model-outputs/bayes-alt-model-2-ob.qs"))
qs::qsave(bayes_alt_model_3,
          here("./outputs/model-outputs/bayes-alt-model-3-ob.qs"))

## compare the models ==========================================================
rstanarm::loo_compare(
  bayes_null_model, bayes_alt_model_1, bayes_alt_model_2,
  bayes_alt_model_3,
  criterion = "waic"
)


## plot the results ============================================================
bayesplot::color_scheme_set("purple")
pairs(bayes_alt_model_3, 
      pars = c("(Intercept)", "log-posterior", "lice_3:certaintycertain", 
               "sigma", "lice_3:certaintyuncertain"))

# trying to look at the divergent transitions 
draws <- as.array(bayes_alt_model_3, 
                  pars = c("(Intercept)", "lice_3:certaintycertain",
                           "lice_3:certaintyuncertain", "sigma"))
np <- bayesplot::nuts_params(bayes_alt_model_3)
bayesplot::color_scheme_set("darkgray")
div_style <- bayesplot::parcoord_style_np(div_color = "green", 
                                          div_size = 0.05, div_alpha = 0.4)

bayesplot::mcmc_parcoord(
  draws,
  transform = function(x) {(x - mean(x)) / sd(x)},
  size = 0.25,
  alpha = 0.1,
  np = np,
  np_style = div_style
)

### plot of the important parameters ===========================================
posterior <- as.matrix(bayes_alt_model_3)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 90% intervals")
bayesplot::mcmc_areas(
  posterior,
  pars = c("(Intercept)", "lice_3:certaintycertain", 
           "lice_3:certaintyuncertain"),
  prob = 0.9
) + 
  plot_title + 
  scale_y_discrete(labels = c("Growth Rate", "Certain Lice", "Uncertain Lice"))

### posterior predict ==========================================================
color_scheme_set("red")
bayesplot::ppc_dens_overlay(
  y = bayes_alt_model_3$y,
  yrep = posterior_predict(bayes_alt_model_3, draws = 500)
)

### Trace plots ================================================================
color_scheme_set("viridis")
dimnames(bayes_alt_model_3)
bayesplot::mcmc_trace(
  bayes_alt_model_3, 
  pars = c("(Intercept)", "lice_3:certaintycertain",
           "lice_3:certaintyuncertain"))

### extract statistics =========================================================
fit_summary <- summary(bayes_alt_model_3)
names(fit_summary)
plot(bayes_alt_model_3, "rhat_hist")
bayesplot::color_scheme_set("red")
plot(bayes_alt_model_3, "neff_hist")
bayesplot::color_scheme_set("purple")
plot(bayes_alt_model_3, "neff_hist")


# deprecated ===================================================================
# frequentist approach =========================================================

# see if the data can be sub-set
# pink_sr_river_counts = pink_sr %>% 
#   group_by(river, brood_year, area) %>% 
#   summarize(n = n())
# pink_sr <- pink_sr %>% 
#   dplyr::left_join(
#     ., 
#     y = pink_sr_river_counts,
#     by = "river"
#   )
# 
# ## model fits ==================================================================
# 
# freq_null_model <- lme4::lmer(survival ~ spawners:river + (1|brood_year/area),
#                          data = pink_sr)
# freq_alt_mod_1 <- lme4::lmer(survival ~ spawners:river + lice_1 +
#                         (1|brood_year/area),
#                       data = pink_sr)
# freq_alt_mod_2 <- lme4::lmer(survival ~ spawners:river + lice_2 +
#                           (1|brood_year/area),
#                         data = pink_sr)
# freq_alt_mod_3 <- lme4::lmer(survival ~ spawners:river + lice_3:certainty +
#                           (1|brood_year/area),
#                         data = pink_sr)
# 
# freq_alt_mod_4 <- lme4::lmer(survival ~ spawners:river + lice_1 +
#                                       (1|brood_year/area) + (1|river),
#                                     data = pink_sr)
# 
# all_fit <- allFit(freq_alt_mod_4, maxfun = 1e05)
# 
# ## process model results =======================================================
# fixef(alt_mod_3)
# ranef(alt_mod_3)
# 
# AIC(null_model, alt_mod_1, alt_mod_2, alt_mod_3, alt_mod_4)
# summary(alt_mod_1)
# summary(alt_mod_2)
# summary(alt_mod_3)
# summary(alt_mod_4)
# -0.354 == -3.540e-01
# 
# coefs_1 <- broom.mixed::tidy(alt_mod_1)
# coefs_2 <- broom.mixed::tidy(alt_mod_2)
# coefs_3 <- broom.mixed::tidy(alt_mod_3)
# coefs_1 %>% 
#   dplyr::filter(term == "lice_1")
# coefs_2 %>% 
#   dplyr::filter(term == "lice_2")
# coefs_3 %>% 
#   dplyr::filter(term %in% c("lice_3:certaintycertain", 
#                             "lice_3:certaintyuncertain"))
# 
# c_df <- data.frame(
#   c = c(-0.354,-0.354,-0.495,-0.5078),
#   std_err = c(0.147, 0.155, 0.155, 0.155),
#   model = c("model 1", "model 2", "model 3 - exp.", "model 3 - pot.")
# ) %>% 
#   dplyr::mutate(
#     up = c + (1.96*std_err),
#     lo = c - (1.96*std_err)
#   )
# 
# ggplot(data = c_df) + 
#   geom_errorbar(aes(x = model, ymin = lo, ymax = up),
#                 width = 0) + 
#   geom_point(aes(x = model, y = c, fill = model), 
#              colour = "black", shape = 21, size = 3) + 
#   geom_hline(aes(yintercept = 0), colour = "grey80", linetype = "dashed") +
#   theme_base()
# 
# ## plot observations in each category ==========================================
# ggplot(data = exposure_df) + 
#   geom_histogram(aes(x = exposure), stat = "count",
#                  colour = "black", fill = c("yellow3", "green4", "red3")) + 
#   theme_base() + 
#   labs(y = "No. of Obs in Each Category")