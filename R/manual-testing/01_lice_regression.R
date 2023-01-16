library(readr)

wild_lice <- read_csv(here("./data/wild-lice/clean/clean-wild-lice-df.csv"))
farm_lice <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))

reg_wild_lice <- wild_lice %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(wild_lice = mean(lep_motiles, na.rm = TRUE))

reg_farm_lice <- farm_lice %>% 
  dplyr::filter(year >= 2005) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(farm_lice = mean(total_lice, na.rm = TRUE))

reg_data <- data.frame(cbind(
  reg_wild_lice, farm_lice = reg_farm_lice$farm_lice
  ))

model <- stats::lm(
  wild_lice ~ farm_lice,
  data = reg_data
)

# extract vals of use
coefs = broom::tidy(model)
fitted_vals = broom::augment(model)
model_vals = broom::glance(model)

model_log <- stats::lm(
  log10(wild_lice) ~ log10(farm_lice),
  data = reg_data
)

# extract vals of use
log_coefs = broom::tidy(model_log)
log_fitted_vals = broom::augment(model_log)
log_model_vals = broom::glance(model_log)



# save all objects
saveRDS(model,
        paste0(mod_path, "wild-farm-regression.rds"))
readr::write_csv(coefs,
                 paste0(mod_path, "coeffs-wild-farm-regression.csv"))
readr::write_csv(fitted_vals,
                 paste0(mod_path, "fiited_vals-wild-farm-regression.csv"))
readr::write_csv(model_vals,
                 paste0(mod_path, "glance_aic-wild-farm-regression.csv"))

ggplot(data = reg_data, aes(x = farm_lice, y = wild_lice)) + 
  geom_point(
    # use the shape and colour to denote which are which
    fill = "30D5C8", shape = 21,
    colour = "black", size = 5) +
  stat_smooth(method = stats::lm, formula = y ~ x,
              colour = "black", alpha = 0.2) +
  ggthemes::theme_base() 


ggplot(data = reg_data, aes(x = log10(farm_lice), y = log10(wild_lice))) + 
  geom_point(
    # use the shape and colour to denote which are which
    fill = "30D5C8", shape = 21,
    colour = "black", size = 5) +
  stat_smooth(method = stats::lm, formula = y ~ x,
              colour = "black", alpha = 0.2) +
  ggthemes::theme_base() 




