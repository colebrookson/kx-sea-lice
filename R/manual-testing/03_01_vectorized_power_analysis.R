# make fake dataframe with the "data"
df <- data.frame(
  year = as.factor(c(rep(c(2010:2015), 10))),
  region = as.factor(c(rep(c("X", "Y", "Z"), 20))),
  popn = as.factor(c(rep(c("river1", "river2", "river3", "river4"), 15)))
)

# draw year level random effects
df_year <- data.frame(
  year = as.factor(c(2010:2015)),
  year_re = rnorm(6, mean = 0, sd = sd_year)
)

# make area within year random effects
df_region_year <- data.frame(
  expand.grid("year" = unique(df$year), "region" = unique(df$region))
)
df_region_year$area_re <- rnorm(nrow(df_region_year), 
                                mean = 0, 
                                sd = sd_area_year)

# make population level density dependence values (i.e. b_i)
df_pop <- data.frame(
  popn = unique(df$popn),
  bi = c(-0.1, 0.2, -0.3, -0.4)
)

dplyr::left_join(df, df_year, by = "year") %>% 
  dplyr::left_join(., df_region_year, by = c("year", "region")) %>% 
  dplyr::left_join(., df_pop, by =)



  

