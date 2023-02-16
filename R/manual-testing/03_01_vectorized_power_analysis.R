df <- data.frame(
  year = as.factor(c(rep(c(2010:2015), 10))),
  region = as.factor(c(rep(c("X", "Y", "Z"), 20))),
  popn = as.factor(c(rep(c("river1", "river2", "river3", "river4"), 15)))
)

df_year <- data.frame(
  year = as.factor(c(2010:2015)),
  year_re = rnorm(6, mean = 0, sd = sd_year)
)

df_region_year <- data.frame(
  expand.grid("year" = unique(df$year), "region" = unique(df$region))
)
df_region_year$area_re <- rnorm(nrow(df_region_year), 
                                mean = 0, 
                                sd = sd_area_year)

df_pop <- data.frame(
  popn = unique(df$popn),
  bi = c(-0.1, 0.2, -0.3, -0.4)
)

  

