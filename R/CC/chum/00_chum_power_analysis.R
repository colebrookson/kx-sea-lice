# set up 
library(readr)
library(magrittr)
library(Matrix)
library(lme4)
library(dplyr, quietly = TRUE)

# set the i value sq
i <- commandArgs(trailingOnly = TRUE)
if(length(i) > 1) {
  i <- i[length(i)]
}
print(i)
# pull in the data with the info needed
fit_items <- readr::read_csv(
  "/home/brookson/scratch/kx-sea-lice/outputs/power-analysis/chum-fit-null-model-objects.csv"
)
chum_sr <- readr::read_csv(
  "/home/brookson/scratch/kx-sea-lice/outputs/power-analysis/chum-sr-data-ready-for-sims.csv"
)
b_i_df <- readr::read_csv(
  "/home/brookson/scratch/kx-sea-lice/outputs/power-analysis/chum-b-i-df.csv"
)
b_i_df$popn <- as.factor(b_i_df$popn)
# add in r
chum_sr$r <- fit_items$r
# make the other dataframes that need to be pulled together

# make a matrix to store values in
c_mat <- matrix(NA, nrow = length(seq(0, 1, 0.01)),
                ncol = 4)
colnames(c_mat) <- c("c", "null_like", "alt_like", "p")

# set the c value for this iteration
start_time <- Sys.time()
matrix_counter <- 1
for(c in seq(0, 1, 0.01)) {
  # year random effects
  year_df <- data.frame(
    year = as.factor(unique(chum_sr$brood_year)),
    year_re = rnorm(n = length(unique(chum_sr$brood_year)),
                    mean = 0,
                    sd = fit_items$sd_year)
  )
  # area and year level random effect
  area_year_df <- data.frame(
    # make a unique combination of each brood year/ area combo
    expand.grid(
      "year" = as.factor(unique(chum_sr$brood_year)),
      "area" = as.factor(unique(chum_sr$area))
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
  chum_sr$year <- as.factor(chum_sr$brood_year) # to have a factor to join with
  chum_sr$popn <- as.factor(chum_sr$river) # to have a factor to join with
  chum_sr$area <- as.factor(chum_sr$area)

  joined_df <- dplyr::left_join(chum_sr, year_df, by = "year") %>%
    dplyr::left_join(., area_year_df, by = c("year", "area")) %>%
    dplyr::left_join(., b_i_df, by = "popn")

  # add in c values
  joined_df$c <- c

  joined_df$survival_temp <-
    # r, and b * N(t-2)
    (joined_df$r + (joined_df$b_i_vals * joined_df$spawners) -
    # the c term and lice goes here
    (joined_df$c * joined_df$lice) +
    # yearly variation
    joined_df$year_re +
    # area within year variation
    joined_df$area_re +
    # residual variation
    joined_df$epsilon)
  print("did the survival calc")
  # now fit the model
  null_mod <- lme4::lmer(survival_temp ~ spawners:river + (1|year/area),
                         data = joined_df)
  alt_mod <- lme4::lmer(survival_temp ~ spawners:river + lice +
                          (1|year/area),
                        data = joined_df)
  print("fit the models")
  null_logLik <- stats::logLik(null_mod)
  alt_logLik <- stats::logLik(alt_mod)
  # do the test
  teststat <- -2 * (as.numeric(null_logLik) - as.numeric(alt_logLik))
  df_diff <- attr(alt_logLik, "df") - attr(null_logLik, "df")
  p_val <- pchisq(teststat, df = df_diff, lower.tail = FALSE)
  
  # fill in matrix row
  c_mat[matrix_counter, ] <- c(c, null_logLik, alt_logLik, p_val)
  
  # iterate matrix counter
  matrix_counter <- matrix_counter + 1
  print(c)
}
end_time <- Sys.time()
print(end_time - start_time)

readr::write_csv(
   #x = test_df,
   x = data.frame(c_mat),
   file = paste0(
     "/home/brookson/scratch/kx-sea-lice/outputs/power-analysis/saved-runs/chum//",
     "c-matrix-", i, ".csv"
   )
 )
