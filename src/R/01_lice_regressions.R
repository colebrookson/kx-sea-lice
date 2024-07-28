##' All regressions for wild lice vs farm lice
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2024-08-19
#'
#' Code to clean all the data using pre-written functions
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

library(magrittr)
library(ggplot2)
source(here::here("./src/R/functions/00_functions_global.R"))
source(here::here("./src/R/functions/02_functions_lice_v_lice_regression.R"))

# normal data frame ============================================================

farm_lice <- readr::read_csv(
    here::here("./data/farm-lice/clean/clean-farm-lice-df.csv")
)
wild_lice <- readr::read_csv(
    here::here("./data/wild-lice/clean/clean-wild-lice-df.csv")
)

# run the model and get the results of both the log and regular regression with
# the versions of the dataframe given
results_list <- lice_regression(
    wild_lice = wild_lice,
    farm_lice = farm_lice,
    mod_output_path = here::here("./outputs/lice-regression/"),
    plot_output_path = here::here("./figs/regression/"),
    name = "all-sites"
)

# write a LaTeX output of all the tables -- note we only want some components
# of the table
original_sum <- extract_model_summary(results_list[[1]], "Original")
log_sum <- extract_model_summary(results_list[[2]], "Log-transformed")

# Combine summaries into one data frame
combined_summary_df <- dplyr::bind_rows(original_sum, log_sum)

# Convert the combined data frame to a LaTeX table
latex_table <- knitr::kable(combined_summary_df,
    format = "latex",
    caption = "Summary of Bayesian GLMs using all sites.",
    col.names = c(
        "Model", "Term", "Estimate", "10\\%", "90\\%", "Bayes $R^2$",
        "$\\hat{R}$", "$n_{eff}$ ratio"
    ),
    booktabs = TRUE,
    escape = FALSE
) %>% kableExtra::kable_classic()

# Save the LaTeX table to a .tex file
writeLines(
    latex_table,
    here::here("./outputs/lice-regression/all-sites-summary.tex")
)

## exclude upper laredo ========================================================
wild_lice_up_lar <- wild_lice %>%
    dplyr::filter(site != "Upper Laredo")

results_list_up_lar <- lice_regression(
    wild_lice = wild_lice_up_lar,
    farm_lice = farm_lice,
    mod_output_path = here::here("./outputs/lice-regression/"),
    plot_output_path = here::here("./figs/regression/"),
    name = "no-up-lar"
)

original_up_lar_sum <- extract_model_summary(
    results_list_up_lar[[1]],
    "Original"
)
log_up_lar_sum <- extract_model_summary(
    results_list_up_lar[[2]],
    "Log-transformed"
)

# Combine summaries into one data frame
combined_summary_up_lar_df <- dplyr::bind_rows(
    original_up_lar_sum,
    log_up_lar_sum
)

# Convert the combined data frame to a LaTeX table
latex_table_up_lar <- knitr::kable(combined_summary_up_lar_df,
    format = "latex",
    caption = "Summary of Bayesian GLMs using all sites except Upper Laredo.",
    col.names = c(
        "Model", "Term", "Estimate", "10\\%", "90\\%", "Bayes $R^2$",
        "$\\hat{R}$", "$n_{eff}$ ratio"
    ),
    booktabs = TRUE,
    escape = FALSE
) %>% kableExtra::kable_classic()
# Write the combined LaTeX string to a text file
write(latex_table_up_lar, file = paste0(
    here::here("./outputs/lice-regression/"),
    "upper-laredo-excluded-combined_tables.tex"
))

## exclude both laredo sites ===================================================
wild_lice_both_lar <- wild_lice %>%
    dplyr::filter(site %notin% c("Lower Laredo", "Upper Laredo"))

results_list_both_lar <- lice_regression(
    wild_lice = wild_lice_both_lar,
    farm_lice = farm_lice,
    mod_output_path = here::here("./outputs/lice-regression/"),
    plot_output_path = here::here("./figs/regression/"),
    name = "no-both-lar"
)
original_both_lar_sum <- extract_model_summary(
    results_list_both_lar[[1]],
    "Original"
)
log_both_lar_sum <- extract_model_summary(
    results_list_both_lar[[2]],
    "Log-transformed"
)

# Combine summaries into one data frame
combined_summary_both_lar_df <- dplyr::bind_rows(
    original_both_lar_sum,
    log_both_lar_sum
)

# Convert the combined data frame to a LaTeX table
latex_table_both_lar <- knitr::kable(combined_summary_both_lar_df,
    format = "latex",
    caption = "Summary of Bayesian GLMs using all sites except either Laredo
    site.",
    col.names = c(
        "Model", "Term", "Estimate", "10\\%", "90\\%", "Bayes $R^2$",
        "$\\hat{R}$", "$n_{eff}$ ratio"
    ),
    booktabs = TRUE,
    escape = FALSE
) %>% kableExtra::kable_classic()
# Write the combined LaTeX string to a text file
write(latex_table_both_lar, file = paste0(
    here::here("./outputs/lice-regression/"),
    "both-lar-excluded-combined_tables.tex"
))
