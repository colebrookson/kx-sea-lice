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
latex_coefs <- knitr::kable(
    results_list$regular[[2]][
        ,
        c("term", "estimate", "std.error", "p.value") # just these columns
    ],
    format = "latex",
    caption = "Model coefficients for the model containing all sites.",
    col.names = c("Term", "Estimate", "Std. Error", "P-value"),
    align = "r",
    booktabs = TRUE
) %>% kableExtra::kable_classic()
latex_model_vals <- knitr::kable(
    results_list$regular[[4]][
        , c(
            "r.squared", "adj.r.squared", "sigma", "p.value", "df",
            "logLik", "deviance", "nobs"
        )
    ],
    format = "latex",
    caption = "Model diagnostic values for the model containing all sites.",
    col.names = c(
        "$R^2$", "Adj. $R^2$", "$\\sigma$", "P-value",
        "DF", "Log. Lik.", "Deviance", "No. Obs"
    ),
    escape = FALSE,
    booktabs = TRUE
) %>% kableExtra::kable_classic()

# Concatenate the LaTeX table strings
combined_latex <- paste(
    latex_coefs, latex_model_vals,
    sep = "\n\n"
)
# repeat for the log version
latex_coefs_log <- knitr::kable(
    results_list$log[[2]][
        ,
        c("term", "estimate", "std.error", "p.value") # just these columns
    ],
    format = "latex",
    caption = "Model coefficients for the $log_{10}$ model containing all
    sites.",
    col.names = c("Term", "Estimate", "Std. Error", "P-value"),
    align = "r",
    booktabs = TRUE
) %>% kableExtra::kable_classic()
latex_model_vals_log <- knitr::kable(
    results_list$log[[4]][
        , c(
            "r.squared", "adj.r.squared", "sigma", "p.value", "df",
            "logLik", "deviance", "nobs"
        )
    ],
    format = "latex",
    caption = "Model diagnostic values for the $log_{10}$ model containing
    all sites.",
    col.names = c(
        "$R^2$", "Adj. $R^2$", "$\\sigma$", "P-value",
        "DF", "Log. Lik.", "Deviance", "No. Obs"
    ),
    escape = FALSE,
    booktabs = TRUE
) %>% kableExtra::kable_classic()

# Concatenate the LaTeX table strings
combined_latex_log <- paste(
    latex_coefs_log, latex_model_vals_log,
    sep = "\n\n"
)
# Write the combined LaTeX string to a text file
write(combined_latex, file = paste0(
    here::here("./outputs/lice-regression/"),
    "all-sites-combined_tables.tex"
))
write(combined_latex_log, file = paste0(
    here::here("./outputs/lice-regression/"),
    "all-sites-log-combined_tables.tex"
))

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

# write a LaTeX output of all the tables -- note we only want some components
# of the table
latex_coefs_up_lar <- knitr::kable(
    results_list$regular[[2]][
        ,
        c("term", "estimate", "std.error", "p.value") # just these columns
    ],
    format = "latex",
    caption = "Model coefficients for the model containing all sites except
    upper laredo.",
    col.names = c("Term", "Estimate", "Std. Error", "P-value"),
    align = "r",
    booktabs = TRUE
) %>% kableExtra::kable_classic()
latex_model_vals_up_lar <- knitr::kable(
    results_list$regular[[4]][
        , c(
            "r.squared", "adj.r.squared", "sigma", "p.value", "df",
            "logLik", "deviance", "nobs"
        )
    ],
    format = "latex",
    caption = "Model diagnostic values for the model containing all sites
    except upper laredo.",
    col.names = c(
        "$R^2$", "Adj. $R^2$", "$\\sigma$", "P-value",
        "DF", "Log. Lik.", "Deviance", "No. Obs"
    ),
    escape = FALSE,
    booktabs = TRUE
) %>% kableExtra::kable_classic()

# Concatenate the LaTeX table strings
combined_latex_up_lar <- paste(
    latex_coefs_up_lar, latex_model_vals_up_lar,
    sep = "\n\n"
)
# Write the combined LaTeX string to a text file
write(combined_latex_up_lar, file = paste0(
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

# write a LaTeX output of all the tables -- note we only want some components
# of the table
latex_coefs_both_lar <- knitr::kable(
    results_list$regular[[2]][
        ,
        c("term", "estimate", "std.error", "p.value") # just these columns
    ],
    format = "latex",
    caption = "Model coefficients for the model containing all sites except
    both laredo sites.",
    col.names = c("Term", "Estimate", "Std. Error", "P-value"),
    align = "r",
    booktabs = TRUE
) %>% kableExtra::kable_classic()
latex_model_vals_both_lar <- knitr::kable(
    results_list$regular[[4]][
        , c(
            "r.squared", "adj.r.squared", "sigma", "p.value", "df",
            "logLik", "deviance", "nobs"
        )
    ],
    format = "latex",
    caption = "Model diagnostic values for the model containing all sites
    except both laredo sites.",
    col.names = c(
        "$R^2$", "Adj. $R^2$", "$\\sigma$", "P-value",
        "DF", "Log. Lik.", "Deviance", "No. Obs"
    ),
    escape = FALSE,
    booktabs = TRUE
) %>% kableExtra::kable_classic()

# Concatenate the LaTeX table strings
combined_latex_both_lar <- paste(
    latex_coefs_both_lar, latex_model_vals_both_lar,
    sep = "\n\n"
)
# Write the combined LaTeX string to a text file
write(combined_latex_up_lar, file = paste0(
    here::here("./outputs/lice-regression/"),
    "both-lar-excluded-combined_tables.tex"
))
