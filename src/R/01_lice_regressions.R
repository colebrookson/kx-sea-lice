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
        c("term", "estimate", "std.error", "p.value")
    ],
    format = "latex",
    caption = "Model coeficients for the model containing all sites.",
    col.names = c("Term", "Estimate", "Std. Error", "P-value"),
    align = "r"
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
        "DF", "Log. Lik.", "Deviance", "# Obs"
    ),
    escape = FALSE
) %>% kableExtra::kable_classic()

# Concatenate the LaTeX table strings
combined_latex <- paste(
    latex_coefs, latex_model_vals,
    sep = "\n\n"
)

# Write the combined LaTeX string to a text file
write(combined_latex, file = paste0(
    here::here("./outputs/lice-regression/"),
    "all-sites-combined_tables.tex"
))

## exclude upper laredo ======================================================
reg_wild_lice_up_lar <- wild_lice %>%
    dplyr::filter(site != "Upper Laredo") %>%
    dplyr::mutate(
        year = as.factor(year)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(wild_lice = mean(lep_total, na.rm = TRUE))

reg_data_up_lar <- data.frame(cbind(
    reg_wild_lice_up_lar,
    farm_lice = reg_farm_lice$farm_lice
))

## exclude both laredo sites =================================================
reg_wild_lice_both_lar <- wild_lice %>%
    dplyr::filter(site %notin% c("Lower Laredo", "Upper Laredo")) %>%
    dplyr::mutate(
        year = as.factor(year)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(wild_lice = mean(lep_total, na.rm = TRUE))

reg_data_both_lar <- data.frame(cbind(
    reg_wild_lice_both_lar,
    farm_lice = reg_farm_lice$farm_lice
))
