##' All regressions for wild lice vs farm lice
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2024-08-22
#'
#' Code to clean all the data using pre-written functions
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

library(magrittr)
library(ggplot2)
source(here::here("./src/R/functions/00_functions_global.R"))
source(here::here("./src/R/functions/03_functions_wild_lice_regression.R"))

wild_lice <- readr::read_csv(
    here::here("./data/wild-lice/clean/clean-wild-lice-df.csv")
)

# regressions ==================================================================

#' We fit a series of models here. With all species we fit:
#'      1. all leps per fish
#'      2. all lice per fish
#'      3. lep copepidites per fish
#'      4. lep motiles per fish
#'      5. lep chalimus per fish
#' Then we separate chum and pink for 2009 to 2017 and model all leps per fish
#' there as well





# predicted_values <- lice_per_year_regression(
#     wild_lice = wild_lice,
#     output_path = here::here("./outputs/model-outputs/lice-per-year/")
# )

# # the above function returns one model object and two dataframes, one of the
# # predicted values of the regressions at the level of stage, the other of the
# # two separate species
# model <- predicted_values[[1]]
# stage_dfs <- predicted_values[[2]] |> transform(year = as.factor(year))
# spp_dfs <- predicted_values[[3]] |> transform(year = as.factor(year))

# # plotting of the results ======================================================

# #' We need a handful of plots, first of the two things we care most about:
# #'      1. all leps per fish, and all lice per fish
# #'      2. all stages separated out
# #' Then for the stages
# #'      1. plot of the predicted values compared between species

# ## species plot first ==========================================================
# axis <- expression(paste(
#     "Estimated number of ", italic("L. salmonis "),
#     "per year"
# ))
# spp_leps_plot <- ggplot(
#     data = spp_dfs[which(spp_dfs$louse_species == "Leps"), ],
#     aes(group = species)
# ) +
#     geom_errorbar(aes(x = year, ymin = lo, ymax = up),
#         width = 0,
#         position = position_dodge(width = 0.5)
#     ) +
#     geom_point(aes(x = year, y = fit, fill = species),
#         colour = "black", shape = 21, size = 3,
#         position = position_dodge(width = 0.5)
#     ) +
#     theme_base() +
#     scale_fill_manual(
#         "Species",
#         values = MoMAColors::moma.colors("OKeeffe")[c(5, 3)]
#     ) +
#     labs(
#         x = "Year",
#         y = axis
#     )
# ggsave(
#     here::here(
#         "./figs/lice-per-year-regression/chum-vs-pink-lep-comparison.png"
#     ),
#     spp_leps_plot,
#     height = 6,
#     width = 7
# )
# (wild_lice[which(wild_lice$year == 2015), "fish_spp"])

# spp_lice_plot <- ggplot(
#     data = spp_dfs[which(spp_dfs$louse_species == "All"), ],
#     aes(group = species)
# ) +
#     geom_errorbar(aes(x = year, ymin = lo, ymax = up),
#         width = 0,
#         position = position_dodge(width = 0.5)
#     ) +
#     geom_point(aes(x = year, y = fit, fill = species),
#         colour = "black", shape = 21, size = 3,
#         position = position_dodge(width = 0.5)
#     ) +
#     theme_base() +
#     scale_fill_manual(
#         "Species",
#         values = MoMAColors::moma.colors("OKeeffe")[c(5, 3)]
#     ) +
#     labs(
#         x = "Year",
#         y = "Estimated Number of Lice per Year"
#     )
# ggsave(
#     here::here(
#         "./figs/lice-per-year-regression/chum-vs-pink-lice-comparison.png"
#     ),
#     spp_lice_plot,
#     height = 6,
#     width = 7
# )

# ## stage plots next ============================================================
# axis2 <- expression(paste(
#     "Estimated number of ", italic("L. salmonis "),
#     "per year by stage"
# ))
# leg_title <- expression(paste(
#     italic("L. salmonis "),
#     "stage"
# ))
# stages_leps_plot <- ggplot(
#     data = stage_dfs[which(stage_dfs$stage != "NA"), ],
#     aes(group = stage)
# ) +
#     geom_errorbar(aes(x = year, ymin = lo, ymax = up),
#         width = 0,
#         position = position_dodge(width = 0.5)
#     ) +
#     geom_point(aes(x = year, y = fit, fill = stage),
#         colour = "black", shape = 21, size = 3,
#         position = position_dodge(width = 0.5)
#     ) +
#     theme_base() +
#     scale_fill_manual(
#         leg_title,
#         values = MoMAColors::moma.colors("OKeeffe")[c(1, 3, 5, 6)]
#     ) +
#     labs(
#         x = "Year",
#         y = axis2
#     )
# ggsave(
#     here::here(
#         "./figs/lice-per-year-regression/stages-comparison.png"
#     ),
#     stages_leps_plot,
#     height = 7.5,
#     width = 10
# )

# # showing the model results ====================================================

# #' Now we'll show the model values and results from the model we'll use -- the
# #' model of all stages of L. salmonis

# # make diagnostic plot
# model_diags <- DHARMa::simulateResiduals(leps_all_glmm_zip)
# png(here::here("./figs/lice-per-year-regression/main-model-diagnostics.png"))
# plot(model_diags)
# dev.off()

# DHARMa::plotResiduals(leps_all_glmm_nb, )

# DHARMa::testOutliers(leps_all_glmm_zip, type = "bootstrap")

# #' WHERE YOU LEFT OFF ON JULY 22 EVENING:
# #' so you were trying to get diagnostics for the main plot to put in the SI
# #' as of right now, i think you should just put in the dharma one since the
# #' actual plot is fine but it's showing a really low significance level,
# #' then just focus on getting the tables you need for the SI and get those in
# #' there. the tables should be in the tutorial on diagnostics for glmmTMB i
# #' think
# #'
# #' OK NEW THOUGHT ---- looking back at all of this, I think it might in fact
# #' just be worth it to go back to the bayesian way of doing things?? the model
# #' estimates might actually be quite comparable, so might as well use that
# #' better way, it'll be just generally more useful i think to have it flow
# #' and be one single way of doing things. So, go back and do it with the
# #' bayes stuff, then remember to do the lice vs. lice as bayesian as well.
# #' Tonight, you have saved the plot that uses 20000 iters with 8000 warmup and
# #' 6 chains. try with a much smaller subset and see if the answer stays




# coefs <- broom::tidy(model)
# model_vals <- broom::glance(model)

# model_coefs_latex <- knitr::kable(
#     coefs[, c("term", "estimate", "std.error", "p.value")],
#     format = "latex",
#     caption = "Model coefficients for the model containing all sites.",
#     col.names = c("Term", "Estimate", "Std. Error", "P-value"),
#     align = "r",
#     booktabs = TRUE
# ) %>% kableExtra::kable_classic()
# model_vals_latex <- knitr::kable(
#     model_vals[
#         , c(
#             "r.squared", "adj.r.squared", "sigma", "p.value", "df",
#             "logLik", "deviance", "nobs"
#         )
#     ],
#     format = "latex",
#     caption = "Model coefficients for the model containing all sites.",
#     col.names = c("Term", "Estimate", "Std. Error", "P-value"),
#     align = "r",
#     booktabs = TRUE
# ) %>% kableExtra::kable_classic()
