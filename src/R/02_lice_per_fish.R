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
#' Then we separate chum and pink for 2009 to 2017 and model all leps and all
#' lice  per fish there as well

lice_per_year <- lice_per_year_regression(
    wild_lice = wild_lice,
    output_path = here::here("./outputs/model-outputs/lice-per-year/"),
    run_or_read_models = "read",
    run_or_read_predictions = "read"
)

stages_df <- lice_per_year[[1]][[1]]
spp_df <- lice_per_year[[1]][[2]]
spp_df$dodge <- interaction(spp_df$species, spp_df$lice)

## stage model prediction ======================================================
stage_plot <- ggplot(data = stages_df, aes(group = stage)) +
    geom_errorbar(aes(x = year, ymin = lo, ymax = hi, linetype = lice),
        width = 0,
        position = position_dodge(width = 0.5)
    ) +
    geom_point(
        aes(
            x = year, y = median, fill = stage, shape = lice,
            colour = lice
        ),
        size = 3,
        position = position_dodge(width = 0.5), stroke = 1
    ) +
    theme_base() +
    scale_shape_manual("Lice species",
        labels = c(
            expression(paste(italic("C. clemensi & L. salmonis"))),
            expression(paste(italic("L. salmonis")))
        ),
        values = c(23, 21)
    ) +
    scale_fill_manual(
        leg_title,
        values = c(MoMAColors::moma.colors("OKeeffe")[c(1, 4:6)], "#36953b"),
        labels = c("All stages", "Chalimus", "Copepodites", "Motiles", "")
    ) +
    scale_colour_manual("Lice species",
        labels = c(
            expression(paste(italic("C. clemensi & L. salmonis"))),
            expression(paste(italic("L. salmonis")))
        ),
        values = c("#8b0f0f", "black")
    ) +
    labs(
        x = "Year",
        y = "Estimated numbers of lice per fish"
    ) +
    scale_linetype_manual(
        "Lice species",
        labels = c(
            expression(paste(italic("C. clemensi & L. salmonis"))),
            expression(paste(italic("L. salmonis")))
        ),
        values = c("88", "solid")
    ) +
    guides(
        fill = guide_legend(
            title = leg_title,
            override.aes = list(
                size = 3,
                fill = c(
                    MoMAColors::moma.colors("OKeeffe")[c(1, 4:6)],
                    "white"
                ),
                colour = c(rep("black", 4), "white"),
                shape = c(21, 21, 21, 21, 20)
            )
        ),
        shape = guide_legend(
            title = "Lice species",
            override.aes = list(
                size = 3,
                fill = c("#36953b", "white"),
                shape = c(23, 21),
                colour = c("#8b0f0f", "black")
            )
        )
    )

ggsave(
    here::here("./figs/lice-per-year-regression/predictions/all-stages.png"),
    stage_plot,
    height = 7, width = 12,
    dpi = 300
)

## salmon species model predictions ============================================

spp_plot <- ggplot(data = spp_df, aes(group = dodge)) +
    geom_errorbar(aes(x = year, ymin = lo, ymax = hi),
        width = 0,
        position = position_dodge(width = 0.5)
    ) +
    geom_point(aes(x = year, y = median, fill = species, shape = lice),
        colour = "black", size = 3,
        position = position_dodge(width = 0.5)
    ) +
    theme_base() +
    scale_fill_manual(
        "Salmon species",
        values = MoMAColors::moma.colors("OKeeffe")[c(6, 3)]
    ) +
    scale_shape_manual(
        "Lice species",
        values = c(21, 22),
        labels = c(
            expression(paste(italic("L. salmonis"))),
            expression(paste(italic("C. clemensi & L. salmonis")))
        )
    ) +
    labs(
        x = "Year",
        y = "Estimated Number of Lice per Year"
    ) +
    guides(
        fill = guide_legend(
            title = "Salmon species",
            override.aes = list(
                fill = MoMAColors::moma.colors("OKeeffe")[c(6, 3)],
                labels = c("Chum", "Pink"),
                shape = 21
            )
        )
    )
ggsave(
    here::here("./figs/lice-per-year-regression/predictions/chum-vs-pink.png"),
    spp_plot,
    height = 7, width = 12,
    dpi = 300
)

# make output tables ===========================================================

# Example list of models and their names
model_list <- unlist(lice_per_year[2:3], recursive = FALSE)
model_names <- names(model_list)

# Extract the main model and its name
main_model <- model_list[["leps-all"]]
main_model_name <- "leps-all"

# Call the function to generate and write all LaTeX tables
generate_all_tables(main_model, main_model_name, model_list, model_names)
