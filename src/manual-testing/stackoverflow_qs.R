library(glmmTMB)
library(dplyr)
library(magrittr)
library(ggplot2)
library(rstanarm)

df <- data.frame(
    expand.grid(
        week = c(1:18),
        year = c(2005:2020),
        site = c("one", "two", "three")
    )
) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        n_obs = sample(c(4:10), 1, replace = TRUE)
    )
# Vector specifying the number of times to replicate each row
replication_times <- df$n_obs

# Replicate rows according to the specified pattern
replicated_df <- df[
    rep(row.names(df), times = replication_times),
    c("week", "site", "year")
]
replicated_df$count <- stats::rnbinom(
    nrow(replicated_df),
    size = 0.112,
    mu = 0.214
)
replicated_df$count[which(replicated_df$year == 2015)] <- stats::rnbinom(
    nrow(replicated_df[which(replicated_df$year == 2015), ]),
    size = 0.45,
    mu = 3.0
)

# (wild_lice$lep_total[which(wild_lice$year != 2015)])
# fitdistr(wild_lice$lep_total[which(wild_lice$year == 2015)],
#     densfun = "negative binomial"
# )

df <- replicated_df %>%
    dplyr::mutate(
        week = as.factor(week),
        year = as.factor(year),
        size = as.factor(site)
    )
freq_mod <- glmmTMB::glmmTMB(
    count ~ year + (1 | week) + (1 | site),
    data = df,
    family = nbinom2
)
predict_data <- data.frame(
    year = as.character(c(2005:2020)),
    week = NA,
    site = NA
)
predicted_yearly <- data.frame(
    year = as.character(c(2005:2020)),
    stats::predict(
        object = freq_mod,
        newdata = predict_data,
        re.form = ~0,
        se.fit = TRUE,
        type = "response"
    )
)
ggsave(
    here::here("./TEST.png"),
    ggplot(predicted_yearly) +
        geom_errorbar(aes(x = year, ymin = fit - se.fit, ymax = fit + se.fit),
            width = 0
        ) +
        geom_point(aes(x = year, y = fit)) +
        theme_bw(),
    height = 5, width = 5
)

bayes_fit <- rstanarm::stan_glmer(
    count ~ year + (1 | week) + (1 | site),
    data = df,
    family = rstanarm::neg_binomial_2(link = "log"),
    iter = 5000,
    warmup = 2000,
    cores = 4
)
predict_data_bayes <- data.frame(
    year = as.character(c(2005:2020)),
    week = "1",
    site = "one"
)
predicted_yearly_bayes <- data.frame(
    year = as.character(c(2005:2020)),
    week = "1",
    site = "one",
    rstanarm::posterior_predict(
        object = bayes_fit,
        newdata = predict_data_bayes,
        re.form = NULL
    )
)
x <- tidybayes::epred_draws(
    object = bayes_fit,
    newdata = predict_data_bayes,
    re_formula = NULL
)
?rstanarm::posterior_predict()
ggsave(
    here::here("./TEST.png"),
    ggplot(
        x,
        aes(
            x = .epred, y = year, fill = year
        )
    ) +
        tidybayes::stat_halfeye(.width = 0.95) +
        scale_fill_manual(values = c(rep("lightpink", 16))) +
        labs(
            x = "Count", y = "Year",
            subtitle = "Posterior predictions"
        ) +
        theme(legend.position = "bottom") +
        theme_bw(),
    width = 5,
    height = 5
)

df_comparison <-
    bayesplot::mcmc_trace(
        as.array(bayes_fit),
        pars = names(bayes_fit$coefficients)[1:16],
        n_warmup = 2000,
        facet_args = list(nrow = 3)
    ) + bayesplot::facet_text(size = 15) +
    theme_base()
