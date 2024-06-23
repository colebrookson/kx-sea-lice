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
    ),
    count = stats::rbinom(n = 864, size = 70, prob = 0.01)
) %>%
    dplyr::mutate(
        week = as.factor(week),
        year = as.factor(year),
        size = as.factor(site)
    )

freq_mod <- glmmTMB::glmmTMB(
    count ~ year + (1 | week) + (1 | site),
    data = df,
    family = glmmTMB::nbinom2(link = "log")
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
ggplot(predicted_yearly) +
    geom_errorbar(aes(x = year, ymin = fit - se.fit, ymax = fit + se.fit)) +
    geom_point(aes(x = year, y = fit)) +
    theme_bw()

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
predicted_yearly_bayes <- rstanarm::posterior_predict(
    object = bayes_fit,
    newdata = predict_data_bayes,
    re.form = NULL
)




bayesplot::mcmc_trace(
    as.array(bayes_fit),
    pars = names(bayes_fit$coefficients)[1:16],
    n_warmup = 2000,
    facet_args = list(nrow = 3)
) + bayesplot::facet_text(size = 15) +
    theme_base()
