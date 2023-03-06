##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-01-18
#'
#' This file contains the functions that process the files run on the compute
#' canada cluster (outside the targets framework) and plots the results
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

library(readr)
library(here)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggthemes)

all_power_sims <- readr::read_csv(here::here("./outputs/power-analysis/all-power-analysis-runs.csv"))

all_power_sims <- all_power_sims %>% 
  # add in a one/zero for if the p value is < 0.05
  dplyr::rowwise() %>% 
  dplyr::mutate(
    better = ifelse(p <= 0.05, 1, 0)
  ) %>% 
  dplyr::group_by(c) %>% 
  dplyr::summarize(
    n = n(), 
    count = sum(better)
  ) %>% 
  dplyr::mutate(
    prop = count / n
  )

ggplot(data = all_power_sims) + 
  geom_point(aes(x = c, y = prop)) + 
  labs(x = "C", y = "Power") + 
  ggthemes::theme_base() + 
  geom_vline(aes(xintercept = 0.19), linetype = "dashed", colour = "red") + 
  geom_hline(aes(
    yintercept = all_power_sims[which(all_power_sims$c == 0.19), "prop"][[1]]
    ), linetype = "dashed", colour = "red")

