library(readr)
library(here)
library(magrittr)
library(dplyr)
library(ggplot2)

farm_lice <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))
pink_sr_df <- read_csv(here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))
wild_lice <- read_csv(here("./data/wild-lice/clean/clean-wild-lice-df.csv"))

# keep only the relevant info for pinks
pink_sr <- pink_sr_df %>% 
  dplyr::select(brood_year, river, species, area, spawners, returns, recruits)
