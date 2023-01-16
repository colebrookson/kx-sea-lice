library(readr)

wild_lice <- read_csv(here("./data/wild-lice/clean/clean-wild-lice-df.csv"))
farm_lice <- read_csv(here("./data/farm-lice/clean/clean-farm-lice-df.csv"))

names(wild_lice)
