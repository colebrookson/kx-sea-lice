library(readr)
library(here)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(lubridate)
library(glmmTMB)
library(stringr)

# read in the file with the yearly ones ========================================
large_land = readRDS(tar_read(large_land))
site_name_combos <- readr::read_csv(
  here("./data/spawner-recruit/clean/site-name-combos-for-exposed-populations.csv")
  ) %>%
  dplyr::mutate(
    out_mig_year = brood_year + 1
  ) %>% 
  dplyr::select(
    -brood_year
  )

# never exposed ================================================================
never_exposed <- c(
  43,92,96,97,6,48,59,53,57,70,93,82,69,50,91,73,60,90,62,68,79,47,84,76,26,33,
  31,39,67,16,38,8,20,7,30,94,19,18,4,1,5,98,66,6,2,3
)

pops_2005 <- site_name_combos %>% 
  dplyr::filter(out_mig_year == 2005) %>% 
  dplyr::select(site_num) %>% 
  unique() 

def_exposed_2005 <- c(
  32,27,11,22,56,95,77,81,83,45,46,72,78,33,88,52,49,65,89,51,80,42,55,74,65,58
)
not_exposed_2005 <- c(
  44,86,54,61,10,40,9,37,35,14
)
possibly_exposed_2005 <- c(
  53,71,13,24,56,71,13,24
)

# look for the ones I've somehow missed so far
pops_2005 %>% 
  dplyr::filter(site_num %notin% c(
    def_exposed_2005, not_exposed_2005, possibly_exposed_2005, never_exposed))





