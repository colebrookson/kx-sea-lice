library(readr)
library(here)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(lubridate)
library(glmmTMB)
library(stringr)
library(targets)

source(here("./R/00_functions_global.R"))
source(here("./R/04_functions_mapping.R"))

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

# farms in which years =========================================================
farms_2005 <- c("kid", "goat")
farms_2006 <- c("kid", "goat", "loch", "jackson")
farms_2007 <- c("kid", "jackson", "loch")
farms_2008 <- c("kid", "goat", "sheep", "loch")
farms_2009 <- c("kid", "goat", "sheep", "lime", "jackson")
farms_2010 <- c("kid", "goat", "sheep", "lime", "jackson")
farms_2011 <- c("kid", "goat", "sheep", "lime", "jackson", "loch")
farms_2012 <- c("kid", "goat", "lime", "jackson")
farms_2013 <- c("goat", "sheep", "jackson", "loch")
farms_2014 <- c("kid", "goat", "sheep", "lime", "jackson", "loch")
farms_2015 <- c("lime", "sheep", "jackson", "loch")
farms_2016 <- c("kid", "goat", "sheep", "jackson", "loch")
farms_2017 <- c("cougar", "alex", "sheep")
farms_2018 <- c("kid", "goat", "alex")
farms_2019 <- c("cougar", "alex", "kid")
farms_2020 <- c("cougar", "alex", "sheep")

func_group_1 <- c(2005)
func_group_2 <- c(2006:2016)
func_group_3 <- c(2017:2020)

# never exposed ================================================================
never_exposed <- c(
  43,92,96,97,6,48,59,53,57,70,93,82,69,50,91,73,60,90,62,68,79,47,84,76,26,33,
  31,39,67,16,38,8,20,7,30,94,19,18,4,1,5,98,66,2,3,63,64,23,75
)
if(any(duplicated(never_exposed))) {
  (never_exposed[which(duplicated(never_exposed))])
  stop("ERROR - Duplicates in the never exposed category")
}

# 2005 =========================================================================
pops_2005 <- site_name_combos %>% 
  dplyr::filter(out_mig_year == 2005) %>% 
  dplyr::select(site_num) %>% 
  unique() 

def_exposed_2005 <- c(
  32,27,11,22,56,95,77,81,83,45,46,72,78,33,88,52,49,65,89,51,80,42,55,74,65,58
)
not_exposed_2005 <- c(
  44,86,54,61,10,40,9,37,35,14,34,36,28,25,41,29,15,12,21,28,25,17
)
possibly_exposed_2005 <- c(
  53,71,13,24,56,71,13,24,87,85
)

pops_missing_2005 <- pops_2005 %>% 
  dplyr::filter(site_num %notin% c(
    def_exposed_2005, not_exposed_2005, possibly_exposed_2005, 
    never_exposed))
pops_missing_2005 <- pops_missing_2005$site_num

if(any(duplicated(pops_missing_2005))) {
  (pops_missing_2005[which(duplicated(pops_missing_2005))])
  stop("ERROR - Duplicates in the the 2005 sites")
}

# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_2005, yr = 2005, site_df = site_name_combos)

exposed_df_2005 <- data.frame(
  sites = pops_2005$site_num,
  year = rep(2005,length(pops_2005$site_num)) 
  ) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      sites %in% def_exposed_2005                   ~ "yes",
      sites %in% possibly_exposed_2005              ~ "maybe",
      sites %in% c(not_exposed_2005, never_exposed) ~ "no"
    )
  )



