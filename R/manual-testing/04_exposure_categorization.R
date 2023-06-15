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

site_name_combos <- readr::read_csv(
  here(
    "./data/spawner-recruit/clean/site-name-combos-for-exposed-populations.csv")
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
# never_exposed <- c(
#   562,1213,896,721,1140,1144,1154,1152,1199,1179,953,938,480,1033,907,
#   1032,932,965,1217,1158,1159,1151,1148,1211,1202,1195,1163,1132,1176,1212,
#   1172,1216,1164,534,1157,217,621,930, 1130,1147,1186,882,878
# )
# if(any(duplicated(never_exposed))) {
#   print(never_exposed[which(duplicated(never_exposed))])
#   stop("ERROR - Duplicates in the never exposed category")
# }

# group_1 ======================================================================
pops_group_1 <- site_name_combos %>% 
  dplyr::filter(out_mig_year == 2005) %>% 
  dplyr::select(site_num) %>% 
  unique() 

def_exposed_group_1 <- c(
  1019,1018,1839,1017,1016,1013,1015,1014,1837,1012,1011,1010,1009,1836,
  1825,1832,1829,1820,1840
)
not_exposed_group_1 <- c(
  553,163,512,1077,1080,1046,1081,1045,1042,1041,1040,1082,1083,1035,1034,1036,
  1037,1030,1029,1907,1906,1908,1905,1086,1085,1087,1910,1909,1904,1903,1911,
  1902,1901,1900,429,1870,1869,1588,1709,1875,1859,1841,1857,1843,1854,1845,
  1844,1846,1851,1846,1850,1474,2687,1422,201,987,1799,2689,999,998,3553,
  1090,1897,1813,1800,1807,989,1801,2615
)
possibly_exposed_group_1 <- c(
  1838,1020,1021,1022,1023,1025,1905,1007,1008,1834,1833,1871
)

group_1_sites_cat <- c(def_exposed_group_1, not_exposed_group_1, 
                       possibly_exposed_group_1)

pops_missing_group_1 <- pops_group_1 %>% 
  dplyr::filter(site_num %notin% group_1_sites_cat)
pops_missing_group_1 <- pops_missing_group_1$site_num

if(any(duplicated(pops_missing_group_1))) {
  (pops_missing_2005[which(duplicated(pops_missing_group_1))])
  stop("ERROR - Duplicates in the the group_1 sites")
}
if(any(group_1_sites_cat %notin% pops_group_1$site_num)) {
  print(group_1_sites_cat[which(group_1_sites_cat %notin% 
                                  pops_group_1$site_num)] )
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't 
       exist in the list of sites")
}

all(group_1_sites_cat %in% pops_group_1$site_num)
all(pops_group_1$site_num %in% group_1_sites_cat)

# look for the ones I've somehow missed so far
plot_given_sites(
  site_nums_missing = pops_missing_group_1, 
  yr = 2005, 
  site_df = site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

exposed_df_group_1 <- data.frame(
  sites = pops_group_1$site_num,
  year = rep(2005,length(pops_group_1$site_num)) 
  ) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      sites %in% def_exposed_group_1                   ~ "yes",
      sites %in% possibly_exposed_group_1              ~ "maybe",
      sites %in% not_exposed_group_1                   ~ "no"
    )
  )

# group_2 ======================================================================
pops_group_2 <- site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2006:2016)) %>% 
  dplyr::select(site_num, site_name) %>% 
  unique() 
n_distinct(pops_group_2$site_num)
plot_given_sites(
  site_nums_missing = pops_group_2$site_num, 
  yr = c(2006:2016), 
  site_df = site_name_combos,
  large_land = readRDS(tar_read(large_land))
  )

def_exposed_group_2 <- c(
  1019,1018,1839,1840,1832,1017,1836,1016,1015,1009,1010,1011,1012,1837,
  1825,1833,1834,1008,1816,1007,1006,1005,1829,1820,1817,1014,1013,195,701,
  1799,2834
)
not_exposed_group_2 <- c(
  1054,1077,1050,1046,1057,1068,1071,1072,512,486,163,553,169,1080,1081,1042,
  1045,1044,1082,1083,1041,1040,1039,1084,1086,1085,1910,1909,1087,1088,1090,
  1903,1911,1902,1901,1900,1588,1897,1872,429,1870,1869,1867,1709,1890,
  1887,1874,1875,1883,1474,1422,439,899,2615,201,1904,3553,1908,1907,1906,
  1030,1035,1034,1036,1037,1038,1029,1850,1851,1846,1845,1844,1853,1852,1855,
  1854,1843,1842,1841,1863
)
possibly_exposed_group_2 <- c(
 1838,1021,1020,1022,1023,1024,1025,1026,1027,1028,1905,1001,1813,2689,993,992,
 991,990,989,1807,1871,999,998,1804,1809,1800,1801,1803,987,988,2687
)

group_2_sites_cat <- c(def_exposed_group_2, not_exposed_group_2, 
                       possibly_exposed_group_2)

pops_missing_group_2 <- pops_group_2 %>% 
  dplyr::filter(site_num %notin% group_2_sites_cat)
pops_missing_group_2 <- pops_missing_group_2$site_num

plot_given_sites(
  site_nums_missing = pops_missing_group_2, 
  yr = c(2006:2016), 
  site_df = site_name_combos,
  large_land = readRDS(tar_read(large_land))
)


if(any(duplicated(group_2_sites_cat))) {
  print(group_2_sites_cat[which(duplicated(group_2_sites_cat))])
  stop("ERROR - Duplicates in the the group_2 sites")
}
if(any(group_2_sites_cat %notin% pops_group_2$site_num)) {
  print(group_2_sites_cat[which(group_2_sites_cat %notin% 
                                  pops_group_2$site_num)] )
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't 
       exist in the list of sites")
}
# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_group_2, 
                 yr = c(2006:2016), 
                 site_df = site_name_combos)

exposed_df_2_large <- site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2006:2016))

exposed_df_group_2 <- data.frame(
  site_num = pops_group_2$site_num
  ) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_group_2                   ~ "yes",
      site_num %in% possibly_exposed_group_2              ~ "maybe",
      site_num %in% not_exposed_group_2                   ~ "no"
    )
  )

exposed_df_2_large <- exposed_df_2_large %>%
  dplyr::select(site_num, out_mig_year) %>% 
  dplyr::left_join(
    x = ., 
    y = exposed_df_group_2, 
    by = "site_num"
    ) %>% 
  dplyr::rename(
    sites = site_num,
    year = out_mig_year
  )

all(group_2_sites_cat %in% pops_group_2$site_num)
all(pops_group_2$site_num %in% group_2_sites_cat)

# group_3 ======================================================================
pops_group_3 <- site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2017:2020)) %>% 
  dplyr::select(site_num, site_name) %>% 
  unique() 
n_distinct(pops_group_3$site_num)
plot_given_sites(
  site_nums_missing = pops_group_3$site_num, 
  yr = c(2017:2020), 
  site_df = site_name_combos,
  large_land = readRDS(tar_read(large_land))
  )

def_exposed_group_3 <- c(
  1820,1829,1825,1840,1839,1017,1018,1016,1013,1015,1012,1011,1010,1009,1014,
  1837
)
not_exposed_group_3 <- c(
  1054,1077,1050,1046,1057,1068,512,1072,486,553,163,169,1080,1081,1045,1044,
  1042,1041,1040,1038,1029,1037,1030,1036,1035,1034,7990614,1083,1084,1086,
  1085,1910,1909,3553,1908,1907,1906,1090,1911,1903,1902,1901,1872,429,1870,
  1869,1897,1890,1588,1709,1883,1817,1005,993,992,991,990,989,1799,1807,988,
  987,1803,201,2615,899,439,1422,2687,1474,1905,1904,1087,1001,1813,2689,999,998
)
possibly_exposed_group_3 <- c(
  1833,1834,1008,1007,1816,1850,1852,1854,1843,1838,1020,1021,1022,1023,1024,
  1025,1026,1871,1027,1028
)

group_3_sites_cat <- c(def_exposed_group_3, not_exposed_group_3, 
                       possibly_exposed_group_3)

pops_missing_group_3 <- pops_group_3 %>% 
  dplyr::filter(site_num %notin% group_3_sites_cat)
pops_missing_group_3 <- pops_missing_group_3$site_num

if(any(duplicated(group_3_sites_cat))) {
  print(group_3_sites_cat[which(duplicated(group_3_sites_cat))])
  stop("ERROR - Duplicates in the the group_3 sites")
}
if(any(group_3_sites_cat %notin% pops_group_3$site_num)) {
  print(group_3_sites_cat[which(group_3_sites_cat %notin% 
                                  pops_group_3$site_num)] )
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't 
       exist in the list of sites")
}

# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_group_3, 
                 yr = c(2017:2020), 
                 site_df = site_name_combos,
                 large_land = readRDS(tar_read(large_land)))

all(group_3_sites_cat %in% pops_group_3$site_num)
all(pops_group_3$site_num %in% group_3_sites_cat)

exposed_df_3_large <- site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2017:2020))

exposed_df_group_3 <- data.frame(
  site_num = pops_group_3$site_num
) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_group_3                   ~ "yes",
      site_num %in% possibly_exposed_group_3              ~ "maybe",
      site_num %in% not_exposed_group_3                   ~ "no"
    )
  )

exposed_df_3_large <- exposed_df_3_large %>%
  dplyr::select(site_num, out_mig_year) %>% 
  dplyr::left_join(
    x = ., 
    y = exposed_df_group_3, 
    by = "site_num"
  ) %>% 
  dplyr::rename(
    sites = site_num,
    year = out_mig_year
  )

# note and save the exposure dataframe =========================================
exposure_df <- rbind(
  exposed_df_group_1, exposed_df_2_large, exposed_df_3_large
)

readr::write_csv(
  exposure_df,
  here("./data/spawner-recruit/clean/exposure-categorization-df.csv")
)