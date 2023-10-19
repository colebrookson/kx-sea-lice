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

pink_site_name_combos <- readr::read_csv(
  here(paste0(
    "./data/spawner-recruit/clean/",
    "pink-site-name-combos-for-exposed-populations.csv" 
  ))
  ) %>%
  dplyr::mutate(
    out_mig_year = brood_year + 1
  ) %>% 
  dplyr::select(
    -brood_year
  )

chum_site_name_combos <- readr::read_csv(
  here(paste0(
    "./data/spawner-recruit/clean/",
    "chum-site-name-combos-for-exposed-populations.csv" 
  ))) %>%
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

# pink populations =============================================================

## never exposed ===============================================================
# never_exposed <- c(
#   562,1213,896,721,1140,1144,1154,1152,1199,1179,953,938,480,1033,907,
#   1032,932,965,1217,1158,1159,1151,1148,1211,1202,1195,1163,1132,1176,1212,
#   1172,1216,1164,534,1157,217,621,930, 1130,1147,1186,882,878
# )
# if(any(duplicated(never_exposed))) {
#   print(never_exposed[which(duplicated(never_exposed))])
#   stop("ERROR - Duplicates in the never exposed category")
# }

## group_1 =====================================================================
pops_pinkgroup_1 <- pink_site_name_combos %>% 
  dplyr::filter(out_mig_year == 2005) %>% 
  dplyr::select(site_num) %>% 
  unique() 

def_exposed_pinkgroup_1 <- c(
  1019,1018,1839,1017,1016,1013,1015,1014,1837,1012,1011,1010,1009,1836,
  1825,1832,1829,1820,1840
)
not_exposed_pinkgroup_1 <- c(
  553,163,512,1077,1080,1046,1081,1045,1042,1041,1040,1082,1083,1035,1034,1036,
  1037,1030,1029,1907,1906,1908,1905,1086,1085,1087,1910,1909,1904,1903,1911,
  1902,1901,1900,429,1870,1869,1588,1709,1875,1859,1841,1857,1843,1854,1845,
  1844,1846,1851,1846,1850,1474,2687,1422,201,987,1799,2689,999,998,3553,
  1090,1897,1813,1800,1807,989,1801,2615
)
possibly_exposed_pinkgroup_1 <- c(
  1838,1020,1021,1022,1023,1025,1905,1007,1008,1834,1833,1871
)

pinkgroup_1_sites_cat <- c(def_exposed_pinkgroup_1, not_exposed_pinkgroup_1, 
                       possibly_exposed_pinkgroup_1)

pops_missing_pinkgroup_1 <- pops_pinkgroup_1 %>% 
  dplyr::filter(site_num %notin% pinkgroup_1_sites_cat)
pops_missing_pinkgroup_1 <- pops_missing_pinkgroup_1$site_num

if(any(duplicated(pops_missing_pinkgroup_1))) {
  (pops_missing_2005[which(duplicated(pops_missing_pinkgroup_1))])
  stop("ERROR - Duplicates in the the pinkgroup_1 sites")
}
if(any(pinkgroup_1_sites_cat %notin% pops_pinkgroup_1$site_num)) {
  print(pinkgroup_1_sites_cat[which(pinkgroup_1_sites_cat %notin% 
                                  pops_pinkgroup_1$site_num)] )
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't 
       exist in the list of sites")
}

all(pinkgroup_1_sites_cat %in% pops_pinkgroup_1$site_num)
all(pops_pinkgroup_1$site_num %in% pinkgroup_1_sites_cat)

# look for the ones I've somehow missed so far
plot_given_sites(
  site_nums_missing = pops_missing_pinkgroup_1, 
  yr = 2005, 
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

exposed_df_pinkgroup_1 <- data.frame(
  sites = pops_pinkgroup_1$site_num,
  year = rep(2005,length(pops_pinkgroup_1$site_num)) 
  ) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      sites %in% def_exposed_pinkgroup_1                   ~ "yes",
      sites %in% possibly_exposed_pinkgroup_1              ~ "maybe",
      sites %in% not_exposed_pinkgroup_1                   ~ "no"
    )
  )

## pinkgroup_2 =================================================================
pops_pinkgroup_2 <- pink_site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2006:2016)) %>% 
  dplyr::select(site_num, site_name) %>% 
  unique() 
n_distinct(pops_pinkgroup_2$site_num)
plot_given_sites(
  site_nums_missing = pops_pinkgroup_2$site_num, 
  yr = c(2006:2016), 
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
  )

def_exposed_pinkgroup_2 <- c(
  1019,1018,1839,1840,1832,1017,1836,1016,1015,1009,1010,1011,1012,1837,
  1825,1833,1834,1008,1816,1007,1006,1005,1829,1820,1817,1014,1013,195,701,
  1799,2834
)
not_exposed_pinkgroup_2 <- c(
  1054,1077,1050,1046,1057,1068,1071,1072,512,486,163,553,169,1080,1081,1042,
  1045,1044,1082,1083,1041,1040,1039,1084,1086,1085,1910,1909,1087,1088,1090,
  1903,1911,1902,1901,1900,1588,1897,1872,429,1870,1869,1867,1709,1890,
  1887,1874,1875,1883,1474,1422,439,899,2615,201,1904,3553,1908,1907,1906,
  1030,1035,1034,1036,1037,1038,1029,1850,1851,1846,1845,1844,1853,1852,1855,
  1854,1843,1842,1841,1863
)
possibly_exposed_pinkgroup_2 <- c(
 1838,1021,1020,1022,1023,1024,1025,1026,1027,1028,1905,1001,1813,2689,993,992,
 991,990,989,1807,1871,999,998,1804,1809,1800,1801,1803,987,988,2687
)

pinkgroup_2_sites_cat <- c(def_exposed_pinkgroup_2, not_exposed_pinkgroup_2, 
                       possibly_exposed_pinkgroup_2)

pops_missing_pinkgroup_2 <- pops_pinkgroup_2 %>% 
  dplyr::filter(site_num %notin% pinkgroup_2_sites_cat)
pops_missing_pinkgroup_2 <- pops_missing_pinkgroup_2$site_num

plot_given_sites(
  site_nums_missing = pops_missing_pinkgroup_2, 
  yr = c(2006:2016), 
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)


if(any(duplicated(pinkgroup_2_sites_cat))) {
  print(pinkgroup_2_sites_cat[which(duplicated(pinkgroup_2_sites_cat))])
  stop("ERROR - Duplicates in the the pinkgroup_2 sites")
}
if(any(pinkgroup_2_sites_cat %notin% pops_pinkgroup_2$site_num)) {
  print(pinkgroup_2_sites_cat[which(pinkgroup_2_sites_cat %notin% 
                                  pops_pinkgroup_2$site_num)] )
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't 
       exist in the list of sites")
}
# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_pinkgroup_2, 
                 yr = c(2006:2016), 
                 site_df = pink_site_name_combos)

exposed_df_2_large <- pink_site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2006:2016))

exposed_df_pinkgroup_2 <- data.frame(
  site_num = pops_pinkgroup_2$site_num
  ) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_pinkgroup_2                   ~ "yes",
      site_num %in% possibly_exposed_pinkgroup_2              ~ "maybe",
      site_num %in% not_exposed_pinkgroup_2                   ~ "no"
    )
  )

exposed_df_2_large <- exposed_df_2_large %>%
  dplyr::select(site_num, out_mig_year) %>% 
  dplyr::left_join(
    x = ., 
    y = exposed_df_pinkgroup_2, 
    by = "site_num"
    ) %>% 
  dplyr::rename(
    sites = site_num,
    year = out_mig_year
  )

all(pinkgroup_2_sites_cat %in% pops_pinkgroup_2$site_num)
all(pops_pinkgroup_2$site_num %in% pinkgroup_2_sites_cat)

## pinkgroup_3 =================================================================
pops_pinkgroup_3 <- pink_site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2017:2020)) %>% 
  dplyr::select(site_num, site_name) %>% 
  unique() 
n_distinct(pops_pinkgroup_3$site_num)
plot_given_sites(
  site_nums_missing = pops_pinkgroup_3$site_num, 
  yr = c(2017:2020), 
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
  )

def_exposed_pinkgroup_3 <- c(
  1820,1829,1825,1840,1839,1017,1018,1016,1013,1015,1012,1011,1010,1009,1014,
  1837
)
not_exposed_pinkgroup_3 <- c(
  1054,1077,1050,1046,1057,1068,512,1072,486,553,163,169,1080,1081,1045,1044,
  1042,1041,1040,1038,1029,1037,1030,1036,1035,1034,7990614,1083,1084,1086,
  1085,1910,1909,3553,1908,1907,1906,1090,1911,1903,1902,1901,1872,429,1870,
  1869,1897,1890,1588,1709,1883,1817,1005,993,992,991,990,989,1799,1807,988,
  987,1803,201,2615,899,439,1422,2687,1474,1905,1904,1087,1001,1813,2689,999,998
)
possibly_exposed_pinkgroup_3 <- c(
  1833,1834,1008,1007,1816,1850,1852,1854,1843,1838,1020,1021,1022,1023,1024,
  1025,1026,1871,1027,1028
)

pinkgroup_3_sites_cat <- c(def_exposed_pinkgroup_3, not_exposed_pinkgroup_3, 
                       possibly_exposed_pinkgroup_3)

pops_missing_pinkgroup_3 <- pops_pinkgroup_3 %>% 
  dplyr::filter(site_num %notin% pinkgroup_3_sites_cat)
pops_missing_pinkgroup_3 <- pops_missing_pinkgroup_3$site_num

if(any(duplicated(pinkgroup_3_sites_cat))) {
  print(pinkgroup_3_sites_cat[which(duplicated(pinkgroup_3_sites_cat))])
  stop("ERROR - Duplicates in the the pinkgroup_3 sites")
}
if(any(pinkgroup_3_sites_cat %notin% pops_pinkgroup_3$site_num)) {
  print(pinkgroup_3_sites_cat[which(pinkgroup_3_sites_cat %notin% 
                                  pops_pinkgroup_3$site_num)] )
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't 
       exist in the list of sites")
}

# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_pinkgroup_3, 
                 yr = c(2017:2020), 
                 site_df = pink_site_name_combos,
                 large_land = readRDS(tar_read(large_land)))

all(pinkgroup_3_sites_cat %in% pops_pinkgroup_3$site_num)
all(pops_pinkgroup_3$site_num %in% pinkgroup_3_sites_cat)

exposed_df_3_large <- pink_site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2017:2020))

exposed_df_pinkgroup_3 <- data.frame(
  site_num = pops_pinkgroup_3$site_num
) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_pinkgroup_3                   ~ "yes",
      site_num %in% possibly_exposed_pinkgroup_3              ~ "maybe",
      site_num %in% not_exposed_pinkgroup_3                   ~ "no"
    )
  )

exposed_df_3_large <- exposed_df_3_large %>%
  dplyr::select(site_num, out_mig_year) %>% 
  dplyr::left_join(
    x = ., 
    y = exposed_df_pinkgroup_3, 
    by = "site_num"
  ) %>% 
  dplyr::rename(
    sites = site_num,
    year = out_mig_year
  )

## note and save the exposure dataframe ========================================
exposure_df <- rbind(
  exposed_df_pinkgroup_1, exposed_df_2_large, exposed_df_3_large
)

readr::write_csv(
  exposure_df,
  here("./data/spawner-recruit/clean/pink-exposure-categorization-df.csv")
)

# chum plots ===================================================================

## chumgroup_1 =================================================================
pops_chumgroup_1 <- chum_site_name_combos %>% 
  dplyr::filter(out_mig_year == 2005) %>% 
  dplyr::select(site_num) %>% 
  unique() 

def_exposed_chumgroup_1 <- c(
  1014,1009,1010,1011,1012,1013,1015,1015,1836,1825,1017,1832,1829,1820,1824,
  1840,1839,1018,1019,1016,1837
)
not_exposed_chumgroup_1 <- c(
  512,1046,1045,1042,1083,1041,1040,1086,1085,1910,1906,1907,3553,1909,1087,
  1905,1090,1903,1911,1902,1901,1900,1897,1869,429,1870,1869,1029,1030,1036,
  1035,1034,1474,2687,2683,1809,1800,1801,987,988,1807,1813,1799,990,991,992,
  993,998,2689,2690,1001,1817,1857,1843,1846,1854,1851,1850,2831,1709,1588,
  1037,1080,486,553,1422,899,999,201,989,439,2691
  
)
possibly_exposed_chumgroup_1 <- c(
  1020,1021,1023,1025,1833,1834,1835,1008,1007,1871
)

chumgroup_1_sites_cat <- c(def_exposed_chumgroup_1, not_exposed_chumgroup_1, 
                       possibly_exposed_chumgroup_1)

pops_missing_chumgroup_1 <- pops_chumgroup_1 %>% 
  dplyr::filter(site_num %notin% chumgroup_1_sites_cat)
pops_missing_chumgroup_1 <- pops_missing_chumgroup_1$site_num

if(any(duplicated(pops_missing_chumgroup_1))) {
  (pops_missing_2005[which(duplicated(pops_missing_chumgroup_1))])
  stop("ERROR - Duplicates in the the chumgroup_1 sites")
}
if(any(chumgroup_1_sites_cat %notin% pops_chumgroup_1$site_num)) {
  print(chumgroup_1_sites_cat[which(chumgroup_1_sites_cat %notin% 
                                  pops_chumgroup_1$site_num)] )
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't 
       exist in the list of sites")
}

all(chumgroup_1_sites_cat %in% pops_chumgroup_1$site_num)
all(pops_chumgroup_1$site_num %in% chumgroup_1_sites_cat)

# look for the ones I've somehow missed so far
plot_given_sites(
  site_nums_missing = pops_missing_chumgroup_1, 
  yr = 2005, 
  site_df = chum_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

exposed_df_chumgroup_1 <- data.frame(
  sites = pops_chumgroup_1$site_num,
  year = rep(2005,length(pops_chumgroup_1$site_num)) 
) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      sites %in% def_exposed_chumgroup_1                   ~ "yes",
      sites %in% possibly_exposed_chumgroup_1              ~ "maybe",
      sites %in% not_exposed_chumgroup_1                   ~ "no"
    )
  )

## chumgroup_2 =================================================================
pops_chumgroup_2 <- chum_site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2006:2016)) %>% 
  dplyr::select(site_num, site_name) %>% 
  unique() 
n_distinct(pops_chumgroup_2$site_num)
plot_given_sites(
  site_nums_missing = pops_chumgroup_2$site_num, 
  yr = c(2006:2016), 
  site_df = chum_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

def_exposed_chumgroup_2 <- c(
 1009,1010,1012,1014,1015,1013,1834,1007,1839,1018,1829,1820,1840,
 1836,1833,1008,1016,1017,1019,2030,1832,1825,1837,1011,7990591,1816,1005,
 1006
)
not_exposed_chumgroup_2 <- c(
  553,486,512,1046,1080,1045,1042,1041,1040,1039,1083,1084,1086,1906,1908,
  1905,1904,1090,1088,1909,1910,1911,1903,1902,1901,1900,1870,1588,1869,
  1897,1035,1034,1036,1037,1030,1029,1850,1851,1846,1852,1844,1854,2831,1843,
  1842,1817,991,993,998,999,2689,2690,989,1799,1807,2691,1001,1813,1800,1801,
  1809,2683,2687,1474,1422,201,996,992,990,988,987,1803,439,2708,1804,2834,
  1841,1855,1874,1875,1883,1887,1709,1890,429,1872,3553,1038,1907,1087,1085,
  1082,1077,1050,1057,1054,1068,1072,1076,163,169,1044,110,701,2615,899
)
possibly_exposed_chumgroup_2 <- c(
  1025,1021,1838,1020,1022,1023,1024,1026,1028,1812,1871
)

chumgroup_2_sites_cat <- c(def_exposed_chumgroup_2, not_exposed_chumgroup_2, 
                       possibly_exposed_chumgroup_2)

pops_missing_chumgroup_2 <- pops_chumgroup_2 %>% 
  dplyr::filter(site_num %notin% chumgroup_2_sites_cat)
pops_missing_chumgroup_2 <- pops_missing_chumgroup_2$site_num

plot_given_sites(
  site_nums_missing = pops_missing_chumgroup_2, 
  yr = c(2006:2016), 
  site_df = chum_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)


if(any(duplicated(chumgroup_2_sites_cat))) {
  print(chumgroup_2_sites_cat[which(duplicated(chumgroup_2_sites_cat))])
  stop("ERROR - Duplicates in the the chumgroup_2 sites")
}
if(any(chumgroup_2_sites_cat %notin% pops_chumgroup_2$site_num)) {
  print(chumgroup_2_sites_cat[which(chumgroup_2_sites_cat %notin% 
                                  pops_chumgroup_2$site_num)] )
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't 
       exist in the list of sites")
}
# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_chumgroup_2, 
                 yr = c(2006:2016), 
                 site_df = chum_site_name_combos)

exposed_df_2_large <- chum_site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2006:2016))

exposed_df_chumgroup_2 <- data.frame(
  site_num = pops_chumgroup_2$site_num
) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_chumgroup_2                   ~ "yes",
      site_num %in% possibly_exposed_chumgroup_2              ~ "maybe",
      site_num %in% not_exposed_chumgroup_2                   ~ "no"
    )
  )

exposed_df_2_large <- exposed_df_2_large %>%
  dplyr::select(site_num, out_mig_year) %>% 
  dplyr::left_join(
    x = ., 
    y = exposed_df_chumgroup_2, 
    by = "site_num"
  ) %>% 
  dplyr::rename(
    sites = site_num,
    year = out_mig_year
  )

all(chumgroup_2_sites_cat %in% pops_chumgroup_2$site_num)
all(pops_chumgroup_2$site_num %in% chumgroup_2_sites_cat)

## chumgroup_3 ======================================================================
pops_chumgroup_3 <- chum_site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2017:2020)) %>% 
  dplyr::select(site_num, site_name) %>% 
  unique() 
n_distinct(pops_chumgroup_3$site_num)
plot_given_sites(
  site_nums_missing = pops_chumgroup_3$site_num, 
  yr = c(2017:2020), 
  site_df = chum_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

def_exposed_chumgroup_3 <- c(
  1015,1014,1009,1011,1010,1016,1017,1019,1839,1840,2030,1820,1834
)
not_exposed_chumgroup_3 <- c(
  169,163,553,1076,512,1072,1068,1057,1050,1077,1054,1046,1045,1044,1042,1080,
  1041,1040,1083,1035,1034,1036,1030,1037,1038,1029,1906,1907,1027,3553,
  1084,1086,1085,1910,1087,1090,1903,1028,1872,429,1870,1869,1887,1883,1474,
  1422,439,899,2615,701,110,201,987,1799,989,1807,1813,990,991,992,993,998,999,
  1007,1005,1809
)
possibly_exposed_chumgroup_3 <- c(
  7990591,1850,1852,1853,1855,1854,1843,1018,1020,1021,1022,1023,1024,1025,1026,
  1833,1871
)

chumgroup_3_sites_cat <- c(def_exposed_chumgroup_3, not_exposed_chumgroup_3, 
                       possibly_exposed_chumgroup_3)

pops_missing_chumgroup_3 <- pops_chumgroup_3 %>% 
  dplyr::filter(site_num %notin% chumgroup_3_sites_cat)
pops_missing_chumgroup_3 <- pops_missing_chumgroup_3$site_num

if(any(duplicated(chumgroup_3_sites_cat))) {
  print(chumgroup_3_sites_cat[which(duplicated(chumgroup_3_sites_cat))])
  stop("ERROR - Duplicates in the the chumgroup_3 sites")
}
if(any(chumgroup_3_sites_cat %notin% pops_chumgroup_3$site_num)) {
  print(chumgroup_3_sites_cat[which(chumgroup_3_sites_cat %notin% 
                                  pops_chumgroup_3$site_num)] )
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't 
       exist in the list of sites")
}

# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_chumgroup_3, 
                 yr = c(2017:2020), 
                 site_df = chum_site_name_combos,
                 large_land = readRDS(tar_read(large_land)))

all(chumgroup_3_sites_cat %in% pops_chumgroup_3$site_num)
all(pops_chumgroup_3$site_num %in% chumgroup_3_sites_cat)

exposed_df_3_large <- chum_site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2017:2020))

exposed_df_chumgroup_3 <- data.frame(
  site_num = pops_chumgroup_3$site_num
) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_chumgroup_3                   ~ "yes",
      site_num %in% possibly_exposed_chumgroup_3              ~ "maybe",
      site_num %in% not_exposed_chumgroup_3                   ~ "no"
    )
  )

exposed_df_3_large <- exposed_df_3_large %>%
  dplyr::select(site_num, out_mig_year) %>% 
  dplyr::left_join(
    x = ., 
    y = exposed_df_chumgroup_3, 
    by = "site_num"
  ) %>% 
  dplyr::rename(
    sites = site_num,
    year = out_mig_year
  )

## note and save the exposure dataframe =========================================
exposure_df_chum <- rbind(
  exposed_df_chumgroup_1, exposed_df_2_large, exposed_df_3_large
)

readr::write_csv(
  exposure_df_chum,
  here("./data/spawner-recruit/clean/chum-exposure-categorization-df.csv")
)
