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
  562,1213,896,721,1140,1144,1154,1152,1199,1179,953,938,480,1033,907,
  1032,932,965,1217,1158,1159,1151,1148,1211,1202,1195,1163,1132,1176,1212,
  1172,1216,1164,534,1157,217,621,930, 1130,1147,1186,882,878
)
if(any(duplicated(never_exposed))) {
  print(never_exposed[which(duplicated(never_exposed))])
  stop("ERROR - Duplicates in the never exposed category")
}

# group_1 ======================================================================
pops_group_1 <- site_name_combos %>% 
  dplyr::filter(out_mig_year == 2005) %>% 
  dplyr::select(site_num) %>% 
  unique() 

def_exposed_group_1 <- c(
  1208,1157,1207,1135,969,888,480,1143,1131,1182,1198,1191,935,1128,1129,
  1171,1185,954
)
not_exposed_group_1 <- c(
  1127,1203,1150,1141,884,1035,883,1027,999,910,896,941,1009,984,217,890,962,
  1041,902,1157,933,944,960,926
)
possibly_exposed_group_1 <- c(
  1205,1134,721,1189,1173,1166,1125,1145
)

group_1_sites_cat <- c(def_exposed_group_1, not_exposed_group_1, 
                       possibly_exposed_group_1, never_exposed)

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

# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_group_1, yr = 2005, site_df = site_name_combos)

exposed_df_group_1 <- data.frame(
  sites = pops_group_1$site_num,
  year = rep(2005,length(pops_group_1$site_num)) 
  ) %>% 
  dplyr::mutate(
    exposure = dplyr::case_when(
      sites %in% def_exposed_group_1                   ~ "yes",
      sites %in% possibly_exposed_group_1              ~ "maybe",
      sites %in% c(not_exposed_group_1, never_exposed) ~ "no"
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
  site_df = site_name_combos)


def_exposed_group_2 <- c(
  1208,1157,1207,969,954,888,935,1143,1131,1182,1191,1198,1128,1129,
  1171,1185,1166,941,1009,982,1026,1135,889
)
not_exposed_group_2 <- c(
  562,1213,721,1140,1144,1154,1152,1199,1179,953,938,1033,907,
  1032,932,965,1217,1158,1159,1151,1148,1211,1202,1195,1163,1132,1176,1212,
  1172,1216,1164,217,621,930, 1130,1147,1186,882,878,1161,1187,
  1165,1137,1138,997,727,818,933,1038,902,
  837,1200,1178,896,1175,1196,1139,1126,1174,980,480,1218,1036,534,886,386,
  967,928,1127,1203,1194,993,1141,1150,1035,884,883,996,999,1204,926,95,998
)
possibly_exposed_group_2 <- c(
  1181,1205,1188,1125,1189,1134,984,890,1024,962,1041,1008,879,976,944,960,
  1145,1173
)

group_2_sites_cat <- c(def_exposed_group_2, not_exposed_group_2, 
                       possibly_exposed_group_2)

pops_missing_group_2 <- pops_group_2 %>% 
  dplyr::filter(site_num %notin% group_2_sites_cat)
pops_missing_group_2 <- pops_missing_group_2$site_num

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
)

# group_3 ======================================================================
pops_group_3 <- site_name_combos %>% 
  dplyr::filter(out_mig_year %in% c(2017:2020)) %>% 
  dplyr::select(site_num, site_name) %>% 
  unique() 
n_distinct(pops_group_3$site_num)
plot_given_sites(
  site_nums_missing = pops_group_3$site_num, 
  yr = c(2017:2020), 
  site_df = site_name_combos)

def_exposed_group_3 <- c(
  1207,1131,1135,1191,1198,1182,1129,1128,1171,1185,935,969,954
)
not_exposed_group_3 <- c(
  1161,621,1178,1187,1165,1138,727,1140,1175,1213,1147,1144,
  1152,1174,1199,1179,1130,953,1186,1164,1216,1148,1211,1151,1159,1202,
  1139,1195,1163,1176,1132,1212,1172,1180,907,1032,1033,1218,965,1217,
  932,878,896,928,926,562,217,818,837,902,1204,1200,
  1041,879,962,890,984,1024,997,938,1158,1145,886,534,1038,933,1157
)
possibly_exposed_group_3 <- c(
  1194,1127,883,1173,1134,1189,721,1125,1188,1205,1181,1126,889,1009,1166,941,
  1026,1035,1157
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
                 site_df = site_name_combos)
