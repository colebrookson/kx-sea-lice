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

source(here("./src/R/00_functions_global.R"))
source(here("./src/R/04_functions_mapping.R"))

large_land <- readRDS(targets::tar_read(larger_land))

# make dataframes to do the exposure cateogrization with

sr_pink_pop_data <- tar_read(clean_pink_spawner_recruit_data)
sr_chum_pop_data <- tar_read(clean_chum_spawner_recruit_data)

pink_site_name_combos <- sr_pink_pop_data %>%
  dplyr::filter(brood_year >= 2004) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(out_mig_year = brood_year + 1) %>%
  dplyr::group_by(out_mig_year, gfe_id) %>%
  unique() %>%
  dplyr::select(river, gfe_id, lat, long, out_mig_year, area) %>%
  dplyr::rename(
    site_num = gfe_id, site_name = river,
    # IMPORTANT ! they're not right before this
    lat = long, long = lat
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(site_name, lat, long, site_num, out_mig_year, area)

chum_site_name_combos <- sr_chum_pop_data %>%
  dplyr::filter(brood_year >= 2004) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(out_mig_year = brood_year + 1) %>%
  dplyr::group_by(out_mig_year, gfe_id) %>%
  unique() %>%
  dplyr::select(river, gfe_id, lat, long, out_mig_year, area) %>%
  dplyr::rename(
    site_num = gfe_id, site_name = river,
    # IMPORTANT ! they're not right before this
    lat = long, long = lat
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(site_name, lat, long, site_num, out_mig_year, area)


# read in the file with the yearly ones ========================================

# pink_site_name_combos <- readr::read_csv(
#   here(paste0(
#     "./data/spawner-recruit/clean/",
#     "pink-site-name-combos-for-exposed-populations.csv"
#   ))
#   ) %>%
#   dplyr::mutate(
#     out_mig_year = brood_year + 1
#   ) %>%
#   dplyr::select(
#     -brood_year
#   )
#
# chum_site_name_combos <- readr::read_csv(
#   here(paste0(
#     "./data/spawner-recruit/clean/",
#     "chum-site-name-combos-for-exposed-populations.csv"
#   ))) %>%
#   dplyr::mutate(
#     out_mig_year = brood_year + 1
#   ) %>%
#   dplyr::select(
#     -brood_year
#   )

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
func_group_3 <- c(2017:2019)
func_group_4 <- c(2020)

# pink populations =============================================================

#' Note there are three exposure possibilities:
#' 1) Where all "maybe" populations are "maybe"
#' 2) Where all northern "maybe" populations are "maybe" and all southern
#' populations are "no"
#' 3) Where all southern "maybe" populations are "maybe" and all northern
#' populations are "no"

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

## pinkgroup_1 =====================================================================
pops_pinkgroup_1 <- pink_site_name_combos %>%
  dplyr::filter(out_mig_year == 2005) %>%
  dplyr::select(site_num) %>%
  unique()

def_exposed_pinkgroup_1 <- c(
  1019, 1018, 1839, 1017, 1016, 1013, 1015, 1014, 1837, 1012, 1011, 1010, 1009, 1836,
  1825, 1832, 1829, 1820, 1840
)

possibly_exposed_pinkgroup_1_all_maybe <- c(
  1838, 1020, 1021, 1022, 1023, 1025, 1007, 1008, 1834, 1833, 2687, 1801, 1800,
  987, 989, 2689, 1813, 999, 998, 1807
)

not_exposed_pinkgroup_1 <- pops_pinkgroup_1$site_num[which(
  pops_pinkgroup_1$site_num %notin%
    c(def_exposed_pinkgroup_1, possibly_exposed_pinkgroup_1_all_maybe)
)]

possibly_exposed_pinkgroup_1_north_maybe <- c(
  1025, 1023, 1020, 1022, 1838, 1021, 1008, 1834, 1007, 1833, 1837
)
possibly_exposed_pinkgroup_1_south_maybe <-
  c(
    possibly_exposed_pinkgroup_1_all_maybe[which(
      possibly_exposed_pinkgroup_1_all_maybe %notin%
        possibly_exposed_pinkgroup_1_north_maybe
    )],
    # need to add the ones that are in seaforth
    c(1007, 1008, 1834, 1833)
  )


pinkgroup_1_sites_cat <- c(
  def_exposed_pinkgroup_1, not_exposed_pinkgroup_1,
  possibly_exposed_pinkgroup_1_all_maybe
)
if (any(duplicated(pinkgroup_1_sites_cat))) {
  stop(paste0(
    "duplicated pops:",
    pinkgroup_1_sites_cat[which(duplicated(pinkgroup_1_sites_cat))]
  ))
}

pops_missing_pinkgroup_1 <- pops_pinkgroup_1 %>%
  dplyr::filter(site_num %notin% pinkgroup_1_sites_cat)
pops_missing_pinkgroup_1 <- pops_missing_pinkgroup_1$site_num

if (any(duplicated(pops_missing_pinkgroup_1))) {
  (pops_missing_2005[which(duplicated(pops_missing_pinkgroup_1))])
  stop("ERROR - Duplicates in the the pinkgroup_1 sites")
}
if (any(pinkgroup_1_sites_cat %notin% pops_pinkgroup_1$site_num)) {
  print(pinkgroup_1_sites_cat[which(pinkgroup_1_sites_cat %notin%
    pops_pinkgroup_1$site_num)])
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

plot_given_sites(
  site_nums_missing = pops_missing_pinkgroup_1,
  yr = 2005,
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

# look at just the maybes to determine north vs south
plot_given_sites(
  site_nums_missing = possibly_exposed_pinkgroup_1_all_maybe,
  yr = 2005,
  site_df = pink_site_name_combos,
  large_land = large_land
)

exposed_df_pinkgroup_1 <- data.frame(
  sites = pops_pinkgroup_1$site_num,
  year = rep(2005, length(pops_pinkgroup_1$site_num))
) %>%
  dplyr::mutate(
    exposure = dplyr::case_when(
      sites %in% def_exposed_pinkgroup_1 ~ "yes",
      sites %in% possibly_exposed_pinkgroup_1_all_maybe ~ "maybe",
      sites %in% not_exposed_pinkgroup_1 ~ "no"
    ),
    maybes = dplyr::case_when(
      sites %in% possibly_exposed_pinkgroup_1_north_maybe ~ "north",
      sites %in% possibly_exposed_pinkgroup_1_south_maybe ~ "south",
      TRUE ~ NA
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
  # large_land = readRDS(tar_read(large_land))
  large_land = large_land
)

def_exposed_pinkgroup_2 <- c(
  1019, 1018, 1839, 1840, 1832, 1017, 1836, 1016, 1015, 1009, 1010, 1011, 1012, 1837,
  1825, 1833, 1834, 1008, 1816, 1007, 1006, 1005, 1829, 1820, 1014, 1013
)

possibly_exposed_pinkgroup_2_all_maybe <- c(
  1838, 1021, 1020, 1022, 1023, 1024, 1025, 1026, 1001, 1813, 2689, 993, 992,
  991, 990, 989, 1807, 999, 998, 1804, 1809, 1800, 1801, 1803, 987, 988, 2687, 1817
)

not_exposed_pinkgroup_2 <- pops_pinkgroup_2$site_num[which(
  pops_pinkgroup_2$site_num %notin%
    c(def_exposed_pinkgroup_2, possibly_exposed_pinkgroup_2_all_maybe)
)]

possibly_exposed_pinkgroup_2_north_maybe <- c(
  1026, 1025, 1024, 1023, 1022, 1021, 1020, 1838
)

possibly_exposed_pinkgroup_2_south_maybe <-
  possibly_exposed_pinkgroup_2_all_maybe[which(
    possibly_exposed_pinkgroup_2_all_maybe %notin%
      possibly_exposed_pinkgroup_2_north_maybe
  )]

pinkgroup_2_sites_cat <- c(
  def_exposed_pinkgroup_2, not_exposed_pinkgroup_2,
  possibly_exposed_pinkgroup_2_all_maybe
)

pops_missing_pinkgroup_2 <- pops_pinkgroup_2 %>%
  dplyr::filter(site_num %notin% pinkgroup_2_sites_cat)
pops_missing_pinkgroup_2 <- pops_missing_pinkgroup_2$site_num

plot_given_sites(
  site_nums_missing = pops_missing_pinkgroup_2,
  yr = c(2006:2016),
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)
plot_given_sites(
  site_nums_missing = possibly_exposed_pinkgroup_2_all_maybe,
  yr = c(2006:2016),
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

if (any(duplicated(pinkgroup_2_sites_cat))) {
  print(pinkgroup_2_sites_cat[which(duplicated(pinkgroup_2_sites_cat))])
  stop("ERROR - Duplicates in the the pinkgroup_2 sites")
}
if (any(pinkgroup_2_sites_cat %notin% pops_pinkgroup_2$site_num)) {
  print(pinkgroup_2_sites_cat[which(pinkgroup_2_sites_cat %notin%
    pops_pinkgroup_2$site_num)])
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't
       exist in the list of sites")
}
# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_pinkgroup_2,
  yr = c(2006:2016),
  site_df = pink_site_name_combos
)

exposed_df_2_large <- pink_site_name_combos %>%
  dplyr::filter(out_mig_year %in% c(2006:2016))

exposed_df_pinkgroup_2 <- data.frame(
  site_num = pops_pinkgroup_2$site_num
) %>%
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_pinkgroup_2 ~ "yes",
      site_num %in% possibly_exposed_pinkgroup_2_all_maybe ~ "maybe",
      site_num %in% not_exposed_pinkgroup_2 ~ "no"
    ),
    maybes = dplyr::case_when(
      site_num %in% possibly_exposed_pinkgroup_2_north_maybe ~ "north",
      site_num %in% possibly_exposed_pinkgroup_2_south_maybe ~ "south",
      TRUE ~ NA
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
  dplyr::filter(out_mig_year %in% c(2017:2019)) %>%
  dplyr::select(site_num, site_name) %>%
  unique()
n_distinct(pops_pinkgroup_3$site_num)
plot_given_sites(
  site_nums_missing = pops_pinkgroup_3$site_num,
  yr = c(2017:2019),
  site_df = pink_site_name_combos,
  large_land = large_land
)

def_exposed_pinkgroup_3 <- c(
  1820, 1829, 1825, 1840, 1839, 1017, 1018, 1016, 1013, 1015, 1011, 1010, 1009, 1014,
  1837
)

possibly_exposed_pinkgroup_3_all_maybe <- c(
  1833, 1007, 1816, 1850, 1852, 1854, 1843, 1838, 1020, 1021, 1022, 1023, 1024,
  1025, 1026, 2687, 1803, 987, 988, 1807, 1005, 1001, 999, 998, 993,
  991, 992, 990, 2689, 989, 1813
)

not_exposed_pinkgroup_3 <- pops_pinkgroup_3$site_num[which(
  pops_pinkgroup_3$site_num %notin%
    c(def_exposed_pinkgroup_3, possibly_exposed_pinkgroup_3_all_maybe)
)]

possibly_exposed_pinkgroup_3_north_maybe <- c(
  1025, 1026, 1024, 1023, 1022, 1021, 1020, 1838, 1850, 1852, 1854,
  1843, 1007, 1833
)
possibly_exposed_pinkgroup_3_south_maybe <-
  c(
    possibly_exposed_pinkgroup_3_all_maybe[which(
      possibly_exposed_pinkgroup_3_all_maybe %notin%
        possibly_exposed_pinkgroup_3_north_maybe
    )],
    c(1007, 1833)
  )

pinkgroup_3_sites_cat <- c(
  def_exposed_pinkgroup_3, not_exposed_pinkgroup_3,
  possibly_exposed_pinkgroup_3_all_maybe
)

pops_missing_pinkgroup_3 <- pops_pinkgroup_3 %>%
  dplyr::filter(site_num %notin% pinkgroup_3_sites_cat)
pops_missing_pinkgroup_3 <- pops_missing_pinkgroup_3$site_num

if (any(duplicated(pinkgroup_3_sites_cat))) {
  print(pinkgroup_3_sites_cat[which(duplicated(pinkgroup_3_sites_cat))])
  stop("ERROR - Duplicates in the the pinkgroup_3 sites")
}
if (any(pinkgroup_3_sites_cat %notin% pops_pinkgroup_3$site_num)) {
  print(pinkgroup_3_sites_cat[which(pinkgroup_3_sites_cat %notin%
    pops_pinkgroup_3$site_num)])
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't
       exist in the list of sites")
}

# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_pinkgroup_3,
  yr = c(2017:2019),
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)
plot_given_sites(c(possibly_exposed_pinkgroup_3_all_maybe),
  yr = c(2017:2019),
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

all(pinkgroup_3_sites_cat %in% pops_pinkgroup_3$site_num)
all(pops_pinkgroup_3$site_num %in% pinkgroup_3_sites_cat)

exposed_df_3_large <- pink_site_name_combos %>%
  dplyr::filter(out_mig_year %in% c(2017:2019))

exposed_df_pinkgroup_3 <- data.frame(
  site_num = pops_pinkgroup_3$site_num
) %>%
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_pinkgroup_3 ~ "yes",
      site_num %in% possibly_exposed_pinkgroup_3_all_maybe ~ "maybe",
      site_num %in% not_exposed_pinkgroup_3 ~ "no"
    ),
    maybes = dplyr::case_when(
      site_num %in% possibly_exposed_pinkgroup_3_north_maybe ~ "north",
      site_num %in% possibly_exposed_pinkgroup_3_south_maybe ~ "south",
      TRUE ~ NA
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

## pinkgroup 4 =================================================================
pops_pinkgroup_4 <- pink_site_name_combos %>%
  dplyr::filter(out_mig_year %in% c(2020)) %>%
  dplyr::select(site_num, site_name) %>%
  unique()
n_distinct(pops_pinkgroup_4$site_num)
plot_given_sites(
  site_nums_missing = pops_pinkgroup_4$site_num,
  yr = 2020,
  site_df = pink_site_name_combos,
  large_land = large_land
)

def_exposed_pinkgroup_4 <- c(
  1820, 1839, 1017, 1018, 1016, 1015, 1011, 1010, 1009, 1014,
  1837, 1833, 1834, 1008, 1012
)

possibly_exposed_pinkgroup_4_all_maybe <- c(
  1007, 1816, 1850, 1852, 1854, 1020, 1021, 1022, 1023,
  1025, 1026, 1803, 987, 988, 1807, 1005, 999, 998, 993,
  991, 992, 990, 2689, 989, 1813
)

not_exposed_pinkgroup_4 <- pops_pinkgroup_4$site_num[which(
  pops_pinkgroup_4$site_num %notin%
    c(def_exposed_pinkgroup_4, possibly_exposed_pinkgroup_4_all_maybe)
)]

possibly_exposed_pinkgroup_4_north_maybe <- c(
  1025, 1026, 1023, 1022, 1021, 1020, 1850, 1852, 1854,
  1843, 1007
)
possibly_exposed_pinkgroup_4_south_maybe <-
  c(
    possibly_exposed_pinkgroup_4_all_maybe[which(
      possibly_exposed_pinkgroup_4_all_maybe %notin%
        possibly_exposed_pinkgroup_4_north_maybe
    )],
    c(1007)
  )

pinkgroup_4_sites_cat <- c(
  def_exposed_pinkgroup_4, not_exposed_pinkgroup_4,
  possibly_exposed_pinkgroup_4_all_maybe
)

pops_missing_pinkgroup_4 <- pops_pinkgroup_4 %>%
  dplyr::filter(site_num %notin% pinkgroup_4_sites_cat)
pops_missing_pinkgroup_4 <- pops_missing_pinkgroup_4$site_num

if (any(duplicated(pinkgroup_4_sites_cat))) {
  print(pinkgroup_4_sites_cat[which(duplicated(pinkgroup_4_sites_cat))])
  stop("ERROR - Duplicates in the the pinkgroup_4 sites")
}
if (any(pinkgroup_4_sites_cat %notin% pops_pinkgroup_4$site_num)) {
  print(pinkgroup_4_sites_cat[which(pinkgroup_4_sites_cat %notin%
    pops_pinkgroup_4$site_num)])
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't
       exist in the list of sites")
}

# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_pinkgroup_4,
  yr = 2020,
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)
plot_given_sites(c(possibly_exposed_pinkgroup_4_all_maybe),
  yr = 2020,
  site_df = pink_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

all(pinkgroup_4_sites_cat %in% pops_pinkgroup_4$site_num)
all(pops_pinkgroup_4$site_num %in% pinkgroup_4_sites_cat)

exposed_df_4_large <- pink_site_name_combos %>%
  dplyr::filter(out_mig_year %in% c(2020))

exposed_df_pinkgroup_4 <- data.frame(
  site_num = pops_pinkgroup_4$site_num
) %>%
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_pinkgroup_4 ~ "yes",
      site_num %in% possibly_exposed_pinkgroup_4_all_maybe ~ "maybe",
      site_num %in% not_exposed_pinkgroup_4 ~ "no"
    ),
    maybes = dplyr::case_when(
      site_num %in% possibly_exposed_pinkgroup_4_north_maybe ~ "north",
      site_num %in% possibly_exposed_pinkgroup_4_south_maybe ~ "south",
      TRUE ~ NA
    )
  )

exposed_df_4_large <- exposed_df_4_large %>%
  dplyr::select(site_num, out_mig_year) %>%
  dplyr::left_join(
    x = .,
    y = exposed_df_pinkgroup_4,
    by = "site_num"
  ) %>%
  dplyr::rename(
    sites = site_num,
    year = out_mig_year
  )

## note and save the exposure dataframe ========================================
exposure_df <- rbind(
  exposed_df_pinkgroup_1, exposed_df_2_large, exposed_df_3_large,
  exposed_df_4_large
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
  1014, 1009, 1010, 1011, 1012, 1013, 1015, 1836, 1825, 1017, 1832, 1829, 1820, 1824,
  1840, 1839, 1018, 1019, 1016, 1837, 1835
)
possibly_exposed_chumgroup_1_all_maybe <- c(
  1020, 1021, 1023, 1025, 1833, 1834, 1008, 1007, 1001, 999, 1817,
  998, 2689, 2690, 1813, 2691, 1807, 1809, 2687, 2683, 1800, 1801, 987, 989, 993, 992, 991, 988
)

not_exposed_chumgroup_1 <- pops_chumgroup_1$site_num[which(
  pops_chumgroup_1$site_num %notin%
    c(def_exposed_chumgroup_1, possibly_exposed_chumgroup_1_all_maybe)
)]

possibly_exposed_chumgroup_1_north_maybe <- c(
  1025, 1023, 1020, 1021, 1008, 1834, 1833, 1007
)
possibly_exposed_chumgroup_1_south_maybe <-
  c(
    possibly_exposed_chumgroup_1_all_maybe[which(
      possibly_exposed_chumgroup_1_all_maybe %notin%
        possibly_exposed_chumgroup_1_north_maybe
    )],
    c(1008, 1007, 1834, 1833)
  )

chumgroup_1_sites_cat <- c(
  def_exposed_chumgroup_1, not_exposed_chumgroup_1,
  possibly_exposed_chumgroup_1_all_maybe
)

if (any(duplicated(chumgroup_1_sites_cat))) {
  print(chumgroup_1_sites_cat[which(duplicated(chumgroup_1_sites_cat))])
  stop("ERROR - Duplicates in the the chumgroup_1_sites_cat sites")
}

pops_missing_chumgroup_1 <- pops_chumgroup_1 %>%
  dplyr::filter(site_num %notin% chumgroup_1_sites_cat)
pops_missing_chumgroup_1 <- pops_missing_chumgroup_1$site_num

if (any(duplicated(pops_missing_chumgroup_1))) {
  (pops_missing_2005[which(duplicated(pops_missing_chumgroup_1))])
  stop("ERROR - Duplicates in the the chumgroup_1 sites")
}
if (any(chumgroup_1_sites_cat %notin% pops_chumgroup_1$site_num)) {
  print(chumgroup_1_sites_cat[which(chumgroup_1_sites_cat %notin%
    pops_chumgroup_1$site_num)])
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

plot_given_sites(
  site_nums_missing = possibly_exposed_chumgroup_1_all_maybe,
  yr = 2005,
  site_df = chum_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

exposed_df_chumgroup_1 <- data.frame(
  sites = pops_chumgroup_1$site_num,
  year = rep(2005, length(pops_chumgroup_1$site_num))
) %>%
  dplyr::mutate(
    exposure = dplyr::case_when(
      sites %in% def_exposed_chumgroup_1 ~ "yes",
      sites %in% possibly_exposed_chumgroup_1_all_maybe ~ "maybe",
      sites %in% not_exposed_chumgroup_1 ~ "no"
    ),
    maybes = dplyr::case_when(
      sites %in% possibly_exposed_chumgroup_1_north_maybe ~ "north",
      sites %in% possibly_exposed_chumgroup_1_south_maybe ~ "south",
      TRUE ~ NA
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
  1009, 1010, 1012, 1014, 1015, 1013, 1834, 1007, 1839, 1018, 1829, 1820, 1840,
  1836, 1833, 1008, 1016, 1017, 1019, 2030, 1832, 1825, 1837, 1011, 7990591, 1816, 1005,
  1006
)

possibly_exposed_chumgroup_2_all_maybe <- c(
  1025, 1021, 1838, 1020, 1022, 1023, 1024, 1026, 1812,
  1817, 991, 993, 998, 999, 2689, 2690, 989, 1807, 2691, 996, 992, 990, 988, 987, 1803,
  2683, 2687, 1001, 1813, 1800, 1801,
  1809, 1804, 2708
)

not_exposed_chumgroup_2 <- pops_chumgroup_2$site_num[which(
  pops_chumgroup_2$site_num %notin%
    c(def_exposed_chumgroup_2, possibly_exposed_chumgroup_2_all_maybe)
)]

possibly_exposed_chumgroup_2_north_maybe <- c(
  1026, 1025, 1024, 1023, 1022, 1838, 1021, 1020
)
possibly_exposed_chumgroup_2_south_maybe <-
  possibly_exposed_chumgroup_2_all_maybe[which(
    possibly_exposed_chumgroup_2_all_maybe %notin%
      possibly_exposed_chumgroup_2_north_maybe
  )]


plot_given_sites(
  site_nums_missing = c(not_exposed_chumgroup_2),
  yr = c(2006:2016),
  site_df = chum_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)
# plot_given_sites(
#   site_nums_missing = possibly_exposed_chumgroup_2,
#   yr = c(2006:2016),
#   site_df = chum_site_name_combos,
#   large_land = readRDS(tar_read(large_land))
# )

chumgroup_2_sites_cat <- c(
  def_exposed_chumgroup_2, not_exposed_chumgroup_2,
  possibly_exposed_chumgroup_2_all_maybe
)

pops_missing_chumgroup_2 <- pops_chumgroup_2 %>%
  dplyr::filter(site_num %notin% chumgroup_2_sites_cat)
pops_missing_chumgroup_2 <- pops_missing_chumgroup_2$site_num

plot_given_sites(
  site_nums_missing = pops_missing_chumgroup_2,
  yr = c(2006:2016),
  site_df = chum_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)


if (any(duplicated(chumgroup_2_sites_cat))) {
  print(chumgroup_2_sites_cat[which(duplicated(chumgroup_2_sites_cat))])
  stop("ERROR - Duplicates in the the chumgroup_2 sites")
}
if (any(chumgroup_2_sites_cat %notin% pops_chumgroup_2$site_num)) {
  print(chumgroup_2_sites_cat[which(chumgroup_2_sites_cat %notin%
    pops_chumgroup_2$site_num)])
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't
       exist in the list of sites")
}
# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_chumgroup_2,
  yr = c(2006:2016),
  site_df = chum_site_name_combos
)

exposed_df_2_large <- chum_site_name_combos %>%
  dplyr::filter(out_mig_year %in% c(2006:2016))

exposed_df_chumgroup_2 <- data.frame(
  site_num = pops_chumgroup_2$site_num
) %>%
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_chumgroup_2 ~ "yes",
      site_num %in% possibly_exposed_chumgroup_2_all_maybe ~ "maybe",
      site_num %in% not_exposed_chumgroup_2 ~ "no"
    ),
    maybes = dplyr::case_when(
      site_num %in% possibly_exposed_chumgroup_2_north_maybe ~ "north",
      site_num %in% possibly_exposed_chumgroup_2_south_maybe ~ "south",
      TRUE ~ NA
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
  1015, 1014, 1009, 1011, 1010, 1016, 1017, 1018, 1019, 1839, 1840, 2030, 1820, 1833, 1834
)

possibly_exposed_chumgroup_3_all_maybes <- c(
  7990591, 1850, 1852, 1853, 1855, 1854, 1843, 1020, 1021, 1022, 1023, 1024, 1025, 1026,
  989, 1807, 1813, 990, 991, 992, 993, 998, 999,
  1007, 1005, 1809, 987
)

not_exposed_chumgroup_3 <- pops_chumgroup_3$site_num[which(
  pops_chumgroup_3$site_num %notin%
    c(def_exposed_chumgroup_3, possibly_exposed_chumgroup_3_all_maybes)
)]

possibly_exposed_chumgroup_3_north_maybes <- c(
  1026, 1025, 1023, 1022, 1850, 1852, 1855, 1854, 1843, 1021, 1020,
  7990591, 1007
)
possibly_exposed_chumgroup_3_south_maybes <-
  c(
    possibly_exposed_chumgroup_3_all_maybes[which(
      possibly_exposed_chumgroup_3_all_maybes %notin%
        possibly_exposed_chumgroup_3_north_maybes
    )],
    c(1007, 7990591)
  )

plot_given_sites(
  site_nums_missing = c(not_exposed_chumgroup_3),
  yr = c(2017:2020),
  site_df = chum_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)
# plot_given_sites(
#   site_nums_missing = possibly_exposed_chumgroup_3,
#   yr = c(2017:2020),
#   site_df = chum_site_name_combos,
#   large_land = readRDS(tar_read(large_land))
# )

chumgroup_3_sites_cat <- c(
  def_exposed_chumgroup_3, not_exposed_chumgroup_3,
  possibly_exposed_chumgroup_3_all_maybes
)

pops_missing_chumgroup_3 <- pops_chumgroup_3 %>%
  dplyr::filter(site_num %notin% chumgroup_3_sites_cat)
pops_missing_chumgroup_3 <- pops_missing_chumgroup_3$site_num


if (any(duplicated(chumgroup_3_sites_cat))) {
  print(chumgroup_3_sites_cat[which(duplicated(chumgroup_3_sites_cat))])
  stop("ERROR - Duplicates in the the chumgroup_3 sites")
}
if (any(chumgroup_3_sites_cat %notin% pops_chumgroup_3$site_num)) {
  print(chumgroup_3_sites_cat[which(chumgroup_3_sites_cat %notin%
    pops_chumgroup_3$site_num)])
  stop("ERROR - Typo somewhere in one of the assigned sites - it doesn't
       exist in the list of sites")
}

# look for the ones I've somehow missed so far
plot_given_sites(pops_missing_chumgroup_3,
  yr = c(2017:2020),
  site_df = chum_site_name_combos,
  large_land = readRDS(tar_read(large_land))
)

all(chumgroup_3_sites_cat %in% pops_chumgroup_3$site_num)
all(pops_chumgroup_3$site_num %in% chumgroup_3_sites_cat)

exposed_df_3_large <- chum_site_name_combos %>%
  dplyr::filter(out_mig_year %in% c(2017:2020))

exposed_df_chumgroup_3 <- data.frame(
  site_num = pops_chumgroup_3$site_num
) %>%
  dplyr::mutate(
    exposure = dplyr::case_when(
      site_num %in% def_exposed_chumgroup_3 ~ "yes",
      site_num %in% possibly_exposed_chumgroup_3_all_maybes ~ "maybe",
      site_num %in% not_exposed_chumgroup_3 ~ "no"
    ),
    maybes = dplyr::case_when(
      site_num %in% possibly_exposed_chumgroup_3_north_maybes ~ "north",
      site_num %in% possibly_exposed_chumgroup_3_south_maybes ~ "south",
      TRUE ~ NA
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
