##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2022-10-14
#'
#' This  file contains all manual testing for data cleaning and organization
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

library(readr)
library(here)
library(magrittr)
library(dplyr)
library(ggplot2)
library(patchwork)

# wild lice ==================================================================== 
wild_lice <- readr::read_csv(
  here::here("./data/wild-lice/raw/klemtu_wild_lice_data_CB.csv"),
  warnings=FALSE)

wild_lice <- wild_lice %>% 
  mutate(
    `Length (mm)` = ifelse(
      `Length (mm)` == "too decomposed", 
      NA,
      ifelse(
        `Length (mm)` == "1.9.", 
        1.9,
        `Length (mm)`
      )
    )
  )

# SR data ======================================================================
psf_df <- readr::read_csv(
  here::here(
    "./data/spawner-recruit/clean/NCC_streams_river-level_SR_data_2023-04-19.csv"))
head(psf_df)
unique(psf_df$Species)


pink_sr <- psf_df %>% 
  dplyr::filter(species %in% c("Pink (even)", "Pink (odd)"),
                parameter %in% c("lnRS", "Recruits", "Spawners")) %>%
  tidyr::pivot_wider(
    names_from = "parameter",
    values_from = "datavalue"
  )
unique(pink_sr$location)

ggplot(data = pink_sr) + 
  geom_point(aes(x = year, y = "Spawners"))

# the stream level data

stream_sr <- read_csv(here("./data/spawner-recruit/raw/river-level-sr/NCC_streams_SR_data.csv")) %>% 
  dplyr::rename(
    gfe_id = GFE_ID,
    brood_year = BroodYear,
    river = River,
    species = Species,
    indicator = Indicator,
    long = xLONG,
    lat = yLAT,
    area = StatArea,
    con_unit = CU,
    con_unit_2 = CU_2,
    spawners = Spawners,
    returns = Returns,
    recruits = Recruits
  )

all_pinks <- stream_sr %>% 
  dplyr::filter(species %in% c("PKE", "PKO")) %>% 
  # get rid of NA's
  dplyr::filter_at(
    vars(spawners, returns), all_vars(!is.na(.))
  )

pinks_even <- all_pinks %>% 
  dplyr::filter(species %in% c("PKE"))
pinks_odd <- all_pinks %>% 
  dplyr::filter(species %in% c("PKO")) 

all_pinks_obs_per_stream <- all_pinks %>% 
  dplyr::mutate(river = as.factor(river)) %>% 
  dplyr::group_by(river) %>% 
  dplyr::summarize(n = n()) 

pke_obs_per_stream <- pinks_even %>% 
  dplyr::mutate(river = as.factor(river)) %>% 
  dplyr::group_by(river) %>% 
  dplyr::summarize(n = n())

pko_obs_per_stream <- pinks_odd %>% 
  dplyr::mutate(river = as.factor(river)) %>% 
  dplyr::group_by(river) %>% 
  dplyr::summarize(n = n())

min_obs <- min(summary(pke_obs_per_stream$n)[[2]], summary(pko_obs_per_stream$n)[[2]])

pke_streams_to_exclude <- pke_obs_per_stream %>% 
  dplyr::filter(
    n < min_obs
  ) 
pko_streams_to_exclude <- pko_obs_per_stream %>% 
  dplyr::filter(
    n < min_obs
  ) 


streams_to_keep <- all_pinks_obs_per_stream %>% 
  dplyr::filter(
    river %notin% c(pke_streams_to_exclude$river, pko_streams_to_exclude$river)
  )

all_pinks_rivers <- all_pinks %>% 
  dplyr::filter(
    river %in% streams_to_keep$river
  ) %>% 
  dplyr::arrange(
    brood_year
  )  

# plot of pinks per area per year ==============================================
pink_df <- readr::read_csv(
  here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))

# group by area/year
pink_df_obs <- pink_df %>% 
  dplyr::mutate(
    area = ifelse(
      area %in% c("02E", "02W"), 2, area
    )
  ) %>% 
  dplyr::filter(
    brood_year >= 2005
  ) %>% 
  dplyr::mutate(brood_year = as.integer(brood_year)) %>% 
  dplyr::group_by(area, brood_year) %>% 
  dplyr::summarize(n = n())

pink_df_obs_all <- pink_df_obs %>% 
  dplyr::group_by(brood_year) %>% 
  dplyr::summarize(n = sum(n)) %>% 
  dplyr::mutate(area = "all") %>% 
  dplyr::select(area, brood_year, n)

pink_df_plot <- rbind(pink_df_obs, pink_df_obs_all)

pink_df_all_only_safe <- pink_df_plot %>% 
  dplyr::filter(area %in% c("6", "7", "8", "9", "10")) %>% 
  dplyr::group_by(brood_year) %>% 
  dplyr::summarize(n = sum(n)) %>% 
  dplyr::mutate(area = "all") %>% 
  dplyr::select(area, brood_year, n)

pink_df_plot_only_safe <- rbind(
  pink_df_plot %>% 
    dplyr::filter(area %in% c("6", "7", "8", "9", "10")),
  pink_df_all_only_safe
) 

p_all_areas <- ggplot(data = pink_df_obs) + 
  geom_point(aes(x = brood_year, y = n, fill = area), shape = 21) + 
  geom_line(aes(x = brood_year, y = n, colour = area, linetype = area),
            alpha = 0.45) +
  scale_x_continuous(breaks = c(2005:2015)) +
  ggthemes::theme_base() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) + 
  labs(x = "Brood Year", y = "Number of Observations per year", 
       title = "All areas")

p_all_all_areas <- ggplot(data = pink_df_plot) + 
  geom_point(aes(x = brood_year, y = n, fill = area), shape = 21) + 
  geom_line(aes(x = brood_year, y = n, colour = area, linetype = area),
            alpha = 0.45) +
  scale_x_continuous(breaks = c(2005:2015)) +
  ggthemes::theme_base() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) + 
  labs(x = "Brood Year", y = "Number of Observations per year", 
       title = "All areas")
p_all_areas + p_all_all_areas


p_only_6_10 <- ggplot(data = pink_df_obs %>% 
         dplyr::filter(area %in% c(6:10))) + 
  geom_point(aes(x = brood_year, y = n, fill = area), shape = 21) + 
  geom_line(aes(x = brood_year, y = n, colour = area, linetype = area),
            alpha = 0.45) +
  scale_x_continuous(breaks = c(2005:2015)) +
  ggthemes::theme_base() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) + 
  labs(x = "Brood Year", y = "Number of Observations per year", 
       title = "Areas 6-10")

p_all_only_6_10 <- ggplot(data = pink_df_plot_only_safe) + 
  geom_point(aes(x = brood_year, y = n, fill = area), shape = 21) + 
  geom_line(aes(x = brood_year, y = n, colour = area, linetype = area),
            alpha = 0.45) +
  scale_x_continuous(breaks = c(2005:2015)) +
  ggthemes::theme_base() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) + 
  labs(x = "Brood Year", y = "Number of Observations per year", 
       title = "Areas 6-10")

p_only_6_10 + p_all_only_6_10


(p_all_areas + p_all_all_areas) / (p_only_6_10 + p_all_only_6_10)

pink_compare <- cbind(pink_df_plot %>% dplyr::filter(area == "all") %>% 
                        dplyr::mutate(n_all = n), 
                      pink_df_plot_only_safe %>% 
                        dplyr::ungroup() %>% 
                        dplyr::filter(area == "all") %>% 
                        dplyr::mutate(n_safe = n) %>% 
                        dplyr::select(n_safe)) %>% 
  dplyr::mutate(
    diff = n_all - n_safe
  )
mean(pink_compare$diff)

# wild lice data ===============================================================
wild_lice <- readr::read_csv(here("./data/wild-lice/raw/klemtu_wild_lice_data_CB.csv"))
readr::problems()

# ignore the problems since they don't really apply to the columns we're 
# interested in

wild_lice_clean <- wild_lice %>% 
  dplyr::mutate(seine_date = as.factor(seine_date)) %>% 
  dplyr::select(
    year, seine_date, site, zone, fish_spp, lep_total, cal_total, lat, long
  )

# figure out the seine_date situation by year 
wild_lice_dates <- wild_lice_clean %>% 
  dplyr::select(year, seine_date) %>% 
  unique()

readr::write_csv(wild_lice_dates, here("./data/wild-lice/raw/unique_dates_for_manual_edit.csv"))

# MANUAL PART HAPPENS HERE

dates_to_join <- readr::read_csv(here("./data/wild-lice/raw/unique_dates_manually_edited.csv")) %>% 
  dplyr::mutate(seine_date = as.factor(seine_date)) %>% 
  dplyr::filter(
    seine_date %notin% c("no date", "NO INFO", "no paper", "none", "Unknown",
                         NA, "?")
  )

wild_lice_to_join <- wild_lice_clean %>% 
  dplyr::select(-year) %>% 
  dplyr::filter(
    seine_date %notin% c("no date", "NO INFO", "no paper", "none", "Unknown",
                         NA, "?")
  )

wild_lice_clean_dates_fixed <- dplyr::left_join(
  wild_lice_to_join,
  dates_to_join,
  by = "seine_date"
)


obs_per_year <- wild_lice_clean_dates_fixed %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(n = dplyr::n())

library(ggplot2)
ggplot(data = obs_per_year) + 
  geom_col(aes(x = year, y = n, fill = n),
           colour = "black") + 
  scale_x_discrete() +
  scale_fill_gradientn("No. of Obs", 
                       colours = wesanderson::wes_palette("Zissou1", 100, 
                                                          type = "continuous"))+
  ggthemes::theme_base() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
  ) + 
  labs(x = "Year", y = "Number of Individual Fish Observed")


# clean lice data 
wild_lice <- readr::read_csv(here("./data/wild-lice/clean/wild_lice_df.csv"))




wild_lice_grouped <- wild_lice %>% 
  dplyr::filter(!is.na(year)) %>% # get rid of the unknown year lice 
  dplyr::mutate(
    year_fac = as.factor(year)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    all_lice = sum(lep_total + cal_total, na.rm = TRUE),
    all_motile_leps = sum(
      lep_pam, lep_paf, lep_am, lep_af, na.rm = TRUE)
  ) %>% 
dplyr::group_by(year_fac) %>% 
  dplyr::summarize(
    mean__lep_total = mean(lep_total, na.rm = TRUE),
    mean__all_lice = mean(all_lice, na.rm = TRUE),
    mean__lep_mots = mean(all_motile_leps, na.rm = TRUE),
    se__all_leps = std_err(lep_total), 
    se__all_lice = std_err(all_lice),
    se__lep_mots = std_err(all_motile_leps)
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    up_ci__all_leps = mean__all_leps + (1.96 * se__all_leps),
    up_ci__all_lice = mean__all_lice + (1.96 * se__all_lice),
    up_ci__lep_mots = mean__lep_mots + (1.96 * se__lep_mots),
    lo_ci__all_leps = mean__all_leps - (1.96 * se__all_leps),
    lo_ci__all_lice = mean__all_lice - (1.96 * se__all_lice),
    lo_ci__lep_mots = mean__lep_mots - (1.96 * se__lep_mots)
  ) %>%
  tidyr::pivot_longer(
    cols = !year_fac,
    names_to = c("measure", "grouping"),
    names_sep = "__",
    values_to = "value"
  )

# farm lice ====================================================================
library(lubridate)
library(dplyr)
library(janitor)
library(tibble)
library(readxl)
library(here)
library(readr)

old_lice <- readxl::read_excel(
  here("./data/farm-lice/raw/klemtu_farm_lice_data_old.xls"),
                               sheet = 9)
new_lice <- readxl::read_excel(
  here("./data/farm-lice/raw/klemtu_farm_lice_data_new.xlsx"), 
                               sheet = 5)


# remove obs with NAs in the date column 
old_lice <- old_lice %>% 
  dplyr::filter(!is.na(DATE))

# since it's an excel sheet it read it in oddly, so now we need to transfer the 
# data back to a readable structure 
old_lice$DATE <- janitor::excel_numeric_to_date(
  as.numeric(
    as.character(old_lice$DATE)), 
  date_system = "modern") 

# get rid of NA's and add in the appropriate columns 
old_lice <- old_lice %>% 
  dplyr::filter(
    !is.na(DATE)
  ) %>% 
  dplyr::mutate(
    day = lubridate::day(DATE),
    month = lubridate::month(DATE),
    year = lubridate::year(DATE)
  )

# standardize names
old_lice <- standardize_names(old_lice) %>% 
  # needs some custom renames too
  dplyr::rename(
    adult_fem_no_egg = `adult_female_w/o_eggs`,
    adult_fem_w_egg = `adult_female____with_eggs`,
    num_fish_sampled = `#_of_fish_sampled`
  )

new_lice <- new_lice %>% 
  dplyr::mutate(
    day = lubridate::day(DATE),
    month = lubridate::month(DATE),
    year = lubridate::year(DATE)
  ) %>% 
  standardize_names(.) %>% 
  # needs some custom renames too
  dplyr::rename(
    adult_fem_no_egg = `adult_female_w/o_eggs`,
    adult_fem_w_egg = `adult_female____with_eggs`,
    num_fish_sampled = `#_of_fish_sampled`
  )

all_farms <- unique(c(unique(new_lice$farm), unique(old_lice$farm)))

farm_locs <- readr::read_csv(
  here("./data/farm-lice/raw/farm_location_metadata.csv")
)

farm_locs <- farm_locs %>% 
  dplyr::filter(
    site %in% c("Lochalsh", "Jackson Pass", "Kid Bay", "Alexander Inlet",
                "Sheep Passage", "Goat Cove", "Lime Point")
  ) %>% 
  dplyr::select(site, latitude, longitude, area) 

# manually put in cougar bay
cougar_add <- data.frame(
  site = "Cougar Bay",
  latitude = 52.743511,
  longitude = -128.587263,
  area = 7
)

all_farm_locs <- rbind(farm_locs, cougar_add) %>% 
  dplyr::rename(
    lat = latitude,
    long = longitude
  ) %>% 
  dplyr::mutate(type = "farm")

# get the sampling area info 
kx_sampling <- read_csv(here("./data/wild-lice/raw/kitasoo_sampling_sites.csv"))

kx_sampling <- kx_sampling %>% 
  dplyr::rename(
    site = name,
    long = lon
  ) %>% 
  dplyr::select(site, lat, long) %>% 
  dplyr::mutate(
    area = 7,
    type = "sampling"
  ) 

all_locations <- rbind(all_farm_locs, kx_sampling)

length(unique(old_lice %>% select(farm, month, year)))

# there's one specific problem, a space in the character vector of the 
# inventories, so I'll change that first
old_lice[which(old_lice$site_inventory == 
                 "846 012"), "site_inventory"] <- "846012"

old_lice_trim <- old_lice %>% 
  dplyr::select(year, month, farm, adult_fem_no_egg, 
                adult_fem_w_egg, site_inventory) %>% 
  dplyr::mutate(
    year = as.factor(year), 
    month = as.factor(month),
    farm = as.factor(farm),
    inventory = as.numeric(as.integer(site_inventory))
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    lice = sum(adult_fem_no_egg, 
               adult_fem_w_egg, na.rm = TRUE)
  ) %>% 
  dplyr::group_by(year, month, farm) %>% 
  dplyr::summarize(
    mean_inventory = mean(inventory, na.rm = TRUE), 
    lice = mean(lice, na.rm = TRUE)
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    total_lice = lice * mean_inventory
  ) %>% 
  dplyr::mutate(
    day = 01,
    year = as.integer(as.character(year)),
    month = as.integer(as.character(month))
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    date = lubridate::make_date(year = as.integer(year), 
                                month = as.integer(month),
                                day = day)
  ) %>% 
  dplyr::arrange(date)

new_lice_trim <- new_lice %>% 
  dplyr::select(year, month, farm, adult_fem_no_egg, 
                adult_fem_w_egg, site_inventory) %>% 
  dplyr::mutate(
    year = as.factor(year), 
    month = as.factor(month),
    farm = as.factor(farm),
    inventory = as.numeric(as.integer(site_inventory))
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    lice = sum(adult_fem_no_egg, 
               adult_fem_w_egg, na.rm = TRUE)
  )   %>% 
  dplyr::group_by(year, month, farm) %>% 
  dplyr::summarize(
    mean_inventory = mean(inventory, na.rm = TRUE), 
    lice = mean(lice, na.rm = TRUE)
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    total_lice = lice * mean_inventory
  ) %>% 
  dplyr::mutate(
    day = 01,
    year = as.integer(as.character(year)),
    month = as.integer(as.character(month))
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    date = lubridate::make_date(year = as.integer(year), 
                                month = as.integer(month),
                                day = day)
  ) %>% 
  dplyr::arrange(date)

all_lice <- rbind(old_lice_trim, new_lice_trim) 

all_lice_by_date <- all_lice %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(
    Inventory = mean(mean_inventory, na.rm = TRUE),
    Lice = mean(total_lice, na.rm = TRUE)
  ) %>% 
  dplyr::filter(!is.na(Inventory))

all_lice_both_measures <- all_lice_by_date %>% 
  tidyr::pivot_longer(
    cols = !date, 
    values_to = "vals"
  ) %>% 
  dplyr::mutate(
    Measurement = as.factor(name)
  )
  


ggplot(data = all_lice_both_measures) + 
  geom_line(aes(x = date, y = vals, colour = Measurement, 
                linetype = Measurement)) +
  geom_point(aes(x = date, y = vals, fill = Measurement), colour = "black",
             shape = 21) +
  ggthemes::theme_base() + 
  scale_fill_manual(values = c("goldenrod1", "purple2")) + 
  scale_colour_manual(values = c("goldenrod1", "purple2")) + 
  scale_x_date(date_breaks = "1 years") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) + 
  labs(x = "Date",  y = "Values")
  

# prepped sr data ==============================================================
pink_sr_data <- read_csv(here("./data/spawner-recruit/clean/pink-sr-data-clean.csv"))

wild_lice_years <- wild_lice %>%
  dplyr::mutate(
    year = as.factor(year)
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(wild_lice = mean(lep_motiles, na.rm = TRUE)) %>% 
  dplyr::mutate(
    area = "7" 
  ) %>% 
  dplyr::mutate(
    brood_year = as.numeric(as.character(year))
  ) %>% 
  dplyr::select(-year)

pink_sr_data_lice <- dplyr::left_join(pink_sr_data, 
                 wild_lice_years,
                 by = c("brood_year", "area")) %>% 
  dplyr::rename(lice = wild_lice) %>% 
  dplyr::mutate(
    lice = ifelse(!is.na(lice), lice, 0)
  )

summary(pink_sr_data$area)
unique(pink_sr_data[which(is.na(pink_sr_data$area)), "brood_year"])
max_year <- max(pink_sr_data$brood_year)



# loop through every year/

