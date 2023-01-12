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
psf_df <- readr::read_csv(here::here("./data/spawner-recruit/psf-2022-12-13.csv"))
head(psf_df)
unique(psf_df$species)


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
