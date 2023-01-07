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





