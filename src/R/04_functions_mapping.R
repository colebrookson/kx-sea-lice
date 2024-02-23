##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-01-18
#'
#' This file contains the functions that map the locations and sites we're
#' interested in for this project
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

# clean_farm_locations =========================================================
clean_farm_locations <- function(farm_locations, output_path) {
  #' Take data on the farm locations and clean and prepare them to be used
  #'
  #' @description Since the data on the farm locations isn't perfect (there's
  #' one farm missing), do that cleaning manually
  #'
  #' @param farm_locations file. Information on all the farms in the region
  #' @param output_path character. Location to write out the locations of
  #' the farms
  #'
  #' @usage clean_farm_locations(farm_locations)
  #' @return clean data frame
  #'

  # filter to just the farms we're concerned with in this analysis
  farm_locs <- farm_locations %>%
    dplyr::filter(
      site %in% c(
        "Lochalsh", "Jackson Pass", "Kid Bay", "Alexander Inlet",
        "Sheep Passage", "Goat Cove", "Lime Point"
      )
    ) %>%
    dplyr::select(site, latitude, longitude, area)

  # manually put in cougar bay since it's not in the dataset
  cougar_add <- data.frame(
    site = "Cougar Bay",
    latitude = 52.743511,
    longitude = -128.587263,
    area = 7
  )

  # rename for ease
  all_farm_locs <- rbind(farm_locs, cougar_add) %>%
    dplyr::rename(
      lat = latitude,
      long = longitude
    ) %>%
    dplyr::mutate(type = "farm")

  readr::write_csv(all_farm_locs, paste0(output_path, "clean-farm-locs.csv"))

  return(all_farm_locs)
}

# clean_kitasoo_sampling =======================================================
clean_kitasoo_sampling <- function(kx_sampling, all_farm_locs) {
  #' Bring in the data on where the sampling happended and clean it
  #'
  #' @description KXSA sampling data needs to be cleaned and renamed so it can
  #' get stitched to the farm location data. Also join to the farm data
  #'
  #' @param kx_sampling file. Information on all the sampling locations in the
  #' sampling region
  #' @param all_farm_locs. dataframe. Information on all the farm locations from
  #' being cleaned earlier.
  #'
  #' @usage clean_kitasoo_sampling(kx_sampling)
  #' @return clean data frame
  #'

  # clean data
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

  # put the different location data together
  all_locations <- rbind(all_farm_locs, kx_sampling) %>%
    dplyr::mutate(
      type = as.factor(type)
    ) %>%
    dplyr::mutate(
      # use this so in the plot I can make the labels different fonts
      ff = ifelse(type == "farm", "bold", "plain")
    )

  return(all_locations)
}

# make_sampling_map ============================================================
make_sampling_map <- function(farm_locations, kx_sampling, geo_data,
                              output_path, farm_path) {
  #' Take the all_locations data and make the plot with the geospatial data
  #'
  #' @description The geospatial data has been pre-downloaded, so take that
  #' data along with the cleaned location data and
  #'
  #' @param kx_sampling file. Information on all the sampling locations in the
  #' sampling region
  #' @param farm_locations file. Information on all the farms in the region
  #' @param geo_data file. The geo-spatial data rds file
  #' @param output_path character. Where to save the plot
  #' @param farm_path character. Where to save the data of the farm locations
  #'
  #' @usage clean_kitasoo_sampling(kx_sampling)
  #' @return clean data frame
  #'

  # make the data that we need
  all_locations <- clean_kitasoo_sampling(
    # this data can be just used
    kx_sampling,
    # use the cleaning function for this one
    clean_farm_locations(farm_locations, farm_path)
  )

  # geospatial stuff
  province <- "British Columbia"
  canada_prov <- geo_data[geo_data$NAME_1 %in% province] # subset to just BC

  ggplot2::ggsave(

    # output part
    paste0(output_path, "study-region-map.png"),

    # make and save the actual plot
    ggplot2::ggplot() +
      geom_polygon(
        data = canada_prov,
        aes(x = long, y = lat, group = group),
        colour = "black",
        linewidth = 0.01,
        fill = "grey65"
      ) +
      coord_cartesian(xlim = c(-128.8, -128.1), ylim = c(52.2, 52.95)) +
      geom_point(
        data = all_locations,
        aes(x = long, y = lat, fill = type, shape = type),
        size = 4
      ) +
      ggthemes::theme_base() +
      labs(x = "Longitude (°)", y = "Latitude (°)") +
      scale_shape_manual("Location", values = c(21, 22)) +
      scale_fill_manual("Location", values = c("purple", "gold2")) +
      ggrepel::geom_text_repel(
        data = all_locations,
        aes(
          x = long, y = lat,
          label = site, fontface = ff
        ),
        size = 3,
        max.overlaps = 20
      ),

    # make the size
    height = 8, width = 9
  )
}

# define manual helper function ================================================
slice_fun <- function(net, temp_edges) {
  #' Find path length of one path
  #'
  #' @description Taking a single path (i.e. set of edges), get the length
  #' of that path, and remove the units on it
  #' @param net sfnetwork. A network of sfnetwork
  #' @param temp_edges vector. The edges of the path at hand
  #'
  #' @return numeric value (units removed) of the length of that path

  return(units::drop_units(net %>%
    activate("edges") %>%
    slice(temp_edges) %>%
    st_as_sf() %>%
    st_combine() %>%
    st_length()))
}

list_reassign <- function(l, nodes_edges) {
  #' Just keep nodes or edges
  #'
  #' @description Small simple function to take in a list of lists, and only
  #' keep the list of name "edges" or "nodes
  #' @param l list. A list of length 2, with each sub-item being a list
  #' @param nodes_edges character. Which of the two we want to keep
  #'
  #' @return list of length 1

  return(l[nodes_edges])
}

# make_map_each_farm ===========================================================
#' make_map_each_farm <- function(utm_geo_data, utm_land_data, farm_locs, network,
#'                                west_network, all_edges_nodes, fig_output) {
#'   #' Maps of each farm's buffer
#'   #'
#'   #' @description Get a sense of what each farm's buffer is and if it makes
#'   #' sense
#'   #'
#'   #' @param utm_geo_data file. The geo-spatial data rds file
#'   #' @param farm_locs dataframe. The cleaned data of the different
#'   #' locations of the farms
#'   #' @param network sfnetwork tibble. The network for the entire region
#'   #' @param west_network sfnetwork tibble. The network for the western region
#'   #' @param all_edges_nodes list. The list of all the lists of edges and nodes
#'   #' to be kept to map onto the region
#'   #' @param fig_output character. Where to save the figure files
#'   #'
#'   #' @usage make_map_each_farm(utm_geo_data, utm_land_data, network,
#'   #' west_network, all_edges_nodes, fig_output)
#'   #' @return NA
#'   #'
#'
#'   farms_sf <- sf::st_as_sf(farm_locs, coords = c("long", "lat"))
#'
#'   # set the coordinates for WGS84
#'   sf::st_crs(farms_sf) <- 4326
#'   # transform to utm
#'   farms_utm <- sf::st_transform(farms_sf,
#'                                 crs="+proj=utm +zone=9 +datum=NAD83 +unit=m")
#'
#'   for(farm in unique(farms_utm$site)) {
#'
#'     # get the nodes
#'     curr_edges_nodes <- all_edges_nodes[
#'       which(names(all_edges_nodes) %in% farm)]
#'     curr_nodes <- sapply(curr_edges_nodes, list_reassign,
#'                          nodes_edges="nodes") %>%
#'       unlist() %>% unname()
#'
#'     if(farm %in% c("Alexander Inlet", "Cougar Bay")) {
#'       net = west_network
#'     } else {
#'       net = network
#'     }
#'
#'     ggplot2::ggplot() +
#'       geom_sf(data = utm_geo_data, fill = "white") +
#'       geom_sf(data = network %>%
#'               activate("nodes") %>%
#'               slice(edges_nodes_to_keep_loch$nodes) %>%
#'               st_as_sf(), fill = "lightpink", colour = "lightpink") +
#'       #geom_sf(data = utm_land_data, fill = "grey70") +
#'       geom_sf(data = farms_utm[which(farms_utm$site == farm), ],
#'               shape = 21, fill = "purple1", colour = "black", size = 2.5) +
#'       theme_base() +
#'       labs(
#'         x = "Longitude", y = "Latitude", main = farm
#'       )
#'
#'   }
#'
#' }

# make_nonexposure_yearly_maps =================================================
make_nonexposure_yearly_maps <- function(sr_pop_data, sr_pop_sites, large_land,
                                         farm_data, farm_locs, network,
                                         all_edges_nodes, fig_output, species,
                                         data_output) {
  #' Population & farm co-occurence without exposure
  #'
  #' @description Maps of each year's population and farm co-occurrence, but
  #' without any of the exposure categorizations, these maps are helpful for
  #' making the classifications
  #'
  #' @param sr_pop_data dataframe. The cleaned pink SR data
  #' @param sr_pop_sites file. Information on all the populations locations
  #' @param large_land file. The geo-spatial data rds file
  #' @param farm_data dataframe. Data of the info for the different farms
  #' @param farm_locs dataframe. The cleaned data of the different
  #' locations of the farms
  #' @param network sfnetwork tibble. The network for the entire region
  #' @param exposure_df file. Dataframe on the exposure options
  #' @param all_edges_nodes list. The list of all the lists of edges and nodes
  #' to be kept to map onto the region
  #' @param species character. The species of salmon being plotted.
  #' @param fig_output character. Where to save the figure files
  #' @param data_output character. Where to save the data files
  #'
  #' @usage make_yearly_popn_maps(sr_pop_data, sr_pop_sites, geo_data,
  #' farm_data, clean_farm_locs, output_path)
  #' @return NA
  #'

  # filter the sites to just the ones in our data
  sr_pop_data_area67 <- sr_pop_data %>%
    dplyr::filter(area %in% c(6, 7))

  # clean the farm data names so they match up between the dataframe
  farm_data <- farm_data %>%
    dplyr::mutate(
      farm = dplyr::case_when(
        farm == "Goat" ~ "Goat Cove",
        farm == "Sheep Pass" ~ "Sheep Passage",
        farm == "Lime" ~ "Lime Point",
        farm == "Alexander" ~ "Alexander Inlet",
        TRUE ~ as.character(farm)
      )
    )

  # this is an empty dataframe to fill in
  site_data_by_year <- data.frame(
    site_name = as.character(),
    brood_year = as.numeric(),
    lat = as.numeric(),
    long = as.numeric(),
    site_num = as.numeric()
  )

  ## set up the farms that we'll need to plot but with sf functions ============
  farms_sf <- sf::st_as_sf(farm_locs, coords = c("long", "lat"))

  # set the coordinates for WGS84
  sf::st_crs(farms_sf) <- 4326
  # transform to utm
  farms_utm <- sf::st_transform(farms_sf,
    crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
  )

  for (yr in 2005:2020) {
    # get the farms in that time period
    farms_temp <- (farm_data %>%
      dplyr::filter(year == yr) %>%
      dplyr::filter(month %in% c(3, 4)) %>%
      dplyr::group_by(farm) %>%
      dplyr::summarize(
        inventory =
          mean(mean_inventory,
            na.rm = TRUE
          )
      ))$farm

    farm_locs_temp <- farm_locs %>%
      dplyr::filter(site %in% farms_temp) %>%
      dplyr::select(site, lat, long)

    sr_pop_sites_filter <- sr_pop_sites %>%
      dplyr::filter(system_site %in% unique(sr_pop_data_area67$river))

    # get the populations in that year
    sr_pop_temp <- sr_pop_data_area67 %>%
      # brood year of yr will pass fish farms in year + 1
      dplyr::filter(brood_year == (yr - 1))

    # subset to just the locations that were shown to be present in that year
    site_year_temp <- sr_pop_sites_filter %>%
      dplyr::filter(system_site %in% unique(sr_pop_temp$river)) %>%
      dplyr::select(system_site, y_lat, x_longt, gfe_id, unique_id) %>%
      unique() %>%
      dplyr::rename(site_name = system_site, lat = y_lat, long = x_longt) %>%
      dplyr::mutate(
        site_num = gfe_id,
        brood_year = unique(sr_pop_temp$brood_year)
      ) %>%
      dplyr::select(site_name, brood_year, lat, long, site_num)

    # keep these data in the larger dataframe to refer back to
    site_data_by_year <- rbind(
      site_data_by_year,
      site_year_temp
    )

    # put the farm locations and the population locations together
    locs_temp <- rbind(
      (farm_locs_temp %>%
        dplyr::mutate(type = "farm")),
      (site_year_temp %>%
        dplyr::mutate(type = "population") %>%
        dplyr::mutate(
          type = as.factor(type),
          site = as.character(site_num)
        ) %>%
        dplyr::select(site, lat, long, type))
    ) %>%
      # this is being done to the whole resulting df
      dplyr::mutate(
        ff = ifelse(type == "farm", "bold", "plain"),
        fsize = ifelse(type == "farm", 1.8, 1.5)
      ) %>%
      sf::st_as_sf(., coords = c("long", "lat"))

    # need to temp make it WGS84
    sf::st_crs(locs_temp) <- 4326

    # transform to utm
    locs_temp_utm <- sf::st_transform(
      locs_temp,
      crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
    ) %>%
      dplyr::mutate(
        X = data.frame(sf::st_coordinates(.))$X,
        Y = data.frame(sf::st_coordinates(.))$Y
      ) %>%
      dplyr::mutate(
        type = as.factor(type),
        ff = as.factor(ff),
        fsize = as.factor(fsize)
      )

    ## figure out what nodes need to be kept for this year =====================
    curr_edges_nodes <- all_edges_nodes[
      which(names(all_edges_nodes) %in% farm_locs_temp$site)
    ]
    curr_nodes <- sapply(curr_edges_nodes, list_reassign,
      nodes_edges = "nodes"
    ) %>%
      unlist()

    ## figure out what nodes need to be kept for this year =====================
    curr_nodes <- all_edges_nodes[which(names(all_edges_nodes) %in%
      farm_locs_temp$site)] %>% unlist()
    # print(curr_nodes)
    # make and save the dataframe
    ggplot2::ggsave(

      # output path
      paste0(fig_output, "map-by-year-", yr, ".png"),

      # make the plot
      ggplot2::ggplot() +
        geom_sf(
          data = network %>%
            activate("nodes") %>%
            slice(curr_nodes) %>%
            sf::st_as_sf(),
          fill = "lightpink", colour = "lightpink"
        ) +
        geom_sf(data = large_land, fill = "white", colour = "grey70") +
        # geom_sf(data = utm_land_data_large, fill = "grey70") +
        geom_sf(data = locs_temp_utm, aes(
          fill = type, shape = type,
          size = type
        )) +
        scale_size_manual("Location", values = c(2.5, 1.8)) +
        scale_shape_manual("Location", values = c(21, 22)) +
        scale_fill_manual("Location", values = c("red", "#30D5C8")) +
        ggrepel::geom_text_repel(
          data = locs_temp_utm,
          aes(
            x = X, y = Y,
            label = site, fontface = ff, size = type
          ),
          max.overlaps = 50
        ) +
        theme_base() +
        coord_sf(
          xlim = c(465674.8, 585488), ylim = c(5761156, 5983932),
          expand = FALSE
        ) +
        theme(
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 90)
        ) +
        labs(
          x = "Longitude", y = "Latitude", title = paste0(species, ", ", yr)
        ),

      # make the size
      height = 8, width = 9
    )
  }

  # write out the data with the corresponding names/numbers for each year
  readr::write_csv(
    site_data_by_year,
    paste0(
      data_output, species,
      "-site-name-combos-for-exposed-populations.csv"
    )
  )
}

# make_yearly_popn_maps ========================================================
make_yearly_popn_maps <- function(sr_pop_data, sr_pop_sites, large_land,
                                  farm_data, farm_locs, network, exposure_df,
                                  all_edges_nodes, fig_output, species,
                                  data_output, size = "small") {
  #' Maps of each year's population and farm co-occurrence
  #'
  #' @description To determine which farms are high, medium, or low risk, we
  #' need yearly maps to show which populations might pass farms
  #'
  #' @param sr_pop_data dataframe. The cleaned pink SR data
  #' @param sr_pop_sites file. Information on all the populations locations
  #' @param large_land file. The geo-spatial data rds file
  #' @param farm_data dataframe. Data of the info for the different farms
  #' @param farm_locs dataframe. The cleaned data of the different
  #' locations of the farms
  #' @param network sfnetwork tibble. The network for the entire region
  #' @param exposure_df file. Dataframe on the exposure options
  #' @param all_edges_nodes list. The list of all the lists of edges and nodes
  #' to be kept to map onto the region
  #' @param species character. The species being plotted (salmon).
  #' @param fig_output character. Where to save the figure files
  #' @param data_output character. Where to save the data files
  #'
  #' @usage make_yearly_popn_maps(sr_pop_data, sr_pop_sites, geo_data,
  #' farm_data, clean_farm_locs, output_path)
  #' @return NA
  #'

  # TESTING
  # sr_pop_data = tar_read(clean_pink_spawner_recruit_data)
  # sr_pop_sites = tar_read(clean_wild_pop_location_data)
  # #large_land = readRDS(tar_read(large_land))
  # large_land = readRDS(here::here("./outputs/geo-objs/fresh/even-large-land-for-plotting.rds"))
  # farm_data = tar_read(clean_farm_lice_data)
  # farm_locs = tar_read(clean_farm_locs)
  # network = qs::qread(tar_read(network))
  # exposure_df = read_csv(tar_read(pink_exposure_df))
  # all_edges_nodes = readRDS(tar_read(all_edges_nodes))
  # species = "Pink"
  # fig_output = here::here("./figs/maps/yearly-pop-maps/pink//")
  # data_output = here::here("./data/spawner-recruit/clean//")

  # filter the sites to just the ones in our data
  # sr_pop_data_area67 <- sr_pop_data %>%
  #   dplyr::filter(area %in% c(6, 7))

  # clean the farm data names so they match up between the dataframe
  farm_data <- farm_data %>%
    dplyr::mutate(
      farm = dplyr::case_when(
        farm == "Goat" ~ "Goat Cove",
        farm == "Sheep Pass" ~ "Sheep Passage",
        farm == "Lime" ~ "Lime Point",
        farm == "Alexander" ~ "Alexander Inlet",
        TRUE ~ as.character(farm)
      )
    )

  ## set up the farms that we'll need to plot but with sf functions ============
  farms_sf <- sf::st_as_sf(farm_locs, coords = c("long", "lat"))

  # set the coordinates for WGS84
  sf::st_crs(farms_sf) <- 4326

  # transform to utm
  farms_utm <- sf::st_transform(farms_sf,
    crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
  )

  # only loop through the years we actually HAVE for the species at hand
  max_yr <- ifelse(species == "Pink", 2020, 2017)
  for (yr in 2005:max_yr) {
    # get the farms in that time period
    farms_temp <- (farm_data %>%
      dplyr::filter(year == yr) %>%
      dplyr::filter(month %in% c(3, 4)) %>%
      dplyr::group_by(farm) %>%
      dplyr::summarize(
        inventory =
          mean(mean_inventory,
            na.rm = TRUE
          )
      ))$farm

    farm_locs_temp <- farm_locs %>%
      dplyr::filter(site %in% farms_temp) %>%
      dplyr::select(site, lat, long)

    sr_pop_sites_filter <- sr_pop_sites %>%
      dplyr::filter(system_site %in% unique(sr_pop_data$river))

    # get the populations in that year
    site_year_temp <- sr_pop_data %>%
      # brood year of yr will pass fish farms in year + 1
      dplyr::filter(brood_year == (yr - 1)) %>%
      dplyr::select(river, lat, long, gfe_id, brood_year) %>%
      unique() %>%
      dplyr::rename(
        site_name = river, lat = long, long = lat,
        site_num = gfe_id
      ) %>%
      dplyr::select(site_name, brood_year, lat, long, site_num)

    # put the farm locations and the population locations together
    locs_temp <- rbind(
      (farm_locs_temp %>%
        dplyr::mutate(type = "farm")),
      (site_year_temp %>%
        dplyr::mutate(type = "population") %>%
        dplyr::mutate(
          type = as.factor(type),
          site = as.character(site_num)
        ) %>%
        dplyr::select(site, lat, long, type))
    ) %>%
      # this is being done to the whole resulting df
      dplyr::mutate(
        ff = ifelse(type == "farm", "bold", "plain"),
        fsize = ifelse(type == "farm", 1.8, 1.5)
      ) %>%
      sf::st_as_sf(., coords = c("long", "lat"))

    # need to temp make it WGS84
    sf::st_crs(locs_temp) <- 4326

    # transform to utm
    locs_temp_utm <- sf::st_transform(
      locs_temp,
      crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
    ) %>%
      dplyr::mutate(
        X = data.frame(sf::st_coordinates(.))$X,
        Y = data.frame(sf::st_coordinates(.))$Y
      ) %>%
      dplyr::mutate(
        type = as.factor(type),
        ff = as.factor(ff),
        fsize = as.factor(fsize)
      )

    ## figure out what nodes need to be kept for this year =====================
    curr_edges_nodes <- all_edges_nodes[
      which(names(all_edges_nodes) %in% farm_locs_temp$site)
    ]
    curr_nodes <- sapply(curr_edges_nodes, list_reassign,
      nodes_edges = "nodes"
    ) %>%
      unlist()

    # get the exposure df ready
    exposure_df_temp <- exposure_df %>%
      dplyr::filter(year == yr) %>%
      dplyr::select(-year) %>%
      dplyr::mutate(sites = as.character(sites))

    # make an exposure fill
    locs_temp_utm <- locs_temp_utm %>%
      dplyr::left_join(
        x = .,
        y = exposure_df_temp,
        by = c("site" = "sites")
      ) %>%
      dplyr::mutate(
        exposure = factor(ifelse(is.na(exposure), " ", exposure),
          levels = c("yes", "maybe", "no", " ")
        ),
        exposure_maybe = factor(
          dplyr::case_when(
            exposure == " " ~ " ",
            exposure == "no" ~ "no",
            exposure == "yes" ~ "yes",
            maybes == "north" ~ "north maybe",
            maybes == "south" ~ "south maybe",
            maybes == "both" ~ "both maybe"
          )
        )
      )

    ## figure out what nodes need to be kept for this year =====================
    curr_nodes <- all_edges_nodes[which(names(all_edges_nodes) %in%
      farm_locs_temp$site)] %>% unlist()
    # print(curr_nodes)


    ### all maybes ============================================================
    if (size == "small") {
      xmin <- 465674.8
      xmax <- 585488
      ymin <- 5761156
      ymax <- 5983932
    } else if (size == "large") {
      xmin <- 465674.8
      xmax <- 650000
      ymin <- 5691156
      ymax <- 5983932
    }

    all <- ggplot2::ggplot() +
      geom_sf(
        data = network %>%
          sfnetworks::activate("nodes") %>%
          slice(curr_nodes) %>%
          sf::st_as_sf(),
        fill = "lightpink", colour = "lightpink"
      ) +
      geom_sf(data = large_land, fill = "white", colour = "grey70") +
      # geom_sf(data = utm_land_data_large, fill = "grey70") +
      geom_sf(data = locs_temp_utm, aes(
        fill = exposure, shape = type,
        size = type
      )) +
      scale_size_manual(values = c(2.5, 1.8)) +
      scale_shape_manual("Location", values = c(21, 22)) +
      scale_fill_manual("Exposure", values = c(
        "red3", "gold2",
        "lightblue2",
        "purple"
      )) +
      ggrepel::geom_text_repel(
        data = locs_temp_utm,
        aes(
          x = X, y = Y,
          label = site, fontface = ff, size = type
        ),
        max.overlaps = 50
      ) +
      theme_base() +
      coord_sf(
        xlim = c(xmin, xmax), ylim = c(ymin, ymax),
        expand = FALSE
      ) +
      theme(
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90)
      ) +
      guides(
        size = "none",
        fill = guide_legend(
          override.aes = list(
            shape = c(21, 21, 21, 21),
            size = c(3, 3, 3, 3),
            fill = c(
              "red3", "gold2", "lightblue2",
              "white"
            ),
            colour = c("black", "black", "black", "white")
          )
        ),
        shape = guide_legend(
          override.aes = list(
            size = c(3, 3)
          )
        )
      ) +
      labs(
        x = "Longitude", y = "Latitude", title = paste0(
          species, ", ", yr,
          " all maybes"
        )
      )

    # make and save the dataframe
    ggplot2::ggsave(
      # output path
      paste0(fig_output, "//all-maybes//", "map-by-year-", yr, ".png"),
      # the plot
      all,
      # make the size
      height = 8, width = 6
    )
    ### north maybes ===========================================================
    locs_temp_utm_n <- locs_temp_utm
    locs_temp_utm_n[which(
      locs_temp_utm_n$exposure_maybe == "south maybe"
    ), "exposure"] <- "no"
    locs_temp_utm_n$exposure

    north <- ggplot2::ggplot() +
      geom_sf(
        data = network %>%
          sfnetworks::activate("nodes") %>%
          slice(curr_nodes) %>%
          sf::st_as_sf(),
        fill = "lightpink", colour = "lightpink"
      ) +
      geom_sf(data = large_land, fill = "white", colour = "grey70") +
      # geom_sf(data = utm_land_data_large, fill = "grey70") +
      geom_sf(data = locs_temp_utm_n, aes(
        fill = exposure, shape = type,
        size = type
      )) +
      scale_size_manual(values = c(2.5, 1.8)) +
      scale_shape_manual("Location", values = c(21, 22)) +
      scale_fill_manual("Exposure", values = c(
        "red3", "gold2", "lightblue2",
        "purple"
      )) +
      ggrepel::geom_text_repel(
        data = locs_temp_utm_n,
        aes(
          x = X, y = Y,
          label = site, fontface = ff, size = type
        ),
        max.overlaps = 50
      ) +
      theme_base() +
      coord_sf(
        xlim = c(465674.8, 585488), ylim = c(5761156, 5983932),
        expand = FALSE
      ) +
      theme(
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90)
      ) +
      guides(
        size = "none",
        fill = guide_legend(
          override.aes = list(
            shape = c(21, 21, 21, 21),
            size = c(3, 3, 3, 3),
            fill = c(
              "red3", "gold2", "lightblue2",
              "white"
            ),
            colour = c("black", "black", "black", "white")
          )
        ),
        shape = guide_legend(
          override.aes = list(
            size = c(3, 3)
          )
        )
      ) +
      labs(
        x = "Longitude", y = "Latitude", title = paste0(
          species, ", ", yr,
          " north maybes"
        )
      )

    ggplot2::ggsave(

      # output path
      paste0(fig_output, "//north-maybes//", "map-by-year-", yr, ".png"),
      # plot
      north,

      # make the size
      height = 8, width = 6
    )

    ### south maybes ===========================================================
    locs_temp_utm_s <- locs_temp_utm
    locs_temp_utm_s[which(
      locs_temp_utm_s$exposure_maybe == "north maybe"
    ), "exposure"] <- "no"
    locs_temp_utm_s$exposure

    south <- ggplot2::ggplot() +
      geom_sf(
        data = network %>%
          sfnetworks::activate("nodes") %>%
          slice(curr_nodes) %>%
          sf::st_as_sf(),
        fill = "lightpink", colour = "lightpink"
      ) +
      geom_sf(data = large_land, fill = "white", colour = "grey70") +
      # geom_sf(data = utm_land_data_large, fill = "grey70") +
      geom_sf(data = locs_temp_utm_s, aes(
        fill = exposure, shape = type,
        size = type
      )) +
      scale_size_manual(values = c(2.5, 1.8)) +
      scale_shape_manual("Location", values = c(21, 22)) +
      scale_fill_manual("Exposure", values = c(
        "red3", "gold2", "lightblue2",
        "purple"
      )) +
      ggrepel::geom_text_repel(
        data = locs_temp_utm_s,
        aes(
          x = X, y = Y,
          label = site, fontface = ff, size = type
        ),
        max.overlaps = 50
      ) +
      theme_base() +
      coord_sf(
        xlim = c(465674.8, 585488), ylim = c(5761156, 5983932),
        expand = FALSE
      ) +
      theme(
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90)
      ) +
      guides(
        size = "none",
        fill = guide_legend(
          override.aes = list(
            shape = c(21, 21, 21, 21),
            size = c(3, 3, 3, 3),
            fill = c(
              "red3", "gold2", "lightblue2",
              "white"
            ),
            colour = c("black", "black", "black", "white")
          )
        ),
        shape = guide_legend(
          override.aes = list(
            size = c(3, 3)
          )
        )
      ) +
      labs(
        x = "Longitude", y = "Latitude", title = paste0(
          species, ", ", yr,
          " south maybes"
        )
      )

    ggplot2::ggsave(
      # output path
      paste0(fig_output, "//south-maybes//", "map-by-year-", yr, ".png"),
      # plot
      south,
      # make the size
      height = 8, width = 6
    )

    # stitch the maps together =================================================
    all_noleg <- all +
      theme(
        legend.position = "none"
      )
    north_noleg <- north +
      theme(
        legend.position = "none"
      )

    all_maps <- all_noleg + north_noleg + south
    ggplot2::ggsave(
      # output path
      paste0(
        fig_output, "//all-north-south-options//",
        "map-by-year-", yr, ".png"
      ),
      # plot
      all_maps,
      # size
      height = 8, width = 14.5
    )
  }
}

# plot_given_sites =============================================================
plot_given_sites <- function(site_nums_missing, yr, site_df, large_land) {
  #' Maps just the populations that are given
  #'
  #' @description To see where the missing farms are, plot just the ones that
  #' are not in the ones that have been given a category yet
  #'
  #' @param site_nums_missing numeric. The integer vector of the farm numbers
  #' @param yr integer. The year to plot
  #' @param site_df dataframe. Data with all of the site numbers and
  #' year information
  #'
  #' @usage plot_given_sites(site_nums_missing, yr, site_df)
  #' @return ggplot2 object
  #'

  if (length(site_nums_missing) == 0) {
    return("No sites missing")
  }

  site_df <- site_df %>%
    dplyr::filter(
      site_num %in% site_nums_missing,
      out_mig_year %in% yr
    ) %>%
    dplyr::select(-out_mig_year) %>%
    unique() %>%
    sf::st_as_sf(., coords = c("long", "lat"))
  # need to temp make it WGS84
  sf::st_crs(site_df) <- 4326
  site_df <-
    sf::st_transform(site_df,
      crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
    ) %>%
    dplyr::mutate(
      X = data.frame(sf::st_coordinates(.))$X,
      Y = data.frame(sf::st_coordinates(.))$Y
    )

  temp_plot <- ggplot2::ggplot() +
    coord_sf(
      datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
    ) +
    geom_sf(data = large_land, fill = "white", colour = "grey70") +
    # geom_sf(data = utm_land_data_large, fill = "grey70") +
    geom_point(data = site_df, aes(x = X, y = Y)) +
    ggrepel::geom_text_repel(
      data = site_df,
      aes(x = X, y = Y, label = site_num),
      max.overlaps = 50
    ) +
    theme_base() +
    coord_sf(
      xlim = c(465674.8, 650000), ylim = c(5691156, 5983932),
      expand = FALSE
    ) +
    theme(
      plot.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 90)
    ) +
    labs(
      x = "Longitude", y = "Latitude"
    )
  return(temp_plot)
}
