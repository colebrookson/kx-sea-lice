# mapping with polygons and distance ===========================================
library(PBSmapping)
library(dplyr)
library(magrittr)
library(sf)

data(nepacLLhigh)

# look at the plot
PBSmapping::plotMap(nepacLLhigh, 
                    xlim = c(-129, -127.75), 
                    ylim = c(52, 53))


# attempt to convert data to a polygon format
poly <- nepacLLhigh %>% 
  sf::st_as_sf(coords = c("lon", "lat"), 
         crs = 32611)