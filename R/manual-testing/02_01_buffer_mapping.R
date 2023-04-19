# mapping with polygons and distance ===========================================
library(PBSmapping)
library(dplyr)
library(magrittr)
library(sf)
library(sp)
library(raster)

data(nepacLLhigh)

# look at the plot
PBSmapping::plotMap(nepacLLhigh, 
                    xlim = c(-129, -127.75), 
                    ylim = c(52, 53))


# attempt to convert data to a polygon format
poly <- nepacLLhigh %>% 
  sf::st_as_sf(coords = c("X", "Y"), 
         crs = 32611) 


sf_airports <- st_as_sf(shp_airports) 
sf_airports_polygons <- st_polygonize(sf_airports)



# data from stamen
library(ggmap)
