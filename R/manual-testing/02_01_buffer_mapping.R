# mapping with polygons and distance ===========================================
library(PBSmapping)
library(dplyr)
library(magrittr)

data(nepacLLhigh)

# filter the data to just the area we care about
nepac_filter <- nepacLLhigh %>% 
  dplyr::filter(Y > 52 & Y < 53) %>% 
  dplyr::filter(X > -129 & X < -127.75)

PBSmapping::plotMap(nepacLLhigh, 
                    xlim = c(-129, -127.75), 
                    ylim = c(52, 53))
