##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-01-18
#'
#' This file contains the functions that map the locations and sites we're 
#' interested in for this project 
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# collect_data =================================================================
collect_data <- function() {
  #' Pull in all the required data to do this analysis and put them together 
  #' into one object
  #' 
  #' @description Since this essentially needs all the data to fit the actual
  #' S-R model, this will be the 
  #' 
  #' @param farm_locations file. Information on all the farms in the region
  #' @param output_path character. Location to write out the locations of 
  #' the farms 
  #'  
  #' @usage clean_farm_locations(farm_locations)
  #' @return clean data frame 
  #'
  
}




x <- seq(-5, 5, 0.1)

sigmoid = function(x) {
  1 / (5 + exp(-x))
}

y <- sigmoid(x)
plot(x, y)

df <- data.frame(
  c = x,
  power = y
)

library(ggplot2)
library(ggthemes)

ggplot(data = df) +
  geom_line(aes(x = c, y = power),
            linewidth = 2) +
  ggthemes::theme_base() +
  scale_x_continuous(breaks = seq(-5, 5, 2.5), 
                 labels = c(0, 0.25, 0.5, 0.75, 1.0)) + 
  scale_y_continuous(breaks = c(0.00, 0.05, 0.1, 0.15, 0.2),
                     labels = c(0, 0.25, 0.5, 0.75, 1)) + 
  geom_vline(aes(xintercept = -3.5), colour = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = df$power[which(df$c == -3.5)]), colour = "red",
             linetype = "dashed") #### 0.13







