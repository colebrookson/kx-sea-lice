##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-01-18
#'
#' This file contains the functions that process the files run on the compute
#' canada cluster (outside the targets framework) and plots the results
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

# plot_power =================================================================
plot_power <- function(all_power_sims, output_path) {
  #' Pull in all the required data to do this analysis and plot the results
  #' 
  #' @description The simulations have all run and now this function takes in 
  #' the values from those simulations and plots the power/c relationship
  #' 
  #' @param all_power_sims file. All of the simulated power analysis info
  #' @param output_path character. Location to write out the plot
  #'  
  #' @usage plot_power(all_power_sims, output_path)
  #' @return NA
  #'
  
  # sort the data and do some calculations =====================================
  all_power_sims <- all_power_sims %>%
    # add in a one/zero for if the p value is < 0.05
    dplyr::rowwise() %>%
    dplyr::mutate(
      better = ifelse(p <= 0.05, 1, 0)
    ) %>%
    dplyr::group_by(c) %>%
    dplyr::summarize(
      n = n(),
      count = sum(better)
    ) %>%
    dplyr::mutate(
      prop = count / n
    )

  # make and save plot =========================================================
  ggplot2::ggsave(

    #filename
    paste0(output_path, "power-plot.png"),

    # the plot itself
    ggplot(data = all_power_sims) +
      geom_smooth(aes(x = c, y = prop)) +
      geom_point(aes(x = c, y = prop), colour = "black", 
                 alpha = 0.5, size = 1) +
      labs(x = "C", y = "Power") +
      ggthemes::theme_base() +
      geom_vline(aes(xintercept = 0.19), linetype = "dashed", colour = "red") +
      geom_hline(aes(
        yintercept = all_power_sims[which(
          all_power_sims$c == 0.19), "prop"][[1]]
      ), linetype = "dashed", colour = "red"),

    # size of the plot
    height = 5, width = 6
  )
}
