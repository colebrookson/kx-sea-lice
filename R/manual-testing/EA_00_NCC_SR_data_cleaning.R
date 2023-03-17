# Date created: 16-Mar-2023
# Last updated: 16-Mar-2023
# Author: Emma Atkinson
# Description: Central Coast river-level SR data compilation
# Notes: Code to compile river-level SR data for all species, updated for sub-contract work for Kitasoo-Xai'xais Stewardship Authority
#        Requires: stream-level escapement, Conservation Unit-level age table, and Statistical Area-level age table.
#        Output: stream-level stock-recruitment (S-R) data for all species (where sufficient data were available)
#        *Note that output S-R data relies on any assumptions made in the compilation of the stream-level escapement 
#          and age-at-return data compilation. 

# --- Prepping environment --- #

rm(list=ls())
graphics.off()

library(here)
setwd(here("data","spawner-recruit","raw"))

# Installing packages #
# install.packages("dpylr")
# install.packages("reshape2")
# install.packages("stringr")

# Loading packages #
library(dplyr)
library(reshape2)
library(stringr)
