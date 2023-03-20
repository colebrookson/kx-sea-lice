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
setwd(here("data","spawner-recruit","raw","EA-river-level-SR-2023-update"))

# Installing packages #
# install.packages("dpylr")
# install.packages("reshape2")
# install.packages("stringr")
# install.packages("readxl")

# Loading packages #
library(dplyr)
library(reshape2)
library(stringr)
library(readxl)

# --- Reading in big salmon data file, as received from PSF --- #

# Getting names of individual datasheets #
sheets = excel_sheets("All-OUTPUT--nonlegacy-mode_20220222.xlsx")[1:7]

# Reading in and separating datasheets (note: NAs will produce warnings - that's fine) #
x = mapply(function(X) read_xlsx("All-OUTPUT--nonlegacy-mode_20220222.xlsx", guess_max = 7000, sheet=X), sheets) 
# Changing them to dataframes
xx = lapply(x, as.data.frame) 
# Giving them their names back
names(xx) <- sheets 

# Pulling out the sheets we want to use #
agebyCU_raw = xx[[1]]
TRTCbyCU = xx[[6]]
escape = xx[[5]]

# Set up version of age table to work with #
agebyCU = agebyCU_raw

# Pulling in the NuSEDS sheet that Eric sent (Mar-2023) #
# Note: ideally, we would actually just pull the 5th sheet from the XLSX file:
#       "OUTPUT NCC Streams Escapement". So for future I have included that code too.

#escape = xx[[5]]
escape = read.csv("NuSEDS_escapement_data_collated_20221101.csv", stringsAsFactors = FALSE)

# --- Infilling 'age' table with info from TRTC table --- #

# adding rows for 2019-2021 #

for (i in unique(agebyCU$CU)) {
  
  if (max(agebyCU[agebyCU$CU==i,"BroodYear"])==2019) {
    
    n = nrow(agebyCU)
    agebyCU[c(n+1, n+2),c("SpeciesId","CU","CU_Name","Age2","Age3","Age4","Age5","Age6","Age7")] = agebyCU[which(agebyCU$CU==i & agebyCU$BroodYear==2019),c("SpeciesId","CU","CU_Name","Age2","Age3","Age4","Age5","Age6","Age7")]  
    agebyCU[c(n+1, n+2),"BroodYear"] = c(2020,2021)
  
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKo") { agebyCU[c(n+1),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2020),"TE"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+2),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2021),"TE"] }
  
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKo") { agebyCU[c(n+1),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2020),"Total ER"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+2),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2021),"Total ER"] }
    
  }
  
  else if (max(agebyCU[agebyCU$CU==i,"BroodYear"])==2018) {
    
    n = nrow(agebyCU)
    agebyCU[c(n+1, n+2, n+3),c("SpeciesId","CU","CU_Name","Age2","Age3","Age4","Age5","Age6","Age7")] = agebyCU[which(agebyCU$CU==i & agebyCU$BroodYear==2018),c("SpeciesId","CU","CU_Name","Age2","Age3","Age4","Age5","Age6","Age7")]  
    agebyCU[c(n+1, n+2, n+3),"BroodYear"] = c(2019,2020,2021)
    
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+1),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2019),"TE"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKo") { agebyCU[c(n+2),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2020),"TE"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+3),"Escape"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2021),"TE"] }
    
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+1),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2019),"Total ER"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKo") { agebyCU[c(n+2),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2020),"Total ER"] }
    if (agebyCU[agebyCU$CU==i,]$SpeciesId[1] != "PKe") { agebyCU[c(n+3),"Total ER"] = TRTCbyCU[which(TRTCbyCU$CU==i & TRTCbyCU$Year==2021),"Total ER"] }
    
  }
  
}

# Clean up extra rows for PKe and PKo CUs because you don't have time to write nice code 
agebyCU = agebyCU[-(which(agebyCU$SpeciesId == "PKe" & agebyCU$BroodYear %in% c(2019, 2021))),]
agebyCU = agebyCU[-(which(agebyCU$SpeciesId == "PKo" & agebyCU$BroodYear %in% c(2020))),]

# infilling age-specific return estimates #
for (i in 1:nrow(agebyCU)){
  
  cu <- agebyCU$CU[i]
  y <- agebyCU$BroodYear[i]
  ymax <- max(agebyCU[agebyCU$CU==cu,]$BroodYear)
  spp <- agebyCU$SpeciesId[i]
 
  if (spp %in% c("PKe", "PKo")) {
    if ((ymax-y) >= 2) { agebyCU[i,"TR2"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+2),"Age2"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+2),"Total Run"] }
    
    if (FALSE %in% is.na(agebyCU[i,c("TR2")])) { 
      agebyCU[i,"Total"] = sum(agebyCU[i,c("TR2")], na.rm=TRUE) 
    } else { 
      agebyCU[i,"Total"] = NA 
    }
  } else {
  
    if ((ymax-y) >= 2) { agebyCU[i,"TR2"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+2),"Age2"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+2),"Total Run"] }
    if ((ymax-y) >= 3) { agebyCU[i,"TR3"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+3),"Age3"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+3),"Total Run"] }
    if ((ymax-y) >= 4) { agebyCU[i,"TR4"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+4),"Age4"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+4),"Total Run"] }
    if ((ymax-y) >= 5) { agebyCU[i,"TR5"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+5),"Age5"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+5),"Total Run"] }
    if ((ymax-y) >= 6) { agebyCU[i,"TR6"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+6),"Age6"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+6),"Total Run"] }
    if ((ymax-y) >= 7) { agebyCU[i,"TR7"] = agebyCU[which(agebyCU$CU==cu & agebyCU$BroodYear==y+7),"Age7"]*TRTCbyCU[which(TRTCbyCU$CU==cu & TRTCbyCU$Year==y+7),"Total Run"] }
  
    if (FALSE %in% is.na(agebyCU[i,c("TR2","TR3","TR4","TR5","TR6","TR7")])) { 
      agebyCU[i,"Total"] = sum(agebyCU[i,c("TR2","TR3","TR4","TR5","TR6","TR7")], na.rm=TRUE) 
      } else { 
       agebyCU[i,"Total"] = NA 
      }
  }
 
}

write.csv(agebyCU, "agebyCU_infilled_2023-Mar-17.csv", row.names = FALSE)
write.csv(TRTCbyCU, "TRTCbyCU_2023-Mar-17.csv", row.names = FALSE)
write.csv(escape, "escape_NCC_2023-Mar-20.csv", row.names = FALSE)
