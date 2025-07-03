
library(rvc)
library(tidyverse)

#Source Functions
source("FishPlots/WrapperFunctions.R")
source("FishPlots/labeler.R")
source("FishPlots/theme_publication.R")
source("FishPlots/QL_binned_LengthFreq.R")
source("FishPlots/QL_SubregionDensityPlot.R")
source("FishPlots/plot_horizontal_bar_shared_axis.R")

#read in data 
keys99_24 = (getRvcData(years = 1999:2024, regions = "FLA KEYS"))

drto99_24 = (getRvcData(years = 1999:2024, regions = "DRY TORT"))

sefcri13_24 = (getRvcData(years = 2013:2024, regions = "SEFCRI"))

#Combine regions
allreg <- rbind(keys99_24, drto99_24, sefcri13_24)
