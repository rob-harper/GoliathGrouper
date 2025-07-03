
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

density_all <- rbind(getDomainDensity(keys99_24, "epi itaj"),
                     getDomainDensity(drto99_24, "epi itaj"),
                     getDomainDensity(sefcri13_24, "epi itaj"))
density_all <- density_all %>% mutate(SE = sqrt(var))

occ_all <- rbind(getDomainOccurrence(keys99_24, "epi itaj"),
                 getDomainOccurrence(drto99_24, "epi itaj"),
                 getDomainOccurrence(sefcri13_24, "epi itaj"))
occ_all <- occ_all %>% mutate(SE = sqrt(var))

ggplot(density_all, aes(x = YEAR, y = density, color = REGION)) + 
  geom_point(size = 2) + 
  geom_line(size = .8) + 
  geom_errorbar(aes(ymin = density - SE, ymax = density + SE),
                width = 0.25,
                size = 0.5,
                alpha = 0.5) + 
  scale_color_manual(values = c("DRY TORT" = "#a6cee3",  
                                "FLA KEYS" = "#1f78b4",  
                                "SEFCRI" = "#b2df8a"),
                     name = "Region",  
                     labels = c("Dry Tortugas", "Florida Keys", "Southeast Florida")) +  
  guides(color = guide_legend(title = "Region", 
                              override.aes = list(size = 4, shape = 16))) +  
  scale_x_continuous(breaks = c(min(density_all$YEAR):max(density_all$YEAR))) +
  theme_Publication() + 
  xlab("Year") + 
  ylab("Density/177m^2") +  
  ggtitle("Goliath Grouper Density: Southern Florida") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) 

#test commit


