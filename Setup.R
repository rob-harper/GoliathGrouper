
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


keys99_24= (getRvcData(years = 1999:2024, regions = "FLA KEYS", SPECIES_CD = "EPI ITAJ"))

drto99_24gg = (getRvcData(years = 1999:2024, regions = "DRY TORT"))

sefcri13_24gg = (getRvcData(years = 2013:2024, regions = "SEFCRI"))
#grab density of GG for all regions and bind
density_all <- rbind(getDomainDensity(keys99_24, "epi itaj"),
                     getDomainDensity(drto99_24, "epi itaj"),
                     getDomainDensity(sefcri13_24, "epi itaj"))
density_all <- density_all %>% mutate(SE = sqrt(var))

#grab occurrence of all regions and bind
occ_all <- rbind(getDomainOccurrence(keys99_24, "epi itaj"),
                 getDomainOccurrence(drto99_24, "epi itaj"),
                 getDomainOccurrence(sefcri13_24, "epi itaj"))
occ_all <- occ_all %>% mutate(SE = sqrt(var))

#plot of density for all regions together
ggplot(occ_all, aes(x = YEAR, y = occurrence, color = REGION)) + 
  geom_point(size = 2) + 
  geom_line(size = .8) + 
  geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE),
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
  ylab("Occurrence") +  
  ggtitle("Goliath Grouper Occurrence: Southern Florida") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) 

keys99_24gg <- keys99_24$sample_data %>% 
              filter(SPECIES_CD == "EPI ITAJ", NUM > 0)

write_csv(keys99_24gg, file = "keys99_24gg.csv")

plot_epi_itaj_counts <- function(df) {
  df %>%
    filter(SPECIES_CD == "EPI ITAJ") %>%
    group_by(YEAR) %>%
    summarise(total_counts = sum(NUM, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = YEAR, y = total_counts)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = round(total_counts, 1)), vjust = -0.5, size = 3) +
    labs(
      title = "Total EPI ITAJ Counts per Year",
      x = "Year",
      y = "Total Counts"
    ) +
    theme_minimal()
}

plot_epi_itaj_counts(keys99_24gg)


