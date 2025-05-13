# Project: Sea cucumber honours project
# Analyzing transect data
# Author: Shelby Connelly
# Date: 03/20/2025 - 05/13/2025

# Installing packages
install.packages("tidyverse")

# Loading packages into R
library(tidyverse)

# Loading transect data into R
clean_transect_data <- read_csv("./clean_data/clean_transect_data.csv")
clean_transect_size_data <- read_csv("./clean_data/clean_transect_size_data.csv")
sea_cucumber_transect_data <- read_csv("./clean_data/sea_cucumber_transect_data.csv")

# Converting site, transect, and diver_position to factors
clean_transect_data <- clean_transect_data %>%
  mutate(site = fct_reorder(site, latitude),
         transect = as.factor(transect),
         diver_position = as.factor(diver_position))

clean_transect_size_data <- clean_transect_size_data %>%
  mutate(site = fct_reorder(site, latitude),
         transect = as.factor(transect),
         diver_position = as.factor(diver_position))

sea_cucumber_transect_data <- sea_cucumber_transect_data %>%
  mutate(site = fct_reorder(site, latitude),
         transect = as.factor(transect),
         diver_position = as.factor(transect))

# DENSITY DATA -----------------------------------------------------------------

# Plotting ambient sea cucumber density by site and saving as a .tiff file
tiff("./plots/ambient_density_plot.tiff",
     height = 2.5,
     width = 5,
     units = "in",
     res = 600)

ambient_density_plot <- ggplot(sea_cucumber_transect_data,
                            aes(x = site, 
                                y = density)) + 
  theme_classic() +
  geom_jitter(width = 0.1,
              alpha = 0.25) +
  stat_summary(fun = mean) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.25) +
  labs(x = NULL,
       y = expression(paste("Ambient density (#/", m^2, ")"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
 
ambient_density_plot

dev.off()

# BIOMASS DATA -----------------------------------------------------------------

# Plotting ambient sea cucumber biomass by site and saving as a .tiff file
tiff("./plots_ambient_biomass_plot.tiff",
     height = 2.5,
     width = 5,
     units = "in",
     res = 600)

ambient_biomass_plot <- ggplot(sea_cucumber_transect_data,
                            aes(x = site,
                                y = total_biomass)) +
  theme_classic() +
  geom_jitter(width = 0.1,
              alpha = 0.25) +
  stat_summary(fun = mean) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.25) +
  annotate("text", x = "Best Point", y = 0, label = "N/A") +
  labs(x = NULL,
       y = expression(paste("Ambient biomass (g/", m^2, ")"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ambient_biomass_plot

dev.off()
