# Project: Sea cucumber honours project
# Analyzing transect data
# Author: Shelby Connelly
# Date: 03/20/2025 - 04/12/2025

# Installing packages
install.packages(c("tidyverse", "glmmTMB", "emmeans", "DHARMa", "patchwork"))

# Loading packages into R
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(patchwork)

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

# Plotting sea cucumber density by site
site_density_plot <- ggplot(sea_cucumber_transect_data,
                            aes(x = site, 
                                y = density)) + 
  theme_classic() +
  geom_jitter(width = 0.1,
              alpha = 0.25) +
  stat_summary(fun = mean) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.25) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = bquote(Density~("#"/m^2)))
       
site_density_plot

# BIOMASS DATA -----------------------------------------------------------------

# Plotting sea cucumber biomass by site
site_biomass_plot <- ggplot(sea_cucumber_transect_data,
                       aes(x = site,
                           y = total_biomass)) +
  theme_classic() +
  geom_jitter(width = 0.1, 
              alpha = 0.25) +
  stat_summary(fun = mean) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               width = 0.25) +
  annotate("text", x = c("boulder_island", "best_point"), y = 0, label = "N/A") +
  labs(x = "Site",
       y = "Biomass (g)") +
  scale_x_discrete(labels = c("Boulder Island", "Jug Island", "Twin Islands", 
                              "Brighton Beach", "Old Buntzen PP", "Best Point", 
                              "S Johnson Bay", "S Croker Island", "N Croker Island")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

site_biomass_plot

# Merging site density and size plots and saving as a .tiff file ---------------
tiff(file = "./plots/transect_density_biomass_plot.tiff", 
     height = 5, 
     width = 5.5, 
     units = "in", 
     res = 400)

transect_density_biomass_plot <- site_density_plot / site_biomass_plot

transect_density_biomass_plot

dev.off()
