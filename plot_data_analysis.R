# Project: Sea cucumber honours project
# Modelling plot data
# Author: Shelby Connelly
# Date: 03/27/2025 - 05/13/2025

# Installing packages 
install.packages(c("tidyverse", "glmmTMB", "DHARMa", "dotwhisker", "ggeffects"))

# Loading packages into R
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(dotwhisker)
library(ggeffects)

# Loading transect and plot data into R
sea_cucumber_transect_data <- read_csv("./clean_data/sea_cucumber_transect_data.csv")
sea_cucumber_plot_data <- read_csv("./clean_data/sea_cucumber_plot_data.csv")

# Converting site, week, and treatment to factors
sea_cucumber_transect_data <- sea_cucumber_transect_data %>%
  mutate(site = fct_reorder(site, latitude))

sea_cucumber_plot_data_continuous <- sea_cucumber_plot_data %>%
  mutate(site = fct_reorder(site, latitude),
         treatment = factor(treatment, levels = c("Control", "Removal", "Addition")))

sea_cucumber_plot_data_discrete <- sea_cucumber_plot_data_continuous %>%
  mutate(week = as.factor(week))

# Calculating ambient sea cucumber density by site
ambient_density_data <- sea_cucumber_transect_data %>%
  group_by(site) %>%
  summarise(ambient_density = mean(density)) %>%
  ungroup()

# Joining plot and ambient density data by site 
sea_cucumber_plot_data_continuous <- left_join(sea_cucumber_plot_data_continuous, ambient_density_data, join_by(site))

# MODELLING SEA CUCUMBER DENSITY OVER TIME -------------------------------------

# Modelling sea cucumber density over time by treatment 
density_model <- glmmTMB(density_experimental_sea_cucumber ~ week * treatment + (1|site), sea_cucumber_plot_data_discrete,
                         family = tweedie(link = "log"))
plot(simulateResiduals(density_model))
summary(density_model)

# Plotting model coefficients and saving as a .tiff file
tiff("./plots/density_coefficients.tiff",
     height = 3,
     width = 5,
     units = "in",
     res = 600)

density_coefficients <- dwplot(density_model) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0,
             linetype = 2) +
  scale_colour_grey() +
  labs(x = "Coefficient",
       tag = "a") +
  scale_y_discrete(labels = c("SD (Intercept)",
                              "Week 6 : Addition", "Week 3 : Addition", "Week 1 : Addition",
                              "Week 6 : Removal", "Week 3 : Removal", "Week 1 : Removal",
                              "Addition", "Removal",
                              "Week 6", "Week 3", "Week 1"))

density_coefficients

dev.off()
  
# Backtransforming model predictions 
predict_density_model <- ggpredict(density_model, terms = c("week", "treatment")) %>%
  rename(week = x,
         density = predicted,
         treatment = group)

# Plotting model predictions and saving as a .tiff file
tiff("./plots/density_predictions.tiff",
     height = 2.5,
     width = 5,
     units = "in",
     res = 300)

density_predictions <- ggplot() +
  theme_classic() +
  facet_wrap("treatment") +
  theme(panel.border = element_rect(fill = NA)) +
  theme(strip.background = element_blank()) +
  theme(strip.text = element_blank()) +
  geom_jitter(data = sea_cucumber_plot_data_discrete,
              aes(x = week,
                  y = density_experimental_sea_cucumber,
                  colour = treatment),
              width = 0.1,
              alpha = 0.25) +
  geom_point(data = predict_density_model,
             aes (x = week,
                  y = density,
                  colour = treatment)) +
  geom_errorbar(data = predict_density_model,
                aes(x = week,
                    y = density,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = treatment),
                width = 0.5) +
  labs(x = "Week",
       y = expression(paste("Density (#/", m^2, ")")),
       colour = "Treatment",
       tag = "b")
  
density_predictions

dev.off()

# MODELLING CHANGE IN SEA CUCUMBER DENSITY OVER TIME ---------------------------

# Filtering plot data by removal treatment
sea_cucumber_removal_data <- sea_cucumber_plot_data_continuous %>%
  filter(treatment == "Removal")

# Modelling change in sea cucumber density by week and ambient density
density_change_model <- glmmTMB(density_change ~ week * ambient_density + (1|site), sea_cucumber_removal_data)
plot(simulateResiduals(density_change_model))
summary(density_change_model)

# Plotting model coefficients and saving as a .tiff file
tiff("./plots/density_change_coefficients.tiff",
     height = 2.5,
     width = 5,
     units = "in",
     res = 600)

density_change_coefficients <- dwplot(density_change_model) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_grey() +
  geom_vline(xintercept = 0,
             linetype = 2) +
  labs(x = "Coefficient",
       tag = "a") +
  scale_y_discrete(labels = c("SD (Observations)", "SD (Intercept)", 
                              "Week : Ambient density", "Ambient density", 
                              "Week"))

density_change_coefficients

dev.off()

# Setting representative values for ambient density
predict_density_change_model <- ggpredict(density_change_model, terms = c("week", "ambient_density [0, 0.4, 0.8]")) %>%
  rename(week = x,
         density_change = predicted,
         ambient_density = group)

# Selecting colours for low, medium, and high-density sites
paletteer_d("LaCroixColoR::Apricot")

ambient_density_colours <- list("0" = "#FFAD0AFF",
                                "0.4" = "#EE6100FF",
                                "0.8" = "#D72000FF")

# Plotting model predictions and saving as a .tiff file
tiff("./plots/density_change_predictions.tiff",
     height = 2.5,
     width = 5,
     units = "in",
     res = 300)

density_change_predictions <- ggplot() +
  theme_classic() +
  facet_wrap("ambient_density") +
  theme(panel.border = element_rect(fill = NA)) +
  theme(strip.background = element_blank()) +
  theme(strip.text = element_blank()) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  geom_point(data = predict_density_change_model,
             aes(x = week,
                 y = density_change,
                 colour = ambient_density)) +
  geom_errorbar(data = predict_density_change_model,
                aes(x = week,
                    y = density_change,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = ambient_density)) +
  scale_colour_manual(values = ambient_density_colours,
                    labels = c("Low", "Medium", "High")) +
  labs(x = "Week",
       y = expression(paste(Delta, "density (#/", m^2, ")")),
       colour = "Ambient density",
       tag = "b")

density_change_predictions

dev.off()