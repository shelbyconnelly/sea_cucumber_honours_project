# Project: Sea cucumber honours project
# Modelling plot data
# Author: Shelby Connelly
# Date: 03/27/2025 - 04/22/2025

# Installing packages 
install.packages(c("tidyverse", "glmmTMB", "DHARMa", "ggeffects"))

# Loading packages into R
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(ggeffects)

# Loading plot data into R
sea_cucumber_plot_data <- read_csv("./clean_data/sea_cucumber_plot_data.csv")

# Converting site, week, and treatment to factors
sea_cucumber_plot_data <- sea_cucumber_plot_data %>%
  mutate(site = fct_reorder(site, latitude),
         week = as.factor(week),
         treatment = factor(treatment, levels = c("Control", "Removal", "Addition")))

# INITIAL PLOT DATA ---------------------------------------------------------

# Plotting initial sea cucumber density by site 
initial_sea_cucumber_plot_data <- sea_cucumber_plot_data %>%
  filter(week == 0)

initial_density_plot <- ggplot(initial_sea_cucumber_plot_data,
                               aes(x = site,
                                   y = density_initial_sea_cucumber)) +
  theme_classic() +
  geom_point(alpha = 0.25) +
  stat_summary(fun = mean) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.5) +
  labs(x = "Site",
       y = "Initial density") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

initial_density_plot

# Plotting initial sea cucumber biomass by site
initial_biomass_plot <- ggplot(initial_sea_cucumber_plot_data,
                               aes(x = site,
                                   y = total_initial_biomass)) +
  theme_classic() +
  geom_point(alpha = 0.25) +
  stat_summary(fun = mean) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.5) +
  labs(x = "Site",
       y = "Initial biomass (g)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

initial_biomass_plot

# Grouping sites by ambient density and biomass
sea_cucumber_plot_data <- sea_cucumber_plot_data %>%
  mutate(ambient_density = factor(levels = c("Low", "Medium", "High"),
                                  case_when(site %in% c("Boulder Island", "Jug Island", "Twin Islands") ~ "Low",
                                            site %in% c("Brighton Beach", "Old Buntzen PP", "Best Point") ~ "Medium",
                                            site %in% c("S Johnson Bay", "S Croker Island", "N Croker Island") ~ "High")),
         ambient_biomass = factor(levels = c("Low", "Medium", "High"),
                                  case_when(site %in% c("Boulder Island", "Jug Island", "Twin Islands") ~ "Low",
                                            site %in% c("Brighton Beach", "Old Buntzen PP", "Best Point") ~ "Medium",
                                            site %in% c("S Johnson Bay", "S Croker Island", "N Croker Island") ~ "High")))

# MODELLING SEA CUCUMBER DENSITY OVER TIME -------------------------------------

# Modelling sea cucumber density over time by treatment 
density_model <- glmmTMB(density_experimental_sea_cucumber ~ week * treatment + (1|site), 
                                      sea_cucumber_plot_data,
                                      family = tweedie(link = "log"))
plot(simulateResiduals(density_model))
summary(density_model)

# Backtransforming model predictions 
predict_density_model <- ggpredict(density_model, terms = c("week", "treatment")) %>%
  rename(week = x,
         density = predicted,
         treatment = group)

# Plotting model outputs
density_plot <- ggplot() +
  theme_classic() +
  facet_wrap("treatment") +
  geom_jitter(data = sea_cucumber_plot_data,
              aes(x = week,
                  y = density_experimental_sea_cucumber,
                  colour = treatment),
              width = 0.1,
              alpha = 0.25) +
  geom_point(data = predict_density_model,
             aes (x = week,
                  y = density,
                  colour = treatment),
             size = 2) +
  geom_errorbar(data = predict_density_model,
                aes(x = week,
                    y = density,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = treatment),
                width = 0.5,
                linewidth = 1) +
  labs(x = "Week",
       y = "Density (#/m2)",
       colour = "Treatment")
  
density_plot

# Plotting coefficients
dwplot(sea_cucumber_density_model) +
  theme_classic() +
  geom_vline(xintercept = 0,
             linetype = 2)

# MODELLING SEA CUCUMBER BIOMASS OVER TIME -------------------------------------

# Modelling sea cucumber biomass by week and treatment
biomass_model <- glmmTMB(experimental_plot_biomass ~ week * treatment + (1|site), sea_cucumber_plot_data,
                         family = tweedie)
plot(simulateResiduals(biomass_model))
summary(biomass_model)
emmeans(biomass_model, pairwise ~ week + treatment)

# Backtransforming model predictions
predict_biomass_model <- ggpredict(biomass_model, terms = c("week", "treatment")) %>%
  rename(week = x,
         biomass = predicted,
         treatment = group)

# Plotting model outputs
ggplot() +
  facet_wrap("treatment") +
  geom_jitter(data = sea_cucumber_plot_data,
             aes(x = week,
                 y = experimental_plot_biomass, 
                 colour = treatment), 
             width = 0.1,
             alpha = 0.25) +
  geom_point(data = predict_biomass_model,
             aes(x = week,
                 y = biomass,
                 colour = treatment)) +
  geom_errorbar(data = predict_biomass_model,
                aes(x = week,
                    y = biomass,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = treatment),
                width = 0.5) +
  labs(x = "Week",
       y = "Biomass (g)") +
  theme_classic()

# Plotting coefficients
dwplot(biomass_model) +
  theme_classic() +
  geom_vline(xintercept = 0,
        linetype = 2)

# MODELLING CHANGE IN SEA CUCUMBER DENSITY OVER TIME ---------------------------

# Filtering plot data by removal treatment
sea_cucumber_removal_data <- sea_cucumber_plot_data %>%
  filter(treatment == "Removal")

# Modelling change in sea cucumber density by treatment and ambient density
density_change_model <- glmmTMB(density_change ~ week * ambient_density + (1|site), sea_cucumber_removal_data)
plot(simulateResiduals(density_change_model))
summary(density_change_model)



predict_density_change_model <- ggpredict(density_change_model, terms = c("week", "ambient_density"), back_transform = FALSE)

# Plotting model predictions
density_change_plot <- ggplot(density_change_model_df,
                              aes(x = )) +
  theme_classic() +
  geom_hline(yintercept = 0,
             linetype = 2) + 
  geom_point(data = density_change_model_df,
             aes(x = variable,
                 y = estimate),
             size = 2) +
  geom_errorbar(data = density_change_model_df,
                aes(x = variable,
                    y = estimate,
                    ymin = lower_CI,
                    ymax = upper_CI),
                width = 0.5) +
  labs(x = "Week",
       y = "Change in density (#/m2)",
       colour = "Ambient density (#/m2)")

density_change_plot

# Plotting coefficients
dwplot(density_change_model) +
  geom_vline(xintercept = 0,
        linetype = 2) +
  theme_classic()
