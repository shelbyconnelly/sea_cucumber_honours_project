# Project: Sea cucumber honours project
# Modelling plot data
# Author: Shelby Connelly
# Date: 03/27/2025 - 04/15/2025

# Installing packages 
install.packages(c("tidyverse", "glmmTMB", "DHARMa", "ggeffects", "emmeans", "dotwhisker"))

# Loading packages into R
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(emmeans)
library(dotwhisker)

# Loading plot data into R
clean_plot_data <- read_csv("./clean_data/clean_plot_data.csv")
clean_plot_size_data <- read_csv("./clean_data/clean_plot_size_data.csv")
sea_cucumber_plot_data <- read_csv("./clean_data/sea_cucumber_plot_data.csv")

# Converting site, week, and treatment to factors
clean_plot_data <- clean_plot_data %>%
  mutate(site = fct_reorder(site, latitude),
         week = as.factor(week),
         treatment = factor(treatment, levels = c("control", "removal", "addition")))

clean_plot_size_data <- clean_plot_size_data %>%
  mutate(site = fct_reorder(site, latitude),
         week = as.factor(week),
         treatment = factor(treatment, levels = c("control", "removal", "addition")))

sea_cucumber_plot_data <- sea_cucumber_plot_data %>%
  mutate(site = fct_reorder(site, latitude),
         week = as.factor(week),
         treatment = factor(treatment, levels = c("control", "removal", "addition")))

# INITIAL PLOT DATA ---------------------------------------------------------

# Plotting initial sea cucumber density by site 
initial_sea_cucumber_plot_data <- sea_cucumber_plot_data %>%
  filter(week == 0)

ggplot(initial_sea_cucumber_plot_data,
       aes(x = site,
           y = density_initial_sea_cucumber)) +
  theme_classic() +
  geom_point(alpha = 0.25) +
  stat_summary(fun = mean) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.5)

# Plotting initial sea cucumber biomass by site
ggplot(initial_sea_cucumber_plot_data,
       aes(x = site,
           y = initial_plot_biomass)) +
  theme_classic() +
  geom_point(alpha = 0.25) +
  stat_summary(fun = mean) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.5)

# Grouping sites by ambient density and biomass
sea_cucumber_plot_data <- sea_cucumber_plot_data %>%
  mutate(ambient_density = factor(case_when(site %in% c("boulder_island", "jug_island", "twin_islands") ~ "low",
                                     site %in% c("brighton_beach", "old_buntzen_power_plant", "best_point") ~ "medium",
                                     site %in% c("south_johnson_bay", "south_croker_island", "north_croker_island") ~ "high")),
         ambient_biomass = factor(case_when(site %in% c("boulder_island", "jug_island", "twin_islands") ~ "low",
                                     site %in% c("brighton_beach", "old_buntzen_power_plant", "best_point") ~ "medium",
                                     site %in% c("south_johnson_bay", "south_croker_island", "north_croker_island") ~ "high")))

# MODELLING SEA CUCUMBER DENSITY OVER TIME -------------------------------------

# Modelling sea cucumber density over time by treatment 
density_model <- glmmTMB(density_experimental_sea_cucumber ~ week * treatment + (1|site), 
                                      sea_cucumber_plot_data,
                                      family = tweedie(link = "log"))
plot(simulateResiduals(sea_cucumber_density_model))
summary(sea_cucumber_density_model)

emmeans(density_model, pairwise ~ week * treatment)

# Backtransforming model predictions 
predict_sea_cucumber_density_model <- ggpredict(sea_cucumber_density_model, terms = c("week", "treatment")) %>%
  rename(week = x,
         density = predicted,
         treatment = group)

# Plotting model outputs
sea_cucumber_density_plot <- ggplot() +
  theme_classic() +
  facet_wrap("treatment", labeller = as_labeller(c("control" = "Control", "removal" = "Removal", "addition" = "Addition"))) +
  geom_jitter(data = sea_cucumber_plot_data,
              aes(x = week,
                  y = density_experimental_sea_cucumber,
                  colour = treatment),
              width = 0.1,
              alpha = 0.25) +
  geom_point(data = predict_sea_cucumber_density_model,
             aes (x = week,
                  y = density,
                  colour = treatment),
             size = 2) +
  geom_errorbar(data = predict_sea_cucumber_density_model,
                aes(x = week,
                    y = density,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = treatment),
                width = 0.5,
                linewidth = 1) +
  labs(x = "Week",
       y = "Density")
  
sea_cucumber_density_plot

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

# Modelling change in sea cucumber density by treatment and ambient density
density_change_model <- glmmTMB(density_change ~ (week * treatment) + (week * ambient_density) + (treatment * ambient_density) + (1|site), sea_cucumber_plot_data)
plot(simulateResiduals(density_change_model))
summary(density_change_model)

emmeans(density_change_model)

# Backtransforming model predictions
predict_density_change_model <- ggpredict(density_change_model, terms = c("week", "treatment", "ambient_density")) %>%
  rename(week = x,
         density_change = predicted,
         treatment = group,
         ambient_density = facet)

# Plotting model output
density_change_plot <- ggplot() +
  theme_classic() +
  facet_wrap("treatment") +
  geom_hline(yintercept = 0,
             linetype = 2) + 
  geom_jitter(data = sea_cucumber_plot_data,
              aes(x = week,
                  y = density_change,
                  colour = ambient_density),
              width = 0.1,
              alpha = 0.25) +
  geom_point(data = predict_density_change_model,
             aes(x = week,
                 y = density_change,
                 colour = ambient_density),
             size = 2) +
  geom_errorbar(data = predict_density_change_model,
                aes(x = week,
                    y = density_change,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = ambient_density),
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

# PART 4: CHANGE IN BIOMASS OVER TIME ------------------------------------------

# Modelling change in biomass by treatment and ambient biomass 
biomass_change_model <- glmmTMB(biomass_change ~ week * treatment + (1|site), sea_cucumber_plot_data)
plot(simulateResiduals(biomass_change_model))
summary(biomass_change_model)

# Backtransforming model predictions
predict_biomass_change_model <- ggpredict(biomass_change_model, terms = c("week", "treatment")) %>%
  rename(week = x,
         biomass_change = predicted,
         treatment = group)

# Plotting model outputs
ggplot() +
  facet_wrap("treatment") +
  geom_jitter(data = sea_cucumber_plot_data,
             aes(x = week,
                 y = biomass_change,
                 colour = treatment),
             width = 0.1,
             alpha = 0.25) +
  geom_point(data = predict_biomass_change_model,
             aes(x = week,
                 y = biomass_change,
                 colour = treatment)) +
  geom_errorbar(data = predict_biomass_change_model,
                aes(x = week,
                    y = biomass_change,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = treatment),
                width = 0.5) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  labs(x = "Week",
       y = "Change in biomass (g)") +
  theme_classic()

# Plotting coefficients
dwplot(biomass_change_model) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_classic()

# PART 5: SUPPLEMENTAL PLOTS ---------------------------------------------------

# Plotting initial sea cucumber biomass by site
initial_plot_data <- sea_cucumber_plot_data %>%
  filter(week == 0)

ggplot(initial_plot_data,
       aes(x = reorder(site, latitude),
           y = initial_plot_biomass)) +
  geom_jitter(width = 0.1,
              alpha = 0.25) +
  stat_summary(fun = mean) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.5) +
  labs(x = "Site",
       y = "Biomass (g)") +
  scale_x_discrete(labels = c("Boulder Island", "Jug Island", "Twin Islands", 
                              "Brighton Beach", "Old Buntzen PP", "Best Point", 
                              "S Johnson Bay", "S Croker Island", "N Croker Island")) +
  theme_classic()

# Plotting sea cucumber density over time by treatment and site
ggplot(sea_cucumber_plot_data,
       aes(x = week,
           y = density_experimental_sea_cucumber,
           colour = treatment)) +
  facet_wrap("site") +
  theme_classic() +
  geom_point() +
  labs(x = "Week",
       y = "Density (#/m2)")
