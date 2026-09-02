# Project: Sea cucumber honours project
# Cleaning raw ROV data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 02/10/2026 - 09/02/2026

# Installing packages
install.packages(c("tidyverse", "glmmTMB", "DHARMa", "ggeffects"))

# Loading packages into R
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(ggeffects)

# Loading clean ROV and transect data into R
rov_data <- read_csv("./clean_data/shallow_vs_deep_rov_data.csv")
transect_data <- read_csv("./clean_data/sea_cucumber_transect_data.csv")

# COMPARING SHALLOW VS DEEP ROV DENSITIES --------------------------------------

# Modelling shallow vs deep ROV densities
shallow_vs_deep_rov_model <- glmmTMB(density_deep ~ density_shallow + (1|site),
                                     data = rov_data,
                                     family = "tweedie")
plot(simulateResiduals(shallow_vs_deep_rov_model))
summary(shallow_vs_deep_rov_model)

# Backtransforming model predictions
shallow_vs_deep_rov_model_predictions <- ggpredict(shallow_vs_deep_rov_model,
                                                 terms = "density_shallow") %>%
  rename(density_shallow = x,
         density_deep = predicted)

# Plotting raw data and model predictions
fig_s1a <- ggplot() +
  theme_classic() +
  geom_jitter(data = rov_data,
              aes(x = density_shallow,
                  y = density_deep),
              colour = "#25848EFF") +
  geom_line(data = shallow_vs_deep_rov_model_predictions,
            aes(x = density_shallow,
                y = density_deep),
            colour = "#25848EFF") +
  geom_ribbon(data = shallow_vs_deep_rov_model_predictions,
              aes(x = density_shallow,
                  y = density_deep,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.25,
              fill = "#25848EFF") +
  labs(x = NULL,
       y = "Deep ROV (> 12 m) density (#/min)",
       tag = "a") +
  theme(legend.position = "none")

fig_s1a

# COMPARING TRANSECT VS ROV DENSITIES ------------------------------------------

# Calculating mean sea cucumber density by site and depth category
mean_transect_data <- transect_data %>%
  group_by(site, latitude, longitude) %>%
  summarise(mean_transect_density = mean(density))

mean_rov_data <- rov_data %>%
  group_by(site, latitude, longitude) %>%
  summarise(n = n(),
            mean_shallow_rov_density = mean(density_shallow),
            sd_shallow_rov_density = sd(density_shallow),
            mean_deep_rov_density = mean(density_deep),
            sd_deep_rov_density = sd(density_deep))

# Joining transect and ROV data
mean_site_data <- left_join(mean_transect_data, mean_rov_data,
                            join_by(site, latitude, longitude))

# Modelling transect vs shallow ROV densities
transect_vs_shallow_rov_model <- lm(mean_transect_density ~ mean_shallow_rov_density,
                                    data = mean_site_data)
plot(simulateResiduals(transect_vs_shallow_rov_model))
summary(transect_vs_shallow_rov_model)

# Backtransforming model predictions
transect_vs_shallow_rov_model_predictions <- ggpredict(transect_vs_shallow_rov_model,
                                                       terms = "mean_shallow_rov_density") %>%
  rename("mean_shallow_rov_density" = x,
         "mean_transect_density" = predicted)

# Plotting mean values and model predictions
fig_s1b <- ggplot() +
  theme_classic() +
  geom_jitter(data = mean_site_data,
              aes(x = mean_shallow_rov_density,
                  y = mean_transect_density),
              colour = "#FB8861FF") +
  geom_line(data = transect_vs_shallow_rov_model_predictions,
            aes(x = mean_shallow_rov_density,
                y = mean_transect_density),
            colour = "#FB8861FF") +
  geom_ribbon(data = transect_vs_shallow_rov_model_predictions,
              aes(x = mean_shallow_rov_density,
                  y = mean_transect_density,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.25,
              fill = "#FB8861FF") +
  labs(x = "Shallow ROV (5-12 m) density (#/min)",
       y = expression(paste("SCUBA (≤ 12 m) density (#/", m^2, ")")),
       tag = "b") 

fig_s1b

# FINAL PLOT -------------------------------------------------------------------

# Combining Figure S1a and b and saving as a .tiff file
tiff("./plots/fig_s1.tiff",
     height = 6.5,
     width = 6.5,
     units = "in",
     res = 600)

fig_s1 <- fig_s1a / fig_s1b

fig_s1

dev.off()
