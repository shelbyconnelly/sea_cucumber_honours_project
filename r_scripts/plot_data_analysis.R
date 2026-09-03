# Project: Sea cucumber honours project
# Modelling plot data
# Author: Shelby Connelly
# Date: 03/27/2025 - 09/03/2026

# Installing packages 
install.packages(c("tidyverse", "incase", "glmmTMB", "DHARMa", "car", 
                   "broom.mixed", "viridis", "ggeffects", "png", "patchwork"))

# Loading packages into R
library(tidyverse)
library(incase)
library(glmmTMB)
library(DHARMa)
library(car)
library(broom.mixed)
library(viridis)
library(ggeffects)
library(png)
library(patchwork)

# Loading clean transect, plot, and ROV data into R
transect_data <- read_csv("./clean_data/sea_cucumber_transect_data.csv")
plot_data <- read_csv("./clean_data/sea_cucumber_plot_data.csv")
rov_data <- read_csv("./clean_data/shallow_vs_deep_rov_data.csv")

# Converting site and treatment to factors
transect_data <- transect_data %>%
  mutate(site = fct_reorder(site, latitude))

plot_data <- plot_data %>%
  mutate(site = fct_reorder(site, latitude),
         treatment = factor(treatment, levels = c("Control", "Removal", "Addition")),
         week_discrete = as.factor(week))

# Calculating ambient sea cucumber density by site
site_transect_data <- transect_data %>%
  group_by(site, latitude, longitude) %>%
  summarise(mean_transect_density = mean(density)) %>%
  ungroup()

site_rov_data <- rov_data %>%
  group_by(site, latitude, longitude) %>%
  summarise(mean_shallow_rov_density = mean(density_shallow),
            mean_deep_rov_density = mean(density_deep)) %>%
  ungroup()

# Joining transect, plot, and ROV density data by site 
complete_site_data <- left_join(site_transect_data, plot_data,
                                join_by(site, latitude, longitude)) %>%
  left_join(., site_rov_data,
            join_by(site, latitude, longitude)) %>%
  mutate(site = fct_reorder(site, latitude))

# INITIAL PLOT DENSITY ---------------------------------------------------------

# Filtering plot data to week 0
initial_density_data <- plot_data %>%
  filter(week == 0) 

# Modelling initial plot density by latitude
initial_density_latitude_model <- glmmTMB(density_initial_sea_cucumber ~ latitude + (1|site),
                                          data = initial_density_data,
                                          family = tweedie())
plot(simulateResiduals(initial_density_latitude_model))
summary(initial_density_latitude_model)

# Plotting initial plot density by site, treatment, and latitude and saving as a .tiff file
tiff("./plots/fig_s2.tiff",
     height = 6.5,
     width = 6.5,
     units = "in",
     res = 300)

fig_s2 <- ggplot() +
  theme_classic() +
  geom_jitter(data = initial_density_data,
              aes(x = site,
                  y = density_initial_sea_cucumber,
                  colour = treatment),
              width = 0.15) +
  stat_summary(data = initial_density_data,
               aes(x = site, 
                   y = density_initial_sea_cucumber),
               fun = mean,
               size = 0.3) +
  stat_summary(data = initial_density_data,
               aes(x = site,
                   y = density_initial_sea_cucumber),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.25) +
  labs(x = NULL,
       y = expression(paste("Initial density (#/", m^2, ")")),
       colour = "Treatment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

fig_s2

dev.off()

# Grouping initial density data by site
initial_density_data_grouped <- complete_site_data %>%
  filter(week == 0) %>%
  group_by(site) %>%
  summarise(mean_initial_density = mean(density_initial_sea_cucumber),
            sd_density = sd(density_initial_sea_cucumber),
            se_density = sd_density/sqrt(n()),
            conf_low = mean_initial_density - se_density,
            conf_high = mean_initial_density + se_density,
            mean_transect_density = mean(mean_transect_density))

# Modelling transect density vs initial plot density
transect_vs_initial_density_model <- lm(mean_transect_density ~ mean_initial_density,
                                        initial_density_data_grouped)
plot(simulateResiduals(transect_vs_initial_density_model))
summary(transect_vs_initial_density_model)

# Backtransforming model predictions
transect_vs_initial_density_model_predictions <- ggpredict(transect_vs_initial_density_model,
                                                           terms = "mean_initial_density") %>%
  rename("mean_initial_density" = x,
         "mean_transect_density" = predicted)

# Plotting model predictions and saving as a .tiff file
tiff("./plots/fig_s3.tiff",
     height = 6.5,
     width = 6.5,
     units = "in",
     res = 600)

fig_s3 <- ggplot() +
  theme_classic() +
  geom_jitter(data = initial_density_data_grouped,
              aes(x = mean_initial_density,
                  y = mean_transect_density)) +
  geom_line(data = transect_vs_initial_density_model_predictions,
            aes(x = mean_initial_density,
                y = mean_transect_density)) +
  geom_ribbon(data = transect_vs_initial_density_model_predictions,
              aes(x = mean_initial_density,
                  y = mean_transect_density,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.25) +
  labs(x = expression(paste("Initial plot density (#/", m^2, ")")),
       y = expression(paste("Ambient density (#/", m^2, ")")))

fig_s3

dev.off()

# MODELLING SEA CUCUMBER DENSITY OVER TIME -------------------------------------

density_model <- glmmTMB(density_experimental_sea_cucumber ~ week_discrete * treatment + (1|site),
                         plot_data, 
                         family = tweedie())
plot(simulateResiduals(density_model))
summary(density_model)

# Plotting model coefficients
tidy_density_model <- tidy(density_model, conf.int = TRUE) %>%
  filter(!term %in% c("(Intercept)", "sd__(Intercept)")) %>%
  mutate(term = fct_recode(term, "Week 1" = "week_discrete1",
                           "Week 3" = "week_discrete3",
                           "Week 6" = "week_discrete6",
                           "Removal" = "treatmentRemoval",
                           "Addition" = "treatmentAddition",
                           "Week 1 : Removal" = "week_discrete1:treatmentRemoval",
                           "Week 3 : Removal" = "week_discrete3:treatmentRemoval",
                           "Week 6 : Removal" =  "week_discrete6:treatmentRemoval",
                           "Week 1 : Addition" = "week_discrete1:treatmentAddition",
                           "Week 3 : Addition" = "week_discrete3:treatmentAddition",
                           "Week 6 : Addition" = "week_discrete6:treatmentAddition"),
         term = fct_relevel(term, "Week 6 : Addition", "Week 3 : Addition", 
                            "Week 1 : Addition", "Week 6 : Removal",
                            "Week 3 : Removal", "Week 1 : Removal",
                            "Addition", "Removal", "Week 6", "Week 3", 
                            "Week 1"))

fig_2a <- ggplot(tidy_density_model,
                 aes(x = estimate,
                     y = term,
                     colour = term)) +
  theme_classic() +
  geom_pointrange(aes(xmin = conf.low,
                      xmax = conf.high),
                  size = 0.25) +
  geom_vline(xintercept = 0,
             linetype = 2,
             linewidth = 0.25) +
  scale_colour_manual(values = c("Week 1" = "#F8766D",
                                 "Week 3" = "#F8766D",
                                 "Week 6" = "#F8766D",
                                 "Removal" = "#00BA38",
                                 "Addition" = "#619CFF",
                                 "Week 1 : Removal" = "#00BA38",
                                 "Week 3 : Removal" = "#00BA38",
                                 "Week 6 : Removal" = "#00BA38",
                                 "Week 1 : Addition" = "#619CFF",
                                 "Week 3 : Addition" = "#619CFF",
                                 "Week 6 : Addition" = "#619CFF")) +
  labs(x = "Coefficient", 
       y = NULL,
       tag = "a") +
  theme(legend.position = "none")

fig_2a

# Backtransforming model predictions
density_model_predictions <- ggpredict(density_model,
                                                terms = c("week_discrete",
                                                          "treatment")) %>%
  rename("week" = x,
         "density" = predicted,
         "treatment" = group)

# Plotting raw data and model predictions step-by-step and saving as .tiff files
tiff("./plots/fig_2b_step_1.tiff",
     height = 14.35,
     width = 23.31,
     units = "cm",
     res = 300)

fig_2b_step_1 <- ggplot() +
  theme_classic() +
  facet_wrap("treatment") +
  theme(panel.border = element_rect(fill = NA)) +
  geom_jitter(data = plot_data %>%
                filter(treatment == "Control"),
              aes(x = week_discrete,
                  y = density_experimental_sea_cucumber,
                  colour = treatment),
              width = 0.25,
              alpha = 0.5) +
  geom_point(data = density_model_discrete_predictions %>%
               filter(treatment == "Control"),
             aes(x = week,
                 y = density,
                 colour = treatment),
             size = 3) +
  geom_errorbar(data = density_model_discrete_predictions %>%
                  filter(treatment == "Control"),
                aes(x = week,
                    y = density,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = treatment),
                width = 0.5,
                linewidth = 1) +
  theme(legend.position = "none") +
  labs(x = "Week",
       y = expression(paste("Density (#/", m^2, ")"))) +
  theme(text = element_text(size = 22))

fig_2b_step_1

dev.off()

tiff("./plots/fig_2b_step_2.tiff",
     height = 14.35,
     width = 23.31,
     units = "cm",
     res = 300)

fig_2b_step_2 <- ggplot() +
  theme_classic() +
  facet_wrap("treatment") +
  theme(panel.border = element_rect(fill = NA)) +
  geom_jitter(data = plot_data %>%
                filter(treatment %in% c("Control", "Removal")),
              aes(x = week_discrete,
                  y = density_experimental_sea_cucumber,
                  colour = treatment),
              width = 0.25,
              alpha = 0.5) +
  geom_point(data = density_model_discrete_predictions %>%
               filter(treatment %in% c("Control", "Removal")),
             aes(x = week,
                 y = density,
                 colour = treatment),
             size = 3) +
  geom_errorbar(data = density_model_discrete_predictions %>%
                  filter(treatment %in% c("Control", "Removal")),
                aes(x = week,
                    y = density,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = treatment),
                width = 0.5,
                linewidth = 1) +
  theme(legend.position = "none") +
  labs(x = "Week",
       y = expression(paste("Density (#/", m^2, ")"))) +
  theme(text = element_text(size = 22)) +
  scale_colour_manual(values = c("#F8766D", "#00BA38"))

fig_2b_step_2

dev.off()

tiff("./plots/fig_2b_step_3.tiff",
     height = 14.35,
     width = 23.31,
     units = "cm",
     res = 300)

fig_2b_step_3 <- ggplot() +
  theme_classic() +
  facet_wrap("treatment") +
  theme(panel.border = element_rect(fill = NA)) +
  geom_jitter(data = plot_data,
              aes(x = week_discrete,
                  y = density_experimental_sea_cucumber,
                  colour = treatment),
              width = 0.25,
              alpha = 0.5) +
  geom_point(data = density_model_discrete_predictions,
             aes(x = week,
                 y = density,
                 colour = treatment),
             size = 3) +
  geom_errorbar(data = density_model_discrete_predictions,
                aes(x = week,
                    y = density,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = treatment),
                width = 0.5,
                linewidth = 1) +
  theme(legend.position = "none") +
  labs(x = "Week",
       y = expression(paste("Density (#/", m^2, ")"))) +
  theme(text = element_text(size = 22))

fig_2b_step_3

dev.off()

fig_2b <- ggplot() +
  theme_classic() +
  facet_wrap("treatment") +
  theme(panel.border = element_rect(fill = NA)) +
  geom_jitter(data = plot_data,
              aes(x = week_discrete,
                  y = density_experimental_sea_cucumber,
                  colour = treatment),
              width = 0.25,
              size = 1,
              alpha = 0.5) +
  geom_point(data = density_model_discrete_predictions,
             aes(x = week,
                 y = density,
                 colour = treatment),
             size = 2) +
  geom_errorbar(data = density_model_discrete_predictions,
                aes(x = week,
                    y = density,
                    ymin = conf.low,
                    ymax = conf.high,
                    colour = treatment),
                width = 0.5,
                linewidth = 0.75) +
  theme(legend.position = "none") +
  labs(x = "Week",
       y = expression(paste("Density (#/", m^2, ")")),
       tag = "b")

fig_2b

# Combining model coefficients and predictions and saving as a .tiff file
tiff("./plots/fig_2.tiff",
     height = 6.5,
     width = 6.5,
     units = "in",
     res = 300)

fig_2 <- fig_2a/free(fig_2b)
fig_2

dev.off()

# MODELLING ∆DENSITY IN THE REMOVAL TREATMENT ----------------------------------

# Filtering plot data by removal treatment
removal_data <- complete_site_data %>%
  filter(treatment == "Removal")

# Standardizing predictors
removal_data <- removal_data %>%
  mutate(mean_transect_density_standardized = scale(mean_transect_density),
         mean_deep_rov_density_standardized = scale(mean_deep_rov_density),
         mean_shallow_rov_density_standardized = scale(mean_shallow_rov_density))

# Calculating representative mean values for low, medium, and high density sites
representative_transect_densities <- removal_data %>%
  mutate(transect_density_level = case_when(mean_transect_density < 0.1 ~ "Low",
                                            mean_transect_density < 0.5 ~ "Medium",
                                            mean_transect_density < 1 ~ "High")) %>%
  group_by(transect_density_level) %>%
  summarise(representative_transect_density = mean(mean_transect_density_standardized))

representative_shallow_rov_densities <- removal_data %>%
  mutate(shallow_rov_density_level = case_when(mean_shallow_rov_density < 0.1 ~ "Low",
                                               mean_shallow_rov_density < 0.34 ~ "Medium",
                                               mean_shallow_rov_density < 1 ~ "High")) %>%
  group_by(shallow_rov_density_level) %>%
  summarise(representative_shallow_rov_density = mean(mean_shallow_rov_density_standardized))

representative_deep_rov_densities <- removal_data %>%
  mutate(deep_rov_density_level = case_when(mean_deep_rov_density < 0.1 ~ "Low",
                                            mean_deep_rov_density < 0.5 ~ "Medium",
                                            mean_deep_rov_density < 2 ~ "High")) %>%
  group_by(deep_rov_density_level) %>%
  summarise(representative_deep_rov_density = mean(mean_deep_rov_density_standardized))

removal_data <- removal_data %>%
  mutate(transect_density_level = in_case_fct(mean_transect_density < 0.1 ~ -1.0310611,
                                              mean_transect_density < 0.5 ~ -0.2385017,
                                              mean_transect_density < 1 ~ 1.26956282,
                                              .ordered = T),
         shallow_rov_density_level = in_case_fct(mean_shallow_rov_density < 0.1 ~ -0.8882844,
                                                 mean_shallow_rov_density < 0.34 ~ -0.7149111,
                                                 mean_shallow_rov_density < 1 ~ 0.3643177,
                                                 .ordered = T),
         deep_rov_density_level = in_case_fct(mean_deep_rov_density < 0.1 ~ -0.84764153,
                                              mean_deep_rov_density < 0.5 ~ -0.27970181,
                                              mean_deep_rov_density < 2 ~ 1.1273433,
                                              .ordered = T))

# Modelling change in sea cucumber density by week, transect density, and deep ROV density
density_change_model <- glmmTMB(density_change ~ week * mean_transect_density_standardized + week * mean_deep_rov_density_standardized + (1|site),
                                removal_data)
plot(simulateResiduals(density_change_model))
summary(density_change_model)

# Checking model for multicollinearity in predictors
vif(lm(density_change ~ week * I(scale(mean_transect_density)) + week * I(scale(mean_deep_rov_density_standardized)),
       removal_data))

# Plotting model coefficients
tidy_density_change_model <- tidy(density_change_model, conf.int = TRUE) %>%
  filter(effect == "fixed",
         !term %in% c("(Intercept)", "sd__(Intercept)", "sd__(Observation")) %>%
  mutate(term = fct_recode(term, "Week" = "week",
                           "Shallow (≤ 12 m) density" = "mean_transect_density_standardized",
                           "Deep (> 12 m) density" = "mean_deep_rov_density_standardized",
                           "Week : Shallow (≤ 12 m) density" = "week:mean_transect_density_standardized",
                           "Week : Deep (> 12 m) density" = "week:mean_deep_rov_density_standardized"),
         term = fct_relevel(term, "Week : Deep (> 12 m) density",
                            "Week : Shallow (≤ 12 m) density",
                            "Deep (> 12 m) density", "Shallow (≤ 12 m) density",
                            "Week"))

fig_3a <- ggplot(tidy_density_change_model,
                 aes(x = estimate,
                     y = term,
                     colour = term)) +
  theme_classic() +
  geom_pointrange(aes(xmin = conf.low,
                      xmax = conf.high),
                  size = 0.25) +
  geom_vline(xintercept = 0,
             linetype = 2,
             linewidth = 0.25) +
  scale_colour_manual(values = c("Week" = "black",
                                 "Shallow (≤ 12 m) density" = "#FB8861FF", 
                                 "Deep (> 12 m) density" = "#25848EFF", 
                                 "Week : Shallow (≤ 12 m) density" = "#FB8861FF",
                                 "Week : Deep (> 12 m) density" = "#25848EFF")) +
  labs(x = "Coefficient", 
       y = NULL,
       tag = "a") +
  theme(legend.position = "none")

fig_3a

# Backtransforming model predictions by representative transect density
density_change_vs_transect_predictions <- ggpredict(density_change_model,
                                                    terms = c("week",
                                                              "mean_transect_density_standardized [-1.0310611, -0.2385017, 1.26956282]")) %>%
  rename("week" = x,
         "density_change" = predicted,
         "transect_density_level" = group)

# Plotting raw data and model predictions by transect density step-by-step and saving as .tiff files
tiff("./plots/fig_3b_step_1.tiff",
     height = 14.35,
     width = 33.86,
     units = "cm",
     res = 300)
     
fig_3b_step_1 <- ggplot() +
  theme_classic() +
  geom_jitter(data = removal_data %>%
                filter(transect_density_level == -1.0310611),
              aes(x = week,
                  y = density_change,
                  colour = transect_density_level),
              width = 0.25,
              size = 2) + 
  geom_line(data = density_change_vs_transect_predictions %>%
              filter(transect_density_level == -1.0310611),
            aes(x = week,
                y = density_change,
                colour = transect_density_level),
            linewidth = 2) +
  geom_ribbon(data = density_change_vs_transect_predictions %>%
                filter(transect_density_level == -1.0310611),
              aes(x = week,
                  y = density_change,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = transect_density_level),
              alpha = 0.25) +
  geom_hline(yintercept = 0,
             linetype = 2,
             linewidth = 0.75) +
  scale_colour_viridis_d(option = "magma", begin = 0.9, end = 0.6,
                         labels = c("Low", "Medium", "High")) +
  scale_fill_viridis_d(option = "magma", begin = 0.9, end = 0.6,
                       labels = c("Low", "Medium", "High")) +
  labs(x = "Week",
       y = expression(paste("∆ density (#/", m^2, ")")),
       colour = "Shallow (≤ 12 m) density",
       fill = "Shallow (≤ 12 m) density") +
  theme(text = element_text(size = 22)) +
  ylim(-0.8, 0.5) 


fig_3b_step_1

dev.off()

tiff("./plots/fig_3b_step_2.tiff",
     height = 14.35,
     width = 33.86,
     units = "cm",
     res = 300)

fig_3b_step_2 <- ggplot() +
  theme_classic() +
  geom_jitter(data = removal_data %>%
                filter(transect_density_level %in% c(-1.0310611, -0.2385017)),
              aes(x = week,
                  y = density_change,
                  colour = transect_density_level),
              width = 0.25,
              size = 2) + 
  geom_line(data = density_change_vs_transect_predictions %>%
              filter(transect_density_level %in% c(-1.0310611, -0.2385017)),
            aes(x = week,
                y = density_change,
                colour = transect_density_level),
            linewidth = 2) +
  geom_ribbon(data = density_change_vs_transect_predictions %>%
                filter(transect_density_level %in% c(-1.0310611, -0.2385017)),
              aes(x = week,
                  y = density_change,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = transect_density_level),
              alpha = 0.25) +
  geom_hline(yintercept = 0,
             linetype = 2,
             linewidth = 0.75) +
  scale_colour_viridis_d(option = "magma", begin = 0.9, end = 0.75,
                         labels = c("Low", "Medium", "High")) +
  scale_fill_viridis_d(option = "magma", begin = 0.9, end = 0.75,
                       labels = c("Low", "Medium", "High")) +
  labs(x = "Week",
       y = expression(paste("∆ density (#/", m^2, ")")),
       colour = "Shallow (≤ 12 m) density",
       fill = "Shallow (≤ 12 m) density") +
  theme(text = element_text(size = 22)) +
  ylim(-0.8, 0.5) 

fig_3b_step_2

dev.off()

tiff("./plots/fig_3b_step_3.tiff",
     height = 14.35,
     width = 33.86,
     units = "cm",
     res = 300)

fig_3b_step_3 <- ggplot() +
  theme_classic() +
  geom_jitter(data = removal_data,
              aes(x = week,
                  y = density_change,
                  colour = transect_density_level),
              width = 0.25,
              size = 2) + 
  geom_line(data = density_change_vs_transect_predictions,
            aes(x = week,
                y = density_change,
                colour = transect_density_level),
            linewidth = 2) +
  geom_ribbon(data = density_change_vs_transect_predictions,
              aes(x = week,
                  y = density_change,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = transect_density_level),
              alpha = 0.25) +
  geom_hline(yintercept = 0,
             linetype = 2,
             linewidth = 0.75) +
  scale_colour_viridis_d(option = "magma", begin = 0.9, end = 0.6,
                         labels = c("Low", "Medium", "High")) +
  scale_fill_viridis_d(option = "magma", begin = 0.9, end = 0.6,
                       labels = c("Low", "Medium", "High")) +
  labs(x = "Week",
       y = expression(paste("∆ density (#/", m^2, ")")),
       colour = "Shallow (≤ 12 m) density",
       fill = "Shallow (≤ 12 m) density") +
  theme(text = element_text(size = 22))

fig_3b_step_3

dev.off()

fig_3b <- ggplot() +
  theme_classic() +
  geom_jitter(data = removal_data,
              aes(x = week,
                  y = density_change,
                  colour = transect_density_level),
              width = 0.25,
              size = 1) +
  geom_line(data = density_change_vs_transect_density_predictions,
            aes(x = week,
                y = density_change,
                colour = transect_density_level)) +
  geom_ribbon(data = density_change_vs_transect_density_predictions,
              aes(x = week,
                  y = density_change,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = transect_density_level),
              alpha = 0.25) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  scale_colour_viridis_d(option = "magma", begin = 0.9, end = 0.6,
                         labels = c("Low", "Medium", "High")) +
  scale_fill_viridis_d(option = "magma", begin = 0.9, end = 0.6,
                       labels = c("Low", "Medium", "High")) +
  labs(x = "Week",
       y = expression(paste("∆ density (#/", m^2, ")")),
       colour = "Shallow (≤ 12 m) density",
       fill = "Shallow (≤ 12 m) density",
       tag = "b")

fig_3b

# Backtransforming model predictions by deep ROV density
density_change_vs_deep_rov_predictions <- ggpredict(density_change_model,
                                                               terms = c("week",
                                                                         "mean_deep_rov_density_standardized [-0.84764153, -0.27970181, 1.1273433]")) %>%
  rename("week" = x,
         "density_change" = predicted,
         "deep_rov_density_level" = group)

# Plotting raw data and model predictions by deep ROV density step-by-step and saving as .tiff files
tiff("./plots/fig_3c_step_1.tiff",
     height = 14.35,
     width = 33.86,
     units = "cm",
     res = 300)

fig_3c_step_1 <- ggplot() +
  theme_classic() +
  geom_jitter(data = removal_data %>%
                filter(deep_rov_density_level == -0.84764153),
              aes(x = week,
                  y = density_change,
                  colour = deep_rov_density_level),
              width = 0.25,
              size = 2) + 
  geom_line(data = density_change_vs_deep_rov_predictions %>%
              filter(deep_rov_density_level == -0.84764153),
            aes(x = week,
                y = density_change,
                colour = deep_rov_density_level),
            linewidth = 2) +
  geom_ribbon(data = density_change_vs_deep_rov_predictions %>%
                filter(deep_rov_density_level == -0.84764153),
              aes(x = week,
                  y = density_change,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = deep_rov_density_level),
              alpha = 0.25) +
  geom_hline(yintercept = 0,
             linetype = 2,
             linewidth = 0.75) +
  scale_colour_viridis_d(option = "viridis", begin = 0.8, end = 0.1,
                         labels = c("Low", "Medium", "High")) +
  scale_fill_viridis_d(option = "viridis", begin = 0.8, end = 0.1,
                       labels = c("Low", "Medium", "High")) +
  labs(x = "Week",
       y = expression(paste("∆ density (#/", m^2, ")")),
       colour = "Deep (> 12 m) density",
       fill = "Deep (> 12 m) density") +
  theme(text = element_text(size = 22)) +
  ylim(-0.75, 0.5)

fig_3c_step_1

dev.off()

tiff("./plots/fig_3c_step_2.tiff",
     height = 14.35,
     width = 33.86,
     units = "cm",
     res = 300)

fig_3c_step_2 <- ggplot() +
  theme_classic() +
  geom_jitter(data = removal_data %>%
                filter(deep_rov_density_level %in% c(-0.84764153, -0.27970181)),
              aes(x = week,
                  y = density_change,
                  colour = deep_rov_density_level),
              width = 0.25,
              size = 2) + 
  geom_line(data = density_change_vs_deep_rov_predictions %>%
              filter(deep_rov_density_level %in% c(-0.84764153, -0.27970181)),
            aes(x = week,
                y = density_change,
                colour = deep_rov_density_level),
            linewidth = 2) +
  geom_ribbon(data = density_change_vs_deep_rov_predictions %>%
                filter(deep_rov_density_level %in% c(-0.84764153, -0.27970181)),
              aes(x = week,
                  y = density_change,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = deep_rov_density_level),
              alpha = 0.25) +
  geom_hline(yintercept = 0,
             linetype = 2,
             linewidth = 0.75) +
  scale_colour_viridis_d(option = "viridis", begin = 0.8, end = 0.45,
                         labels = c("Low", "Medium", "High")) +
  scale_fill_viridis_d(option = "viridis", begin = 0.8, end = 0.45,
                       labels = c("Low", "Medium", "High")) +
  labs(x = "Week",
       y = expression(paste("∆ density (#/", m^2, ")")),
       colour = "Deep (> 12 m) density",
       fill = "Deep (> 12 m) density") +
  theme(text = element_text(size = 22)) +
  ylim(-0.75, 0.5)

fig_3c_step_2

dev.off()

tiff("./plots/fig_3c_step_3.tiff",
     height = 14.35,
     width = 33.86,
     units = "cm",
     res = 300)

fig_3c_step_3 <- ggplot() +
  theme_classic() +
  geom_jitter(data = removal_data,
              aes(x = week,
                  y = density_change,
                  colour = deep_rov_density_level),
              width = 0.25,
              size = 2) + 
  geom_line(data = density_change_vs_deep_rov_predictions,
            aes(x = week,
                y = density_change,
                colour = deep_rov_density_level),
            linewidth = 2) +
  geom_ribbon(data = density_change_vs_deep_rov_predictions,
              aes(x = week,
                  y = density_change,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = deep_rov_density_level),
              alpha = 0.25) +
  geom_hline(yintercept = 0,
             linetype = 2,
             linewidth = 0.75) +
  scale_colour_viridis_d(option = "viridis", begin = 0.8, end = 0.1,
                         labels = c("Low", "Medium", "High")) +
  scale_fill_viridis_d(option = "viridis", begin = 0.8, end = 0.1,
                       labels = c("Low", "Medium", "High")) +
  labs(x = "Week",
       y = expression(paste("∆ density (#/", m^2, ")")),
       colour = "Deep (> 12 m) density",
       fill = "Deep (> 12 m) density") +
  theme(text = element_text(size = 22))

fig_3c_step_3

dev.off()

fig_3c <- ggplot() +
  theme_classic() +
  geom_jitter(data = removal_data,
              aes(x = week,
                  y = density_change,
                  colour = deep_rov_density_level),
              width = 0.25,
              size = 1) + 
  geom_line(data = density_change_vs_deep_rov_predictions,
            aes(x = week,
                y = density_change,
                colour = deep_rov_density_level)) +
  geom_ribbon(data = density_change_vs_deep_rov_predictions,
              aes(x = week,
                  y = density_change,
                  ymin = conf.low,
                  ymax = conf.high,
                  fill = deep_rov_density_level),
              alpha = 0.25) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  scale_colour_viridis_d(option = "viridis", begin = 0.8, end = 0.1,
                         labels = c("Low", "Medium", "High")) +
  scale_fill_viridis_d(option = "viridis", begin = 0.8, end = 0.1,
                       labels = c("Low", "Medium", "High")) +
  labs(x = "Week",
       y = expression(paste("∆ density (#/", m^2, ")")),
       colour = "Deep (> 12 m) density",
       fill = "Deep (> 12 m) density",
       tag = "c")

fig_3c

# Adding diver and ROV icons
diver_icon <- readPNG("./plots/diver.png",
                      native = T)

rov_icon <- readPNG("./plots/rov.png",
                    native = T)

# Combining model coefficients, predictions, and icons and saving as a .tiff file
tiff("./plots/fig_3.tiff",
     height = 5.5,
     width = 6.5,
     units = "in",
     res = 300)

fig_3b_full <- fig_3b + diver_icon + plot_layout(widths = c(2, 1))
fig_3b_full

fig_3c_full <- fig_3c + rov_icon + plot_layout(widths = c(2, 1))
fig_3c_full

fig_3 <- fig_3a / free(fig_3b_full) / free(fig_3c_full)

fig_3 

dev.off()

# Modelling ∆density by week, shallow ROV density, and deep ROV density
density_change_model_supplemental <- glmmTMB(density_change ~ week * mean_shallow_rov_density + week * mean_deep_rov_density + (1|site),
                                             removal_data)
plot(simulateResiduals(density_change_model_supplemental))
summary(density_change_model_supplemental)

# Checking model for multicollinearity in predictors
vif(lm(density_change ~ week * mean_shallow_rov_density + week * mean_deep_rov_density,
       removal_data))

# SUPPLEMENTAL FIGURES ---------------------------------------------------------

# Plotting ∆density by site and saving as a .tiff file
tiff("./plots/fig_s4.tiff",
     height = 6.5,
     width = 6.5,
     units = "in",
     res = 300)

fig_s4 <- ggplot(complete_site_data,
                 aes(x = week,
                     y = density_change,
                     colour = treatment)) +
  theme_classic() +
  facet_wrap("site") +
  theme(panel.border = element_rect(fill = NA)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = 2) +
  labs(x = "Week",
       y = expression(paste("∆ density (#/", m^2, ")")),
       colour = "Treatment")

fig_s4

dev.off()

# Exporting complete site data frame as .csv file for site mapping -------------
write_csv(complete_site_data, "./clean_data/complete_site_data.csv")
