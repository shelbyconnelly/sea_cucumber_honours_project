# Project: Sea cucumber honours project
# Cleaning raw plot data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 03/18/2025 - 05/13/2025

# Installing packages
install.packages(c("tidyverse", "DHARMa"))

# Loading packages into R 
library(tidyverse)
library(DHARMa)

# Loading site and plot data into R 
site_data <- read_csv("./raw_data/site_data.csv")
raw_plot_data <- read_csv("./raw_data/raw_plot_data.csv")
raw_plot_size_data <- read_csv("./raw_data/raw_plot_size_data.csv")

# CLEANING RAW DATA ------------------------------------------------------------

# Joining site and plot data
clean_plot_data <- right_join(site_data, raw_plot_data, join_by(site))
clean_plot_size_data <- right_join(site_data, raw_plot_size_data, join_by(site))

# Deleting unnecessary columns and relabelling site and treatment levels
clean_plot_data <- clean_plot_data %>%
  select(-c(surveyor, buddy, weather, visibility, notes)) %>%
  mutate(site = case_match(site, "boulder_island" ~ "Boulder Island",
                           "jug_island" ~ "Jug Island",
                           "twin_islands" ~ "Twin Islands",
                           "brighton_beach" ~ "Brighton Beach",
                           "old_buntzen_power_plant" ~ "Old Buntzen PP",
                           "best_point" ~ "Best Point",
                           "south_johnson_bay" ~ "S Johnson Bay",
                           "south_croker_island" ~ "S Croker Island",
                           "north_croker_island" ~ "N Croker Island"),
         treatment = case_match(treatment, "control" ~ "Control",
                                "removal" ~ "Removal",
                                "addition" ~ "Addition"))

clean_plot_size_data <- clean_plot_size_data %>%
  select(-c(surveyor, buddy, weather, visibility)) %>%
  mutate(site = case_match(site, "boulder_island" ~ "Boulder Island",
                           "jug_island" ~ "Jug Island",
                           "twin_islands" ~ "Twin Islands",
                           "brighton_beach" ~ "Brighton Beach",
                           "old_buntzen_power_plant" ~ "Old Buntzen PP",
                           "best_point" ~ "Best Point",
                           "south_johnson_bay" ~ "S Johnson Bay",
                           "south_croker_island" ~ "S Croker Island",
                           "north_croker_island" ~ "N Croker Island"),
         treatment = case_match(treatment, "control" ~ "Control",
                                "removal" ~ "Removal",
                                "addition" ~ "Addition"),
         initial_treatment = case_match(initial_treatment, "control" ~ "Control",
                                        "removal" ~ "Removal",
                                        "addition" ~ "Addition"))

# Calculating survey time and mean tide
clean_plot_data <- clean_plot_data %>%
  rowwise %>%
  mutate(survey_time = as.numeric((end_time - start_time), units = "mins"),
         mean_tide = rowMeans(cbind(start_tide, end_tide), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-c(start_time, end_time, start_tide, end_tide))

clean_plot_size_data <- clean_plot_size_data %>%
  rowwise %>%
  mutate(survey_time = as.numeric((end_time - start_time), units = "mins"),
         mean_tide = rowMeans(cbind(start_tide, end_tide), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-c(start_time, end_time, start_tide, end_tide))

# Calculating plot depth at chart datum and plot slope
plot_depth_data <- clean_plot_data %>%
  filter(week == 1) %>%
  rowwise() %>%
  mutate(chart_datum = (middle_depth - mean_tide),
         slope = (bottom_depth - top_depth)/5) %>%
  ungroup() %>%
  select(c(site, treatment, chart_datum, slope))

# Joining plot and depth data
clean_plot_data <- left_join(clean_plot_data, plot_depth_data, join_by(site, treatment)) %>%
  select(-c(top_depth, middle_depth, bottom_depth))

clean_plot_size_data <- left_join(clean_plot_size_data, plot_depth_data, join_by(site, treatment)) %>%
  select(-c(top_depth, middle_depth, bottom_depth))

# CALCULATING DENSITY ----------------------------------------------------------

# Calculating sea cucumber and sea star densities by plot
clean_plot_data <- clean_plot_data %>%
  pivot_longer(c(initial_sea_cucumber, experimental_sea_cucumber, blood_star, 
                 leather_star, mottled_star,ochre_star, pink_star, 
                 sunflower_star),
               names_to = "species",
               values_to = "abundance") %>%
  mutate(density = abundance/25)

# Calculating change from initial to experimental sea cucumber density
sea_cucumber_plot_data <- clean_plot_data %>%
  filter(species %in% c("initial_sea_cucumber", "experimental_sea_cucumber")) %>%
  pivot_wider(names_from = species,
              values_from = c(abundance, density)) %>%
  rowwise() %>%
  mutate(density_change = (density_experimental_sea_cucumber - density_initial_sea_cucumber)) %>%
  ungroup()

# CALCULATING BIOMASS ----------------------------------------------------------

# Loading Em's sea cucumber excretion data into R
sea_cucumber_excretion_data <- read_csv("./biomass_calculations/all_cuke_excretion.csv")

# Modelling relationship between sea cucumber size index and biomass 
sea_cucumber_size_model <- lm(weight_g ~ size_index, sea_cucumber_excretion_data)
plot(simulateResiduals(sea_cucumber_size_model))
summary(sea_cucumber_size_model)

# Saving model coefficients
int <- coef(sea_cucumber_size_model)[1]
slope <- coef(sea_cucumber_size_model)[2]

# Calculating sea cucumber size index and biomass
clean_plot_size_data <- clean_plot_size_data %>%
  mutate(size_index = sqrt(length * circumference),
         biomass = (slope * size_index) + int)

# Calculating mean initial and experimental sea cucumber biomass
initial_biomass_data <- clean_plot_size_data %>%
  filter(week == 0) %>%
  group_by(site, initial_treatment) %>%
  summarise(mean_initial_biomass = mean(biomass)) %>%
  rename(treatment = initial_treatment)

experimental_biomass_data <- clean_plot_size_data %>%
  group_by(site, week, treatment) %>%
  summarise(mean_experimental_biomass = mean(biomass))

# Joining sea cucumber density and biomass data
sea_cucumber_plot_data <- left_join(sea_cucumber_plot_data, initial_biomass_data, join_by(site, treatment))
sea_cucumber_plot_data <- left_join(sea_cucumber_plot_data, experimental_biomass_data, join_by(site, week, treatment)) %>%
  mutate(mean_initial_biomass = if_else(abundance_initial_sea_cucumber == 0, 0, mean_initial_biomass),
         mean_experimental_biomass = if_else(abundance_experimental_sea_cucumber == 0, 0, mean_experimental_biomass))

# Calculating change from initial to experimental plot biomass
sea_cucumber_plot_data <- sea_cucumber_plot_data %>%
  rowwise() %>%
  mutate(total_initial_biomass = abundance_initial_sea_cucumber * mean_initial_biomass / 25,
         total_experimental_biomass = abundance_experimental_sea_cucumber * mean_experimental_biomass / 25,
         biomass_change = total_experimental_biomass - total_initial_biomass) %>%
  ungroup()

# Downloading data frames as .csv files ----------------------------------------
write_csv(clean_plot_data, "./clean_data/clean_plot_data.csv")
write_csv(clean_plot_size_data, "./clean_data/clean_plot_size_data.csv")
write_csv(sea_cucumber_plot_data, "./clean_data/sea_cucumber_plot_data.csv")
