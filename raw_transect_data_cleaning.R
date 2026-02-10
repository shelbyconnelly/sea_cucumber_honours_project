# Project: Sea cucumber honours project
# Cleaning raw transect data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 03/18/2025 - 02/09/2026

# Installing packages 
install.packages(c("tidyverse", "DHARMa"))

# Loading packages into R
library(tidyverse)
library(DHARMa)

# Loading site and transect data into R 
site_data <- read_csv("./raw_data/site_data.csv")
raw_transect_data <- read_csv("./raw_data/raw_transect_data.csv")
raw_transect_size_data <- read_csv("./raw_data/raw_transect_size_data.csv")

# CLEANING RAW DATA ------------------------------------------------------------

# Joining site and transect data 
clean_transect_data <- right_join(site_data, raw_transect_data, join_by(site))
clean_transect_size_data <- right_join(site_data, raw_transect_size_data, join_by(site))

# Deleting unnecessary columns and relabelling sites 
clean_transect_data <- clean_transect_data %>%
  select(-c(surveyor, buddy, weather, visibility, transect, notes)) %>%
  mutate(site = recode(site, boulder_island = "Boulder Island",
                       jug_island = "Jug Island",
                       twin_islands = "Twin Islands",
                       brighton_beach = "Brighton Beach",
                       old_buntzen_power_plant = "Old Buntzen PP",
                       best_point = "Best Point",
                       south_johnson_bay = "S Johnson Bay",
                       north_croker_island = "N Croker Island",
                       south_croker_island = "S Croker Island"))

clean_transect_size_data <- clean_transect_size_data %>%
  select(-c(surveyor, buddy, weather, visibility, transect)) %>%
  mutate(site = recode(site, boulder_island = "Boulder Island",
                       jug_island = "Jug Island",
                       twin_islands = "Twin Islands",
                       brighton_beach = "Brighton Beach",
                       old_buntzen_power_plant = "Old Buntzen PP",
                       best_point = "Best Point",
                       south_johnson_bay = "S Johnson Bay",
                       north_croker_island = "N Croker Island",
                       south_croker_island = "S Croker Island"))

# Calculating survey time and mean transect depth at chart datum
clean_transect_data <- clean_transect_data %>%
  rowwise() %>%
  mutate(survey_time = as.numeric((end_time - start_time), units = "mins"),
         mean_tide = mean(start_tide, end_tide),
         mean_depth = mean(start_depth, end_depth),
         chart_datum = (mean_depth - mean_tide)) %>%
  ungroup() %>%
  select(-c(start_time, end_time, start_tide, end_tide, start_depth, end_depth))

clean_transect_size_data <- clean_transect_size_data %>%
  rowwise() %>%
  mutate(survey_time = as.numeric((end_time - start_time), units = "mins"),
         mean_tide = mean(start_tide, end_tide),
         mean_depth = mean(start_depth, end_depth),
         chart_datum = (mean_depth - mean_tide)) %>%
  ungroup() %>%
  select(-c(start_time, end_time, start_tide, end_tide, start_depth, end_depth))

# CALCULATING DENSITY DATA -----------------------------------------------------

# Calculating sea cucumber and sea star densities by transect
clean_transect_data <- clean_transect_data %>%
  pivot_longer(c(sea_cucumber, blood_star, leather_star, mottled_star, 
                 ochre_star, pink_star, sunflower_star),
               names_to = "species",
               values_to = "abundance") %>%
  group_by(site, latitude, longitude, date, diver_position, species) %>%
  summarise(abundance = sum(abundance)) %>%
  mutate(density = abundance/50)

# CALCULATING BIOMASS DATA -----------------------------------------------------

# Loading Em's sea cucumber excretion data into R
sea_cucumber_excretion_data <- read_csv("./biomass_calculations/all_cuke_excretion.csv")

# Modelling relationship between sea cucumber size index and biomass
sea_cucumber_size_model <- lm(weight_g ~ size_index, sea_cucumber_excretion_data)
plot(simulateResiduals(sea_cucumber_size_model))
summary(sea_cucumber_size_model)

# Saving model coefficients
intercept <- coef(sea_cucumber_size_model)[1]
slope <- coef(sea_cucumber_size_model)[2] 

# Calculating sea cucumber size index and biomass
clean_transect_size_data <- clean_transect_size_data %>%
  rowwise() %>%
  mutate(circumference = pi * diameter,
         size_index = sqrt(length * circumference),
         biomass = (slope * size_index) + intercept) %>%
  ungroup()

# Calculating mean sea cucumber biomass by transect
grouped_transect_size_data <- clean_transect_size_data %>%
  group_by(site, diver_position) %>%
  summarise(mean_biomass = mean(biomass))

# Joining sea cucumber density and biomass data
sea_cucumber_transect_data <- clean_transect_data %>%
  filter(species == "sea_cucumber")

sea_cucumber_transect_data <- left_join(sea_cucumber_transect_data, 
                                        grouped_transect_size_data, 
                                    join_by(site, diver_position))

# Calculating total sea cucumber biomass by transect
sea_cucumber_transect_data <- sea_cucumber_transect_data %>%
  rowwise() %>%
  mutate(total_biomass = abundance * mean_biomass / 25) %>%
  ungroup() %>%
  mutate(total_biomass = if_else(abundance == 0, 0, total_biomass),
         total_biomass = if_else(site == "Best Point", NA, total_biomass))

# Exporting data frames as .csv files ------------------------------------------
write_csv(clean_transect_data, "./clean_data/clean_transect_data.csv")
write_csv(clean_transect_size_data, "./clean_data/clean_transect_size_data.csv")
write_csv(sea_cucumber_transect_data, "./clean_data/sea_cucumber_transect_data.csv")
