# Project: Sea Cucumber Honours Project
# Cleaning raw transect data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 03/18/2025 - 03/20/2025

# Installing packages
install.packages(tidyverse)

# Loading packages into R
library(tidyverse)

# Loading site and transect data into R
raw_transect_data <- read_csv("./raw_data/raw_transect_data.csv")
raw_transect_size_data <- read_csv("./raw_data/raw_transect_size_data.csv")

# Converting site, transect, and diver_position to factors and removing unnecessary columns
clean_transect_data <- raw_transect_data %>%
  mutate(site = as.factor(site), 
         transect = as.factor(transect),
         diver_position = as.factor(diver_position)) %>%
  select(-c(surveyor, buddy, weather, visibility, notes))

clean_transect_size_data <- raw_transect_size_data %>%
  mutate(site = as.factor(site),
         transect = as.factor(transect),
         diver_position = as.factor(diver_position)) %>%
  select(-c(surveyor, buddy, weather, visibility))

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

# Calculating sea cucumber and sea star density by transect and site
clean_transect_data <- clean_transect_data %>%
  pivot_longer(c(sea_cucumber, blood_star, leather_star, mottled_star, ochre_star, pink_star,
                 sunflower_star),
               names_to = "species",
               values_to = "abundance") %>%
  mutate(density = abundance/25)

site_density_data <- clean_transect_data %>%
  group_by(site, species) %>%
  summarise(mean_density = mean(density),
            sd_density = sd(density),
            se_density = (sd_density/sqrt(length(density))))

# Calculating sea cucumber size indices by site
clean_transect_size_data <- clean_transect_size_data %>%
  rowwise() %>%
  mutate(size_index = sqrt(length * diameter)) %>%
  ungroup()

site_size_data <- clean_transect_size_data %>%
  group_by(site) %>%
  summarise(mean_size_index = mean(size_index),
            sd_size_index = sd(size_index),
            se_size_index = (sd_size_index/sqrt(length(size_index))))

# Merging sea cucumber density and size index by site
sea_cucumber_density_data <- site_density_data %>%
  filter(species == "sea_cucumber")

sea_cucumber_data <- left_join(sea_cucumber_density_data, site_size_data, join_by(site))

# Exporting data frames as .csv files
write.csv(site_density_data, "./clean_data/clean_transect_density_data.csv", row.names = FALSE)
write.csv(sea_cucumber_data, "./clean_data/sea_cucumber_transect_data.csv", row.names = FALSE)
