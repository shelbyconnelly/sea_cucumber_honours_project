# Project: Sea Cucumber Honours Project
# Cleaning raw transect data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 03/18/2025 - 04/11/2025

# Installing packages 
install.packages(tidyverse)

# Loading packages into R 
library(tidyverse)

# Loading site and transect data into R 
site_data <- read_csv("./raw_data/site_data.csv")
raw_transect_data <- read_csv("./raw_data/raw_transect_data.csv")
raw_transect_size_data <- read_csv("./raw_data/raw_transect_size_data.csv")

# Joining site and transect data 
clean_transect_data <- right_join(site_data, raw_transect_data, join_by(site))
clean_transect_size_data <- right_join(site_data, raw_transect_size_data, 
                                       join_by(site))

# Deleting unnecessary columns 
# and converting site, transect, and diver_position to factors 
clean_transect_data <- clean_transect_data %>%
  select(-c(surveyor, buddy, weather, visibility, notes)) %>%
  mutate(site = as.factor(site),
         transect = as.factor(transect),
         diver_position = as.factor(diver_position))

clean_transect_size_data <- clean_transect_size_data %>%
  select(-c(surveyor, buddy, weather, visibility)) %>%
  mutate(site = as.factor(site),
         transect = as.factor(transect),
         diver_position = as.factor(diver_position))

# Calculating survey time and mean transect depth at chart datum ---------------
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

# Calculating sea cucumber and sea star densities by transect and site
clean_transect_data <- clean_transect_data %>%
  pivot_longer(c(sea_cucumber, blood_star, leather_star, mottled_star, ochre_star, pink_star,
                 sunflower_star),
               names_to = "species",
               values_to = "abundance") %>%
  mutate(density = abundance/25)

site_density_data <- clean_transect_data %>%
  group_by(site, latitude, longitude, species) %>%
  summarise(mean_density = mean(density),
            sd_density = sd(density),
            se_density = (sd_density/sqrt(length(density))))

# Calculating sea cucumber size index and biomass
clean_transect_size_data <- clean_transect_size_data %>%
  rowwise() %>%
  mutate(circumference = pi * diameter,
         size_index = sqrt(length * circumference)) %>%
  ungroup()

site_size_data <- clean_transect_size_data %>%
  group_by(site, latitude, longitude) %>%
  summarise(mean_size_index = mean(size_index),
            sd_size_index = sd(size_index),
            se_size_index = (sd_size_index/sqrt(length(size_index))))

# Joining sea cucumber density and size data by site
sea_cucumber_site_density_data <- site_density_data %>%
  filter(species == "sea_cucumber")

sea_cucumber_site_data <- left_join(sea_cucumber_site_density_data, site_size_data, 
                                    join_by(site, latitude, longitude))

# Exporting data frames as .csv files
write_csv(clean_transect_data, "./clean_data/clean_transect_data.csv")
write_csv(clean_transect_size_data, "./clean_data/clean_transect_size_data.csv")
write_csv(site_density_data, "./clean_data/site_density_data.csv")
write_csv(sea_cucumber_site_data, "./clean_data/sea_cucumber_site_data.csv")
