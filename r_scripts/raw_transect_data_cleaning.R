# Project: Sea cucumber honours project
# Cleaning raw transect data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 03/18/2025 - 08/03/2026

# Installing packages 
install.packages("tidyverse")

# Loading packages into R
library(tidyverse)

# Loading site and transect data into R 
site_data <- read_csv("./raw_data/site_data.csv")
raw_transect_data <- read_csv("./raw_data/raw_transect_data.csv")

# CLEANING RAW DATA ------------------------------------------------------------

# Joining site and transect data 
clean_transect_data <- right_join(site_data, raw_transect_data, join_by(site))

# Deleting unnecessary columns and relabelling sites 
clean_transect_data <- clean_transect_data %>%
  select(-c(date, surveyor, buddy, weather, visibility_m, diver_position, 
            start_tide_m, end_tide_m, notes)) %>%
  mutate(site = case_match(site, "boulder_island" ~ "Boulder Island",
                           "jug_island" ~ "Jug Island",
                           "twin_islands" ~ "Twin Islands",
                           "brighton_beach" ~ "Brighton Beach",
                           "old_buntzen_pp" ~ "Old Buntzen PP",
                           "best_pt" ~ "Best Pt",
                           "s_johnson_bay" ~ "S Johnson Bay",
                           "n_croker_island" ~ "N Croker Island",
                           "s_croker_island" ~ "S Croker Island"))

# CALCULATING DESCRIPTIVE STATISTICS -------------------------------------------

# Calculating survey time and mean transect depth
clean_transect_data <- clean_transect_data %>%
  rowwise() %>%
  mutate(survey_time = as.numeric((end_time - start_time), units = "mins"),
         depth_m = mean(start_depth_m, end_depth_m)) %>%
  ungroup() %>%
  select(-c(start_time, end_time, start_depth_m, end_depth_m))

# Summarizing transect depth data
transect_depth_data_summary <- clean_transect_data %>%
  summarize(min_depth_m = min(depth_m),
            max_depth_m = max(depth_m),
            mean_depth_m = mean(depth_m),
            sd_depth_m = sd(depth_m))

# CALCULATING DENSITY DATA -----------------------------------------------------

# Calculating sea cucumber and sea star densities by transect
clean_transect_data <- clean_transect_data %>%
  pivot_longer(c(sea_cucumber, blood_star, leather_star, mottled_star, 
                 ochre_star, pink_star, sunflower_star),
               names_to = "species",
               values_to = "abundance") %>%
  group_by(site, latitude, longitude, transect, species) %>%
  summarise(abundance = sum(abundance)) %>%
  mutate(density = abundance/50)

# Filtering sea cucumber data
sea_cucumber_transect_data <- clean_transect_data %>%
  filter(species == "sea_cucumber")

# Exporting data frames as .csv files ------------------------------------------
write_csv(clean_transect_data, "./clean_data/clean_transect_data.csv")
write_csv(sea_cucumber_transect_data, "./clean_data/sea_cucumber_transect_data.csv")
