# Project: Sea cucumber honours project
# Cleaning raw ROV data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 02/10/2026 - 03/05/2026

# Installing packages
install.packages("tidyverse")

# Loading packages into R
library(tidyverse)

# Loading site and ROV data into R
site_data <- read_csv("./raw_data/site_data.csv")
raw_rov_data <- read_csv("./raw_data/raw_rov_data.csv",
                         col_types = cols(start_time = col_character(),
                                          end_time = col_character()))

# CLEANING RAW DATA ------------------------------------------------------------

# Joining site and ROV data
clean_rov_data <- right_join(site_data, raw_rov_data, join_by(site))

# Relabeling and reordering sites by latitude
clean_rov_data <- clean_rov_data %>%
  mutate(site = case_match(site, "boulder_island" ~ "Boulder Island",
                           "jug_island" ~ "Jug Island",
                           "twin_islands" ~ "Twin Islands",
                           "brighton_beach" ~ "Brighton Beach",
                           "old_buntzen_power_plant" ~ "Old Buntzen PP",
                           "best_point" ~ "Best Pt",
                           "south_johnson_bay" ~ "S Johnson Bay",
                           "south_croker_island" ~ "S Croker Island",
                           "north_croker_island" ~ "N Croker Island"),
         site = fct_reorder(site, latitude))

# CALCULATING DESCRIPTIVE STATISTICS -------------------------------------------

# Calculating survey time
clean_rov_data <- clean_rov_data %>%
  mutate(start_time = ms(start_time),
         end_time = ms(end_time),
         survey_time = as.numeric((end_time - start_time), units = "mins"))

# Binning observations into shallow and deep depth categories
shallow_vs_deep_rov_data <- clean_rov_data %>%
  mutate(depth_category = case_when(observation_depth <= 12 ~ "shallow",
                                    observation_depth > 12 ~ "deep",
                                    is.na(observation_depth) ~ "shallow"))

# Binning observations into 4 m depth categories
binned_rov_data <- clean_rov_data %>%
  mutate(depth_bin = cut(observation_depth, 
                         breaks = c(4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 
                                    48, 52, 56, 60)),
         depth_bin = replace_na(depth_bin, "(4,8]"))

# Calculating sea cucumber abundance and density by transect and depth category/bin
shallow_vs_deep_rov_data <- shallow_vs_deep_rov_data %>%
  group_by(site, latitude, longitude, transect, survey_time, depth_category) %>%
  summarise(abundance = sum(abundance)) %>%
  mutate(density = abundance/survey_time) %>%
  ungroup()

binned_rov_data <- binned_rov_data %>%
  group_by(site, latitude, longitude, transect, survey_time, depth_bin) %>%
  summarise(abundance = sum(abundance)) %>%
  mutate(density = abundance/survey_time) %>%
  ungroup()
  
# Converting shallow vs deep ROV data from long to wide format and adding zero values
shallow_vs_deep_rov_data_wide <- shallow_vs_deep_rov_data %>%
  pivot_wider(names_from = depth_category,
              values_from = c(abundance, density)) %>%
  mutate(abundance_shallow = replace_na(abundance_shallow, 0),
         abundance_deep = replace_na(abundance_deep, 0),
         density_shallow = replace_na(density_shallow, 0),
         density_deep = replace_na(density_deep, 0))

binned_rov_data <- binned_rov_data %>%
  complete(nesting(site, latitude, longitude), transect, depth_bin,
           fill = list(abundance = 0,
                       density = 0))

# Downloading data frames as .csv files ----------------------------------------
write_csv(shallow_vs_deep_rov_data_wide, "./clean_data/shallow_vs_deep_rov_data.csv")
write_csv(binned_rov_data, "./clean_data/binned_rov_data.csv")
