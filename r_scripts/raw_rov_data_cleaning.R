# Project: Sea cucumber honours project
# Cleaning raw ROV data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 02/10/2026 - 09/04/2026

# Installing packages
install.packages("tidyverse")

# Loading packages into R
library(tidyverse)

# Loading site and ROV data into R
site_data <- read_csv("./raw_data/site_data.csv")
raw_rov_data <- read_csv("./raw_data/raw_rov_data.csv",
                         col_types = cols(start_time = col_character(),
                                          end_time = col_character()))
raw_rov_fov_data <- read_csv("./raw_data/raw_rov_fov_data.csv")

# CLEANING RAW DATA ------------------------------------------------------------

# Joining site and ROV data
clean_rov_data <- right_join(site_data, raw_rov_data, join_by(site))

# Relabeling and reordering sites by latitude
clean_rov_data <- clean_rov_data %>%
  mutate(site = recode_values(site, "boulder_island" ~ "Boulder Island",
                              "jug_island" ~ "Jug Island",
                              "twin_islands" ~ "Twin Islands",
                              "brighton_beach" ~ "Brighton Beach",
                              "old_buntzen_pp" ~ "Old Buntzen PP",
                              "best_pt" ~ "Best Pt",
                              "s_johnson_bay" ~ "S Johnson Bay",
                              "s_croker_island" ~ "S Croker Island",
                              "n_croker_island" ~ "N Croker Island"),
         site = fct_reorder(site, latitude))

# CALCULATING DESCRIPTIVE STATISTICS -------------------------------------------

# Calculating survey time and length
clean_rov_data <- clean_rov_data %>%
  mutate(start_time = ms(start_time),
         end_time = ms(end_time),
         survey_time = as.numeric((end_time - start_time), units = "mins"),
         survey_length_m = start_depth_m - end_depth_m)

# Binning observations into shallow and deep depth categories
shallow_vs_deep_rov_data <- clean_rov_data %>%
  mutate(depth_category = case_when(observation_depth_m <= 12 ~ "shallow",
                                    observation_depth_m > 12 ~ "deep",
                                    is.na(observation_depth_m) ~ "shallow"))

# Calculating sea cucumber abundance and density by transect and depth category
shallow_vs_deep_rov_data <- shallow_vs_deep_rov_data %>%
  group_by(site, latitude, longitude, transect, survey_time, depth_category) %>%
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

# Summarizing sea cucumber densities by site and depth category
rov_density_summary <- shallow_vs_deep_rov_data_wide %>%
  group_by(site) %>%
  summarize(mean_density_shallow = mean(density_shallow),
            sd_density_shallow = sd(density_shallow),
            mean_density_deep = mean(density_deep),
            sd_density_deep = sd(density_deep))

rov_density_summary

# Calculating mean field of view in ROV surveys
rov_fov <- raw_rov_fov_data %>%
  summarize(mean_fov = mean(width_m),
            sd_fov = sd(width_m))

rov_fov

# Downloading data frame as .csv file ------------------------------------------
write_csv(shallow_vs_deep_rov_data_wide, 
          "./clean_data/shallow_vs_deep_rov_data.csv")
