# Project: Sea cucumber honours project
# Cleaning raw plot data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 03/18/2025 - 03/09/2026

# Installing packages
install.packages("tidyverse")

# Loading packages into R 
library(tidyverse)

# Loading site and plot data into R 
site_data <- read_csv("./raw_data/site_data.csv")
raw_plot_data <- read_csv("./raw_data/raw_plot_data.csv")

# CLEANING RAW DATA ------------------------------------------------------------

# Joining site and plot data
clean_plot_data <- right_join(site_data, raw_plot_data, join_by(site))

# Deleting unnecessary columns and relabeling site and treatment levels
clean_plot_data <- clean_plot_data %>%
  select(-c(surveyor, buddy, weather, visibility, notes)) %>%
  mutate(site = case_match(site, "boulder_island" ~ "Boulder Island",
                           "jug_island" ~ "Jug Island",
                           "twin_islands" ~ "Twin Islands",
                           "brighton_beach" ~ "Brighton Beach",
                           "old_buntzen_power_plant" ~ "Old Buntzen PP",
                           "best_point" ~ "Best Pt",
                           "south_johnson_bay" ~ "S Johnson Bay",
                           "south_croker_island" ~ "S Croker Island",
                           "north_croker_island" ~ "N Croker Island"),
         treatment = case_match(treatment, "control" ~ "Control",
                                "removal" ~ "Removal",
                                "addition" ~ "Addition"))

# Calculating survey time and mean tide
clean_plot_data <- clean_plot_data %>%
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

# Downloading data frames as .csv files ----------------------------------------
write_csv(clean_plot_data, "./clean_data/clean_plot_data.csv")
write_csv(sea_cucumber_plot_data, "./clean_data/sea_cucumber_plot_data.csv")
