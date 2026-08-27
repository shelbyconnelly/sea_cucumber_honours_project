# Project: Sea cucumber honours project
# Cleaning raw plot data and calculating descriptive statistics
# Author: Shelby Connelly
# Date: 03/18/2025 - 08/27/2026

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
  select(-c(surveyor, buddy, weather, visibility_m, start_tide_m, end_tide_m, 
            notes)) %>%
  mutate(site = recode_values(site, "boulder_island" ~ "Boulder Island",
                              "jug_island" ~ "Jug Island",
                              "twin_islands" ~ "Twin Islands",
                              "brighton_beach" ~ "Brighton Beach",
                              "old_buntzen_pp" ~ "Old Buntzen PP",
                              "best_pt" ~ "Best Pt",
                              "s_johnson_bay" ~ "S Johnson Bay",
                              "s_croker_island" ~ "S Croker Island",
                              "n_croker_island" ~ "N Croker Island"),
         treatment = recode_values(treatment, "control" ~ "Control",
                                   "removal" ~ "Removal",
                                   "addition" ~ "Addition"))

# CALCULATING DESCRIPTIVE STATISTICS -------------------------------------------

# Calculating survey time
clean_plot_data <- clean_plot_data %>%
  rowwise %>%
  mutate(survey_time = as.numeric((end_time - start_time), units = "mins")) %>%
  ungroup() %>%
  select(-c(start_time, end_time))

# Summarizing plot depth and slope data
plot_depth_data_summary <- clean_plot_data %>%
  filter(week == 1) %>%
  rowwise() %>%
  mutate(slope = ((bottom_depth_m - top_depth_m)/5)) %>%
  ungroup() %>%
  summarise(n = n(),
            min_top_depth_m = min(top_depth_m),
            max_top_depth_m = max(top_depth_m),
            mean_top_depth_m = mean(top_depth_m),
            sd_top_depth_m = sd(top_depth_m),
            min_middle_depth_m = min(middle_depth_m),
            max_middle_depth_m = max(middle_depth_m),
            mean_middle_depth_m = mean(middle_depth_m),
            sd_middle_depth_m = sd(middle_depth_m),
            min_bottom_depth_m = min(bottom_depth_m),
            max_bottom_depth_m = max(bottom_depth_m),
            mean_bottom_depth_m = mean(bottom_depth_m),
            sd_bottom_depth_m = sd(bottom_depth_m),
            min_slope = min(slope),
            max_slope = max(slope),
            mean_slope = mean(slope),
            sd_slope = sd(slope))
  
# Calculating percent of sea cucumbers not removed from removal plots
unmeasured_sea_cucumbers <- clean_plot_data %>%
  filter(week == 0,
         treatment == "Removal") %>%
  summarize(total_counted = sum(initial_sea_cucumber),
            total_unmeasured = sum(unmeasured_sea_cucumber),
            percent_unmeasured = (total_unmeasured/total_counted)*100)

unmeasured_sea_cucumbers

# CALCULATING DENSITIES --------------------------------------------------------

# Calculating sea cucumber and sea star densities by plot
clean_plot_data <- clean_plot_data %>%
  pivot_longer(c(initial_sea_cucumber, experimental_sea_cucumber, blood_star, 
                 leather_star, mottled_star, ochre_star, pink_star, 
                 sunflower_star),
               names_to = "species",
               values_to = "abundance") %>%
  mutate(density = abundance/25)

# Calculating change from initial to experimental sea cucumber density
sea_cucumber_plot_data <- clean_plot_data %>%
  filter(species == c("initial_sea_cucumber", "experimental_sea_cucumber")) %>%
  pivot_wider(names_from = species,
              values_from = c(abundance, density)) %>%
  rowwise() %>%
  mutate(density_change = (density_experimental_sea_cucumber - density_initial_sea_cucumber)) %>%
  ungroup()

# Downloading data frames as .csv files ----------------------------------------
write_csv(clean_plot_data, "./clean_data/clean_plot_data.csv")
write_csv(sea_cucumber_plot_data, "./clean_data/sea_cucumber_plot_data.csv")
