# Project: Sea cucumber honours project
# Mapping Indian Arm study sites
# Author: Shelby Connelly
# Date: 04/10/2025 - 05/13/2025

# Installing R packages
install.packages(c("tidyverse", "sf", "paletteer", "ggspatial"))

# Loading packages into R
library(tidyverse)
library(sf)
library(paletteer)
library(ggspatial)

# Loading site data into R
site_data <- read_csv("./clean_data/clean_transect_data.csv")

# Converting site data to a spatial dataset
site_data_sf <- site_data %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

# Transforming projection system to NAD 1983 BC Environment Albers
site_data_sf <- site_data_sf %>%
  st_transform(crs = 3005)

# Loading PFMA subarea data into R
pfma_subareas <- read_sf("./spatial_data/pfma_subareas/DFO_BC_PFMA_SUBAREAS_CHS_V3_G.shp") %>%
  st_transform(crs = 3005)

# Cropping PFMA subarea data to Indian Arm
site_bbox <- site_data_sf %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_buffer(dist = 4000) %>%
  st_bbox()

indian_arm_pfma_subareas <- st_intersection(pfma_subareas, st_as_sf(st_as_sfc(site_bbox)))

# SITE MAP ---------------------------------------------------------------------

# Plotting Indian Arm site data and saving as a .tiff file
tiff("./plots/site_map.tiff", 
     height = 5,
     width = 4, 
     units = "in", 
     res = 600)

site_map <- ggplot() +
  theme_classic() +
  geom_sf(data = indian_arm_pfma_subareas,
          fill = "white",
          linetype = 0) +
  geom_sf_text(data = site_data_sf,
               aes(label = site),
               size = 2,
               vjust = -1) +
  theme(panel.background = element_rect(fill = "lightgrey")) +
  geom_sf(data = site_data_sf) +
  coord_sf(xlim = c(site_bbox[[1]], site_bbox[[3]]),
           ylim = c(site_bbox[[2]], site_bbox[[4]]),
           expand = FALSE) +
  theme(axis.title = element_blank()) +
  annotation_scale() +
  annotation_north_arrow(location = "tl",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"))

site_map

dev.off()

# SITE MAP WITH PFMA SUBAREAS --------------------------------------------------

# Plotting site map with PFMA subareas and saving as a .tiff file
tiff("./plots/site_map_pfma.tiff", 
     height = 5, 
     width = 5, 
     units = "in", 
     res = 300)

site_map_pfma_bw <- ggplot() +
  theme_classic() +
  geom_sf(data = indian_arm_pfma_subareas,
         aes(fill = LABEL)) +
  scale_fill_paletteer_d("musculusColors::Bmlunge") +
  theme(panel.background = element_rect(fill = "lightgrey")) +
  geom_sf(data = site_data_sf,
          size = 1) +
  geom_sf_text(data = site_data_sf,
               aes(label = site),
               size = 2,
               vjust = -1) +
  coord_sf(xlim = c(site_bbox[[1]], site_bbox[[3]]),
           ylim = c(site_bbox[[2]], site_bbox[[4]]),
           expand = FALSE) +
  labs(x = NULL,
       y = NULL,
       fill = "PFMA subarea") +
  annotation_scale() +
  annotation_north_arrow(location = "tl",
                         height = unit(0.75, "cm"),
                         width = unit(0.75, "cm"))

site_map_pfma_bw

dev.off()