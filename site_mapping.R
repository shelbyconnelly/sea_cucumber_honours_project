# Project: Sea cucumber honours project
# Mapping Indian Arm study sites
# Author: Shelby Connelly
# Date: 04/10/2025 - 04/15/2025

# Installing R packages
install.packages(c("tidyverse", "sf", "ggspatial"))

# Loading packages into R
library(tidyverse)
library(sf)
library(ggspatial)

# Loading site data into R
site_data <- read_csv("./raw_data/site_data.csv")

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

# SITE MAPS --------------------------------------------------------------------

# Plotting Indian Arm site data in black and white and saving as a .tiff file
tiff("./plots/site_map_bw.tiff", height = 5, width = 6, units = "in", res = 600)

site_map_bw <- ggplot() +
  theme_classic() +
  geom_sf(data = indian_arm_pfma_subareas,
          fill = "white",
          linetype = 0) +
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

site_map_bw

dev.off()

# Plotting with colour
tiff("./plots/site_map_colour.tiff", height = 5, width = 6, units = "in", res = 300)

site_map_colour <- ggplot() +
  theme_classic() +
  geom_sf(data = indian_arm_pfma_subareas,
          fill = "paleturquoise",
          linetype = 0) +
  theme(panel.background = element_rect(fill = "darkseagreen")) +
  geom_sf(data = site_data_sf) +
  coord_sf(xlim = c(site_bbox[[1]], site_bbox[[3]]),
           ylim = c(site_bbox[[2]], site_bbox[[4]]),
           expand = FALSE) +
  theme(axis.title = element_blank()) +
  annotation_scale() +
  annotation_north_arrow(location = "tl",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"))

site_map_colour

dev.off()

# SITE MAPS WITH PFMA SUBAREAS -------------------------------------------------

# Plotting site map in black and white with PFMA subareas
tiff("./plots/site_map_pfma_bw.tiff", height = 5, width = 6, units = "in", res = 600)

site_map_pfma_bw <- ggplot() +
  theme_classic() +
  geom_sf(data = indian_arm_pfma_subareas,
          fill = "white") +
  theme(panel.background = element_rect(fill = "lightgrey")) +
  geom_sf_text(data = indian_arm_pfma_subareas,
          aes(label = LABEL),
          fontface = "bold",
          size = 2.5) +
  geom_sf(data = site_data_sf) +
  coord_sf(xlim = c(site_bbox[[1]], site_bbox[[3]]),
           ylim = c(site_bbox[[2]], site_bbox[[4]]),
           expand = FALSE) +
  theme(axis.title = element_blank()) +
  annotation_scale() +
  annotation_north_arrow(location = "tl",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"))

site_map_pfma_bw

dev.off()

# Plotting in colour
tiff("./plots/site_map_pfma_colour.tiff", height = 5, width = 6, units = "in", res = 300)

site_map_pfma_colour <- ggplot() +
  theme_classic() +
  geom_sf(data = indian_arm_pfma_subareas,
          fill = "paleturquoise") +
  theme(panel.background = element_rect(fill = "darkseagreen")) +
  geom_sf_text(data = indian_arm_pfma_subareas,
               aes(label = LABEL),
               fontface = "bold",
               size = 2.5) +
  geom_sf(data = site_data_sf) +
  coord_sf(xlim = c(site_bbox[[1]], site_bbox[[3]]),
           ylim = c(site_bbox[[2]], site_bbox[[4]]),
           expand = FALSE) +
  theme(axis.title = element_blank()) +
  annotation_scale() +
  annotation_north_arrow(location = "tl",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"))

site_map_pfma_colour

dev.off()

# SITE MAPS WITH PFMA SUBAREAS AND SFU -----------------------------------------

# Making data frame of SFU coordinates
sfu_data <- data.frame(
  label = "SFU",
  latitude = 49.279089,
  longitude = -122.920179)

# Converting SFU data to a spatial dataset
sfu_data_sf <- sfu_data %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

# Transforming projection system 
sfu_data_sf <- sfu_data_sf %>%
  st_transform(crs = 3005)

# Plotting site map with PFMA subareas and SFU in colour
tiff("./plots/site_map_sfu.tiff", height = 5, width = 6, units = "in", res = 300)

site_map_sfu <- ggplot() +
  theme_classic() +
  geom_sf(data = indian_arm_pfma_subareas,
          fill = "lightblue") +
  theme(panel.background = element_rect(fill = "darkseagreen")) +
  geom_sf_text(data = indian_arm_pfma_subareas,
               aes(label = LABEL),
               fontface = "bold",
               size = 2.5) +
  geom_sf(data = site_data_sf) +
  geom_sf_label(data = sfu_data_sf,
                aes(label = label),
                fontface = "bold",
                colour = "red",
                size = 2.5) +
  coord_sf(xlim = c(site_bbox[[1]], site_bbox[[3]]),
           ylim = c(site_bbox[[2]], site_bbox[[4]]),
           expand = FALSE) +
  theme(axis.title = element_blank()) +
  annotation_scale(location = "tr") +
  annotation_north_arrow(location = "tl",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"))

site_map_sfu  

dev.off()
