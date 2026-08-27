# Project: Sea cucumber honours project
# Mapping Indian Arm study sites and ambient sea cucumber densities
# Author: Shelby Connelly
# Date: 04/10/2025 - 08/24/2026

# Installing R packages
install.packages(c("tidyverse", "sf", "viridis", "ggtext", "ggspatial", 
                   "patchwork"))

# Loading packages into R
library(tidyverse)
library(sf)
library(viridis)
library(ggtext)
library(ggspatial)
library(patchwork)

# Loading site data into R
site_data <- read_csv("./clean_data/complete_site_data_continuous.csv")

# Converting site data to a spatial dataset
site_data_sf <- site_data %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

# Transforming projection system to NAD 1983 BC Environment Albers
site_data_sf <- site_data_sf %>%
  st_transform(crs = 3005)

# Loading base maps into R and transforming projection system 
hakai_bc <- read_sf("./spatial_data/hakai_bc/hakai_bc.shp") %>%
  st_transform(crs = 3005)

salish_sea <- read_sf("./spatial_data/salish_sea/salish_sea.shp") %>%
  st_transform(crs = 3005) 

pnw <- read_sf("./spatial_data/pnw/pnw.shp") %>%
  st_transform(crs = 3005)

# Cropping BC base map to Vancouver Harbour (includes Indian Arm)
vancouver_harbour <- salish_sea %>%
  filter(Name == "Vancouver Harbour")

vancouver_harbour_bbox <- st_bbox(vancouver_harbour)

# SITE MAP ---------------------------------------------------------------------

# Mapping ambient sea cucumber densities by site
site_map <- ggplot() +
  theme_classic() +
  geom_sf(data = hakai_bc,
          fill = "grey90",
          linetype = 0) +
  theme(panel.background = element_rect(fill = "white")) +
  geom_sf(data = site_data_sf,
          shape = 21,
          aes(fill = mean_transect_density),
          size = 3) +
  geom_sf_text(data = site_data_sf,
               aes(label = site),
               size = 3,
               vjust = -1) +
  scale_fill_viridis_c(option = "magma",
                       begin = 0.9,
                       end = 0.4) +
  theme(axis.title = element_blank()) +
  labs(fill = "Shallow (≤ 12 m)<br>density (#/m<sup>2</sup>)") +
  theme(legend.title = element_markdown()) +
  annotation_scale() +
  annotation_north_arrow(location = "tr",
                         height = unit(0.75, "cm"),
                         width = unit(0.75, "cm")) +
  coord_sf(xlim = c(vancouver_harbour_bbox[[1]],
                    vancouver_harbour_bbox[[3]]),
           ylim = c(vancouver_harbour_bbox[[2]], 
                    vancouver_harbour_bbox[[4]]))

site_map

# INSET MAP --------------------------------------------------------------------

# Cropping base map to the Salish Sea
bc_bbox <- st_bbox(hakai_bc)
salish_sea_bbox <- st_bbox(salish_sea)

# Mapping location of Indian Arm within the Salish Sea
inset_map <- ggplot() +
  theme_void() +
  geom_sf(data = hakai_bc,
          fill = "grey90",
          linetype = 0) +
  geom_sf(data = pnw,
          fill = "grey90",
          linetype = 0) +
  theme(panel.background = element_rect(fill = "white")) +
  geom_sf(data = vancouver_harbour,
          fill = "red",
          linetype = 0) +
  theme(panel.border = element_rect(colour = "black",
                                    linewidth = 1)) +
  coord_sf(xlim = c(salish_sea_bbox[[1]],
                    salish_sea_bbox[[3]]),
           ylim = c(bc_bbox[[2]], 
                    salish_sea_bbox[[4]]),
           expand = FALSE) +
  annotation_scale()

inset_map

# FINAL MAP --------------------------------------------------------------------

# Combining site and inset maps and saving as a .tiff file
tiff("./plots/fig_1.tiff", 
     height = 5,
     width = 6.5, 
     units = "in", 
     res = 300)

fig_1 <- site_map + inset_element(inset_map,
                                  left = 0.05,
                                  top = 0.95,
                                  right = 0.55,
                                  bottom = 0.45)

fig_1

dev.off()
