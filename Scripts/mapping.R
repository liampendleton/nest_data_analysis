extrafont::loadfonts("win")
library(cowplot)
library(here) #setup
library(sf) #vector data         
library(ncdf4) #net cdf rasters  
library(terra) #raster data (most things)
library(raster) #loading netCDF files
library(tidyverse) #data manipulation
library(sp)
library(ggplot2)
library(tmap)
library(ggspatial)
library(sfheaders)
library(units)

## Set up basic map data & constraints
state <- read_sf(here("Data", "Spatial Data", "cb_2018_us_state_500k.shp"))
wa <- state %>% filter(NAME == "Washington")
wa_box <- st_bbox(wa)
comp_coords <- c(x = -122, y = 48)
scale_coords <- c(x = -122, y = 48)

## Set up "point of interest" data
coord_cities <- data.frame(
  city = c("Port Townsend", "Discovery Bay", "Strait of Juan de Fuca", "Protection Island"),
  lat = c(48.117428, 48.044409, 48.253593, 48.127288),
  long = c(-122.760748, -122.854857, -123.007165, -122.929698))

###########################
## Map of Protection Island
PI_map <- ggplot() +
  geom_sf(data = wa, color = "lemonchiffon3", fill = "ivory2", lwd = 0.75) +   #make base land map
  coord_sf(expand = F,
           xlim = c(-123.1, -122.725),
           ylim = c(48.05, 48.225)) +   #zoom in on area of interest
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", linewidth = 1,
                                    linetype = "solid", fill = NA)) +
  ## Add geographic features labels
  annotate(geom="text", x=-122.89, y=48.065, label="Discovery Bay", colour="gray80",
           size=5, family="Times", fontface="italic", angle=-30) +
  annotate(geom = "text", x = -122.9, y = 48.185, label = "Strait of Juan de Fuca", color = "gray80",
           size = 12, family = "Times", fontface = "italic", angle = -4) +
  ## Add scale and compass
  annotation_scale(location = "br",
                   height = unit(0.25, "cm"),
                   plot_unit = "km") +
  annotation_north_arrow(data = wa,
                         width = unit(1, "cm"),
                         location = "br",
                         pad_y = unit(0.75, "cm")) +
  ## Add place labels and symbols
  geom_text(data = coord_cities[10,], aes(x = long, y = lat, label = city), nudge_y = 0.01) +
  geom_point(data = coord_cities[10,], aes(x = long, y = lat, label = city) , col = "black", fill = "red", shape = 25, size = 3) +
  geom_text(data = coord_cities[1,], aes(x = long, y = lat, label = city), nudge_y = 0.01) +
  geom_point(data = coord_cities[1,], aes(x = long, y = lat, label = city), col = "black", fill = "red", shape = 25, size = 3) +
  ## Clean things up
  labs(y = "Latitude", x = "Longitude") +
  theme_bw() +
  theme(legend.title = element_text(size = 15),
        legend.position = "right",
        legend.justification = "top",
        legend.box.background = element_rect(color = "black", size = 1)) +
  guides(colour = guide_legend(override.aes = list(size = 3))) #increase size of legend points to be visible

print(PI_map)