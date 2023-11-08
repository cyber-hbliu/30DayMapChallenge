library(sf)
library(tidyverse)
library(viridis)
library(tmap)
library(tidycensus)
library(tigris)
library(ggplot2)
library(dplyr)
library(mapview)
library(rjson)
library(viridis)

crime <- read_csv("/Users/cyberhbliu/Desktop/30daymapchallenge/4/Crime_Data_from_2020_to_Present_20231107.csv") %>%
  rename(vic_sex = "Vict Sex")

crime_to_female <- crime %>%
  filter(vic_sex == "F") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326)

boundary <- st_read("/Users/cyberhbliu/Desktop/30daymapchallenge/4/City Boundary of Los Angeles/geo_export_781ccb97-6300-4b31-b9d2-7cbd0954c06a.shp")

crime_to_female <- st_transform(crime_to_female, crs = st_crs(boundary))

crime_to_female <- crime_to_female %>%
  mutate(longitude = st_coordinates(geometry)[, 1],
         latitude = st_coordinates(geometry)[, 2]) %>%
  filter(longitude != 0)

p <- ggplot(data = crime_to_female) +
  stat_density_2d(
    aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
    geom = "polygon",
    color = "white",
    size = 0.02,
    bins = 100  # You can adjust the number of bins for smoother or rougher density
  ) +
  geom_sf(data = boundary, fill = "transparent", color = "purple") +
  scale_fill_viridis_c()+
  scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
  coord_sf(datum = NA) +
  theme_void() +
  labs(title = "Density Map of Crimes Against Females in Los Angeles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

ggsave("/Users/cyberhbliu/Desktop/30daymapchallenge/4/crime_density_map.svg", plot = p, width = 10, height = 8, device = "svg")

crime_to_female_sf <- st_as_sf(crime_to_female, coords = c("longitude", "latitude"), crs = 4326)
st_write(crime_to_female_sf, "/Users/cyberhbliu/Desktop/30daymapchallenge/4/crime_to_female_shapefile.shp")
