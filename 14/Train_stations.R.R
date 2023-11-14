library(sf)
library(viridis)
library(tmap)
library(ggplot2)
library(dplyr)
library(mapview)
library(rnaturalearth)
library(raster)
library(sp)

station <- read_csv("/Users/cyberhbliu/Desktop/30daymapchallenge/14/train_stations_europe.csv")%>%
  filter(latitude != "NA")
station$latitude <- as.numeric(station$latitude)
station$longitude <- as.numeric(station$longitude)
station <- st_as_sf(station, coords = c("longitude", "latitude"), crs = 4326)

cities <- st_read("/Users/cyberhbliu/Desktop/30daymapchallenge/14/ref-countries-2020-20m.shp/CNTR_LB_2020_4326.shp/CNTR_LB_2020_4326.shp") %>%
  filter(NAME_ENGL %in% c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia" ,"Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom", "Vatican City")) %>%
  filter(NAME_ENGL != "Russia")


world <- st_read("/Users/cyberhbliu/Desktop/30daymapchallenge/14/ref-countries-2020-20m.shp/CNTR_RG_20M_2020_4326.shp/CNTR_RG_20M_2020_4326.shp")
europe <- world %>%
  filter(NAME_ENGL %in% c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia" ,"Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom", "Vatican City")) %>%
  filter(NAME_ENGL != "Russia")

europe_fishnet <- st_make_grid(europe, cellsize = 0.5) %>%
  st_sf() 
europe_fishnet <- europe_fishnet[europe,]

station_net <- 
  dplyr::select(station) %>% 
  mutate(countstation = 1) %>% 
  aggregate(., europe_fishnet, sum) %>%
  mutate(countstation = replace_na(countstation, 0),replace = TRUE)

station_net <- station_net %>%
  mutate(countstation_log = ifelse(countstation > 0, log(countstation), 0),
         countstation_normalized = scale(countstation))

mapview(station_net, zcol = "countstation_log")

world <- st_transform(world, crs = 3035)
europe <- st_transform(europe, crs = 3035)
station_net <- st_transform(station_net, crs = 3035)
europe_fishnet <- st_transform(europe_fishnet, crs = 3035)

library(RColorBrewer)
europe_map <- ggplot() +
  geom_sf(data = station_net, aes(fill = countstation_log), linewidth = 0.08) +
  geom_sf(data = cities, size = 0.5) +
  geom_sf(data = europe, fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = "YlGnBu"), name = "Log Station Count") +
  labs(title = "Count stations fishnet") +
  geom_text(data = cities, aes(label = CAPT, x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]), check_overlap = TRUE, size = 1.5, hjust = "right") +
  theme_minimal() 

ggsave("/Users/cyberhbliu/Desktop/30daymapchallenge/14/europe_map.svg", europe_map, units = "in")



#===========kernel density====================
station_main_station <- station %>%
  filter(is_main_station == TRUE)
station_city <- station %>%
  filter(is_city == TRUE)
station_airport <- station %>%
  filter(is_airport == TRUE)

station_main_station <- st_transform(station_main_station, crs = 3035)
station_airport <- st_transform(station_airport, crs = 3035)
station_city <- st_transform(station_city, crs = 3035)
europe <- st_transform(europe, crs = 3035)

density1 <- ggplot() +
  geom_sf(data = europe, fill = "#ffffd9", color = "lightgrey", linewidth = 0.25) +
  stat_density2d(data = data.frame(st_coordinates(station_main_station)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 1, bins = 100, geom = 'polygon') +
  scale_fill_gradient(low = "#afdfb7", high = "#2373b2", name = "Density") +
  scale_alpha(range = c(0, 1), guide = FALSE) +
  labs(title = "Density of Main Station") +
  theme_minimal() +
  theme_void()

density2 <- ggplot() +
  geom_sf(data = europe, fill = "#ffffd9", color = "lightgrey", linewidth = 0.25) +
  stat_density2d(data = data.frame(st_coordinates(station_airport)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 1, bins = 10, geom = 'polygon') +
  scale_fill_gradient(low = "#afdfb7", high = "#2373b2", name = "Density") +
  scale_alpha(range = c(0.00, 0.50), guide = FALSE) +
  labs(title = "Density of Airport Station") +
  theme_minimal() +
  theme_void()

density3 <- ggplot() +
  geom_sf(data = europe, fill = "#ffffd9", color = "lightgrey", linewidth = 0.25) +
  stat_density2d(data = data.frame(st_coordinates(station_city)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 1, bins = 10, geom = 'polygon') +
  scale_fill_gradient(low = "#afdfb7", high = "#2373b2", name = "Density") +
  scale_alpha(range = c(0.00, 0.50), guide = FALSE) +
  labs(title = "Density of Municipal Station") +
  theme_minimal() +
  theme_void()

ggsave("/Users/cyberhbliu/Desktop/30daymapchallenge/14/density1.svg", density1, units = "in")
ggsave("/Users/cyberhbliu/Desktop/30daymapchallenge/14/density2.svg", density2, units = "in")
ggsave("/Users/cyberhbliu/Desktop/30daymapchallenge/14/density3.svg", density3, units = "in")

