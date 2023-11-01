library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)
library(rnaturalearth)
library(mapview)
library(ggforce)
library(maps)

location <- read_sf("/Users/cyberhbliu/Desktop/30daymapchallenge/1/Location.csv")

location_sf <- st_as_sf(location, coords = c("Longitude", "Latitude"), crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")

# List of North American countries and territories
north_america <- c("United States of America", "Canada", "Mexico", "Belize", "Costa Rica", 
                   "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", "Greenland", 
                   "Antigua and Barbuda", "Bahamas", "Barbados", "Cuba", "Dominica", "Dominican Republic", 
                   "Grenada", "Haiti", "Jamaica", "Saint Kitts and Nevis", "Saint Lucia", 
                   "Saint Vincent and the Grenadines", "Trinidad and Tobago")

na_countries <- world[world$admin %in% north_america, ]

# Transform North America to Lambert Conformal Conic projection
na_lambert <- st_transform(na_countries, "+proj=lcc +lat_1=33 +lat_2=45 +lon_0=-96")

# Clip the location data to only those points within North America
location_sf_na <- st_intersection(location_sf, na_countries)

# Transform the clipped location data to Lambert Conformal Conic projection
location_sf_lambert_na <- st_transform(location_sf_na, "+proj=lcc +lat_1=33 +lat_2=45 +lon_0=-96")

# Plotting
map_na_lambert <- ggplot() +
  geom_sf(data = na_lambert, fill = "lightgray", color = "transparent") +
  geom_sf(data = location_sf_lambert_na, 
          aes(color = Season), 
          size = 0.2,
          alpha = 0.2) +
  theme_void() +
  theme_minimal()

# Display the map
print(map_na_lambert)
