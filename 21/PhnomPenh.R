library(tidyverse)
library(sf)
library(osmdata)
library(terra)
library(httr)
library(XML)

url <-
  "https://glad.umd.edu/users/Potapov/GLCLUC2020/Built-up_change_2000_2020/"

get_raster_links <- function() {
  res <- httr::GET(url) # make http request
  parse <- XML::htmlParse(res) # parse data to html format
  links <- XML::xpathSApply( # scrape all the href tags
    parse,
    path = "//a", XML::xmlGetAttr, "href"
  )
  lnks <- links[-c(1:5)] # grab links
  for (l in lnks) { # make all links and store in a list
    rlinks <- paste0(url, lnks)
  }
  
  return(rlinks)
}

rlinks <- get_raster_links()

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

load_builtup_data <- function() {
  l <- rlinks[grepl("20N_100E", rlinks, rlinks)]
  builtup_data <- terra::rast(l)
  terra::crs(builtup_data) <- crsLONGLAT
  
  return(builtup_data)
}

builtup_data <- load_builtup_data()
terra::plot(builtup_data)


# PhnomPenh BOUNDARIES FROM OSM DATA
#--------------------------------------
city <- "PhnomPenh"

PhnomPenh_border <- osmdata::getbb(
  city,
  format_out = "sf_polygon"
) |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crsLONGLAT)

plot(PhnomPenh_border, add = T)


# MAKE BUFFER AROUND PhnomPenh
#----------------------------
get_buffer <- function() {
  PhnomPenh_cents <- sf::st_centroid(PhnomPenh_border)
  PhnomPenh_circle <- sf::st_buffer(
    PhnomPenh_cents,
    dist = units::set_units(70, km)
  ) |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crs = crsLONGLAT)
  
  return(PhnomPenh_circle)
}

PhnomPenh_circle <- get_buffer()

# plot
ggplot() +
  geom_sf(
    data = PhnomPenh_border, color = "#3036ff",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = PhnomPenh_circle, color = "#e83778",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))


crop_builtup_data <- function() {
  PhnomPenh_vect <- terra::vect(PhnomPenh_circle)
  PhnomPenh_raster <- terra::crop(builtup_data, PhnomPenh_vect)
  PhnomPenh_raster_cropped <- terra::mask(
    PhnomPenh_raster, PhnomPenh_vect
  )
  return(PhnomPenh_raster_cropped)
}

PhnomPenh_raster_cropped <- crop_builtup_data()
terra::plot(PhnomPenh_raster_cropped)


# 4. IMAGE TO DATA.FRAME
#-----------------------

raster_to_df <- function() {
  PhnomPenh_df <- terra::as.data.frame(
    PhnomPenh_raster_cropped,
    xy = T
  )
  
  return(PhnomPenh_df)
}

PhnomPenh_df <- raster_to_df()
head(PhnomPenh_df)
names(PhnomPenh_df)[3] <- "value"

# define categorical values
PhnomPenh_df$cat <- round(PhnomPenh_df$value, 0)
PhnomPenh_df$cat <- factor(PhnomPenh_df$cat,
                        labels = c("No Built-up", "New", "Existing")
)

# 5. GET PhnomPenh ROADS FROM OSM DATA
#---------------------------------
road_tags <- c(
  "motorway", "trunk", "primary", "secondary",
  "tertiary", "motorway_link", "trunk_link", 
  "primary_link", "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
  bbox <- sf::st_bbox(PhnomPenh_circle)
  roads <- bbox |>
    opq() |>
    add_osm_feature(
      key = "highway",
      value = road_tags
    ) |>
    osmdata::osmdata_sf()
  
  return(roads)
}

roads <- get_osm_roads()
PhnomPenh_roads <- roads$osm_lines |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crs = crsLONGLAT)

ggplot() +
  geom_sf(
    data = PhnomPenh_circle, fill = "transparent",
    color = "#3036ff", size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = PhnomPenh_roads,
    color = "#e83778", inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# 6. CROP PhnomPenh ROADS WITH BUFFER
#--------------------------------
PhnomPenh_roads_cropped <- sf::st_intersection(
  PhnomPenh_roads, PhnomPenh_circle
)

ggplot() +
  geom_sf(
    data = PhnomPenh_circle,
    color = "#3036ff", fill = NA,
    size = 1.2, inherit.aes = FALSE
  ) +
  geom_sf(
    data = PhnomPenh_roads_cropped, fill = "transparent",
    color = "#e83778", inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# 7. MAP
#-------
colrs <- c(
  "grey95", "#FCDD0F", "#e83778"
)

p <- ggplot() +
  geom_raster(
    data = PhnomPenh_df,
    aes(x = x, y = y, fill = cat),
    alpha = 1
  ) +
  geom_sf(
    data = PhnomPenh_roads_cropped,
    color = "grey95",
    size = .05,
    alpha = .3,
    fill = "transparent"
  ) +
  geom_sf(
    data = PhnomPenh_circle,
    color = "grey20",
    size = 2,
    fill = "transparent"
  ) +
  geom_sf(data = PhnomPenh_border,
          color = "grey20",
          fill = "transparent",
          size = 1) +
  scale_fill_manual(
    name = "",
    values = colrs,
    drop = F
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(35, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "top"
    )
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(.5, 1.05),
    legend.text = element_text(size = 12, color = "grey20"),
    legend.title = element_text(size = 14, color = "grey20"),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_line(color = "grey20", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      size = 20, color = "grey20", hjust = .5, vjust = 2
    ),
    plot.caption = element_text(
      size = 9, color = "grey20", hjust = .5, vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = 0, b = 0, l = 0), "lines"
    ),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(colour = "transparent"),
    panel.border = element_blank()
  ) +
  labs(
    x = "",
    y = NULL,
    title = "PhnomPenh",
    subtitle = "",
    caption = "©2023 Milos Popovic (https://milospopovic.net)\nData: GLAD Built-up Change Data & ©OpenStreetMap contributors"
  )

ggsave(
  filename = "/Users/cyberhbliu/Desktop/30daymapchallenge/21 raster/PhnomPenh_built_up.png",
  width = 6, height = 6, dpi = 600,
  device = "png", p
)
p


