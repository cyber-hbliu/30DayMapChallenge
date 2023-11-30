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
  l <- rlinks[grepl("40N_130E", rlinks, rlinks)]
  builtup_data <- terra::rast(l)
  terra::crs(builtup_data) <- crsLONGLAT
  
  return(builtup_data)
}

builtup_data <- load_builtup_data()
terra::plot(builtup_data)


# osaka BOUNDARIES FROM OSM DATA
#--------------------------------------
city <- "Osaka, Japan"

osaka_border <- osmdata::getbb(
  city,
  format_out = "sf_polygon"
) |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crsLONGLAT)

plot(osaka_border, add = T)


# MAKE BUFFER AROUND osaka
#----------------------------
get_buffer <- function() {
  osaka_cents <- sf::st_centroid(osaka_border)
  osaka_circle <- sf::st_buffer(
    osaka_cents,
    dist = units::set_units(20, km)
  ) |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crs = crsLONGLAT)
  
  return(osaka_circle)
}

osaka_circle <- get_buffer()

# plot
ggplot() +
  geom_sf(
    data = osaka_border, color = "#3036ff",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = osaka_circle, color = "#e83778",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))


crop_builtup_data <- function() {
  osaka_vect <- terra::vect(osaka_circle)
  osaka_raster <- terra::crop(builtup_data, osaka_vect)
  osaka_raster_cropped <- terra::mask(
    osaka_raster, osaka_vect
  )
  return(osaka_raster_cropped)
}

osaka_raster_cropped <- crop_builtup_data()
terra::plot(osaka_raster_cropped)


# 4. IMAGE TO DATA.FRAME
#-----------------------

raster_to_df <- function() {
  osaka_df <- terra::as.data.frame(
    osaka_raster_cropped,
    xy = T
  )
  
  return(osaka_df)
}

osaka_df <- raster_to_df()
head(osaka_df)
names(osaka_df)[3] <- "value"

# define categorical values
osaka_df$cat <- round(osaka_df$value, 0)
osaka_df$cat <- factor(osaka_df$cat,
                        labels = c("No Built-up", "New", "Existing")
)

# 5. GET osaka ROADS FROM OSM DATA
#---------------------------------
road_tags <- c(
  "motorway", "trunk", "primary", "secondary",
  "tertiary", "motorway_link", "trunk_link", 
  "primary_link", "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
  bbox <- sf::st_bbox(osaka_circle)
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
osaka_roads <- roads$osm_lines |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crs = crsLONGLAT)

ggplot() +
  geom_sf(
    data = osaka_circle, fill = "transparent",
    color = "#3036ff", size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = osaka_roads,
    color = "#e83778", inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# 6. CROP osaka ROADS WITH BUFFER
#--------------------------------
osaka_roads_cropped <- sf::st_intersection(
  osaka_roads, osaka_circle
)

ggplot() +
  geom_sf(
    data = osaka_circle,
    color = "#3036ff", fill = NA,
    size = 1.2, inherit.aes = FALSE
  ) +
  geom_sf(
    data = osaka_roads_cropped, fill = "transparent",
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
    data = osaka_df,
    aes(x = x, y = y, fill = cat),
    alpha = 1
  ) +
  geom_sf(
    data = osaka_roads_cropped,
    color = "grey95",
    size = .05,
    alpha = .3,
    fill = "transparent"
  ) +
  geom_sf(
    data = osaka_circle,
    color = "grey20",
    size = 2,
    fill = "transparent"
  ) +
  geom_sf(data = osaka_border,
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
    title = "Osaka",
    subtitle = "",
    caption = "©2023 Milos Popovic (https://milospopovic.net)\nData: GLAD Built-up Change Data & ©OpenStreetMap contributors"
  )

ggsave(
  filename = "/Users/cyberhbliu/Desktop/30daymapchallenge/21 raster/osaka_built_up.svg",
  width = 6, height = 6, dpi = 600,
  device = "svg", p
)
p

