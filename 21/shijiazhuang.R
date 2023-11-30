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
  l <- rlinks[grepl("40N_100E", rlinks, rlinks)]
  builtup_data <- terra::rast(l)
  terra::crs(builtup_data) <- crsLONGLAT
  
  return(builtup_data)
}

builtup_data <- load_builtup_data()
terra::plot(builtup_data)


# Shijiazhuang BOUNDARIES FROM OSM DATA
#--------------------------------------
city <- "Shijiazhuang"

Shijiazhuang_border <- osmdata::getbb(
  city,
  format_out = "sf_polygon"
) |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crsLONGLAT)

plot(Shijiazhuang_border, add = T)


# MAKE BUFFER AROUND Shijiazhuang
#----------------------------
get_buffer <- function() {
  Shijiazhuang_cents <- sf::st_centroid(Shijiazhuang_border)
  Shijiazhuang_circle <- sf::st_buffer(
    Shijiazhuang_cents,
    dist = units::set_units(20, km)
  ) |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crs = crsLONGLAT)
  
  return(Shijiazhuang_circle)
}

Shijiazhuang_circle <- get_buffer()

# plot
ggplot() +
  geom_sf(
    data = Shijiazhuang_border, color = "#3036ff",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = Shijiazhuang_circle, color = "#e83778",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))


crop_builtup_data <- function() {
  Shijiazhuang_vect <- terra::vect(Shijiazhuang_circle)
  Shijiazhuang_raster <- terra::crop(builtup_data, Shijiazhuang_vect)
  Shijiazhuang_raster_cropped <- terra::mask(
    Shijiazhuang_raster, Shijiazhuang_vect
  )
  return(Shijiazhuang_raster_cropped)
}

Shijiazhuang_raster_cropped <- crop_builtup_data()
terra::plot(Shijiazhuang_raster_cropped)


# 4. IMAGE TO DATA.FRAME
#-----------------------

raster_to_df <- function() {
  Shijiazhuang_df <- terra::as.data.frame(
    Shijiazhuang_raster_cropped,
    xy = T
  )
  
  return(Shijiazhuang_df)
}

Shijiazhuang_df <- raster_to_df()
head(Shijiazhuang_df)
names(Shijiazhuang_df)[3] <- "value"

# define categorical values
Shijiazhuang_df$cat <- round(Shijiazhuang_df$value, 0)
Shijiazhuang_df$cat <- factor(Shijiazhuang_df$cat,
                        labels = c("No Built-up", "New", "Existing")
)

# 5. GET Shijiazhuang ROADS FROM OSM DATA
#---------------------------------
road_tags <- c(
  "motorway", "trunk", "primary", "secondary",
  "tertiary", "motorway_link", "trunk_link", 
  "primary_link", "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
  bbox <- sf::st_bbox(Shijiazhuang_circle)
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
Shijiazhuang_roads <- roads$osm_lines |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crs = crsLONGLAT)

ggplot() +
  geom_sf(
    data = Shijiazhuang_circle, fill = "transparent",
    color = "#3036ff", size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = Shijiazhuang_roads,
    color = "#e83778", inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# 6. CROP Shijiazhuang ROADS WITH BUFFER
#--------------------------------
Shijiazhuang_roads_cropped <- sf::st_intersection(
  Shijiazhuang_roads, Shijiazhuang_circle
)

ggplot() +
  geom_sf(
    data = Shijiazhuang_circle,
    color = "#3036ff", fill = NA,
    size = 1.2, inherit.aes = FALSE
  ) +
  geom_sf(
    data = Shijiazhuang_roads_cropped, fill = "transparent",
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
    data = Shijiazhuang_df,
    aes(x = x, y = y, fill = cat),
    alpha = 1
  ) +
  geom_sf(
    data = Shijiazhuang_roads_cropped,
    color = "grey95",
    size = .05,
    alpha = .3,
    fill = "transparent"
  ) +
  geom_sf(
    data = Shijiazhuang_circle,
    color = "grey20",
    size = 2,
    fill = "transparent"
  ) +
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
    title = "Shijiazhuang",
    subtitle = "",
    caption = "©2023 Milos Popovic (https://milospopovic.net)\nData: GLAD Built-up Change Data & ©OpenStreetMap contributors"
  )

ggsave(
  filename = "/Users/cyberhbliu/Desktop/30daymapchallenge/21 raster/Shijiazhuang_built_up.svg",
  width = 6, height = 6, dpi = 600,
  device = "svg", p
)
p

