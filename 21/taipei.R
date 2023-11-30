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
  l <- rlinks[grepl("30N_120E", rlinks, rlinks)]
  builtup_data <- terra::rast(l)
  terra::crs(builtup_data) <- crsLONGLAT
  
  return(builtup_data)
}

builtup_data <- load_builtup_data()


# taipei BOUNDARIES FROM OSM DATA
#--------------------------------------
city <- "Taibei"

taipei_border <- osmdata::getbb(
  city,
  format_out = "sf_polygon"
) |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crsLONGLAT)

terra::plot(builtup_data)
plot(taipei_border, add = T)


# MAKE BUFFER AROUND taipei
#----------------------------
get_buffer <- function() {
  taipei_cents <- sf::st_centroid(taipei_border)
  taipei_circle <- sf::st_buffer(
    taipei_cents,
    dist = units::set_units(20, km)
  ) |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crs = crsLONGLAT)
  
  return(taipei_circle)
}

taipei_circle <- get_buffer()

# plot
ggplot() +
  geom_sf(
    data = taipei_border, color = "#3036ff",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = taipei_circle, color = "#e83778",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))


crop_builtup_data <- function() {
  taipei_vect <- terra::vect(taipei_circle)
  taipei_raster <- terra::crop(builtup_data, taipei_vect)
  taipei_raster_cropped <- terra::mask(
    taipei_raster, taipei_vect
  )
  return(taipei_raster_cropped)
}

taipei_raster_cropped <- crop_builtup_data()
terra::plot(taipei_raster_cropped)


# 4. IMAGE TO DATA.FRAME
#-----------------------

raster_to_df <- function() {
  taipei_df <- terra::as.data.frame(
    taipei_raster_cropped,
    xy = T
  )
  
  return(taipei_df)
}

taipei_df <- raster_to_df()
head(taipei_df)
names(taipei_df)[3] <- "value"

# define categorical values
taipei_df$cat <- round(taipei_df$value, 0)
taipei_df$cat <- factor(taipei_df$cat,
                        labels = c("No Built-up", "New", "Existing")
)

# 5. GET taipei ROADS FROM OSM DATA
#---------------------------------
road_tags <- c(
  "motorway", "trunk", "primary", "secondary",
  "tertiary", "motorway_link", "trunk_link", 
  "primary_link", "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
  bbox <- sf::st_bbox(taipei_circle)
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
taipei_roads <- roads$osm_lines |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crs = crsLONGLAT)

ggplot() +
  geom_sf(
    data = taipei_circle, fill = "transparent",
    color = "#3036ff", size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = taipei_roads,
    color = "#e83778", inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# 6. CROP taipei ROADS WITH BUFFER
#--------------------------------
taipei_roads_cropped <- sf::st_intersection(
  taipei_roads, taipei_circle
)

ggplot() +
  geom_sf(
    data = taipei_circle,
    color = "#3036ff", fill = NA,
    size = 1.2, inherit.aes = FALSE
  ) +
  geom_sf(
    data = taipei_roads_cropped, fill = "transparent",
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
    data = taipei_df,
    aes(x = x, y = y, fill = cat),
    alpha = 1
  ) +
  geom_sf(
    data = taipei_roads_cropped,
    color = "grey95",
    size = .05,
    alpha = .3,
    fill = "transparent"
  ) +
  geom_sf(
    data = taipei_circle,
    color = "grey20",
    size = 2,
    fill = "transparent"
  ) +
  geom_sf(data = taipei_border,
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
    title = "Taipei",
    subtitle = "",
    caption = "©2023 Milos Popovic (https://milospopovic.net)\nData: GLAD Built-up Change Data & ©OpenStreetMap contributors"
  )

ggsave(
  filename = "/Users/cyberhbliu/Desktop/30daymapchallenge/21 raster/taipei_built_up.png",
  width = 6, height = 6, dpi = 600,
  device = "png", p
)
p


#=-------------------------
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
  l <- rlinks[grepl("30N_120E", rlinks, rlinks)]
  builtup_data <- terra::rast(l)
  terra::crs(builtup_data) <- crsLONGLAT
  
  return(builtup_data)
}

builtup_data <- load_builtup_data()


# taipei BOUNDARIES FROM OSM DATA
#--------------------------------------
city <- "Taibei"

taipei_border <- osmdata::getbb(
  city,
  format_out = "sf_polygon"
) |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crsLONGLAT)

terra::plot(builtup_data)
plot(taipei_border, add = T)


# MAKE BUFFER AROUND taipei
#----------------------------
get_buffer <- function() {
  taipei_cents <- sf::st_centroid(taipei_border)
  taipei_circle <- sf::st_buffer(
    taipei_cents,
    dist = units::set_units(20, km)
  ) |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crs = crsLONGLAT)
  
  return(taipei_circle)
}

taipei_circle <- get_buffer()

# plot
ggplot() +
  geom_sf(
    data = taipei_border, color = "#3036ff",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = taipei_circle, color = "#e83778",
    fill = "transparent", size = 1.5,
    inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))


crop_builtup_data <- function() {
  taipei_vect <- terra::vect(taipei_circle)
  taipei_raster <- terra::crop(builtup_data, taipei_vect)
  taipei_raster_cropped <- terra::mask(
    taipei_raster, taipei_vect
  )
  return(taipei_raster_cropped)
}

taipei_raster_cropped <- crop_builtup_data()
terra::plot(taipei_raster_cropped)


# 4. IMAGE TO DATA.FRAME
#-----------------------

raster_to_df <- function() {
  taipei_df <- terra::as.data.frame(
    taipei_raster_cropped,
    xy = T
  )
  
  return(taipei_df)
}

taipei_df <- raster_to_df()
head(taipei_df)
names(taipei_df)[3] <- "value"

# define categorical values
taipei_df$cat <- round(taipei_df$value, 0)
taipei_df$cat <- factor(taipei_df$cat,
                        labels = c("No Built-up", "New", "Existing")
)

# 5. GET taipei ROADS FROM OSM DATA
#---------------------------------
road_tags <- c(
  "motorway", "trunk", "primary", "secondary",
  "tertiary", "motorway_link", "trunk_link", 
  "primary_link", "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
  bbox <- sf::st_bbox(taipei_circle)
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
taipei_roads <- roads$osm_lines |>
  sf::st_set_crs(crsLONGLAT) |>
  sf::st_transform(crs = crsLONGLAT)

ggplot() +
  geom_sf(
    data = taipei_circle, fill = "transparent",
    color = "#3036ff", size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = taipei_roads,
    color = "#e83778", inherit.aes = FALSE
  ) +
  theme_void() +
  theme(panel.grid.major = element_line("transparent"))

# 6. CROP taipei ROADS WITH BUFFER
#--------------------------------
taipei_roads_cropped <- sf::st_intersection(
  taipei_roads, taipei_circle
)

ggplot() +
  geom_sf(
    data = taipei_circle,
    color = "#3036ff", fill = NA,
    size = 1.2, inherit.aes = FALSE
  ) +
  geom_sf(
    data = taipei_roads_cropped, fill = "transparent",
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
    data = taipei_df,
    aes(x = x, y = y, fill = cat),
    alpha = 1
  ) +
  geom_sf(
    data = taipei_roads_cropped,
    color = "grey95",
    size = .05,
    alpha = .3,
    fill = "transparent"
  ) +
  geom_sf(
    data = taipei_circle,
    color = "grey20",
    size = 2,
    fill = "transparent"
  ) +
  geom_sf(data = taipei_border,
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
    title = "Taipei",
    subtitle = "",
    caption = "©2023 Milos Popovic (https://milospopovic.net)\nData: GLAD Built-up Change Data & ©OpenStreetMap contributors"
  )

ggsave(
  filename = "/Users/cyberhbliu/Desktop/30daymapchallenge/21 raster/taipei_built_up.svg",
  width = 6, height = 6, dpi = 600,
  device = "svg", p
)
p
