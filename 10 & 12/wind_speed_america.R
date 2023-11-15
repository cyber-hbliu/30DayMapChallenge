
# 1. INSTALL & LOAD LIBRARIES
#----------------------------

libs <- c(
  "ecmwfr", "tidyverse", "metR",
  "terra", "sf", "giscoR", "classInt"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == F)){
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(libs, library, character.only = T))

# 1. QUERY WIND DATA
#--------------------

my_api <- "267474" # PLEASE INSERT YOUR UID
my_key <- "e15e2588-3af4-41a6-b0e9-e6d841208a14" # PLEASE INSERT YOUR API KEY

library(lubridate)
library(hms)

time <- seq(
  from = lubridate::ymd_hms(
    paste(2023, 11, 10, 00, 00, 00,
          sep = "-"
    )
  ), 
  to = lubridate::ymd_hms(
    paste(2023, 11, 10, 23, 00, 00,
          sep = "-"
    )
  ),
  by = "1 hours"
)

hours <- hms::as_hms(time)

ymin <- -64.00
xmin <- -180.25
ymax <- 84.94
xmax <- -0.25

request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c(
    "10m_u_component_of_wind",
    "10m_v_component_of_wind"
  ),
  year = "2023",
  month = "11",
  day = "10",
  time = hours,
  area = c(
    ymax, xmin, ymin, xmax
  ),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "download.nc"
)

ecmwfr::wf_set_key(
  user = my_api,
  key = my_key,
  service = "cds"
)

ecmwfr::wf_request(
  request = request,
  user = my_api,
  path = getwd()
)

# 'area': [
#   84.7, -172.5, -11.3,
#   -10,
# ],

  # 2. LOAD WIND DATA
#--------------------

north_america_wind <- terra::rast(
  "download.nc"
)

north_america_wind_df <- as.data.frame(north_america_wind, xy = TRUE, na.rm = TRUE)

head(north_america_wind_df)


# 3. GET AVERAGE U & V COMPONENT
#-------------------------------

u <- north_america_wind_df |>
  dplyr::select(
    x, y, 
    dplyr::starts_with(
      "u"
    )
  ) |>
  tidyr::pivot_longer(
    !c("x", "y"),
    names_to = "time",
    values_to = "u10"
  ) |>
  dplyr::group_by(x, y, .groups = "keep") |>
  dplyr::summarise(
    u = mean(u10)
  ) |>
  dplyr::rename(
    "lon" = "x",
    "lat" = "y"
  ) |>
  dplyr::select(
    lon, lat, u
  )

head(u)

v <- north_america_wind_df |>
  dplyr::select(
    x, y, 
    dplyr::starts_with(
      "v"
    )
  ) |>
  tidyr::pivot_longer(
    !c("x", "y"),
    names_to = "time",
    values_to = "v10"
  ) |>
  dplyr::group_by(x, y, .groups = "keep") |>
  dplyr::summarise(
    v = mean(v10)
  ) |>
  dplyr::rename(
    "lon" = "x",
    "lat" = "y"
  ) |>
  dplyr::select(
    lon, lat, v
  )

head(v)

# 4. MERGE U & V COMPONENT
#-------------------------

north_america_wind_stream <- dplyr::inner_join(
  u, v, by = c("lon", "lat"),
  relationship = "many-to-many"
) |>
  dplyr::as_tibble()

# 5. North America SHAPEFILE
#--------------------

get_na_sf <- function(){
  na_sf <- giscoR::gisco_get_countries(
    region = c("Americas"),
    resolution = "3"
  )
  
  return(na_sf)
}

na_sf <- get_na_sf()



# 6. BREAKS
#----------

north_america_wind_stream$speed <- 
  sqrt(north_america_wind_stream$u^2 + north_america_wind_stream$v^2)

breaks <- classInt::classIntervals(
  north_america_wind_stream$speed,
  n = 6,
  style = "equal"
)$brks

# 6. MAP
#-------

p <- ggplot(data = north_america_wind_stream) +
  geom_sf(
    data = na_sf,
    fill = "black",
    color = "#262626",
    size = .5,
    alpha = 0.5
  ) +
  metR::geom_streamline(
    aes(
      x = lon,
      y = lat,
      dx = u,
      dy = v,
      color = sqrt(..dx..^2 + ..dy..^2),
      alpha = sqrt(..dx..^2 + ..dy..^2),
      linewidth = sqrt(..dx..^2 + ..dy..^2) / 2,
    ),
    L = 4,
    res = 10,
    n = 100,
    arrow = NULL,
    lineend = "round",
    inherit.aes = F
  ) +
  scale_color_gradientn(
    name = "Average speed (m/s)",
    colours = hcl.colors(
      12, "Mako"
    ),
    breaks = round(breaks, 0)
  ) +
  scale_alpha(
    range = c(.2, 1)
  ) +
  scale_linewidth(
    range = c(.1, .5)
  ) +
  coord_sf(
    crs = "EPSG:4326",
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax)
  ) +
  guides(
    alpha = "none",
    linewidth = "none",
    color = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = 0,
      nrow = 1
    )
  ) + theme_void() +
  theme(
    legend.position = c(.85, 1.01),
    legend.title = element_text(
      size = 11,
      color = "white"
    ),
    legend.text = element_text(
      size = 9,
      color = "white"
    ),
    plot.title = element_text(
      size = 16,
      color = "white",
      hjust = .1,
      vjust = -1
    ),
    plot.subtitle = element_text(
      size = 9,
      color = "white",
      hjust = .2,
      vjust = -1
    ),
    plot.background = element_rect(
      fill = "#262626",
      color = NA
    )
    # plot.margin = unit(
    #   c(
    #     t = 0, r = -3,
    #     b = -3, l = -3
    #   ), "lines"
    # )
  ) +
  labs(
    title = "Wind speed on 10th November 2023",
    subtitle = "Source: Climate Change Service, ERA5 hourly data on single levels from 1940 to present",
    x = "",
    y = ""
  )

p

ggsave(p, filename = "/Users/cyberhbliu/Desktop/30daymapchallenge/10 north_america/p.png")
