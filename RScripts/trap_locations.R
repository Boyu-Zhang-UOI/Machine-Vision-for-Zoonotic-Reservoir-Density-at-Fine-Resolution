library(tidyverse)
library(terra)
library(terrainr)
library(here)

trap_data <- read_csv(here("Data/Trap_Data/Clean_Both_Data_By_Trap.csv"))
sites <- trap_data |> pull(Site) |> unique()

cols <- c(bare = "#ffff00",
          grass = "#ff6eb4",
          tree = "#a020f0",
          burned = "#a9a9a9",
          rice = "#ff0000",
          cloud = "#ffffff",
          water = "#87ceeb",
          mound = "#ffa500")

cols <- tibble(color = cols, land_cover = names(cols)) |>
  mutate(value = 1:n() - 1)

map(sites, function(site_name) {

  site_data <- trap_data |>
    filter(Site == site_name) |>
    mutate(trap = "Trap") # Needed for color legend in ggplot2.

  # The 4e-4 just adds a little border around the edge trap locations.
  site_extent <- raster::extent(min(site_data$Longitude, na.rm=T) - 4e-4,
                                max(site_data$Longitude, na.rm=T) + 4e-4,
                                min(site_data$Latitude, na.rm=T) - 4e-4,
                                max(site_data$Latitude, na.rm=T) + 4e-4)

  asp <-  (site_extent[4] - site_extent[3]) / (site_extent[2] - site_extent[1])

  site_rast <- terra::rast(paste0("Data/", site_name, ".tif")) |>
    crop(site_extent)

  site_rast_df <- site_rast |>
    as.data.frame(xy = T) |>
    setNames(c("x", "y", "r", "g", "b", "alpha")) %>%
    mutate(grey = . |> select(r, g, b) |> rowMeans(na.rm=T))

  land_cover_rast <- terra::rast(here("Data/land_cover_predictions/rasters_lc/", paste0(site_name, "_lc_fea_25m_res_1m.tif")))

  # land_cover_rast[[c(-9, -6, -5)]]
  land_cover_rast <- land_cover_rast[[-9]]

  if(site_name == "Bafodia") {
    anchor_x = site_extent[1] + 0.0007
    anchor_y = site_extent[4] - 0.0004
    stdist = 0.03
    scaledist = 150
  }

  if(site_name == "Bantou") {
    anchor_x = site_extent[1] + 0.0007
    anchor_y = site_extent[3] + 0.0015
    stdist = 0.03
    scaledist = 500
  }

  if(site_name == "Tanganya") {
    anchor_x = site_extent[2] - 0.0034
    anchor_y = site_extent[3] + 0.0006
    stdist = 0.02
    scaledist = 150
  }

  land_cover_df <- land_cover_rast |>
    crop(site_extent) |>
    terra::which.max() |> # Choose band with highest type probability
    as.data.frame(xy = T) |>
    setNames(c("long","lat","z")) |>
    mutate(z = factor(z-1, levels = c(0:7)))

  site_code <- case_when(site_name == "Tanganya" ~ "GTB",
                         site_name == "Bafodia" ~ "SLTA",
                         site_name == "Bantou" ~ "GTA")

  site_data <- site_data |> mutate(Type = factor(Type, levels = c("InTown", "OutTown", "House")))

  site_plot <- ggplot() +
    geom_raster(data = land_cover_df, aes(x=long, y=lat, fill=z)) +
    scale_fill_manual(name = "Landcover",
                      values = cols$color |> as.vector(),
                      labels = cols$land_cover |> as.vector(),
                      drop=FALSE) +
    ggnewscale::new_scale_fill() +
    geom_raster(data = site_rast_df, aes(x=x, y=y, fill=grey), alpha = 0.6) +
    scale_fill_distiller(type = "seq",
                         direction = -1,
                         palette = "Greys",
                         guide = "none") +
    coord_equal() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_classic() +
    geom_point(data = site_data,
               aes(x = Longitude,
                   y = Latitude,
                   col = as.factor(Type),
                   shape = as.factor(Type)),
               size = 1.25) +
    scale_shape_manual(name = "Traps", values = c(15, 17, 16)) +
    scale_color_manual(name = "Traps", values = c("black", "darkblue", "darkgreen")) +
    ggtitle(site_code) +
    labs(x = "Longitude",
         y = "Latitude",
         color = "") +
    theme(panel.border = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          aspect.ratio = asp) +
    guides(shape = guide_legend(override.aes = list(size = 2))) +
    ggsn::scalebar(x.min = site_extent[1],
                   x.max = site_extent[2],
                   y.min = site_extent[3],
                   y.max = site_extent[4],
                   location = "topleft",
                   anchor = c(
                     x = anchor_x,
                     y = anchor_y),
                   dist = scaledist,
                   dist_unit = "m",
                   st.dist = stdist,
                   st.size = 3,
                   st.color = 'black',
                   transform = T,
                   model = 'WGS84')

  filename <- paste0("Figures/", site_name, ".png")
  ggsave(filename, site_plot, width = 5, height = asp * 5)
})



# geom_spatial_rgb(
#   data = site_rast |>
#     as.data.frame(xy = T) |>
#     setNames(c("x","y","r", "g", "b", "alpha")) |>
#     drop_na(r,g,b),
#   mapping = aes(
#     x = x,
#     y = y,
#     r = r,
#     g = g,
#     b = b)) +
# ggnewscale::new_scale("fill") +
# geom_raster(data = site_land_cover, aes(x = x, y = y, fill = prediction), alpha = 0.3) +
# scale_fill_manual(values = cols) +
