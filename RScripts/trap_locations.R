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
          cultivation = "#ffa500")

cols <- tibble(color = cols, land_cover = names(cols)) |> mutate(value = 1:n() - 1)

map(sites, function(site_name) {

  site_data <- trap_data |>
    filter(Site == site_name,
           Type == "InTown") |>
    mutate(trap = "Trap")

  # The 4e-4 just adds a little border around the edge trap locations.
  site_extent <- raster::extent(min(site_data$Longitude, na.rm=T) - 4e-4,
                                max(site_data$Longitude, na.rm=T) + 4e-4,
                                min(site_data$Latitude, na.rm=T) - 4e-4,
                                max(site_data$Latitude, na.rm=T) + 4e-4)

  site_rast <- terra::rast(here("data", paste0(site_name, ".tif"))) |>
    crop(site_extent) |>
    as.data.frame(xy = T) |>
    setNames(c("x", "y", "r", "g", "b", "alpha")) %>%
    mutate(grey = . |> select(r, b, g) |> rowMeans(na.rm=T))

  ggplot(site_rast, aes(x=x, y=y, r=r, b=b, g=g)) + geom_spatial_rgb()

  site_land_cover <- terra::rast(here("data/land_cover_predictions/", paste0(tolower(site_name), "_prediction.tif"))) |>
    crop(site_extent) |>
    as.data.frame(xy = T)

  site_land_cover <- site_land_cover[,c(1,2,3)] |>
    setNames(c("x", "y", "value")) |>
    left_join(cols)

  site_name <- case_when(site_name == "Tanganya" ~ "GTB",
                         site_name == "Bafodia" ~ "SLTA",
                         site_name == "Bantou" ~ "GTA")

  site_plot <- ggplot() +
    geom_raster(data = site_land_cover, aes(x=x, y=y, fill = land_cover)) +
    scale_fill_manual(name = "Land cover", values = cols$color) +
    ggnewscale::new_scale_fill() +
    geom_raster(data = site_rast, aes(x=x, y=y, fill = grey), alpha = 0.6) +
    scale_fill_distiller(type = "seq",
                         direction = -1,
                         palette = "Greys",
                         guide = "none") +
    coord_equal() +
    theme_classic() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_point(data = site_data,
               aes(x = Longitude,
                   y = Latitude,
                   col = as.factor(trap)),
               size = 1) +
    scale_color_manual(values = c("black")) +
    ggtitle(site_name) +
    labs(x = "Longitude",
         y = "Latitude",
         color = "") +
    theme(panel.border = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank())

  filename <- paste0("Figures/", site_name, "_traps.tiff")
  ggsave(filename, site_plot)
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
