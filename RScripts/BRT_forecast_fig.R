library(tidyverse)
library(here)
library(terrainr)
library(terra)
library(viridis)
library(raster)

# Load in visit data to auto crop rasters
pnts <- read.csv("data/Trap_Data/Clean_Both_Data_By_Trap.csv")

tanganya <- rast(here("data", "Tanganya.tif"))
tanganya_extent <- pnts |>
  filter(Site == "Tanganya") |>
  summarize(xmin = min(Longitude),
            xmax = max(Longitude),
            ymin = min(Latitude),
            ymax = max(Latitude)) |>
  unlist() |>
  extent()

tanganya_brt <- raster(here("Predictions/Tanganya.tif"))
tanganya_brt <- crop(tanganya_brt, tanganya_extent)
tanganya_brt <- rasterToPoints(tanganya_brt, spatial = T)
tanganya_brt <- tanganya_brt |> as.data.frame()
tanganya_brt_extent <- tanganya_brt |> summarize(xmin = min(x),
                                                 xmax = max(x),
                                                 ymin = min(y),
                                                 ymax = max(y)) |>
  unlist() |>
  extent()

tanganya <- crop(tanganya, tanganya_extent)
tanganya <- crop(tanganya, tanganya_brt_extent)

ggplot() +
  theme_void() +
  geom_spatial_rgb(
    data = tanganya,
    mapping = aes(
      x = x,
      y = y,
      r = red,
      g = green,
      b = blue)) +
  coord_equal()

  ggsave("tanganya_base.png")

  tanganya_brt_plt <- ggplot(tanganya_brt, aes(x=x, y=y, z=Tanganya, col=Tanganya, fill=Tanganya)) +
    theme_void() +
    geom_tile() +
    scale_color_viridis(name="", limits=c(0, 0.2), breaks=seq(0,0.2,by=0.05)) +
    scale_fill_viridis(name="", limits=c(0, 0.2), breaks=seq(0,0.2,by=0.05)) +
    coord_equal()

  ggsave("tanganya_brt.png", width = 5, height = 6)
