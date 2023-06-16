library(tidyverse)
library(here)
library(terrainr)
library(terra)
library(viridis)
library(ggnewscale)

# Load in visit data to auto crop rasters
pnts <- read.csv("data/Trap_Data/Clean_Both_Data_By_Trap.csv")

Bafodia <- rast(here("data", "Bafodia.tif"))
Bafodia_extent <- pnts |>
  filter(Site == "Bafodia") |>
  summarize(xmin = min(Longitude),
            xmax = max(Longitude),
            ymin = min(Latitude),
            ymax = max(Latitude)) |>
  unlist() |>
  extent()

Bafodia_brt <- raster(here("Predictions/Bafodia.tif"))
Bafodia_brt <- crop(Bafodia_brt, Bafodia_extent)
Bafodia_brt <- rasterToPoints(Bafodia_brt, spatial = T)
Bafodia_brt <- Bafodia_brt |> as.data.frame()
Bafodia_brt_extent <- Bafodia_brt |> summarize(xmin = min(x),
                                                 xmax = max(x),
                                                 ymin = min(y),
                                                 ymax = max(y)) |>
  unlist() |>
  extent()

Bafodia <- crop(Bafodia, Bafodia_extent)
Bafodia <- crop(Bafodia, Bafodia_brt_extent)
Bafodia <- as.data.frame(Bafodia, xy= TRUE) |>
  rename(Red = Bafodia_1,
         Green = Bafodia_2,
         Blue = Bafodia_3)

Bafodia_base_plt <- ggplot() +
  theme_void() +
  theme(legend.position = "none") +
  geom_raster(data = Bafodia,
              aes(x=x, y=y,
                  fill=rgb(r=Red,
                           g=Green,
                           b=Blue,
                           maxColorValue = 255))) +
  scale_fill_identity() +
  theme(legend.position = "none") +
  coord_equal()

ggsave("Bafodia_base.png", width = 8, height = 8)

Bafodia_brt_plt <- Bafodia_base_plt +
  ggnewscale::new_scale_fill() +
  geom_raster(data = Bafodia_brt, aes(x = x, y = y, fill=Bafodia), alpha = 0.5) +
  scale_fill_viridis_c(name="", option = 'magma')

ggsave("Bafodia_brt.png", width = 8, height = 8)

Bafodia_brt_legend <- get_legend(Bafodia_brt_plt +
                                    theme(legend.position = "right", legend.text = element_text(size=12)) +
                                    guides(shape = guide_legend(override.aes = list(size = 5)))) |>
  as_ggplot()

ggsave("Bafodia_brt_legend.png", Bafodia_brt_legend)
