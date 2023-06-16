library(tidyverse)
library(here)
library(terrainr)
library(terra)
library(viridis)
library(ggnewscale)
library(ggpubr)

# Load in visit data to auto crop rasters
pnts <- read.csv("data/Trap_Data/Clean_Both_Data_By_Trap.csv")

Bantou <- rast(here("data", "Bantou.tif"))
Bantou_extent <- pnts |>
  filter(Site == "Bantou") |>
  summarize(xmin = min(Longitude),
            xmax = max(Longitude),
            ymin = min(Latitude),
            ymax = max(Latitude)) |>
  unlist() |>
  extent()

Bantou_brt <- raster(here("Predictions/Bantou.tif"))
Bantou_brt <- crop(Bantou_brt, Bantou_extent)
Bantou_brt <- rasterToPoints(Bantou_brt, spatial = T)
Bantou_brt <- Bantou_brt |> as.data.frame()
Bantou_brt_extent <- Bantou_brt |> summarize(xmin = min(x),
                                                 xmax = max(x),
                                                 ymin = min(y),
                                                 ymax = max(y)) |>
  unlist() |>
  extent()

Bantou <- crop(Bantou, Bantou_extent)
Bantou <- crop(Bantou, Bantou_brt_extent)
Bantou <- as.data.frame(Bantou, xy= TRUE) |>
  rename(Red = Bantou_1,
         Green = Bantou_2,
         Blue = Bantou_3)

Bantou_base_plt <- ggplot() +
  theme_void() +
  theme(legend.position = "none") +
  geom_raster(data = Bantou,
              aes(x=x, y=y,
                  fill=rgb(r=Red,
                           g=Green,
                           b=Blue,
                           maxColorValue = 255))) +
  scale_fill_identity() +
  theme(legend.position = "none") +
  coord_equal()

ggsave("Bantou_base.png", width = 8, height = 8)

Bantou_brt_plt <- Bantou_base_plt +
  ggnewscale::new_scale_fill() +
  geom_raster(data = Bantou_brt, aes(x = x, y = y, fill=Bantou), alpha = 0.5) +
  scale_fill_viridis_c(name="", option = 'magma')

ggsave("Bantou_brt.png", width = 8, height = 8)

Bantou_brt_legend <- get_legend(Bantou_brt_plt +
                                   theme(legend.position = "right", legend.text = element_text(size=12)) +
                                   guides(shape = guide_legend(override.aes = list(size = 5)))) |>
  as_ggplot()

ggsave("Bantou_brt_legend.png", Bantou_brt_legend)
