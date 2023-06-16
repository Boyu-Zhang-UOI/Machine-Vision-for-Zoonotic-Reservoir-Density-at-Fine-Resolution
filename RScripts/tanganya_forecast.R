library(tidyverse)
library(here)
library(terrainr)
library(terra)
library(viridis)
library(ggnewscale)

# Load in visit data to auto crop rasters
pnts <- read.csv("data/Trap_Data/Clean_Both_Data_By_Trap.csv")

Tanganya <- rast(here("data", "Tanganya.tif"))
Tanganya_extent <- pnts |>
  filter(Site == "Tanganya") |>
  summarize(xmin = min(Longitude),
            xmax = max(Longitude),
            ymin = min(Latitude),
            ymax = max(Latitude)) |>
  unlist() |>
  extent()

Tanganya_brt <- raster(here("Predictions/Tanganya.tif"))
Tanganya_brt <- crop(Tanganya_brt, Tanganya_extent)
Tanganya_brt <- rasterToPoints(Tanganya_brt, spatial = T)
Tanganya_brt <- Tanganya_brt |> as.data.frame()
Tanganya_brt_extent <- Tanganya_brt |> summarize(xmin = min(x),
                                                 xmax = max(x),
                                                 ymin = min(y),
                                                 ymax = max(y)) |>
  unlist() |>
  extent()

Tanganya <- crop(Tanganya, Tanganya_extent)
Tanganya <- crop(Tanganya, Tanganya_brt_extent)
Tanganya <- as.data.frame(Tanganya, xy= TRUE) |>
  rename(Red = Tanganya_1,
         Green = Tanganya_2,
         Blue = Tanganya_3)

Tanganya_base_plt <- ggplot() +
  theme_void() +
  theme(legend.position = "none") +
  geom_raster(data = Tanganya,
              aes(x=x, y=y,
                  fill=rgb(r=Red,
                           g=Green,
                           b=Blue,
                           maxColorValue = 255))) +
  scale_fill_identity() +
  theme(legend.position = "none") +
  coord_equal()

ggsave("Tanganya_base.png", width = 8, height = 8)

Tanganya_brt_plt <- Tanganya_base_plt +
  ggnewscale::new_scale_fill() +
  geom_raster(data = Tanganya_brt, aes(x = x, y = y, fill=Tanganya), alpha = 0.5) +
  scale_fill_viridis_c(name="", option = 'magma')

ggsave("Tanganya_brt.png", width = 8, height = 8)

Tanganya_brt_legend <- get_legend(Tanganya_brt_plt +
                                    theme(legend.position = "right", legend.text = element_text(size=12)) +
                                    guides(shape = guide_legend(override.aes = list(size = 5)))) |>
  as_ggplot()

ggsave("Tanganya_brt_legend.png", Tanganya_brt_legend)
