library(tidyverse)
library(here)
library(terrainr)
library(raster)
library(viridis)
library(ggnewscale)
library(ggpubr)

house <- 1

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

Bantou_brt <- raster(paste0("Predictions/Bantou_house_",house,".tif"))
Bantou_brt <- crop(Bantou_brt, Bantou_extent)
Bantou_brt_extent <- extent(Bantou_brt)

Bantou_brt_extent[2] <- Bantou_brt_extent[1] + 0.006433385
Bantou_brt_extent[4] <- Bantou_brt_extent[3] + 0.0055786
Bantou_brt <- crop(Bantou_brt, Bantou_brt_extent)

Bantou_brt <- rasterToPoints(Bantou_brt, spatial = T)
Bantou_brt <- Bantou_brt |> as.data.frame()

anchor_x = Bantou_brt_extent[1] + 0.0002
anchor_y = Bantou_brt_extent[3] + 0.00018
stdist = 0.025
scaledist = 100
scaleheight = 0.05

Bantou <- crop(Bantou, Bantou_extent)
Bantou <- crop(Bantou, Bantou_brt_extent)
Bantou <- as.data.frame(Bantou, xy= TRUE) |>
  rename(Red = Bantou_1,
         Green = Bantou_2,
         Blue = Bantou_3) |>
  dplyr::select(-Bantou_4)

Bantou_base_plt <- ggplot() +
  theme_void() +
  theme(legend.position = "none") +
  geom_raster(data = Bantou |> mutate(Red = replace_na(Red, 255),
                                      Green = replace_na(Green, 255),
                                      Blue = replace_na(Blue,255)),
              aes(x=x, y=y,
                  fill=rgb(r=Red,
                           g=Green,
                           b=Blue,
                           maxColorValue = 255))) +
  scale_fill_identity() +
  theme(legend.position = "none") +
  coord_equal()

Bantou_base_plt_bar <- Bantou_base_plt +
  ggsn::scalebar(x.min = Bantou_brt_extent[1],
                 x.max = Bantou_brt_extent[2],
                 y.min = Bantou_brt_extent[3],
                 y.max = Bantou_brt_extent[4],
                 location = "topleft",
                 anchor = c(
                   x = anchor_x,
                   y = anchor_y),
                 dist = scaledist,
                 dist_unit = "m",
                 st.size = 9,
                 st.dist = stdist,
                 st.color = 'white',
                 height = scaleheight,
                 transform = T,
                 st.bottom = F,
                 model = 'WGS84')

ggsave(paste0("Figures/Bantou_base_house_",house,".png"), Bantou_base_plt_bar, width = 8, height = 8)

Bantou_base_plt_traps <- Bantou_base_plt +
  geom_point(data = pnts |> filter(Site == "Bantou",
                                   Longitude > Bantou_brt_extent[1],
                                   Longitude < Bantou_brt_extent[2],
                                   Latitude > Bantou_brt_extent[3],
                                   Latitude < Bantou_brt_extent[4]),
             aes(x = Longitude,
                 y = Latitude,
                 col = as.factor(Type),
                 shape = as.factor(Type)), size = 1.25) +
  ggsn::scalebar(x.min = Bantou_brt_extent[1],
                 x.max = Bantou_brt_extent[2],
                 y.min = Bantou_brt_extent[3],
                 y.max = Bantou_brt_extent[4],
                 location = "topleft",
                 anchor = c(
                   x = anchor_x,
                   y = anchor_y),
                 dist = scaledist,
                 dist_unit = "m",
                 st.size = 9,
                 st.dist = stdist,
                 st.color = 'white',
                 height = scaleheight,
                 transform = T,
                 st.bottom = F,
                 model = 'WGS84')

ggsave(paste0("Figures/Bantou_traps_house_",house,".png"), Bantou_base_plt_traps, width = 8, height = 8)

Bantou_brt_plt <- Bantou_base_plt +
  ggnewscale::new_scale_fill() +
  geom_raster(data = Bantou_brt |> set_names(c("Bantou", "x", "y")), aes(x = x, y = y, fill=Bantou), alpha = 0.5) +
  scale_fill_viridis_c(name="", option = 'magma') +
  ggsn::scalebar(x.min = Bantou_brt_extent[1],
                 x.max = Bantou_brt_extent[2],
                 y.min = Bantou_brt_extent[3],
                 y.max = Bantou_brt_extent[4],
                 location = "topleft",
                 anchor = c(
                   x = anchor_x,
                   y = anchor_y),
                 dist = scaledist,
                 dist_unit = "m",
                 st.size = 9,
                 st.dist = stdist,
                 st.color = 'white',
                 height = scaleheight,
                 transform = T,
                 st.bottom = F,
                 model = 'WGS84')

ggsave(paste0("Figures/Bantou_brt_house_",house,".png"), width = 8, height = 8)

Bantou_brt_legend <- get_legend(Bantou_brt_plt +
                                   theme(legend.position = "right", legend.text = element_text(size=12)) +
                                   guides(shape = guide_legend(override.aes = list(size = 5)))) |>
  as_ggplot()

ggsave(paste0("Figures/Bantou_brt_legend_house_",house,".png"), Bantou_brt_legend, height = 8)
