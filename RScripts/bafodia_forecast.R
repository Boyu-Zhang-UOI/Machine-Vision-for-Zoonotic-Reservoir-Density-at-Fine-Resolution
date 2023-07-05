library(tidyverse)
library(here)
library(terrainr)
library(terra)
library(viridis)
library(ggnewscale)

house <- 1


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

Bafodia_brt <- raster(paste0("Predictions/Bafodia_house_",house,".tif"))
Bafodia_brt <- crop(Bafodia_brt, Bafodia_extent)
Bafodia_brt_extent <- extent(Bafodia_brt)

# Common extent
Bafodia_brt_extent[2] <- Bafodia_brt_extent[1] + 0.006433385
Bafodia_brt_extent[4] <- Bafodia_brt_extent[3] + 0.0055786

Bafodia_brt <- crop(Bafodia_brt, Bafodia_brt_extent)

Bafodia_brt <- rasterToPoints(Bafodia_brt, spatial = T)
Bafodia_brt <- Bafodia_brt |> as.data.frame()

anchor_x = Bafodia_brt_extent[1] + 0.0002
anchor_y = Bafodia_brt_extent[3] + 0.00018
stdist = 0.025
scaledist = 100
scaleheight = 0.035

Bafodia <- crop(Bafodia, Bafodia_extent)
Bafodia <- crop(Bafodia, Bafodia_brt_extent)
Bafodia <- as.data.frame(Bafodia, xy= TRUE) |>
  rename(Red = Bafodia_1,
         Green = Bafodia_2,
         Blue = Bafodia_3)

Bafodia_base_plt <- ggplot() +
  theme_void() +
  theme(legend.position = "none") +
  geom_raster(data = Bafodia |> mutate(Red = replace_na(Red, 255),
                                    Green = replace_na(Green, 255),
                                    Blue = replace_na(Blue, 255)),
              aes(x=x, y=y,
                  fill=rgb(r=Red,
                           g=Green,
                           b=Blue,
                           maxColorValue = 255))) +
  scale_fill_identity() +
  theme(legend.position = "none") +
  coord_equal()

Bafodia_base_plt_bar <- Bafodia_base_plt + ggsn::scalebar(x.min = Bafodia_extent[1],
               x.max = Bafodia_extent[2],
               y.min = Bafodia_extent[3],
               y.max = Bafodia_extent[4],
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

ggsave(paste0("Figures/Bafodia_base_house_",house,".png"), Bafodia_base_plt_bar, width = 8, height = 8)

Bafodia_base_plt_traps <- Bafodia_base_plt +
  geom_point(data = pnts |> filter(Site == "Bafodia",
                                   Longitude > Bafodia_brt_extent[1],
                                   Longitude < Bafodia_brt_extent[2],
                                   Latitude > Bafodia_brt_extent[3],
                                   Latitude < Bafodia_brt_extent[4]),
             aes(x = Longitude,
                 y = Latitude,
                 col = as.factor(Type),
                 shape = as.factor(Type)), size = 1.25) +
  ggsn::scalebar(x.min = Bafodia_extent[1],
                   x.max = Bafodia_extent[2],
                   y.min = Bafodia_extent[3],
                   y.max = Bafodia_extent[4],
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

ggsave(paste0("Figures/Bafodia_traps_house_",house,".png"), Bafodia_base_plt_traps, width = 8, height = 8)

Bafodia_brt_plt <- Bafodia_base_plt +
  ggnewscale::new_scale_fill() +
  geom_raster(data = Bafodia_brt |> set_names(c("Bafodia", "x", "y")) , aes(x = x, y = y, fill=Bafodia), alpha = 0.5) +
  scale_fill_viridis_c(name="", option = 'magma') +
  ggsn::scalebar(x.min = Bafodia_extent[1],
                 x.max = Bafodia_extent[2],
                 y.min = Bafodia_extent[3],
                 y.max = Bafodia_extent[4],
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

ggsave(paste0("Figures/Bafodia_brt_house_",house,".png"), width = 8, height = 8)

Bafodia_brt_legend <- get_legend(Bafodia_brt_plt +
                                    theme(legend.position = "right", legend.text = element_text(size=12)) +
                                    guides(shape = guide_legend(override.aes = list(size = 5)))) |>
  as_ggplot()

ggsave(paste0("Figures/Bafodia_brt_legend_house_",house,".png"), Bafodia_brt_legend)

Bafodia_scalebar <- ggplot() +
  theme_void() +
  theme(legend.position = "none") +
  geom_raster(data = Bafodia |> mutate(Red = replace_na(Red, 255),
                                       Green = replace_na(Green, 255),
                                       Blue = replace_na(Blue, 255)),
              aes(x=x, y=y,
                  fill=NA)) +
  scale_fill_identity() +
  theme(legend.position = "none") +
  coord_equal() +
  ggsn::scalebar(x.min = Bafodia_extent[1],
                                              x.max = Bafodia_extent[2],
                                              y.min = Bafodia_extent[3],
                                              y.max = Bafodia_extent[4],
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

ggsave(paste0("Figures/Bafodia_scalebar_",house,".png"), Bafodia_scalebar, height = 8)

