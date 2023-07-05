library(tidyverse)
library(here)
library(terrainr)
library(terra)
library(viridis)
library(ggnewscale)

house <- 1

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

Tanganya_brt <- raster(paste0("Predictions/Tanganya_house_",house,".tif"))
Tanganya_brt <- crop(Tanganya_brt, Tanganya_extent)
Tanganya_brt_extent <- extent(Tanganya_brt)

Tanganya_brt_extent[2] <- Tanganya_brt_extent[1] + 0.006433385
Tanganya_brt_extent[4] <- Tanganya_brt_extent[3] + 0.0055786

Tanganya_brt <- crop(Tanganya_brt, Tanganya_brt_extent)

Tanganya_brt <- rasterToPoints(Tanganya_brt, spatial = T)
Tanganya_brt <- Tanganya_brt |> as.data.frame()

anchor_x = Tanganya_brt_extent[1] + 0.0002
anchor_y = Tanganya_brt_extent[3] + 0.00018
stdist = 0.010
scaledist = 100
scaleheight = 0.018

Tanganya <- crop(Tanganya, Tanganya_extent)
Tanganya <- crop(Tanganya, Tanganya_brt_extent)
Tanganya <- as.data.frame(Tanganya, xy= TRUE) |>
  rename(Red = Tanganya_1,
         Green = Tanganya_2,
         Blue = Tanganya_3) |>
  mutate(intensity = sum(Red, Green, Blue))

Tanganya_base_plt <- ggplot() +
  theme_void() +
  theme(legend.position = "none") +
  geom_raster(data = Tanganya |> mutate(Red = replace_na(Red, 255),
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

Tanganya_base_plt_bar <- Tanganya_base_plt + ggsn::scalebar(x.min = Tanganya_extent[1],
               x.max = Tanganya_extent[2],
               y.min = Tanganya_extent[3],
               y.max = Tanganya_extent[4],
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

ggsave(paste0("Figures/Tanganya_base_house_",house,".png"), Tanganya_base_plt_bar, width = 8, height = 8)

Tanganya_base_plt_traps <- Tanganya_base_plt +
  geom_point(data = pnts |> filter(Site == "Tanganya",
                                   Longitude > Tanganya_brt_extent[1],
                                   Longitude < Tanganya_brt_extent[2],
                                   Latitude > Tanganya_brt_extent[3],
                                   Latitude < Tanganya_brt_extent[4]),
             aes(x = Longitude,
                 y = Latitude,
                 col = as.factor(Type),
                 shape = as.factor(Type)), size = 1.25) +
  ggsn::scalebar(x.min = Tanganya_extent[1],
                   x.max = Tanganya_extent[2],
                   y.min = Tanganya_extent[3],
                   y.max = Tanganya_extent[4],
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

ggsave(paste0("Figures/Tanganya_traps_house_",house,".png"), Tanganya_base_plt_traps, width = 8, height = 8)

Tanganya_brt_plt <- Tanganya_base_plt +
  ggnewscale::new_scale_fill() +
  geom_raster(data = Tanganya_brt |> set_names(c("Tanganya", "x", "y")), aes(x = x, y = y, fill=Tanganya), alpha = 0.5) +
  scale_fill_viridis_c(name="", option = 'magma') +
  ggsn::scalebar(x.min = Tanganya_extent[1],
                 x.max = Tanganya_extent[2],
                 y.min = Tanganya_extent[3],
                 y.max = Tanganya_extent[4],
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

ggsave(paste0("Figures/Tanganya_brt_house_",house,".png"), width = 8, height = 8)

Tanganya_brt_legend <- get_legend(Tanganya_brt_plt +
                                    theme(legend.position = "right", legend.text = element_text(size=12)) +
                                    guides(shape = guide_legend(override.aes = list(size = 5)))) |>
  as_ggplot()

ggsave(paste0("Figures/Tanganya_brt_legend_house_",house,".png"), Tanganya_brt_legend, height = 4)
