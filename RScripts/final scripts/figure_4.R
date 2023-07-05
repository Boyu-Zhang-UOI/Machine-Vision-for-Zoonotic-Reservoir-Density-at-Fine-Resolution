library(tidyverse)
library(here)
library(cowplot)
library(ggpubr)
library(png)
library(grid)
library(gridExtra)

lay <- c(1, 2, 3)
lay <- rbind(lay,
             lay + 3,
             4,
             lay + 4,
             8,
             lay + 8)

instance <- text_grob("Aerial imagery", face = "bold", col = "black", size = 20)
building <- text_grob("BRT prediction", face = "bold", col = "black", size = 20)

arrow <- rasterGrob(readPNG(here("old_figures/segmented", "arrow.png")))

  tanganya <- list(base = readPNG(here("Figures", "Tanganya_base_house_1.png")),
                 brt = readPNG(here("Figures", "Tanganya_brt_house_1.png")),
                 legend = readPNG(here("Figures", "Tanganya_brt_legend_house_1.png")))
bantou <- list(base = readPNG(here("Figures", "Bantou_base_house_1.png")),
               brt = readPNG(here("Figures", "Bantou_brt_house_1.png")),
               legend = readPNG(here("Figures", "Bantou_brt_legend_house_1.png")))
bafodia <- list(base = readPNG(here("Figures", "Bafodia_base_house_1.png")),
                brt = readPNG(here("Figures", "Bafodia_brt_house_1.png")),
                legend = readPNG(here("Figures", "Bafodia_brt_legend_house_1.png")))

tanganya <- map(tanganya, ~rasterGrob(.x))
bantou <- map(bantou, ~rasterGrob(.x))
bafodia <- map(bafodia, ~rasterGrob(.x))

plts <- grid.arrange(instance, nullGrob(), building, nullGrob(), nullGrob(),
                     nullGrob(), nullGrob(), nullGrob(), nullGrob(),nullGrob(),
                     bafodia$base, arrow, bafodia$brt, nullGrob(), bafodia$legend,
                     nullGrob(), nullGrob(), nullGrob(), nullGrob(), nullGrob(),
                     bantou$base, arrow, bantou$brt, nullGrob(), bantou$legend,
                     nullGrob(), nullGrob(), nullGrob(), nullGrob(), nullGrob(),
                     tanganya$base, arrow, tanganya$brt, nullGrob(), tanganya$legend,
                     # layout_matrix = lay,
                     widths = c(45/100, 5/100, 45/100, 5/100, 10/100),
                     heights = c(10, 10, 100, 10, 100, 10, 100))

ggsave(here("Figures", "brt_prediction.png"), plts, width = 8.5, height = 8)


