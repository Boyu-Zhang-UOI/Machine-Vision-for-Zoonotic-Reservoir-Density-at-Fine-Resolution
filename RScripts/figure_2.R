library(tidyverse)
library(here)
library(cowplot)
library(ggpubr)
library(png)
library(grid)
library(gridExtra)

# palette
cols <- c(bare = "#ffff00",
         grass = "#ff6eb4",
         tree = "#a020f0",
         burned = "#a9a9a9",
         rice = "#ff0000",
         cloud = "#ffffff",
         water = "#87ceeb",
         cultivation = "#ffa500")

sp = 5
lay <- c(1, 2, 3)
lay <- rbind(lay,
             lay + 3,
             4,
             lay + 4,
             8,
             lay + 8)



instance <- text_grob("Landcover", face = "bold", col = "black", size = 20)
building <- text_grob("Buildings", face = "bold", col = "black", size = 20)

arrow <- rasterGrob(readPNG(here("old_figures/segmented", "arrow.png")))
arrow <- nullGrob()

tanganya <- list(cover = readPNG(here("old_figures/segmented", "tanganya_cover.png")),
                 building = readPNG(here("old_figures/segmented", "tanganya_building.png")))


bantou <- list(cover = readPNG(here("old_figures/segmented", "bantou_cover.png")),
               building = readPNG(here("old_figures/segmented", "bantou_building.png")))


bafodia <- list(cover = readPNG(here("old_figures/segmented", "bafodia_cover.png")),
                building = readPNG(here("old_figures/segmented", "bafodia_building.png")))

tanganya <- map(tanganya, ~rasterGrob(.x))


bantou <- map(bantou, ~rasterGrob(.x))


bafodia <- map(bafodia, ~rasterGrob(.x))

cover_legend <- tibble(`Land Cover` = c("bare", "grass", "tree", "burned", "rice", "cloud", "water", "cultivation"),
              val = runif(8))

cover_legend <- get_legend(cover_legend |> ggplot(aes(x=val, fill = `Land Cover`)) +
                             geom_histogram() +
                             theme(legend.title = element_text(size=18),
                                   legend.text = element_text(size=16)) +
                             scale_fill_manual(values = cols))

building_legend <- iris |> mutate(Buildings = recode(Species, setosa = "modern", versicolor = "traditional")) |> filter(Buildings %in% c("traditional", "modern"))
building_legend <- get_legend(building_legend |> ggplot(aes(x=Sepal.Length, y=Sepal.Width)) +
                                geom_point(aes(shape=Buildings, color=Buildings), fill = NA, stroke = 1.5, size = 4) +
                                theme_bw() +
                                theme(legend.title = element_text(size=18),
                                      legend.text = element_text(size=16)) +
                                scale_color_manual(values = c("red", "blue")) +
                                scale_shape_manual(values=c(0, 1)))

plts <- grid.arrange(instance, building,
                     nullGrob(), nullGrob(),
                     bafodia$cover, bafodia$building,
                     nullGrob(), nullGrob(),
                     bantou$cover, bantou$building,
                     nullGrob(), nullGrob(),
                     tanganya$cover, tanganya$building,
                     # layout_matrix = lay,
                     # widths = c(8/16, 8/19),
                     heights = c(10, 10, 100, 10, 100, 10, 100))

legends <- grid.arrange(nullGrob(), building_legend, cover_legend, nullGrob(), ncol = 1, heights = c(1/10, 2/10, 4/10, 3/10))
plot2 <- grid.arrange(plts, legends, nrow = 1, widths = c(7/9,2/9))
ggsave("segmentation.png", plot2, width = 8.5, height = 8)


