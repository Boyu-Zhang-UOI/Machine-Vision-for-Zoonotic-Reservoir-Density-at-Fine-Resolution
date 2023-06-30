library(tidyverse)
library(here)
library(ggdist)
library(ggpubr)
library(grid)
library(scales)

# Load in partial dependence data
# pdp can give feature importance but for xgboost model.feature_importances_ is better.
pdp <- read_csv(here("Figures_agg", "pdp_info.csv"))

pdp |> select(prefix) |> unique()

# Load in partial dependence data
# pdp can give feature importance but for xgboost model.feature_importances_ is better.
vi <- read_csv(here("Figures_agg", "feature_importance.csv"))
vi_types <- read_csv(here("Figures_agg", "vi_types.csv"))

# Pivot longer and replace town names with codes
vi <- vi |> pivot_longer(-Variable, values_to = "vi")
vi <- vi |> separate(name,into=c("training", "validation"),
               sep=",", convert = TRUE, extra = "merge") |>
  mutate(across(c("training", "validation"), ~gsub("[\\(\\'\\) ]", "", .x))) |>
  left_join(vi_types)

write.csv(vi, here("Figures_agg", "vi_cleaned.csv"))

vi <- vi |> mutate(Type = ifelse(Type == "Inside outside", "Trap location", Type))

# Remove mean columns and standardize vi values
# Mean in main sum in supplemental.
vi_by_type <- vi |> mutate(Type = ifelse(Type == "Building density scaled", "Building density", Type)) |>
  filter(!is.na(validation)) |>
  group_by(Type) |>
  summarize(vi = sum(vi)) |>
  ungroup() |>
  mutate(Type = gsub("_", " ", Type),
         dummy = NA)

type.plt <- vi_by_type |> ggplot(aes(x=reorder(Type, vi), y=vi)) +
  theme_ggdist() +
  geom_bar(stat = "identity", width=0.05, col="grey27", fill="grey27") +
  geom_point(size=4, col="grey27") +
  facet_wrap(~Range) +
  coord_flip() +
  theme(axis.text=element_text(size=12),
        title=element_text(size=15),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 14)) +
  facet_wrap(~dummy, labeller=label_bquote("Predictor type")) +
  labs(title = "A",
       x = "",
       y = "feature importance")

# Density range
density.plt <- vi |> filter(Type == "Building density" & !is.na(Range) & !is.na(validation)) |>
  mutate(subtype = gsub("\\..*", "", gsub(".*_", "", Variable) |> tolower()),
         dummy = NA) |>
  group_by(Type, Range, subtype, dummy) |> summarize(vi = sum(vi)) |>
  ggplot(aes(x=reorder(Range, Range), y=vi, col=subtype, fill=subtype, group=Type)) +
  theme_ggdist() +
  geom_bar(stat = "identity", width=0.4) +
  # geom_point(aes(y=mean(vi))) +
  coord_flip() +
  theme(axis.text=element_text(size=12),
        title=element_text(size=15),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 14)) +
        # legend.position = "none") +
  scale_x_discrete(labels = function(x) {format(paste0(x, "m"))}) +
  facet_wrap(~dummy, labeller=label_bquote("Building density")) +
  labs(title = "B",
       fill = "",
       col = "",
       x = "",
       y = "")

# Density subtype
cover.plt <- vi |> filter(Type == "Land cover" & !is.na(Range) & !is.na(validation)) |>
  mutate(subtype = gsub("\\..*", "", gsub(".*_", "", Variable)),
         dummy = NA) |>
  ggplot(aes(x=reorder(Range, Range), y=vi, col=subtype, fill=subtype, group=Type)) +
  theme_ggdist() +
  geom_bar(stat = "identity", width=0.4) +
  # geom_point(aes(y=mean(vi))) +
  coord_flip() +
  theme(axis.text=element_text(size=12),
        title=element_text(size=15),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 14)) +
  # legend.position = "none") +
  scale_x_discrete(labels = function(x) {format(paste0(x, "m"))}) +
  facet_wrap(~dummy, labeller=label_bquote("Land cover")) +
  labs(title = "D",
       fill = "",
       col = "",
       x = "",
       y = "feature importance")

# Precip
 precip.plt <- vi |> filter(Type == "Precip" & !is.na(validation)) |>
   mutate(lag = gsub("[a-zA-Z]", "", Variable) |> as.integer()) |>
   group_by(lag) |>
   summarize(vi = sum(vi)) |>
   ungroup() |>
   mutate(dummy = NA) |>
   ggplot(aes(x=lag, y=vi)) +
   facet_wrap(~dummy, labeller=label_bquote("Precipitation")) +
   theme_ggdist() +
   theme(axis.text=element_text(size=12),
         title=element_text(size=15),
         axis.title=element_text(size=14),
         strip.text.x = element_text(size = 14)) +
   geom_bar(stat = "identity", fill = "cornflowerblue", col="cornflowerblue") +
   labs(title = "C",
        x="Lag (months)",
        y="feature importance") +
   scale_x_reverse(breaks=12:1)

plt <- ggarrange(type.plt, density.plt, ggarrange(nullGrob(), precip.plt, widths=c(0.22,0.78), nrow = 1), ggarrange(cover.plt, nullGrob(), widths=c(0.995, 0.05), nrow = 1), nrow = 2, ncol = 2)
plt

ggsave(h("Figures/vi_fig.png"), plt, width = 12, height = 9)


rank_curves_by_visit_scaled <- read_csv(here("data", "rank_curves_by_visit_scaled.csv"))

scaled_top <- rank_curves_by_visit_scaled |> dplyr::select(matches("^sc.*")) |> map_dbl(~which.max(.x))
scaled_top_val <- rank_curves_by_visit_scaled |> dplyr::select(matches("^sc.*")) |> map2_dbl(scaled_top, ~.x[.y])


unscaled_top <- rank_curves_by_visit_scaled |> dplyr::select(matches("^Density.*")) |> map_dbl(~which.max(.x))
unscaled_top_val <- rank_curves_by_visit_scaled |> dplyr::select(matches("^Density.*")) |> map2_dbl(scaled_top, ~.x[.y])


rank_curves_by_visit_scaled |> filter(sc_Density_Buildings.1000 == 1)

