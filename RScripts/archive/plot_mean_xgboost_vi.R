library(tidyverse)

for (r_file in list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) try(source(r_file))

# xgboost variable importance. Does not depend on test sets
xgboost_importance <- read_csv("Figures_agg/pdp_info.csv") |>
  mutate(type = case_when(
  grepl("P", feature) ~ "Rainfall",
  grepl("Nigth", feature) ~ "Night",
  grepl("Building", feature) ~ "Buildings",
  grepl("House", feature) ~ "House",
  .default = "Landcover"))




# Figure 4
fig4_dat <- xgboost_importance |>
  group_by(prefix, train_site, val_site, test_site, type) |>
  summarize(mean = mean(value))
