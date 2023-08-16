library(tidyverse)
library(ggtext) # for italics in axis titles
library(ggpubr) # for easy correlation calculations

h <- here::here

#==============================================================================


# Import data frame with predictions from the full XGBoost model
d <- read_csv(h("Data/elnet_preds.csv"))

d <- d |> rename(ji = Jiggle) |> filter(elnet_preds < 0.9)

# Filter data for plotting
d2 <- d %>%
  # Grab only rows of data that correspond to a train/validate/test combo
  filter(!is.na(train_site)) %>%
  # Generate new variables to use as labels for plotting
  mutate(
    train_mod = case_when(
      train_site == "Bafodia" ~ "SLTA",
      train_site == "Bantou" ~ "GTA",
      train_site == "Tanganya" ~ "GTB"
    ),
    validate_mod = case_when(
      val_site == "Bafodia" ~ "SLTA",
      val_site == "Bantou" ~ "GTA",
      val_site == "Tanganya" ~ "GTB"
    ),
    test_mod = case_when(
      test_site == "Bafodia" ~ "SLTA",
      test_site == "Bantou" ~ "GTA",
      test_site == "Tanganya" ~ "GTB"
    ),
    train_label = paste("Training dataset:", train_mod),
    validate_label = paste("Validation dataset:", validate_mod),
    test_label = paste("Test dataset:", test_mod)
  )

# Plot of all predictions
all <- d2 %>%
  ggplot(aes(x = TS_Mn, y = elnet_preds)) +
  geom_point() +
  ylim(-0.1, 1) +
  ggtitle("Elastic Net Predictions") +
  xlab("Observed *Mastomys natalensis* trap success") +
  ylab("Predicted *Mastomys natalensis* trap success") +
  facet_wrap(test_label ~ train_label, ncol = 3, dir = "v") +
  stat_cor(
    aes(label = after_stat(r.label)),
    size = 6, method = "pearson"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_markdown(size = 16),
    axis.title.y = element_markdown(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "ghostwhite")
  )

# Plot of only outside of house predictions
outside <- d2 %>%
  filter(House == 0) %>%
  ggplot(aes(x = TS_Mn, y = elnet_preds)) +
  geom_point() +
  ylim(-0.1, 1) +
  ggtitle("Elastic Net Predictions - Outside of Houses Only") +
  xlab("Observed *Mastomys natalensis* trap success") +
  ylab("Predicted *Mastomys natalensis* trap success") +
  facet_wrap(test_label ~ train_label, ncol = 3, dir = "v") +
  stat_cor(
    aes(label = after_stat(r.label)),
    size = 6, method = "pearson"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_markdown(size = 16),
    axis.title.y = element_markdown(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "ghostwhite")
  )

# Plot of only inside of house predictions
in.house <- d2 %>%
  filter(House == 1) %>%
  ggplot(aes(x = TS_Mn, y = elnet_preds)) +
  geom_point() +
  ylim(-0.1, 1) +
  ggtitle("Elastic Net Predictions - Inside of Houses Only") +
  xlab("Observed *Mastomys natalensis* trap success") +
  ylab("Predicted *Mastomys natalensis* trap success") +
  facet_wrap(test_label ~ train_labels, ncol = 3, dir = "v") +
  stat_cor(
    aes(label = after_stat(r.label)),
    size = 6, method = "pearson"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_markdown(size = 16),
    axis.title.y = element_markdown(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "ghostwhite"),
  )

# Save the full predictions figure
ggsave(
  "Figures/enr_predictions.png", plot = all,
  width = 3000, height = 2000, units = "px"
)

# Filter data for plotting
d2 <- d %>%
  # Grab only rows of data that correspond to a train/validate/test combo
  filter(!is.na(train_site)) %>%
  # Generate new variables to use as labels for plotting
  mutate(
    train_mod = case_when(
      train_site == "Bafodia" ~ "SLTA",
      train_site == "Bantou" ~ "GTA",
      train_site == "Tanganya" ~ "GTB"
    ),
    validate_mod = case_when(
      val_site == "Bafodia" ~ "SLTA",
      val_site == "Bantou" ~ "GTA",
      val_site == "Tanganya" ~ "GTB"
    ),
    test_mod = case_when(
      test_site == "Bafodia" ~ "SLTA",
      test_site == "Bantou" ~ "GTA",
      test_site == "Tanganya" ~ "GTB"
    ),
    train_label = paste("Training dataset:", train_mod),
    validate_label = paste("Validation dataset:", validate_mod),
    test_label = paste("Test dataset:", test_mod)
  ) |> group_by(test_label, ji, boxi, House) |> summarize(TS_Mn = mean(TS_Mn),
                                                          pred = mean(pred),
                                                          elnet_preds = mean(elnet_preds))


# Plot of all predictions
all <- d2 %>%
  ggplot(aes(x = TS_Mn, y = elnet_preds)) +
  geom_point() +
  ylim(-0.1, 1) +
  ggtitle("Elastic Net Predictions") +
  xlab("Observed *Mastomys natalensis* trap success") +
  ylab("Predicted *Mastomys natalensis* trap success") +
  facet_wrap(~test_label, ncol = 3, dir = "v") +
  stat_cor(
    aes(label = after_stat(r.label)),
    size = 6, method = "pearson"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_markdown(size = 16),
    axis.title.y = element_markdown(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "ghostwhite")
  )

# Plot of only outside of house predictions
outside <- d2 %>%
  filter(House == 0) %>%
  ggplot(aes(x = TS_Mn, y = elnet_preds)) +
  geom_point() +
  ylim(-0.1, 1) +
  ggtitle("Elastic Net Predictions - Outside of Houses Only") +
  xlab("Observed *Mastomys natalensis* trap success") +
  ylab("Predicted *Mastomys natalensis* trap success") +
  facet_wrap(~test_label, ncol = 3, dir = "v") +
  stat_cor(
    aes(label = after_stat(r.label)),
    size = 6, method = "pearson"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_markdown(size = 16),
    axis.title.y = element_markdown(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "ghostwhite")
  )

# Plot of only inside of house predictions
in.house <- d2 %>%
  filter(House == 1) %>%
  ggplot(aes(x = TS_Mn, y = elnet_preds)) +
  geom_point() +
  ylim(-0.1, 1) +
  ggtitle("Elastic Net Predictions - Inside of Houses Only") +
  xlab("Observed *Mastomys natalensis* trap success") +
  ylab("Predicted *Mastomys natalensis* trap success") +
  facet_wrap(~test_label, ncol = 3, dir = "v") +
  stat_cor(
    aes(label = after_stat(r.label)),
    size = 6, method = "pearson"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_markdown(size = 16),
    axis.title.y = element_markdown(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "ghostwhite"),
  )

# Save the full predictions figure
ggsave(
  "Figures/enr_predictions_agg.png", plot = all,
  width = 3000, height = 2000, units = "px"
)
