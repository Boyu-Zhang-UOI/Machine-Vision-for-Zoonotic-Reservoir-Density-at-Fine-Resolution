library(tidyverse)
library(ggtext) # for italics in axis titles
library(ggpubr) # for easy correlation calculations

h <- here::here

#==============================================================================


# Import data frame with predictions from the full XGBoost model
d <- read_csv(h("Figures_agg_scaled/rank_curves_base_all_earlystopping100_whouse_wprec.csv"))

# Verify that the dataset contains predictions from each train, validate, and
# test town combination (only one model each, the full model)
d %>%
  group_by(Site, test_site, train_site, val_site) %>%
  summarize(
    n_models = n_distinct(Model_Type),
    n_jitters = n_distinct(ji)
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
  ) |> group_by(test_label, ji, boxi, House) |> summarize(TotTraps = mean(TotTraps),
                                                          TS_Mn = mean(TS_Mn),
                                                          pred = mean(pred))

# Verify that the reported observed trap success is equal to the
# reported Mastomys natalensis captures divided by reported trap totals,
# accounting for rounding issues
# all.equal(d2$Tot_Mn/d2$TotTraps, d2$TS_Mn)

# Plot of all predictions
all <- d2 %>%
  ggplot(aes(y = TS_Mn, x = pred)) +
  geom_point(aes(size = TotTraps, col=TotTraps)) +
  ylim(0,0.7) +
  xlim(0,0.7) +
  scale_color_continuous(limits=c(0, 40), breaks=c(1, 10, 20, 30, 40)) +
  scale_size_continuous(limits=c(0, 40), breaks=c(1, 10, 20, 30, 40)) +
  guides(color= guide_legend(reverse = T), size=guide_legend(reverse = T)) +
  ggtitle("XGBoost Predictions") +
  ylab("Observed *Mastomys natalensis* trap success") +
  xlab("Predicted *Mastomys natalensis* trap success") +
  facet_wrap(~test_label, ncol = 3, scales = "free") +
  stat_cor(
    aes(label = after_stat(r.label)),
    size = 6, method = "pearson"
  ) +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  theme(
    axis.title.x = element_markdown(size = 16),
    axis.title.y = element_markdown(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "ghostwhite")
  )

# Save the full predictions figure
ggsave(
  "Figures/xgboost_predictions_agg.png", plot = all,
  width = 4500, height = 2000, units = "px"
)

# Plot of only outside of house predictions
outside <- d2 %>%
  filter(House == 0) %>%
  ggplot(aes(y = TS_Mn, x = pred)) +
  geom_point() +
  ylim(-0.1, 1) +
  ggtitle("XGBoost Predictions - Outside of Houses Only") +
  ylab("Observed *Mastomys natalensis* trap success") +
  xlab("Predicted *Mastomys natalensis* trap success") +
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
  ggplot(aes(y = TS_Mn, x = pred)) +
  geom_point() +
  ylim(-0.1, 1) +
  ggtitle("XGBoost Predictions - Inside of Houses Only") +
  ylab("Observed *Mastomys natalensis* trap success") +
  xlab("Predicted *Mastomys natalensis* trap success") +
  facet_wrap(~test_label, ncol = 3, dir = "v") +
  stat_cor(
    aes(label = after_stat(r.label)),
    size = 6, method = "pearson"
  ) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  theme_minimal() +
  theme(
    axis.title.x = element_markdown(size = 16),
    axis.title.y = element_markdown(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "ghostwhite")
  )


