library(tidyverse)
library(data.table)
library(ggdist)
library(ggstance)
library(here)
library(ggpubr)
library(lme4)

# Unpack
# Evan's awesome feedback:
# All possible combinations vs random pair shuffly repeat
# Pairwise comparisons. Uneven pairwise comparisons in the set.
# Without replacement.
rank.metric <- function(x, y, sims = 1000){
  prob = 0
  for(s in 1:sims) {
    samp1 = sample(1:length(x), replace = TRUE)
    samp2 = sample(1:length(x), replace = TRUE)
    corr.rank = 0
    mask = (y[samp1]!=y[samp2])
    samp1 = samp1[mask]
    samp2 = samp2[mask]

    corr.rank = sum(((y[samp1] < y[samp2]) & (x[samp1] < x[samp2])) |
                      ((y[samp1] > y[samp2]) & (x[samp1] > x[samp2])))

    prob = prob + corr.rank / length(samp1)
  }
  return(prob / sims)
}

dat <- read.csv(here("data", 'ranked.csv'))

dat_visit <- read.csv(here("data", 'rank_curves_by_visit.csv'))
dat_visit$constraints = 'Full model'
dat_visit$house_name = c('All', 'Inside', 'Outside')[dat_visit$house_num +1]

inout_TS <- dat_visit |> filter(house_name != "All")
inout <- glmer(TS_Mn ~ house_name + (1|Visit) + (1|Night), data = inout_TS, family="binomial")

inout_TS |> ggplot(aes(x = TS_Mn, fill = house_name, col = house_name)) +
  stat_slab(alpha = 0.5)

# Subset of data just `both`
# pool jitter and visit
data.table::setDT(dat_visit)
ranked = dat_visit[,.(prob = rank.metric(pred, TS_Mn),
                      date = min(Date),
                      TS_Mn = TS_Mn),
                   by = list(test_site, ji, visit, Model_Type, house_num, constraints)]
# by = list(test_site, ji, Model_Type, house_num, visit, constraints)]
ranked$house_name = c('All', 'Inside', 'Outside')[ranked$house_num +1]
ranked$constraints = factor(ranked$constraints,
                            levels = c('Spatial', 'Spatial + in/out',
                                       'Spatial + in/out + precip',
                                       'Full model'),
                            ordered = TRUE)
ranked$model_name = 'zero'
ranked[Model_Type=='NullRandom']$model_name = 'Random'
ranked[Model_Type=='NullSmart']$model_name = 'Buildings'
ranked[Model_Type=='Model']$model_name = 'BRT'
ranked[test_site=='Bantou' & house_name == 'Inside'] = NA
ranked$site_visit = paste(ranked$test_site, ranked$visit, sep = '')
ranked <- ranked[Model_Type%in%c('Model', 'NullSmart', 'NullRandom') &
                   constraints == "Full model",] |> drop_na(Model_Type)


write.csv(ranked |> distinct(), "ranked_temp.csv")
ranked <- read.csv("ranked_temp.csv")

in_out_ts_check <- ranked |>
  filter(house_name != "All") |>
  ggplot(aes(x=))

ranked <- ranked |> mutate(test_site = case_when(
  test_site == "Tanganya" ~ "GTB",
  test_site == "Bafodia" ~ "SLTA",
  test_site == "Bantou" ~ "GTA"))

pairwise_model <- ranked |>
  filter(house_name == "All") |>
  dplyr::select(test_site, house_name, model_name, prob) |>
  distinct() |>
  ggplot(aes(x=prob, fill=model_name, color=model_name)) +
  theme_ggdist() +
  geom_vline(aes(xintercept = 0.5), alpha = 0.5, col = "grey", lwd = 0.4, lty="dashed") +
  stat_dots(data = ~.x |> filter(model_name == "Random"),
            side = "left",
            width = 0.1,
            height = 0.15,
            alpha = 0.8,
            position = position_nudge(y = 0.2)) +
  stat_slab(data = ~.x |> filter(model_name == "Random"),
            col = "dimgrey",
            lwd = 0.5,
            alpha = 0.8,
            height = 0.15,
            position = position_nudge(y = 0.2)) + #position = ggstance::position_dodgev()) +
  stat_dots(data = ~.x |> filter(model_name == "Buildings"),
            side = "left",
            height = 0.15,
            width = 0.1,
            alpha = 0.8) +
  stat_slab(data = ~.x |> filter(model_name == "Buildings"),
            col = "dimgrey",
            lwd = 0.5,
            height = 0.15,
            alpha = 0.8) + #position = ggstance::position_dodgev()) +
  stat_dots(data = ~.x |> filter(model_name == "BRT"),
            side = "left",
            width = 0.1,
            height = 0.15,
            alpha = 0.8,
            position = position_nudge(y = -.2)) +
  stat_slab(data = ~.x |> filter(model_name == "BRT"),
            col = "dimgrey",
            lwd = 0.5,
            alpha = 0.8,
            height = 0.15,
            position = position_nudge(y = -.2)) + #position = ggstance::position_dodgev()) +
  scale_fill_discrete(drop = FALSE) +
  theme(axis.line.y=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y="") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none") +
  guides(colour = guide_legend(byrow = TRUE, override.aes = list(alpha = 0.6))) +
  guides(fill = guide_legend(byrow = TRUE, override.aes = list(linewidth = 0.4, alpha = 0.4))) +
  facet_wrap(~test_site, ncol = 1) +
  labs(y = "",
       fill="Model",
       colour="Model",
       x="Probability of correct ranking")
pairwise_model
ggsave('pairwise_models.png', width = 7, height = 6)


pairwise_house <- ranked |>
  filter(house_name != "All") |>
  # mutate(model_name = ordered(model_name, levels = rev(sort(unique(model_name))))) |>
  dplyr::select(test_site, house_name, model_name, prob) |>
  distinct() |>
  ggplot(aes(x=prob, y=house_name, fill=model_name, color=model_name)) +
  theme_ggdist() +
  geom_vline(aes(xintercept = 0.5), alpha = 0.5, col = "grey", lwd = 0.4, lty="dashed") +
  # coord_flip() +
  stat_dots(data = ~.x |> filter(model_name == "Random"),
            side = "left",
            width = 0.1,
            height = 0.25,
            alpha = 0.8,
            position = position_nudge(y = 0.2)) +
  stat_slab(data = ~.x |> filter(model_name == "Random"),
            col = "dimgrey",
            lwd = 0.5,
            alpha = 0.8,
            height = 0.25,
            position = position_nudge(y = 0.2)) + #position = ggstance::position_dodgev()) +
  stat_dots(data = ~.x |> filter(model_name == "Buildings"),
            side = "left",
            height = 0.25,
            width = 0.1,
            alpha = 0.8) +
  stat_slab(data = ~.x |> filter(model_name == "Buildings"),
            col = "dimgrey",
            lwd = 0.5,
            height = 0.25,
            alpha = 0.8) + #position = ggstance::position_dodgev()) +
  stat_dots(data = ~.x |> filter(model_name == "BRT"),
            side = "left",
            width = 0.1,
            height = 0.25,
            alpha = 0.8,
            position = position_nudge(y = -.2)) +
  stat_slab(data = ~.x |> filter(model_name == "BRT"),
            col = "dimgrey",
            lwd = 0.5,
            alpha = 0.8,
            height = 0.25,
            position = position_nudge(y = -.2)) + #position = ggstance::position_dodgev()) +
  facet_wrap(~test_site) +
  labs(y = "",
       fill="Model",
       colour="Model",
       x="Probability of correct ranking") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(colour = guide_legend(byrow = TRUE, override.aes = list(alpha = 0.6))) +
  guides(fill = guide_legend(byrow = TRUE, override.aes = list(linewidth = 0.4, alpha = 0.4)))
pairwise_house
ggsave('pairwise_house.png', width = 7, height = 4.5)

ggsave("pairwise_combined.png", ggarrange(pairwise_model, pairwise_house, widths = c(0.3, 0.7), label.y = 0.16, label.x = c(0.14, 0.13), labels = c("A","B")), width = 10, height = 6)

