library(ggdist)
library(tidyverse)
# Unpack
# Evan's awesome feedback:
# All possible combinations vs random pair shuffle repeat
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

dat_visit <- read.csv(here("data", 'rank_curves_by_visit.csv'))
dat_visit$constraints = 'Full model'

# Subset of data just `both`
# pool jitter and visit
data.table::setDT(dat_visit)
ranked = dat_visit[,.(prob = rank.metric(pred, TS_Mn),
                      date = min(Date)),
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

ranked <- ranked |> mutate(test_site = case_when(
  test_site == "Tanganya" ~ "GTB",
  test_site == "Bafodia" ~ "SLTA",
  test_site == "Bantou" ~ "GTA"))

write.csv(ranked |> distinct(), "ranked_temp.csv")
ranked <- read.csv("ranked_temp.csv")

# ranked[Model_Type%in%c('Model', 'NullSmart', 'NullRandom') & house_name=='All' &
#          constraints == "Full model",] |>
#   ggplot(aes(x=prob, y=test_site, fill=Model_Type, color=Model_Type)) +
#   theme_ggdist() +
#   xlim(c(0,1)) +
#   stat_slab(
#     lwd = 0.75,
#     height = 0.9,
#     alpha = 0.7,
#     color = "gray15",
#     expand = TRUE,
#     trim = FALSE) +
#   #stat_pointinterval(position = position_nudge(x = 0, y = -0.075)) +
#   #stat_pointinterval(position = position_dodge(width = .3, preserve = "single")) +
#   #guides(color = guide_legend(override.aes = list(linewidth = c(0.4,0.4,0.4)))) +
#   guides(fill = guide_legend(byrow = TRUE)) +
#   labs(y="") +
#   labs(fill="Model",
#        colour="Model")
#


# model_comparison = ranked |>
#   select(test_site, model_name, prob) |>
#   distinct() |>
#   ggplot(aes(x = model_name, y = prob , color = model_name)) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_wrap(vars(test_site), scales = 'free_x') +
#   ylab('Probability of correctly ranking') +
#   xlab("") +
#   scale_y_continuous(breaks = seq(0, 1, by=0.05), limits = c(0.3, 1))
# model_comparison$labels$colour = 'Model type'
# model_comparison
# ggsave('model_comparison_fig_unscaled.png', width = 8, height = 5)

model_comparison_dist <- ranked |> filter(model_name == "Buildings") |>
  dplyr::select(test_site, house_name, model_name, prob) |>
  distinct() |>
  ggplot(aes(x=prob, y=house_name, group=house_name, fill=house_name, color=house_name)) +
  theme_ggdist() +
  stat_dots(side = "left", col="grey") +
  stat_halfeye(col = "dimgrey") +
    # lwd = 0.5,
    # height = 3,
    # alpha = 0.7,
    # color = "gray15",
    # # position = position_dodge(width = 0.6),
    # trim = T) +
  # stat_pointinterval(position = position_nudge(x = 0, y = -0.075)) +
  # stat_pointinterval(position = position_dodge(width = .3, preserve = "single")) +
  # guides(color = guide_legend(override.aes = list(lwd = c(0.01,0.01,0.01))),
  #        fill = guide_legend(byrow = TRUE)) +
  scale_fill_discrete(drop = FALSE) +
  labs(y="") +
  facet_wrap(~test_site, ncol = 2) +
  labs(fill="Model",
       colour="Model")
model_comparison_dist
ggsave('model_comparison_dist.png', width = 8, height = 5)


out = ggplot(ranked, aes(x = model_name, y = prob , color = as.factor(house_name))) +
  geom_boxplot() +
  facet_wrap(vars(test_site)) +
  ylab('Probability of correctly ranking') +
  xlab('Model type')
out$labels$colour = 'Trap location'
out
ggsave(paste0(model.path, 'Compare_house.png'), width = 7, height = 3)
