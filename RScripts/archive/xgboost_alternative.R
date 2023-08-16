library(tidyverse)
library(glmnet)
library(here)
library(bettermc) # Lets you do multicore apply

set.seed(12345)
class_weights = F

for (r_file in list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) try(source(r_file))

# Use relative paths so code will run on any computer
h <- here::here

# Load in xgboost prediction dataframe
xgboost_preds <- read_csv("Figures_agg/rank_curves_base_all_earlystopping100_whouse_wprec.csv")[-c(1,2)]

# xgboost variable importance. Does not depend on test sets
xgboost_importance <- read_csv("Figures_agg/pdp_info.csv")

# response <- c("TS_Other", "TS_Mn")
response <- c("Tot_Other", "Tot_Mn")
predictors <- xgboost_importance$feature |> unique() # Use same set of features as in xgboost model

# Create storage directory for fitted models and figures
grid_size_m = 50;

# Choose predictors, load and aggregate data
cleaned_data = read_csv(h("Data/Trap_Data/Clean_Both_Data_By_Trap.csv"))[,-1] |>
  select(-Trap_ID) # -1 is to skip column row ids. Trap_ID has a lot of NA's ditch here


# Identify x an y ranges in degrees
x_range <- c(xmin = min(cleaned_data$Longitude),
             xmax = max(cleaned_data$Longitude))
y_range <- c(ymin = min(cleaned_data$Latitude),
             ymax = max(cleaned_data$Latitude))

# What is the magic number 111139?
# grid spacing in meters = 50m
# grid spacing in degrees = 50m * 1 degree / 111139 m
# https://sciencing.com/radius-earth-5895686.html
grid_size_degrees = grid_size_m * 1/111139 # 50m in degrees.

# This lays out a grid based on an established grid size (in degrees)
# then overlays the grid on the data n_jitters times jittering
# where the grid lands by up to one cell width each time
jittered_data <- get_grid(cleaned_data,
                          x_range,
                          y_range,
                          grid_size_degrees,
                          n_jitters = 5)

write_csv(jittered_data, "Data/jittered_data.csv")

bafodia <- jittered_data |> filter(Site == "Bafodia")
hist(bafodia$TS_Mn)

tanganya <- jittered_data |> filter(Site == "Tanganya")
hist(tanganya$TS_Mn)

bantou <- jittered_data |> filter(Site == "Bantou")
hist(bantou$TS_Mn)

# Villages
villages <- c("Bafodia", "Bantou", "Tanganya")

# Set up Train-Validate-Test plan
# Produces a list where each element is a named vector assigning sites
# to Train, Validate, or Testing (TVT) roles.
TVT_plan <- combinat::permn(villages) |> map(~.x |> setNames(c("train", "validate", "test")))

# Map across the TVT_plan setting up data for each TVT permutation
# Produces a list of datasets with a `Role` column with the TVT status of each row.
TVT_data <- map(TVT_plan, function(TVT) {
  jittered_data |> rowwise() |> mutate(Role = names(which(Site == TVT)))
})

# Mixture hyper-parameter between ridge (0) and lasso (1)
alphas = seq(0, 1, length.out = 100)
# alphas = c(0.3,0.4,0.5, 0.6, 0.7)

# Fit an elastic net model to each
# permutation of training and validating villages
# also perform a search for the best hyper-parameters
# for each village calculating deviance, MAE ect
# against both training and validation villages
elnet_val <- validate_elnet(TVT_data,
                            response,
                            predictors,
                            alphas,
                            n_workers = 4,
                            weights = F)

# Save the full hyperparameter search
saveRDS(elnet_val,  h("data", "elnet_val.rds"))
elnet_val <- readRDS(h("data", "elnet_val.rds"))

# Average across jitters minimizing deviance against
# the validation village
elnet_best <- elnet_val |>
  filter(comparison == "validate") |>
  group_by(train, validate, jitter) |>
  group_map(function(grp, grp_key) {
    grp_key |> bind_cols(grp |> filter(mse == min(mse)))
  }) |>
  bind_rows() |>
  group_by(train, validate) |>
  summarize(penalty = mean(penalty, na.rm = T),
            mixture = mean(mixture, na.rm = T),
            mae = mean(mae)) |>
  left_join(TVT_plan |> bind_rows())

# Refit using just the best hyperparameters found above
# This is a nested list of all the 6 TVT plans * 5 jitter model fits
elnet_best_fits <- fit_best_elnet(TVT_data,
                                  response,
                                  predictors,
                                  elnet_best,
                                  weights = F)

# Save the best fit models. One for each train-validate-test
# scenario.
saveRDS(elnet_best_fits, h("Data/elnet_best_fits.rds"))
elnet_best_fits <- readRDS(h("Data", "elnet_best_fits.rds"))

xgboost_preds <- xgboost_preds |> mutate(Tot_Other = TotTraps - Tot_Mn)
all.equal(xgboost_preds$Tot_Mn/xgboost_preds$TotTraps, xgboost_preds$TS_Mn)

# Jiggle the data for prediction
jiggled_data <- get_grid(cleaned_data,
                          x_range,
                          y_range,
                          grid_size_degrees,
                          n_jitters = 25) |>
  rename(Jiggle = Jitter)

# Map across the TVT_plan setting up data for each TVT permutation
# Produces a list of datasets with a `Role` column with the TVT status of each row.
TVT_pred_data <- map(TVT_plan, function(TVT) {
  jiggled_data |> rowwise() |> mutate(Role = names(which(Site == TVT)))
})

# We can just re-use the jiggles in xgboost_preds
elnet_preds <- get_elnet_preds(TVT_pred_data,
                               elnet_best_fits,
                               response,
                               predictors,
                               TVT_plan)

write_csv(elnet_preds, "Data/elnet_preds.csv")

# We can just re-use the jiggles in xgboost_preds
elnet_vi <- get_elnet_vi(xgboost_preds,
                         elnet_best_fits,
                         response,
                         predictors,
                         TVT_plan)

write_csv(elnet_preds, "Data/elnet_vi.csv")
