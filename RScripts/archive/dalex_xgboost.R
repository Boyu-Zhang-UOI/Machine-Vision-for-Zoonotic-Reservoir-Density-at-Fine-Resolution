library(tidyverse)
library(xgboost)
library(here)
library(DALEX)

# Names reflect train_validate

SLTA_GTA_file <- here("Fitted_models/Fit_all_scaled_multigrid1_md1_new50/par_0_fold_Bantou.txt")
SLTA_GTA_model <- xgb.load(SLTA_GTA_file)
cleaned_data <- read_csv(here("Data/Trap_Data/Clean_Both_Data_By_Trap.csv"))
xgboost_importance <- read_csv(here("Figures_agg/pdp_info.csv"))
predictors <- xgboost_importance$feature |> unique() # Use same set of features as in xgboost model


# Identify x an y ranges in degrees
x_range <- c(xmin = min(cleaned_data$Longitude),
             xmax = max(cleaned_data$Longitude))
y_range <- c(ymin = min(cleaned_data$Latitude),
             ymax = max(cleaned_data$Latitude))

# Create storage directory for fitted models and figures
grid_size_m = 50;

# What is the magic number 111139?
# grid spacing in meters = 50m
# grid spacing in degrees = 50m * 1 degree / 111139 m
# https://sciencing.com/radius-earth-5895686.html
grid_size_degrees = grid_size_m * 1/111139 # 50m in degrees.

# Jiggle the data for prediction
jiggled_data <- get_grid(cleaned_data,
                         x_range,
                         y_range,
                         grid_size_degrees,
                         n_jitters = 25) |>
  rename(Jiggle = Jitter)

# Set up Train-Validate-Test plan
# Produces a list where each element is a named vector assigning sites
# to Train, Validate, or Testing (TVT) roles.
TVT_plan <- tibble(train = c("Bafodia", "Bafodia", "Bantou", "Bantou", "Tanganya", "Tanganya"),
                   validate = c("Bantou", "Tanganya", "Bafodia", "Tanganya", "Bafodia", "Bantou"),
                   test = c("Tanganya", "Bantou", "Tanganya", "Bafodia", "Bantou", "Bafodia"),
                   file = c("Fitted_models/Fit_all_scaled_multigrid1_gamma_new50/par_332_fold_Bafodia.txt",
                            "Fitted_models/Fit_all_scaled_multigrid1_gamma_new50/par_475_fold_Bafodia.txt",
                            "Fitted_models/Fit_all_scaled_multigrid1_md1_new50/par_74_fold_Bantou.txt",
                            "Fitted_models/Fit_all_scaled_multigrid1_md1_new50/par_69_fold_Bantou.txt",
                            "Fitted_models/Fit_all_scaled_multigrid1_md1_new50/par_49_fold_Tanganya.txt",
                            "Fitted_models/Fit_all_scaled_multigrid1_gamma_new50/par_405_fold_Tanganya.txt"))

# Map across the TVT_plan setting up data for each TVT permutation
# Produces a list of datasets with a `Role` column with the TVT status of each row.
TVT_data <- map(1:nrow(TVT_plan), function(TVT) {
  jiggled_data |> rowwise() |> mutate(Role = names(TVT_plan)[which(Site == TVT_plan[TVT,])])
})

# Function to predict from Jiggles.
permutation_vi <- map(1:nrow(TVT_plan), function(tvt.x) {

  TVT <- TVT_plan[tvt.x,]
  model <- xgb.load(TVT |> dplyr::pull(file))

  test_data = TVT_data[[tvt.x]] |> filter(Role == "test") |>
    dplyr::select_at(c(predictors, "TS_Mn")) |>
    drop_na()

  xgb_d <- xgb.DMatrix(data = test_data |>
                select_at(predictors) |>
                as.matrix(),
              label = test_data |>
                pull("TS_Mn"))

  preds <- predict(model, newdata = xgb_d)

  explainer <- DALEX::explain(model,
                 data = test_data |>
                   select_at(predictors) |>
                   as.matrix(),
                 test_data |>
                   pull("TS_Mn"))

  list(TVT = TVT,
       preds = preds,
       explainer = explainer,
       TVT_vi <- DALEX::variable_importance(explainer))

})

# I finally get why walk can be useful...
walk(permutation_vi, ~plot(.x$TVT_vi, title = paste("Train:", TVT$train, "Validate:", TVT$validate, "Test:", TVT$test), max_vars = 10))
