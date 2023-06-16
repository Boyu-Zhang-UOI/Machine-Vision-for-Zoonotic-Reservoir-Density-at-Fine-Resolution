library(tidyverse)
library(glmnet)
library(here)

for (r_file in list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) try(source(r_file))

# Use relative paths so code will run on any computer
h <- here::here

# Create storage directory for fitted models and figures
grid_size_m = 50;
prefix = paste0("Fit_all_elastic_net_", grid_size_m)
model_path = h("Fitted_models/", prefix)
model_fig_path = h(model_path, "Figures")

# Choose predictors, load and aggregate data
cleaned_data = read_csv(h("Data/Trap_Data/Clean_Both_Data_By_Trap.csv"))[,-1] # -1 is to skip column row ids

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

# We're going to use elastic net and the glmnet package.
# First we need to set up a grid of hyper-parameters. These hyper-parameters
# control the lambda (penalty) and alpha terms (L1,L2 mixing term).
# alpha = 1 is lasso and alpha = 0 is ridge regression. Anything in-between is elastic.

# A latin hypercube is a way of constructing a random-ish grid of parameters that
# ensures that any portion of space has a combination of parameters near it.
# In other words random but also no holes. https://dials.tidymodels.org/reference/grid_max_entropy.html
hyperparameters = dials::grid_latin_hypercube(dials::penalty(),
                                              dials::mixture(), # relative amount of L1 and L2 penalties
                                              size = 100)

# Previously we ran 'reg:logistic' model for the xgboost model
# Verify that this approach is correct.
# Yes it is but unlike xgboost glment doesn't handle proportions.
# Instead we need a 2 column matrix response variable of `losses` vs
# `wins`. This also means we don't need a weighting variable - that's only
# necessary for proportions.

# response <- c("TS_Mn")
response <- c("Tot_Other", "Tot_Mn")
weights <- "Tot_Traps"
predictors <- c("Night",
                "House",
                "Frac_bare.25",
                "Frac_grass.25",
                "Frac_tree.25",
                "Frac_burn.25",
                "Frac_rice.25",
                "Frac_water.25",
                "Frac_mound.25",
                "Frac_bare.50",
                "Frac_grass.50",
                "Frac_tree.50",
                "Frac_burn.50",
                "Frac_rice.50",
                "Frac_water.50",
                "Frac_mound.50",
                "Frac_bare.100",
                "Frac_grass.100",
                "Frac_tree.100",
                "Frac_burn.100",
                "Frac_rice.100",
                "Frac_water.100",
                "Frac_mound.100",
                "Frac_bare.200",
                "Frac_grass.200",
                "Frac_tree.200",
                "Frac_burn.200",
                "Frac_rice.200",
                "Frac_water.200",
                "Frac_mound.200",
                "Frac_bare.500",
                "Frac_grass.500",
                "Frac_tree.500",
                "Frac_burn.500",
                "Frac_rice.500",
                "Frac_water.500",
                "Frac_mound.500",
                "Frac_bare.1000",
                "Frac_grass.1000",
                "Frac_tree.1000",
                "Frac_burn.1000",
                "Frac_rice.1000",
                "Frac_water.1000",
                "Frac_mound.1000",
                "Density_Moderns.25",
                "Density_Traditionals.25",
                "Density_Buildings.25",
                "Density_Moderns.50",
                "Density_Traditionals.50",
                "Density_Buildings.50",
                "Density_Moderns.100",
                "Density_Traditionals.100",
                "Density_Buildings.100",
                "Density_Moderns.200",
                "Density_Traditionals.200",
                "Density_Buildings.200",
                "Density_Moderns.500",
                "Density_Traditionals.500",
                "Density_Buildings.500",
                "Density_Moderns.1000",
                "Density_Traditionals.1000",
                "Density_Buildings.1000",
                "sc_Density_Moderns.25",
                "sc_Density_Traditionals.25",
                "sc_Density_Buildings.25",
                "sc_Density_Moderns.50",
                "sc_Density_Traditionals.50",
                "sc_Density_Buildings.50",
                "sc_Density_Moderns.100",
                "sc_Density_Traditionals.100",
                "sc_Density_Buildings.100",
                "sc_Density_Moderns.200",
                "sc_Density_Traditionals.200",
                "sc_Density_Buildings.200",
                "sc_Density_Moderns.500",
                "sc_Density_Traditionals.500",
                "sc_Density_Buildings.500",
                "sc_Density_Moderns.1000",
                "sc_Density_Traditionals.1000",
                "sc_Density_Buildings.1000",
                "P1",
                "P2",
                "P3",
                "P4",
                "P5",
                "P6",
                "P7",
                "P8",
                "P9",
                "P10",
                "P11",
                "P12")

# predictors <- c("Density_Moderns.25",
#                 "Density_Traditionals.25",
#                 "Density_Buildings.25",
#                 "Density_Moderns.50",
#                 "Density_Traditionals.50",
#                 "Density_Buildings.50",
#                 "Density_Moderns.100",
#                 "Density_Traditionals.100",
#                 "Density_Buildings.100",
#                 "Density_Moderns.200",
#                 "Density_Traditionals.200",
#                 "Density_Buildings.200",
#                 "Density_Moderns.500",
#                 "Density_Traditionals.500",
#                 "Density_Buildings.500",
#                 "Density_Moderns.1000",
#                 "Density_Traditionals.1000",
#                 "Density_Buildings.1000",
#                 "sc_Density_Moderns.25",
#                 "sc_Density_Traditionals.25",
#                 "sc_Density_Buildings.25",
#                 "sc_Density_Moderns.50",
#                 "sc_Density_Traditionals.50",
#                 "sc_Density_Buildings.50",
#                 "sc_Density_Moderns.100",
#                 "sc_Density_Traditionals.100",
#                 "sc_Density_Buildings.100",
#                 "sc_Density_Moderns.200",
#                 "sc_Density_Traditionals.200",
#                 "sc_Density_Buildings.200",
#                 "sc_Density_Moderns.500",
#                 "sc_Density_Traditionals.500",
#                 "sc_Density_Buildings.500",
#                 "sc_Density_Moderns.1000",
#                 "sc_Density_Traditionals.1000",
#                 "sc_Density_Buildings.1000")

# Work on formula specifying with interactions.

# Fit an elastic net model to each
# permutation of training and validating villages
# also perform a search for the best hyper-parameters
# for each village. Then fit an elastic net regression
# to the set of hyper-parameters for the given permutation.
# Note: `validate_mae_mean` is the mean mae across jitters
# for one set of hyperparameters + training and validating villages.
elnet_val <- validate_elnet(TVT_data,
                            response,
                            predictors,
                            hyperparameters)

# Find the lowest mse for every train / validate combination.
elnet_best <- elnet_val |>
  group_by(train, validate) |>
  slice(which.min(validate_mae_mean)) |>
  left_join(TVT_plan |> bind_rows()) # Add back in the testing site

elnet_best

# This contains the actual model objects for each jitter
elnet_best_fits <- fit_best_elnet(TVT_data,
                                  response,
                                  predictors,
                                  elnet_best)

saveRDS(elnet_best_fits,  h("data", "elnet_best_fits.rds"))

# This is a summary table averaging test_mae across all fits
elnet_bestest <- elnet_best_fits |>
  group_by(train, validate, test, .drop = F) |>
  group_map(function(grp, grp_key) {
    grp_key |> bind_cols(grp |> mutate(prop_Mn = mean(prop_Mn),
                                       test_mae_mean = mean(test_mae),
                                       test_mae_sd = sd(test_mae)) |>
      select(-Jitter, -test_mae, -vi, -fit, -fit_X, -fit_Y, -fit_pred, -fit_TS_Mn) |> head(1))
  }) |> bind_rows()

# elnet_bestest
elnet_bestest



write_csv(elnet_bestest, h("data", "elnet.csv"))

# Plot predictions
TS_Mn <- elnet_best_fits$fit_TS_Mn |> unlist()
TS_Mn_predicted <- elnet_best_fits$fit_pred |> unlist()
plot(TS_Mn, TS_Mn_predicted)

# overall VI is element-wise mean of vi column
vi_dfr <- elnet_best_fits |> rowwise() |> mutate(vi = list(vi |> DALEX::model_parts() |> mutate(permutation = permutation + 10 * (Jitter - 1))))
vi_list <- vi_dfr |> group_by(train, validate, test) |> group_split()
vi <- map(vi_list, ~.x$vi |> bind_rows())
ggsave("Figures/elnet_vi.png", vi[[3]] |> plot(show_boxplots = FALSE) + theme(plot.background = element_rect(fill = 'white')))


