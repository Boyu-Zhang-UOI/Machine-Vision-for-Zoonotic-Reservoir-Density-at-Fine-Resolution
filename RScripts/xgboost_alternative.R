library(tidyverse)
library(glmnet)
library(here)

library(furrr)
plan(multisession, workers = 6)

#best so far: 1234567 (1 flat val and 1 flat test)
#123456 class_ratio <- 0.5 also pretty good


set.seed(123456)

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

write_csv(jittered_data, "Data/jittered_data.csv")

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

# response <- c("TS_Mn")
response <- c("Tot_Other", "Tot_Mn")
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

# Mixture hyper-parameter between ridge (0) and lasso (1)
alphas = seq(0, 1, by=0.01)

# Fit an elastic net model to each
# permutation of training and validating villages
# also perform a search for the best hyper-parameters
# for each village calculating deviance, mae ect
# against both training and validation villages
elnet_val <- validate_elnet(TVT_data,
                            response,
                            predictors,
                            alphas)

# Save the full hyperparameter search
saveRDS(elnet_val,  h("data", "elnet_val.rds"))
elnet_val <- readRDS(h("data", "elnet_val.rds"))

# Average across jitters minimizing deviance against
# the validation village
elnet_best <- elnet_val |>
  filter(comparison == "validate") |>
  group_by(train, validate, jitter) |>
  group_map(function(grp, grp_key) {
    grp_key |> bind_cols(grp |> filter(mae == min(mae)))
  }) |>
  bind_rows() |>
  group_by(train, validate) |>
  summarize(penalty = mean(penalty, na.rm = T),
            mixture = mean(mixture, na.rm = T)) |>
  left_join(TVT_plan |> bind_rows())

# Refit using just the best hyperparameters found above
elnet_best_fits <- fit_best_elnet(TVT_data,
                                  response,
                                  predictors,
                                  elnet_best)

# Save the best fit models. One for each train-validate-test
# scenario.
saveRDS(elnet_best_fits, h("Data/elnet_best_fits.rds"))
elnet_best_fits <- readRDS(h("Data", "elnet_best_fits.rds"))

# Plot predictions
test <- elnet_best_fits |>
  group_by(train, validate, test) |>
  group_map(function(grp, grp_key) {
    bind_cols(grp_key,
              TS_Mn = grp$validate_TS_Mn |> unlist(),
              fit_pred = grp$validate_preds |> unlist())
  }) |> bind_rows() |>
  mutate(train = paste("train:", train),
         validate = paste("validate:", validate),
         test = paste("test:", test))

# Check predictions vs actual trapping success
ggplot(test, aes(x=TS_Mn, y=fit_pred, sep="_")) +
  facet_wrap(train ~ validate) +
  geom_point()

# Why x axis ranges not same for different TS_Mn test sites?

# # overall VI is element-wise mean of vi column
# vi_dfr <- elnet_best_fits |> rowwise() |> mutate(vi = list(vi |> DALEX::model_parts() |> mutate(permutation = permutation + 10 * (Jitter - 1))))
# vi_list <- vi_dfr |> group_by(train, validate, test) |> group_split()
# vi <- map(vi_list, ~.x$vi |> bind_rows())
# ggsave("Figures/elnet_vi.png", vi[[3]] |> plot(show_boxplots = FALSE) + theme(plot.background = element_rect(fill = 'white')), width = 5.32, height = 10)


# Notes from convo w/ Evan.
# Problem with test and validate and facet axis.
# Add back in number of traps as weight. Sigh.
