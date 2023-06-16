#' Validate an elastic net model across a list of data inputs
#'
#' @param TVT_data # List of different data inputs
#' @param response # Response column name
#' @param predictors # Vector of predictor column names
#' @param weight # Vector of weight column name
#' @param hyperparameters # Dataframe with one column for penalyt and one for mixture
#'
#' @return elnet # A dataframe row with name of train town, validation town, hyperparmeters tried and the mse
#' @export
#'
#' @examples
validate_elnet <- function(TVT_data,
                           response,
                           predictors,
                           hyperparameters) {

  # Fit model for each TVT permutation in the plan
  elnet_val <- TVT_data |> map_dfr(function(TVT) {

    # glmnet can not handle na's
    TVT <- TVT |> drop_na()

    # Pmap is essentially a row-wise apply equivalent.
    pmap_dfr(hyperparameters, function(penalty, mixture) {

      TVT |> group_by(Jitter) |>
        group_map(function(jitter, jitter_key) {

          # Identify our training and validation villages for this run
          train <- jitter |> filter(Role == "train")
          validate <- jitter |> filter(Role == "validate")

          # Fit the model using x row of hyper-parameters object
          # Sneaky trick to fit proportions. I'm pretty sure this works.
          TVT_fit <- glmnet::glmnet(y = train |> select_at(response) |> as.matrix(),
                                    x = train |> select_at(predictors) |> as.matrix(),
                                    family = "binomial",
                                    lambda = penalty,
                                    alpha = mixture)

          # Predict from the model using validation data.
          # For some reason type = "response" here always
          # produces output on the linear predictor scale. Inverse logit is required to get back to proportion.
          TVT_pred <- glmnet::predict.glmnet(TVT_fit,
                                             newx = validate |> select_at(predictors) |> as.matrix())

          # Calculate MAE of predictions against validation village truth.
          Ts_Mn <- validate |> pull(response[2]) / validate |> select_at(response) |> as.matrix() |> rowSums()
          validate_mae <- mean(abs(exp(TVT_pred) / (1 + exp(TVT_pred)) - Ts_Mn ))

          # Return a dataframe row with name of train town, validation town, hyper-parameters tried and the mae
          tibble(jitter = jitter_key,
                 train = train$Site |> unique(),
                 validate = validate$Site |> unique(),
                 lambda = penalty,
                 alpha = mixture,
                 validate_mae = validate_mae)
        }) |>
        bind_rows() |>
        group_by(train, validate, lambda, alpha) |>
        summarize(validate_mae_mean = mean(validate_mae),
                  validate_mae_sd = sd(validate_mae),
                  .groups = "drop")
      }) |> bind_rows()
  })

  return(elnet_val)
}



# Avg distance to rice between sites.
