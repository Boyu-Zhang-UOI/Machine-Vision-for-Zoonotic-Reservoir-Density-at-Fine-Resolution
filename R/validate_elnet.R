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
                           alphas) {

  # Fit model for each TVT permutation in the plan
  elnet_val <- map_dfr(1:length(TVT_data), function(tvt.x) {

    TVT <- TVT_data[[tvt.x]]

    # glmnet can not handle na's
    TVT <- TVT |> drop_na()

    # Pmap is essentially a row-wise apply equivalent.
    map_dfr(alphas, function(mixture) {

      print(paste("TVT:", tvt.x, "mixture:", mixture))

      TVT |> group_by(Jitter) |>
        group_map(function(jitter, jitter_key) {

          class_ratio <- sum(jitter$TS_Mn > 0) / nrow(jitter)
          jitter <- jitter |> mutate(class_weight = ifelse(TS_Mn > 0, 1-class_ratio, class_ratio))

          # Identify our training and validation villages for this run
          train <- jitter |> filter(Role == "train")
          validate <- jitter |> filter(Role == "validate")

          # Fit the model using x row of hyper-parameters object
          # Sneaky trick to fit proportions. I'm pretty sure this works.
          TVT_fit <- glmnet::glmnet(y = train |> select_at(response) |> as.matrix(),
                                    x = train |> select_at(predictors) |> as.matrix(),
                                    family = "binomial",
                                    # lambda = penalty,
                                    alpha = mixture,
                                    weight = train |> pull(class_weight))

          TVT_train <- glmnet::assess.glmnet(TVT_fit,
                                             newy = train |> select_at(response) |> as.matrix(),
                                             newx = train |> select_at(predictors) |> as.matrix(),
                                             family = "binomial",
                                             weight = train |> pull(class_weight)) |>
            bind_rows() |>
            mutate(penalty = TVT_fit$lambda,
                   comparison = "train")

          TVT_validate <- glmnet::assess.glmnet(TVT_fit,
                                                newy = validate |> select_at(response) |> as.matrix(),
                                                newx = validate |> select_at(predictors) |> as.matrix(),
                                                family = "binomial",
                                                weight = validate |> pull(class_weight)) |>
            bind_rows() |>
            mutate(penalty = TVT_fit$lambda,
                   comparison = "validate")

          TVT_best <- bind_rows(TVT_train, TVT_validate) |>
            bind_cols(mixture = mixture) |>
            mutate(jitter = jitter_key |> unlist(),
                   train = train$Site |> unique(),
                   validate = validate$Site |> unique()) |>
            select(train,
                   validate,
                   jitter,
                   penalty,
                   mixture,
                   everything())

          TVT_best
        })
      })
  }) |> bind_rows()

  return(elnet_val)
}
