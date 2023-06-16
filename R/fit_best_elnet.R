#' Title
#'
#' @param TVT_data
#' @param response
#' @param predictors
#' @param weight
#' @param elnet_best
#'
#' @return
#' @export
#'
#' @examples
fit_best_elnet <- function(TVT_data,
                           response,
                           predictors,
                           elnet_best) {

  # Fit model for each TVT permutation in the plan
  elnet_fits <- TVT_data |> map_dfr(function(TVT) {

    # glmnet can not handle na's
    TVT <- TVT |> drop_na()

    training_site <- TVT |> filter(Role == "train") |> pull(Site) |> unique()
    testing_site <- TVT |> filter(Role == "test") |> pull(Site) |> unique()

    best_params <- elnet_best |> filter(train == training_site,
                                        test == testing_site)

    jitter_fits <- TVT |> group_by(Jitter) |>
      group_map(function(jitter, jitter_key) {

        # Identify our training and validation villages for this run
        train <- jitter |> filter(Role == "train")
        test <- jitter |> filter(Role == "test")

        # Fit the model using x row of hyper-parameters object
        # Sneaky trick to fit proportions. I'm pretty sure this works.
        TVT_fit <- glmnet::glmnet(y = train |> select_at(response) |> as.matrix(),
                                  x = train |> select_at(predictors) |> as.matrix(),
                                  family = "binomial",
                                  lambda = best_params |> pull(lambda),
                                  alpha = best_params |> pull(alpha),
                                  standardize = T)

        TVT_pred <- glmnet::predict.glmnet(TVT_fit,
                                newx = test |> select_at(predictors) |> as.matrix(),
                                s="lambda.min")

        TVT_pred <- exp(TVT_pred) / (1 + exp(TVT_pred))

        # Calculate MAE of predictions against validation village truth.
        test_mae <- mean(abs(TVT_pred - test |> pull("TS_Mn")))

        vi_dropout_loss <- DALEX::explain(TVT_fit,
                                          data = test |> select_at(predictors) |> as.matrix(),
                                          y = test |> pull("TS_Mn"))

        prop_Mn <- jitter |> select_at(response) |> pull(last_col())
        prop_Mn <- sum(prop_Mn > 0) / length(prop_Mn)

        best_params |> bind_cols(jitter = jitter_key,
                                 tibble(test_mae = test_mae,
                                        prop_Mn = prop_Mn,
                                        fit_Y = list(jitter |> select_at(response) |> as.matrix()),
                                        fit_X = list(jitter |> select_at(predictors)),
                                        fit_TS_Mn = list(test |> pull("TS_Mn")),
                                        fit_pred = list(TVT_pred),
                                        vi = list(vi_dropout_loss),
                                        fit = list(TVT_fit)))
      }) |> bind_rows()

    jitter_fits

  }) |> bind_rows()
}


glmnet_pred <- function(model, newx) {


  # Fit the model using x row of hyper-parameters object
  # Sneaky trick to fit proportions. I'm pretty sure this works.
  TVT_pred <- glmnet::predict.glmnet(model,
                                     newx = newx)

  TVT_pred

}
