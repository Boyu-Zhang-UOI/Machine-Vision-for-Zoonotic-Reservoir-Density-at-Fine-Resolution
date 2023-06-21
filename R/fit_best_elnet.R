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

    train_site <- TVT |> filter(Role == "train") |> pull(Site) |> unique()
    validate_site <- TVT |> filter(Role == "validate") |> pull(Site) |> unique()
    test_site <- TVT |> filter(Role == "test") |> pull(Site) |> unique()

    best_params <- elnet_best |> filter(train == train_site,
                                        validate == validate_site,
                                        test == test_site)

    jitter_fits <- TVT |> group_by(Jitter) |>
      group_map(function(jitter, jitter_key) {

        class_ratio <- sum(jitter$TS_Mn > 0) / nrow(jitter)
        jitter <- jitter |> mutate(class_weight = ifelse(TS_Mn > 0, 1-class_ratio, class_ratio))

        # Identify our training and validation villages for this run
        train <- jitter |> filter(Role == "train")
        test <- jitter |> filter(Role == "test")
        validate <- jitter |> filter(Role == "validate")

        TVT_fit <- glmnet::glmnet(y = train |> select_at(response) |> as.matrix(),
                                  x = train |> select_at(predictors) |> as.matrix(),
                                  family = "binomial",
                                  lambda = best_params |> pull(penalty),
                                  alpha = best_params |> pull(mixture),
                                  weight = train |> pull(class_weight))

        TVT_train <- glmnet::assess.glmnet(TVT_fit,
                                          newy = train |> select_at(response) |> as.matrix(),
                                          newx = train |> select_at(predictors) |> as.matrix()) |>
          bind_rows() %>%
          setNames(paste(names(.), "train", sep="_"))

        TVT_validate <- glmnet::assess.glmnet(TVT_fit,
                                          newy = validate |> select_at(response) |> as.matrix(),
                                          newx = validate |> select_at(predictors) |> as.matrix()) |>
          bind_rows() %>%
          setNames(paste(names(.), "validate", sep="_"))

        TVT_test <- glmnet::assess.glmnet(TVT_fit,
                                          newy = test |> select_at(response) |> as.matrix(),
                                          newx = test |> select_at(predictors) |> as.matrix()) |>
          bind_rows() %>%
          setNames(paste(names(.), "test", sep="_"))

        vi_dropout_loss <- DALEX::explain(TVT_fit,
                                          data = test |> select_at(predictors) |> as.matrix(),
                                          y = test |> select_at(response) |> as.matrix())

        TVT_pred <- glmnet::predict.glmnet(TVT_fit,
                                           newx = validate |> select_at(predictors) |> as.matrix(),
                                           family = "binomial",
                                           type = "link")

        TVT_pred <- exp(TVT_pred) / (1 + exp(TVT_pred))

        best_params |> bind_cols(jitter = jitter_key,
                                 TVT_train,
                                 TVT_validate,
                                 TVT_test,
                                 tibble(fit_Y = list(jitter |> select_at(response) |> as.matrix()),
                                        fit_X = list(jitter |> select_at(predictors)),
                                        fit_TS_Mn = list(validate |> pull("TS_Mn")),
                                        fit_pred = list(TVT_pred),
                                        vi = list(vi_dropout_loss),
                                        fit = list(TVT_fit)))
      }) |> bind_rows()

    jitter_fits

  }) |> bind_rows()
}
