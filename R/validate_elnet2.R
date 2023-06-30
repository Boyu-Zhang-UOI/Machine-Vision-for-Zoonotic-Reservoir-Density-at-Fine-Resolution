#' Validate an elastic net model across a list of data inputs
#'
#' @param TVT_data # List of different data inputs
#' @param response # Response column name
#' @param predictors # Vector of predictor column names
#' @param alphas # vector of values setting ridge vs lasso mixture
#'
#' @return elnet # A dataframe row with name of train town, validation town, hyperparmeters tried and the mse
#' @export
#'
#' @examples
validate_elnet2 <- function(TVT_data,
                           response,
                           predictors,
                           alphas,
                           n_workers = 1) {

  # Fit model for each TVT permutation in the plan
  elnet_val <- map_dfr(1:length(TVT_data), function(tvt.x) {

    TVT <- TVT_data[[tvt.x]]

    train <- TVT |> filter(Role == "train") |> select_at(c("Site", response, predictors, "Tot_Traps")) |> drop_na()
    validate <- TVT |> filter(Role == "validate") |> select_at(c("Site", response, predictors, "Tot_Traps")) |> drop_na()

    print(paste("TVT:", tvt.x))

    bettermc::mclapply(alphas,
                       mc.silent = F,
                       mc.progress = T,
                       mc.allow.fatal = T,
                       mc.preschedule = F, # mc.preschedule = F is dynamic scheduling
                       mc.cores = getOption("mc.cores", n_workers),
                       function(mixture) {
                         # Fit the model using x row of hyper-parameters object
                         TVT_fit <- glmnet::glmnet(y = train |> select_at(response) |> as.matrix(),
                                                   x = train |> select_at(predictors) |> as.matrix(),
                                                   family = "binomial",
                                                   nlambda = 100,
                                                   # lambda = seq(0,2,length.out = 100),
                                                   alpha = mixture,
                                                   weight = train |> pull(Tot_Traps))


                         TVT_train <- glmnet::assess.glmnet(TVT_fit,
                                                            newy = train |> select_at(response) |> as.matrix(),
                                                            newx = train |> select_at(predictors) |> as.matrix(),
                                                            family = "binomial",
                                                            weight = train |> pull(Tot_Traps)) |>
                           bind_rows() |>
                           mutate(penalty = TVT_fit$lambda,
                                  comparison = "train")

                           TVT_validate <- glmnet::assess.glmnet(TVT_fit,
                                                                 newy = validate |> select_at(response) |> as.matrix(),
                                                                 newx = validate |> select_at(predictors) |> as.matrix(),
                                                                 family = "binomial",
                                                                 weight = validate |> pull(Tot_Traps)) |>
                             bind_rows() |>
                             mutate(penalty = TVT_fit$lambda,
                                    comparison = "validate")

                           TVT_best <- bind_rows(TVT_train, TVT_validate) |>
                             bind_cols(mixture = mixture) |>
                             mutate(train = train$Site |> unique(),
                                    validate = validate$Site |> unique()) |>
                             select(train,
                                    validate,
                                    penalty,
                                    mixture,
                                    everything())
                           TVT_best
             })
  })
  return(elnet_val)
}
