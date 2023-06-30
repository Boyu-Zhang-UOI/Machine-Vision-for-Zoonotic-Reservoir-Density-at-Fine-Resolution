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
                           elnet_best,
                           weights = T) {

  # Fit model for each TVT permutation in the plan
  elnet_fits <- TVT_data |> map(function(TVT) {

    train_site <- TVT |> filter(Role == "train") |> pull(Site) |> unique()
    validate_site <- TVT |> filter(Role == "validate") |> pull(Site) |> unique()
    test_site <- TVT |> filter(Role == "test") |> pull(Site) |> unique()

    best_params <- elnet_best |> filter(train == train_site,
                                        validate == validate_site,
                                        test == test_site)

    map(unique(TVT$Jitter), function(ji) {

        train <- TVT |> filter(Role == "train", Jitter == ji) |> select_at(c("Site", response, predictors, "Tot_Traps")) |> drop_na()

        train <- train |> mutate(weights = Tot_Traps)
        if(!weights) train <- train |> mutate(weights = 1)

        TVT_fit <- glmnet::glmnet(y = train |> select_at(response) |> as.matrix(),
                                  x = train |> select_at(predictors) |> as.matrix(),
                                  family = "binomial",
                                  lambda = best_params |> pull(penalty),
                                  alpha = best_params |> pull(mixture),
                                  weight = train |> pull(weights))

        TVT_fit
      })
  })
}
