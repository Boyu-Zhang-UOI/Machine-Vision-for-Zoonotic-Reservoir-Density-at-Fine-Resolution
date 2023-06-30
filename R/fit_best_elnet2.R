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
fit_best_elnet2 <- function(TVT_data,
                           response,
                           predictors,
                           elnet_best,
                           class_weights) {

  # Fit model for each TVT permutation in the plan
  elnet_fits <- TVT_data |> map(function(TVT) {

    train_site <- TVT |> filter(Role == "train") |> pull(Site) |> unique()
    validate_site <- TVT |> filter(Role == "validate") |> pull(Site) |> unique()
    test_site <- TVT |> filter(Role == "test") |> pull(Site) |> unique()

    train <- TVT |> filter(Role == "train") |> select_at(c("Site", response, predictors, "Tot_Traps")) |> drop_na()

    best_params <- elnet_best |> filter(train == train_site,
                                        validate == validate_site,
                                        test == test_site)

    TVT_fit <- glmnet::glmnet(y = train |> select_at(response) |> as.matrix(),
                              x = train |> select_at(predictors) |> as.matrix(),
                              family = "binomial",
                              lambda = best_params |> pull(penalty),
                              alpha = best_params |> pull(mixture),
                              weight = train |> pull(Tot_Traps))

    TVT_fit
  })
}
