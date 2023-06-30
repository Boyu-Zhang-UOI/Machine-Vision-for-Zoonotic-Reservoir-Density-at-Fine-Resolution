#' Title
#'
#' @param models
#' @param newdata
#'
#' @return
#' @export
#'
#' @examples
get_elnet_vi <- function(xgboost_preds,
                         elnet_best_fits,
                         response,
                         predictors,
                         TVT_plan) {

  library(DALEX)

  # 6 plans. 5 jitters. 25 jiggles. Ugh.
  map(1:length(TVT_plan), function(tvt.x) {

    TVT <- elnet_best_fits[[tvt.x]]

    train = TVT_plan[[tvt.x]]['train']
    validate = TVT_plan[[tvt.x]]['validate']
    test = TVT_plan[[tvt.x]]['test']

    dat <- xgboost_preds |>
      filter(train_site == train,
             val_site == validate,
             test_site == test) |>
      mutate(TS_Other = Tot_Other / TotTraps)

    map(1:length(TVT), function(jitter) {

      TVT_fit <- TVT[[jitter]]

      explain(TVT_fit,
              y = dat |> select_at(response) |> as.matrix(),
              data = dat |> select_at(predictors) |> as.matrix()) %>%
        DALEX::model_parts()

    })
  })
}
