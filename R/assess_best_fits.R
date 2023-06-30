
assess_best_fits <- function(newdata,
                             elnet_best_fits,
                             response,
                             predictors) {

  # 6 plans. 5 jitters. 25 jiggles. Ugh.
  map_dfr(1:length(TVT_plan), function(tvt.x) {
    TVT <- elnet_best_fits[[tvt.x]]
    map_dfr(1:length(TVT), function(fit.x) {
      TVT_fit <- TVT[[fit.x]]
      map_dfr(unique(xgboost_preds$ji), function(jiggle) {

        newdata <- xgboost_preds |> filter(ji == jiggle)

        TVT_pred <- glmnet::predict.glmnet(TVT_fit,
                                           newy = newdata |> select_at(response) |> as.matrix(),
                                           newx = newdata |> select_at(predictors) |> as.matrix(),
                                           family = "binomial") #|> bind_rows()

        TVT_pred <- exp(TVT_pred) / (1 + exp(TVT_pred))

        bind_cols(TVT = tvt.x, jitter = fit.x, jiggle = jiggle + 1, TVT_pred) # +1 because python zero index
      })
    })
  })

}
