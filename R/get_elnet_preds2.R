
get_elnet_preds2 <- function(xgboost_preds,
                             elnet_best_fits,
                             response,
                             predictors,
                             TVT_plan) {

  # 6 plans. 5 jitters. 25 jiggles. Ugh.
  preds <- map_dfr(1:length(TVT_plan), function(tvt.x) {

    TVT_fit <- elnet_best_fits[[tvt.x]]

    train = TVT_plan[[tvt.x]]['train']
    validate = TVT_plan[[tvt.x]]['validate']
    test = TVT_plan[[tvt.x]]['test']

    map_dfr(unique(xgboost_preds$ji), function(jiggle) {

      xgboost_preds |>
        filter(ji == jiggle,
               train_site == train,
               val_site == validate,
               test_site == test) %>%
        mutate(elnet_preds = predict(TVT_fit,
                                    newx = . |> select_at(predictors) |> as.matrix(),
                                    family = "binomial",
                                    type= "response") |> as.vector())
    })
  })

  preds
}
