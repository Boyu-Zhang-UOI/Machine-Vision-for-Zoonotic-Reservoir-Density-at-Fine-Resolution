
get_elnet_preds <- function(TVT_pred_data,
                            elnet_best_fits,
                            response,
                            predictors,
                            TVT_plan) {

  # 6 plans. 5 jitters. 25 jiggles. Ugh.
  preds <- map_dfr(1:length(TVT_pred_data), function(tvt.x) {

    TVT <- TVT_pred_data[[tvt.x]]
    TVT_fits <- elnet_best_fits[[tvt.x]]
    TVT_p <- TVT_plan[[tvt.x]]

    map_dfr(unique(TVT$Jiggle), function(jiggle) {

      pred_data <- TVT |> filter(Jiggle == jiggle,
                                 Site == TVT_p['test'])

      map_dfr(1:length(TVT_fits), function(jitter) {

        model <- TVT_fits[[jitter]]
        preds <- predict(model,
                         newx = pred_data |> select_at(predictors) |> as.matrix(),
                         family = "binomial",
                         type= "response") |> as.vector()

        pred_data |> ungroup() |> mutate(elnet_preds = preds,
                                         Jitter = jitter,
                                         train_site = TVT_p['train'],
                                         val_site = TVT_p['validate'],
                                         test_site = TVT_p['test'])
      })
    })
  })

  preds
}
