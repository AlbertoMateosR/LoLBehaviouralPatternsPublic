#' Title
#'
#' @param sts_data
#'
#' @return Return a list with both the best model (field $best) and the list of a
#' all fitted models (field $all)
#' @export
#'
#' @examples
fit_and_select_hmm = function(sts_data) {
  fittedModels <- list()

  hmmMod <- build_hmm(sts_data, n_states = 2)
  hmmFittedMod <- fit_model(hmmMod, threads = 3)

  fittedModels <- fittedModels %>% append(hmmFittedMod)
  bestModel <- hmmFittedMod$model

  for (i in 3:8) {
    hmmMod = build_hmm(sts_data, n_states = i)
    hmmFittedMod = fit_model(hmmMod, threads = 3)

    fittedModels <- fittedModels %>% append(hmmFittedMod)

    if (BIC(bestModel) > BIC(hmmFittedMod$model)){
      bestModel <- hmmFittedMod$model
    }
  }

  return(list(best = bestModel, all = fittedModels))
}
