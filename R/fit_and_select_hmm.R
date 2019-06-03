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
  bestModel <- as.double(1)

  fittedModels = lapply(3:8, function (i) {
    startT <- Sys.time()

    hmmMod = build_hmm(sts_data, n_states = i)
    hmmFittedMod = fit_model(hmmMod, threads = 8)

    endT <- Sys.time()

    if(i == 2) {
      bestModel <- hmmFittedMod$model
    } else if (BIC(bestModel) > BIC(hmmFittedMod$model)){
      bestModel <- hmmFittedMod$model
    }

    list(model = hmmFittedMod, time = (endT - startT))
  })

  return(list(best = bestModel, all = fittedModels))
}
