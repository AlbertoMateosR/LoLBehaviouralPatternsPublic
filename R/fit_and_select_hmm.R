#' Title
#'
#' @param sts_data
#'
#' @return Return a list with both the best model (field $best) and the list of a
#' all fitted models (field $all)
#' @export
#'
#' @examples
fit_and_select_hmm = function(sts_data, nstates_range = 3:10) {
  fittedModels = lapply(nstates_range, function (i) {
    startT <- Sys.time()

    hmmMod = build_hmm(sts_data, n_states = i)
    hmmFittedMod = fit_model(hmmMod, threads = 8)

    endT <- Sys.time()

    list(model = hmmFittedMod, time = (endT - startT), BIC = BIC(hmmFittedMod$model))
  })

  bestBIC = sapply(fittedModels, function(x) {x$BIC}) %>%
    min()

  bestModel = Filter(function(x) x$BIC == bestBIC, fittedModels)[[1]]

  return(list(best = bestModel, all = fittedModels))
}
