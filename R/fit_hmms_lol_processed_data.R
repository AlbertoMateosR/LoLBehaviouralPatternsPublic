#' Title
#'
#' @param processed_data Estructura conocida de campos
#'
#' @return Misma estructura que argumento original, pero con modelos en vez de con datos
#' @export
#'
#' @examples
fit_hmms_to_lol_processed_data = function(processed_data) {
  b_res1_events <- fit_and_select_hmm(processed_data$bResult1$events_blue)
  b_res0_events <- fit_and_select_hmm(processed_data$bResult0$events_blue)
  print("Events blue done")

  r_res1_events <- fit_and_select_hmm(processed_data$bResult1$events_red)
  r_res0_events <- fit_and_select_hmm(processed_data$bResult0$events_red)
  print("Events red done")

  g_res1_diff <- fit_and_select_hmm(processed_data$bResult1$gold_diff)
  g_res0_diff <- fit_and_select_hmm(processed_data$bResult0$gold_diff)

  return(list (bResult1 = list(events_blue = b_res1_events, events_red = r_res1_events, gold_diff = g_res1_diff), bResult0 = list(events_blue = b_res0_events, events_red = r_res0_events, gold_diff = g_res0_diff)))
}
