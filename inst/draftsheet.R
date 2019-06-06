library(tidyr)
library(ggplot2)

# Pintar la evolucion del BIC
BIC_df = data.frame(
  N_states = sapply(TimedMD$bResult1$events_blue$all, function(x) x$model$model$n_states),
  bResult1_eventsBlue = sapply(TimedMD$bResult1$events_blue$all, function(x) {x$BIC}),
  bResult1_eventsRed = sapply(TimedMD$bResult1$events_red$all, function(x){x$BIC}),
  bResult1_golddiff = sapply(TimedMD$bResult1$gold_diff$all, function(x) {x$BIC}),
  bResult0_eventsBlue = sapply(TimedMD$bResult0$events_blue$all, function(x) {x$BIC}),
  bResult0_eventsRed = sapply(TimedMD$bResult0$events_red$all, function(x) {x$BIC}),
  bResult0_golddiff = sapply(TimedMD$bResult0$gold_diff$all, function(x) {x$BIC})
)

BIC_df_plot = gather(data = BIC_df, key = "model", value = "BIC", -N_states)

ggplot(data = BIC_df_plot,
       mapping = aes(x = N_states, y = BIC)) +
  facet_wrap(facets = ~ model, scales = "free_y") +
  geom_line() + geom_point()

# Tiempos de ejecucion
time_df = data.frame(
  N_states = sapply(TimedMD$bResult1$events_blue$all, function(x) x$model$model$n_states),
  bResult1_eventsBlue = sapply(TimedMD$bResult1$events_blue$all, function(x) {x$time}),
  bResult1_eventsRed = sapply(TimedMD$bResult1$events_red$all, function(x){x$time}),
  bResult1_golddiff = sapply(TimedMD$bResult1$gold_diff$all, function(x) {x$time}),
  bResult0_eventsBlue = sapply(TimedMD$bResult0$events_blue$all, function(x) {x$time}),
  bResult0_eventsRed = sapply(TimedMD$bResult0$events_red$all, function(x) {x$time}),
  bResult0_golddiff = sapply(TimedMD$bResult0$gold_diff$all, function(x) {x$time})
)

time_df_plot = gather(data = time_df, key = "model", value = "Time_to_fit", -N_states)

ggplot(data = time_df_plot,
       mapping = aes(x = N_states, y = Time_to_fit)) +
  facet_wrap(facets = ~ model, scales = "free_y") +
  geom_line() + geom_point()


# Modelos
plot(TimedMD$bResult1$events_blue$best)
