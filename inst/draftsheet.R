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
plot(TimedMD$bResult1$events_blue$best$model$model)
plot(TimedMD$bResult1$events_blue$best$model$model, layout = layout_in_circle, edge.curved = FALSE, edge.arrow.size = 0.8, edge.label = NA, legend.prop = 0.3, vertex.size = 35, combine.slices = 0.1, cex.edge.width = 1.35, combined.slice.label = "States with probability < 0.1")
 #Triming
plot(TimedMD$bResult1$events_blue$best$model$model, layout = layout_in_circle, edge.curved = FALSE, edge.arrow.size = 1.35, edge.label = NA, legend.prop = 0.3, vertex.size = 35, combine.slices = 0.1, cex.edge.width = 1.35, combined.slice.label = "States with probability < 0.1", trim = 0.1)
