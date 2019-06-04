library(tidyr)
library(ggplot2)

# Pintar la evolucion del BIC
BIC_df = data.frame(
  N_states = sapply(TimedMD$bResult1$events_blue$all, function(x) x$model$model$n_states),
  bResult1_eventsBlue = sapply(TimedMD$bResult1$events_blue$all, function(x) BIC(x$model$model)),
  bResult1_eventsRed = sapply(TimedMD$bResult1$events_red$all, function(x) BIC(x$model$model)),
  bResult1_golddiff = sapply(TimedMD$bResult1$gold_diff$all, function(x) BIC(x$model$model)),
  bResult0_eventsBlue = sapply(TimedMD$bResult0$events_blue$all, function(x) BIC(x$model$model)),
  bResult0_eventsRed = sapply(TimedMD$bResult0$events_red$all, function(x) BIC(x$model$model)),
  bResult0_golddiff = sapply(TimedMD$bResult0$gold_diff$all, function(x) BIC(x$model$model))
)

BIC_df_plot = gather(data = BIC_df, key = "model", value = "BIC", -N_states)

ggplot(data = BIC_df_plot,
       mapping = aes(x = N_states, y = BIC)) +
  facet_wrap(facets = ~ model, scales = "free_y") +
  geom_line() + geom_point()

# Tiempos de ejecucion

# Modelos
plot(TimedMD$bResult1$events_blue$best)
