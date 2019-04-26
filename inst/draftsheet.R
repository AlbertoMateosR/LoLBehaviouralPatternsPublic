# Formateo de data.frame a sts (SOLO SE CONVIERTE TOTALMENTE A STS PARA EL AZUL)
aux_blue = data$Blue %>%
  nest(-Address) %>%
  mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
  mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))

aux_red = data$Red %>%
  nest(-Address) %>%
  mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
  mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))


aux_blue_2 = aux_blue$data_list %>% purrr::map(as.list) %>% bind_rows
rownames(aux_blue_2) = aux_blue$Address
blue_sts = seqdef(aux_blue_2)

# Filtrado de atos por tipo de evnto, y agrupacion de eventos comunes
data$Blue %>%
  mutate(Event, Event = case_when(
    Event == "MID_LANE/BASE_TURRET" |
      Event == "BOT_LANE/BASE_TURRET" |
      Event == "TOP_LANE/BASE_TURRET" ~ "BASE_TURRET",
    Event == "MID_LANE/INNER_TURRET" |
      Event == "BOT_LANE/INNER_TURRET" |
      Event == "TOP_LANE/INNER_TURRET" ~ "BASE_TURRET",
    TRUE ~ Event
  ))

# TODO: Filtrado de partidas por numero de simulaciones (inner_join)
MIN_EVENTS = 5
foo = data$Blue %>% count(Address)
bar = data$Red %>% count(Address)
valid_addresses = inner_join(foo, bar, by = "Address") %>%
  dplyr::filter(n.x >= MIN_EVENTS & n.y >= MIN_EVENTS) %>%
  pull(Address)
data$Blue = data$Blue %>% filter(Address %in% valid_addresses)

