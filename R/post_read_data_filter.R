
#' Title
#'
#' @param data
#' @param time_step_duration Longitud del intervalo. La funcion debe retornar error si 2
#' eventos coinciden en el mismo time step
#'
#' @return
#' @export
#'
#' @examples
post_read_data_filter <- function(data) {

  BEWMutated <- data$BEW %>%
    mutate(Event, Event = case_when(
      Event == "MID_LANE_BASE_TURRET" |
        Event == "BOT_LANE_BASE_TURRET" |
        Event == "TOP_LANE_BASE_TURRET" ~ "BASE_TURRET",
      Event == "MID_LANE_NEXUS_TURRET" ~ "NEXUS_TURRET",
      Event == "MID_LANE_Inhib" |
        Event == "BOT_LANE_Inhib" |
        Event == "TOP_LANE_Inhib" ~ "Inhib",
      Event == "EARTH_DRAGON" |
        Event == "FIRE_DRAGON" |
        Event == "WATER_DRAGON" |
        Event == "AIR_DRAGON" ~ "ELEMENTAL_DRAGON",
      TRUE ~ Event
    )) %>% filter(Event != "BASE_TURRET")

  BELMutated <- data$BEL %>%
    mutate(Event, Event = case_when(
      Event == "MID_LANE_BASE_TURRET" |
        Event == "BOT_LANE_BASE_TURRET" |
        Event == "TOP_LANE_BASE_TURRET" ~ "BASE_TURRET",
      Event == "MID_LANE_NEXUS_TURRET" ~ "NEXUS_TURRET",
      Event == "MID_LANE_Inhib" |
        Event == "BOT_LANE_Inhib" |
        Event == "TOP_LANE_Inhib" ~ "Inhib",
      Event == "EARTH_DRAGON" |
        Event == "FIRE_DRAGON" |
        Event == "WATER_DRAGON" |
        Event == "AIR_DRAGON" ~ "ELEMENTAL_DRAGON",
      TRUE ~ Event
    )) %>% filter(Event != "BASE_TURRET")

  REWMutated <- data$REW %>%
    mutate(Event, Event = case_when(
      Event == "MID_LANE_BASE_TURRET" |
        Event == "BOT_LANE_BASE_TURRET" |
        Event == "TOP_LANE_BASE_TURRET" ~ "BASE_TURRET",
      Event == "MID_LANE_NEXUS_TURRET" ~ "NEXUS_TURRET",
      Event == "MID_LANE_Inhib" |
        Event == "BOT_LANE_Inhib" |
        Event == "TOP_LANE_Inhib" ~ "Inhib",
      Event == "EARTH_DRAGON" |
        Event == "FIRE_DRAGON" |
        Event == "WATER_DRAGON" |
        Event == "AIR_DRAGON" ~ "ELEMENTAL_DRAGON",
      TRUE ~ Event
    )) %>% filter(Event != "BASE_TURRET")

  RELMutated <- data$REL %>%
    mutate(Event, Event = case_when(
      Event == "MID_LANE_BASE_TURRET" |
        Event == "BOT_LANE_BASE_TURRET" |
        Event == "TOP_LANE_BASE_TURRET" ~ "BASE_TURRET",
      Event == "MID_LANE_NEXUS_TURRET" ~ "NEXUS_TURRET",
      Event == "MID_LANE_Inhib" |
        Event == "BOT_LANE_Inhib" |
        Event == "TOP_LANE_Inhib" ~ "Inhib",
      Event == "EARTH_DRAGON" |
        Event == "FIRE_DRAGON" |
        Event == "WATER_DRAGON" |
        Event == "AIR_DRAGON" ~ "ELEMENTAL_DRAGON",
      TRUE ~ Event
    )) %>% filter(Event != "BASE_TURRET")

  GDWMutated <- data$GDW %>%
    mutate(goldBdiff, goldBdiff = case_when(
      goldBdiff <= 2500 & goldBdiff > 300 ~ "bWinLow",
      goldBdiff <= 6500 & goldBdiff > 2500 ~ "bWinMedium",
      goldBdiff > 6500 ~ "bWinHigh",
      goldBdiff >= -2500 & goldBdiff < -300 ~ "bLoseLow",
      goldBdiff >= -6500 & goldBdiff < -2500 ~ "bLoseMedium",
      goldBdiff < -6500 ~ "bLoseHigh",
      goldBdiff >= -300 & goldBdiff <= 300 ~ "Same"
    )) %>% rename(Event = goldBdiff)

  GDLMutated <- data$GDL %>%
    mutate(goldBdiff, goldBdiff = case_when(
      goldBdiff <= 2500 & goldBdiff > 300 ~ "bWinLow",
      goldBdiff <= 6500 & goldBdiff > 2500 ~ "bWinMedium",
      goldBdiff > 6500 ~ "bWinHigh",
      goldBdiff >= -2500 & goldBdiff < -300 ~ "bLoseLow",
      goldBdiff >= -6500 & goldBdiff < -2500 ~ "bLoseMedium",
      goldBdiff < -6500 ~ "bLoseHigh",
      goldBdiff >= -300 & goldBdiff <= 300 ~ "Same"
    )) %>% rename(Event = goldBdiff)

  # TODO: Discretizar las secuencias
  # 1. Calcular la partida con mas tiempo
  # 2. Establecer el numero maximo de time steps que va a haber en base a la longitud
  # de esa partida y el argumento time_step_duration

  BEWMutatedaux = BEWMutated %>%
    nest(-Address) %>%
    mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
    mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))

  BELMutatedaux = BELMutated %>%
    nest(-Address) %>%
    mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
    mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))

  REWMutatedaux = REWMutated %>%
    nest(-Address) %>%
    mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
    mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))

  RELMutatedaux = RELMutated %>%
    nest(-Address) %>%
    mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
    mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))

  GDWMutatedaux = GDWMutated %>%
    nest(-Address) %>%
    mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
    mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))

  GDLMutatedaux = GDLMutated %>%
    nest(-Address) %>%
    mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
    mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))


  BEWMutatedaux2 = BEWMutatedaux$data_list %>% purrr::map(as.list) %>% bind_rows
  rownames(BEWMutatedaux2) = BEWMutatedaux$Address
  BELMutatedaux2 = BELMutatedaux$data_list %>% purrr::map(as.list) %>% bind_rows
  rownames(BELMutatedaux2) = BELMutatedaux$Address
  REWMutatedaux2 = REWMutatedaux$data_list %>% purrr::map(as.list) %>% bind_rows
  rownames(REWMutatedaux2) = REWMutatedaux$Address
  RELMutatedaux2 = RELMutatedaux$data_list %>% purrr::map(as.list) %>% bind_rows
  rownames(RELMutatedaux2) = RELMutatedaux$Address
  GDWMutatedaux2 = GDWMutatedaux$data_list %>% purrr::map(as.list) %>% bind_rows
  rownames(GDWMutatedaux2) = GDWMutatedaux$Address
  GDLMutatedaux2 = GDLMutatedaux$data_list %>% purrr::map(as.list) %>% bind_rows
  rownames(GDLMutatedaux2) = GDLMutatedaux$Address


  stsBEW <- seqdef(BEWMutatedaux2)
  stsBEL <- seqdef(BELMutatedaux2)
  stsREW <- seqdef(REWMutatedaux2)
  stsREL <- seqdef(RELMutatedaux2)
  stsGDW <- seqdef(GDWMutatedaux2)
  stsGDL <- seqdef(GDLMutatedaux2)


  #Estos resultados pueden usarse con seqdef ya que su formato es apto, y despues pueden visualizarse con seqiplot por ejemplo
  return(list (bResult1 = list(events_blue = stsBEW, events_red = stsREW, gold_diff = stsGDW), bResult0 = list(events_blue = stsBEL, events_red = stsREL, gold_diff = stsGDL)))
}
