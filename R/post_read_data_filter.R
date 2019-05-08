#' Title
#'
#' @param input_df String containing the filepath of the input file
#'
#' @return
#' @export
#'
#' @examples
post_read_data_filter <- function(data) {
  MIN_EVENTS = 5
  foo = data$Blue %>% count(Address)
  bar = data$Red %>% count(Address)
  valid_addresses = inner_join(foo, bar, by = "Address") %>%
    dplyr::filter(n.x >= MIN_EVENTS & n.y >= MIN_EVENTS) %>%
    pull(Address)
  data$Blue = data$Blue %>% filter(Address %in% valid_addresses)
  data$Red = data$Red %>% filter(Address %in% valid_addresses)

  blueMutated <- data$Blue %>%
    mutate(Event, Event = case_when(
      Event == "MID_LANE_BASE_TURRET" |
        Event == "BOT_LANE_BASE_TURRET" |
        Event == "TOP_LANE_BASE_TURRET" ~ "BASE_TURRET",
      Event == "MID_LANE_INNER_TURRET" |
        Event == "BOT_LANE_INNER_TURRET" |
        Event == "TOP_LANE_INNER_TURRET" ~ "INNER_TURRET",
      Event == "MID_LANE_OUTER_TURRET" |
        Event == "BOT_LANE_OUTER_TURRET" |
        Event == "TOP_LANE_OUTER_TURRET" ~ "OUTER_TURRET",
      Event == "MID_LANE_NEXUS_TURRET" ~ "OUTER_TURRET",
      Event == "MID_LANE_Inhib" |
        Event == "BOT_LANE_Inhib" |
        Event == "TOP_LANE_Inhib" ~ "Inhib",
      TRUE ~ Event
    ))

  redMutated <- data$Red %>%
    mutate(Event, Event = case_when(
      Event == "MID_LANE_BASE_TURRET" |
        Event == "BOT_LANE_BASE_TURRET" |
        Event == "TOP_LANE_BASE_TURRET" ~ "BASE_TURRET",
      Event == "MID_LANE_INNER_TURRET" |
        Event == "BOT_LANE_INNER_TURRET" |
        Event == "TOP_LANE_INNER_TURRET" ~ "INNER_TURRET",
      Event == "MID_LANE_OUTER_TURRET" |
        Event == "BOT_LANE_OUTER_TURRET" |
        Event == "TOP_LANE_OUTER_TURRET" ~ "OUTER_TURRET",
      Event == "MID_LANE_NEXUS_TURRET" ~ "NEXUS_TURRET",
      Event == "MID_LANE_Inhib" |
        Event == "BOT_LANE_Inhib" |
        Event == "TOP_LANE_Inhib" ~ "Inhib",
      TRUE ~ Event
    ))

  aux_blue = blueMutated %>%
    nest(-Address) %>%
    mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
    mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))

  aux_red = redMutated %>%
    nest(-Address) %>%
    mutate(data_list = purrr::map(data, function(x) x %>% arrange(Timestamp) %>% pull(Event))) %>%
    mutate(data_list = purrr::map(data_list, function(x) purrr::set_names(x = x, nm = paste0("event", seq(1:length(x))))))


  aux_blue_2 = aux_blue$data_list %>% purrr::map(as.list) %>% bind_rows
  rownames(aux_blue_2) = aux_blue$Address
  aux_red_2 = aux_red$data_list %>% purrr::map(as.list) %>% bind_rows
  rownames(aux_red_2) = aux_red$Address

  #Estos resultados pueden usarse con seqdef ya que su formato es apto, y despues pueden visualizarse con seqiplot por ejemplo
  return(list(Blue = aux_blue_2, Red = aux_red_2))
}
