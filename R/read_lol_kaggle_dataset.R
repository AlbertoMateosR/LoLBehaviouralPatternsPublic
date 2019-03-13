#' Title
#'
#' @param input_df String containing the filepath of the input file
#'
#' @return
#' @export
#'
#' @examples
read_lol_kaggle_dataset <- function(input_df) {
  Data <- read.csv(file = input_df, header = TRUE, sep = ",")

  #Data <- dplyr::filter(DataPre, Year > 2016) %>% dplyr::slice(91:nrows(DataPre))

  bDataDragons <- Data %>%
    mutate(bDragons = purrr::map(bDragons, function(x) {
      aux <- stringr::str_replace_all(x, pattern = "None", replacement = "null")
      asd <- jsonlite::fromJSON(stringr::str_replace_all(aux, "'", '"')) %>%
        as.data.frame() %>%
        drop_na()
    })) %>%
    filter(purrr::map_lgl(bDragons, ~ nrow(.) > 0)) %>%
    tidyr::unnest(bDragons) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2)

  # Save only modern matches (valid addresses that have typed dragons)
  valid_addresses = unique(bDataDragons$Address)

  # Filter the original Data
  Data <- Data %>% filter(Address %in% valid_addresses)

  bDataTowers <- Data %>%
    mutate(bTowers = purrr::map(bTowers, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(bTowers) %>%
    dplyr::select(Address, Timestamp = V1 , Lane = V2, Tower_Type = V3) %>%
    mutate(Timestamp = as.numeric(Timestamp)) %>%
    tidyr::unite("Event", sep = "/", Lane, Tower_Type)

  bDataInhibs <- Data %>%
    mutate(bInhibs = purrr::map(bInhibs, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(bInhibs) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2) %>%
    mutate(Timestamp = as.numeric(Timestamp)) %>%
    mutate(Event = purrr::map_chr(Event, function(x) {
      paste(x, "Inhib", sep = "/")
    }))

  bDataBarons <- Data %>%
    mutate(bBarons = purrr::map(bBarons, function(x) {
      jsonlite::fromJSON(toString(x)) %>% as.data.frame()
    })) %>%
    tidyr::unnest(bBarons) %>%
    dplyr::select(Address, Timestamp = V1) %>%
    mutate(Event = "Baron")

  bDataHeralds <- Data %>%
    mutate(bHeralds = purrr::map(bHeralds, function(x) {
      jsonlite::fromJSON(toString(x)) %>% as.data.frame()
    })) %>%
    tidyr::unnest(bHeralds) %>%
    dplyr::select(Address, Timestamp = V1) %>%
    mutate(Event = "Herald")

  rDataTowers <- Data %>%
    mutate(rTowers = purrr::map(rTowers, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(rTowers) %>%
    dplyr::select(Address, Timestamp = V1 , Lane = V2, Tower_Type = V3) %>%
    mutate(Timestamp = as.numeric(Timestamp)) %>%
    tidyr::unite("Event", sep = "/", Lane, Tower_Type)


  rDataInhibs <- Data %>%
    mutate(rInhibs = purrr::map(rInhibs, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(rInhibs) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2) %>%
    mutate(Timestamp = as.numeric(Timestamp)) %>%
    mutate(Event = purrr::map_chr(Event, function(x) {
      paste(x, "Inhib", sep = "/")
    }))

  #rDataDragons <- Data %>%
  # mutate(rDragons = purrr::map(rDragons, function(x) {
  #  elems <- c("'|None")
  # jsonlite::fromJSON(stringr::str_replace_all(x, elems, '"')) %>% as.data.frame()
  #})) %>%
  #tidyr::unnest(rDragons) %>%
  #dplyr::select(Address, Timestamp = V1)

  rDataDragons <- Data %>%
    mutate(rDragons = purrr::map(rDragons, function(x) {
      aux <- stringr::str_replace_all(x, pattern = "None", replacement = "null")
      asd <- jsonlite::fromJSON(stringr::str_replace_all(aux, "'", '"')) %>%
        as.data.frame() %>%
        drop_na()
    })) %>%
    filter(purrr::map_lgl(rDragons, ~ nrow(.) > 0)) %>%
    tidyr::unnest(rDragons) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2)

  rDataBarons <- Data %>%
    mutate(rBarons = purrr::map(rBarons, function(x) {
      jsonlite::fromJSON(toString(x)) %>% as.data.frame()
    })) %>%
    tidyr::unnest(rBarons) %>%
    dplyr::select(Address, Timestamp = V1) %>%
    mutate(Event = "Baron")

  rDataHeralds <- Data %>%
    mutate(rHeralds = purrr::map(rHeralds, function(x) {
      jsonlite::fromJSON(toString(x)) %>% as.data.frame()
    })) %>%
    tidyr::unnest(rHeralds) %>%
    dplyr::select(Address, Timestamp = V1) %>%
    mutate(Event = "Herald")

  FinalDataBlue <- bind_rows(bDataTowers, bDataInhibs, bDataBarons, bDataHeralds)
  FinalDataRed <- bind_rows(rDataTowers, rDataInhibs, rDataBarons, rDataHeralds)

  return(list(Blue = FinalDataBlue, Red = FinalDataRed))
}

#print(read_lol_kaggle_dataset("inst/extdata/LeagueofLegends.csv")$bTowers[1])
#View(read_lol_kaggle_dataset("inst/extdata/LeagueofLegends.csv"))


