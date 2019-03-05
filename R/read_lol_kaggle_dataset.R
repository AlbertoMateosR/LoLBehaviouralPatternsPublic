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

  bDataTowers <- Data %>%
    mutate(bTowers = purrr::map(bTowers, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(bTowers) %>%
    dplyr::select(Address, Timestamp = V1 , Lane = V2, Tower_Type = V3) %>%
    tidyr::unite("Event", sep = "/", Lane, Tower_Type)

  bDataInhibs <- Data %>%
    mutate(bInhibs = purrr::map(bInhibs, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(bInhibs) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2) %>%
    mutate(Event = purrr::map(Event, function(x) {
      paste(x, "Inhib", sep = "/")
    }))

  #bDataDragons <- Data %>%
   # mutate(bDragons = purrr::map(bDragons, function(x) {
    #  elems <- c("'|None")
     # jsonlite::fromJSON(stringr::str_replace_all(x, elems, '"')) %>% as.data.frame()
    #})) %>%
    #tidyr::unnest(bDragons) %>%
    #dplyr::select(Address, Timestamp = V1)

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
    tidyr::unite("Event", sep = "/", Lane, Tower_Type)

  rDataInhibs <- Data %>%
    mutate(rInhibs = purrr::map(rInhibs, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(rInhibs) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2) %>%
    mutate(Event = purrr::map(Event, function(x) {
      paste(x, "Inhib", sep = "/")
    }))

  #rDataDragons <- Data %>%
  # mutate(rDragons = purrr::map(rDragons, function(x) {
  #  elems <- c("'|None")
  # jsonlite::fromJSON(stringr::str_replace_all(x, elems, '"')) %>% as.data.frame()
  #})) %>%
  #tidyr::unnest(rDragons) %>%
  #dplyr::select(Address, Timestamp = V1)

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

  FinalDataBlue <- dplyr::bind_rows(bDataTowers, bDataInhibs, bDataBarons, bDataHeralds)
  FinalDataRed <- dplyr::bind_rows(rDataTowers, rDataInhibs, rDataBarons, rDataHeralds)

  return(FinalDataBlue)
}

#print(read_lol_kaggle_dataset("inst/extdata/LeagueofLegends.csv")$bTowers[1])
View(read_lol_kaggle_dataset("inst/extdata/LeagueofLegends.csv"))


