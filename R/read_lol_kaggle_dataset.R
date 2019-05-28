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

  bDataDragons <- Data %>%
    mutate(bDragons = purrr::map(bDragons, function(x) {
      aux <- stringr::str_replace_all(x, pattern = "None", replacement = "null")
      asd <- jsonlite::fromJSON(stringr::str_replace_all(aux, "'", '"')) %>%
        as.data.frame() %>%
        drop_na()
    })) %>%
    filter(purrr::map_lgl(bDragons, ~ nrow(.) > 0)) %>%
    tidyr::unnest(bDragons) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2) %>%
    mutate(Timestamp = as.numeric(Timestamp))

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
    tidyr::unite("Event", sep = "_", Lane, Tower_Type)

  bDataInhibs <- Data %>%
    mutate(bInhibs = purrr::map(bInhibs, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(bInhibs) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2) %>%
    mutate(Timestamp = as.numeric(Timestamp)) %>%
    mutate(Event = purrr::map_chr(Event, function(x) {
      paste(x, "Inhib", sep = "_")
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

  #RED TEAM DATA

  rDataTowers <- Data %>%
    mutate(rTowers = purrr::map(rTowers, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(rTowers) %>%
    dplyr::select(Address, Timestamp = V1 , Lane = V2, Tower_Type = V3) %>%
    mutate(Timestamp = as.numeric(Timestamp)) %>%
    tidyr::unite("Event", sep = "_", Lane, Tower_Type)


  rDataInhibs <- Data %>%
    mutate(rInhibs = purrr::map(rInhibs, function(x) {
      jsonlite::fromJSON(stringr::str_replace_all(x, "'", '"')) %>% as.data.frame()
    })) %>%
    tidyr::unnest(rInhibs) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2) %>%
    mutate(Timestamp = as.numeric(Timestamp)) %>%
    mutate(Event = purrr::map_chr(Event, function(x) {
      paste(x, "Inhib", sep = "_")
    }))

  rDataDragons <- Data %>%
    mutate(rDragons = purrr::map(rDragons, function(x) {
      aux <- stringr::str_replace_all(x, pattern = "None", replacement = "null")
      asd <- jsonlite::fromJSON(stringr::str_replace_all(aux, "'", '"')) %>%
        as.data.frame() %>%
        drop_na()
    })) %>%
    filter(purrr::map_lgl(rDragons, ~ nrow(.) > 0)) %>%
    tidyr::unnest(rDragons) %>%
    dplyr::select(Address, Timestamp = V1, Event = V2) %>%
    mutate(Timestamp = as.numeric(Timestamp))

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

  FinalDataBlue <- bind_rows(bDataTowers, bDataInhibs, bDataBarons, bDataHeralds, bDataDragons)
  FinalDataRed <- bind_rows(rDataTowers, rDataInhibs, rDataBarons, rDataHeralds, rDataDragons)

  MIN_EVENTS = 5
  foo = FinalDataBlue %>% count(Address)
  bar = FinalDataRed %>% count(Address)
  valid_addresses = inner_join(foo, bar, by = "Address") %>%
    dplyr::filter(n.x >= MIN_EVENTS & n.y >= MIN_EVENTS) %>%
    pull(Address)
  FinalDataBlue = FinalDataBlue %>% filter(Address %in% valid_addresses)
  FinalDataRed = FinalDataRed %>% filter(Address %in% valid_addresses)
  Data <- Data %>% filter(Address %in% valid_addresses)

  goldiff <- Data %>%
    mutate(golddiff = purrr::map(golddiff, function(x) {
      jsonlite::fromJSON(toString(x)) %>% as.data.frame()
    })) %>%
    tidyr::unnest(golddiff) %>%
    dplyr::select(Address, goldBdiff = ".") %>%
    group_by(Address) %>%
    mutate(Timestamp = row_number())

  result <- Data %>% dplyr::select(Address, bResult)
  win <- result %>% filter(bResult == 1)
  lose <- result %>% filter(bResult == 0)

  FDBwin = FinalDataBlue %>% filter(Address %in% unique(win$Address))
  FDRwin = FinalDataRed %>% filter(Address %in% unique(win$Address))
  GDwin = goldiff %>% filter(Address %in% unique(win$Address))

  FDBlose = FinalDataBlue %>% filter(Address %in% lose$Address)
  FDRlose = FinalDataRed %>% filter(Address %in% lose$Address)
  GDlose = goldiff %>% filter(Address %in% lose$Address)



  return(list(BEW = FDBwin, REW = FDRwin, GDW = GDwin, BEL = FDBlose, REL = FDRlose, GDL = GDlose))
}

#res <- read_lol_kaggle_dataset("inst/extdata/LeagueofLegends.csv")
#blue <- res$Blue
#blueai <- blue %>% mutate(Address = as.integer(Address))
#blueg <- blueai %>% group_by(Address)
#bluego <- blueg[order(blueg$Address, blueg$Timestamp), ]
#bluegoint <- bluego %>% mutate(Timestamp = as.integer(Timestamp))
#tseblue <- seqecreate(id = bluegoint$Address, timestamp = bluegoint$Timestamp, event = bluegoint$Event)
#seqpcplot(tseblue)
#usando TraMineRextras:
# se puede usar TSE_to_STS con bluegoint en principio, pero salta un error (list) object cannot be coerced to type 'double'
