#' Title
#'
#' @param input_df
#'
#' @return
#' @export
#'
#' @examples
read_lol_kaggle_dataset <- function(input_df) {
  Data <- read.csv(file = input_df, header = TRUE, sep = ",")

  for (row in 1:nrow(Data)) {
    matchId <- Data[row, "Address"]
    #Ahora habria que recorrer las columnas de equipo por evento, y añadir a nuestro dataframe una fila por cada elemento
    #de dicho recorrido, con la division de columnas adecuadas, pero no estoy seguro del acceso
  }

  return(Data)
}

#print(read_lol_kaggle_dataset("inst/extdata/LeagueofLegends.csv")$bTowers[1])
print(nrow(read_lol_kaggle_dataset("inst/extdata/LeagueofLegends.csv")))


