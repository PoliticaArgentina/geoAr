#' Geometrías de CENSOS
#' (\emph{geo ID's dictoniary})
#'
#'@description
#' Función que devuelve un \emph{data.frame} con listado de las geometrías de los CENSOS disponible
#' @examples
#' show_available_censos()
#' @return tibble con información auxiliar para descarga de CENSOS históricos con  \code{\link{get_censo}}
#'
#' @export



show_available_censos <- function(){

  print(censos)



}
