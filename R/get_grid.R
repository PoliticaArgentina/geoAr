#'Carga grillas de districts de Argentina
#'   (\emph{Load grids of districts of Argentina})
#'@description
#'Función que descarga grillas (\emph{facet}) para acomodarlas como si fueran mapas de provincias y deparamentos
#' de Argentina.
#'
#'
#' @examples
#'
#' get_grid("TUCUMAN")
#'
#'@param district un character con el nombre del district que se quiere descargar. Disponibles grillas para Argentina
#' y para las 24 provincias. Se pueden chequear los parametros con \code{\link{show_arg_codes}}.
#'@export


get_grid <- function(district = NULL) {


  # Check parameters

  assertthat::assert_that(!is.null(district),
                          msg = "debe ingresar un district valido. Chequear opciones con 'show_arg_codes()")

  assertthat::assert_that(is.character(district),
                          msg = "district debe ser del tipo 'character'. Chequear opciones con 'show_arg_codes()")

  assertthat::assert_that(district %in% c("ARGENTINA", "CABA", "CATAMARCA", "CHACO", "CHUBUT", "CORDOBA", "CORRIENTES",
                                          "ENTRE RIOS", "FORMOSA", "JUJUY", "LA PAMPA", "LA RIOJA", "MENDOZA", "MISIONES",
                                          "NEUQUEN","PBA", "RIO NEGRO", "SALTA", "SANTA CRUZ", "SANTA FE", "SANTIAGO DEL ESTERO",
                                          "SAN JUAN", "SAN LUIS", "TIERRA DEL FUEGO", "TUCUMAN", "AGLOMERADOS"),
                          msg = "no es un district valido. Chequearlos con show_arg_codes()")


  # Cargo geo-grids


  grillas <- grillas_geofacet %>%
    dplyr::bind_rows(.id = "name_provincia") %>%
    dplyr::group_by(name_provincia) %>%
    dplyr::select(row, col, code, name, name_provincia) %>%
    tidyr::nest()


  grillas %>%
    dplyr::filter(name_provincia == district) %>%
    tidyr::unnest(cols = c(data)) %>% as.data.frame()

}
