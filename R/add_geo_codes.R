#' Agrega columnas con id geográficos asignados a poligonos de provincias o departamentos de Argentina
#'   (\emph{Augment Argentina's districts polygons id})
#'@description
#'
#'Función que permite agregar columnas con ids geográficos para utilización de mapas de Argentina y de departamentos para los 24 distritos
#'  sub nacionales para una más sencilla la vinculación con bases de datos de diversas fuentes.
#'
#'@return Los valores por defecto en \code{\link{get_geo}} son \code{codprov_censo} para provincia y \code{coddepto_censo} para departamentos, respectivamente.
#' Estos corresponden a la codificación de INDEC.
#' Con el agregado usando \code{add_geo_codes} se puede optar por las nomenclaturas de \code{'indra'} - correspondiente a los escrutinios provisorios de elecciones nacionales,
#'  con las variantes \code{codprov} y \code{coddepto}, o la de \code{'iso'} con \code{\*_iso} , estas últimas hasta el nivel
#'  provincial.
#'
#'@details Respecto el origen de los datos se puede consultar la documentación de
#' \href{https://www.iso.org/obp/ui/#iso:code:3166:AR}{\code{ISO 3166-2} - International Organization for Standardization} y
#' del \href{https://www.iso.org/obp/ui/#iso:code:3166:AR}{\emph{INDEC}}.
#'
#'@details \code{codprov} y \code{coddepto} son las codificaciones de las bases de datos de \emph{INDRA}, empresa encargada por
#' muchos años de la tarea del escrutinio provisorio y utilizados en \href{https://electorarg.github.io/polAr/}{polAr}.
#
#'@param data data.frame obtenido con \code{\link{get_geo}}. Los mapas disponibles
#' se pueden chequear con \code{\link{show_arg_codes}}.
#'
#'
#'
#' @examples
#'
#' get_geo("TUCUMAN")
#'
#'
#' get_geo("TUCUMAN") %>%
#'    add_geo_codes()
#'
#'
#'@export

add_geo_codes <- function(data){


  # Check parameters

  assertthat::assert_that(is.data.frame(data),
                          msg = glue::glue("{data} debe ser un 'data.frame' obtenido con la funcion get_geo()"))

  assertthat::assert_that('geometry' %in% names(data),
                          msg = glue::glue("{data} debe ser un 'data.frame' obtenido con la funcion get_geo()"))




  level <-  base::names(data)


  if("coddepto_censo" %in% level){


  data %>%
    dplyr::left_join(geo_metadata, by = c("codprov_censo", "coddepto_censo")) %>%
    dplyr::relocate(geometry, .after = dplyr::last_col())



  } else {


    data %>%
      dplyr::left_join(geo_metadata %>%
                         dplyr::select(dplyr::contains("codprov"), name_iso) %>%
                         dplyr::distinct(), by = c("codprov_censo")) %>%
      dplyr::relocate(geometry, .after = dplyr::last_col())




  }




}

