#'Recodifica id asignados a poligonos de provincias o departamentos de Argentina
#'   (\emph{Recode Argentina's districs polygons id})
#'@description
#'
#'Función que permite re codificar etiquetas para utilización de mapas de Argentina y de departamentos para los 24 distritos
#'  sub nacionales. Ello permite hacer mas sencilla la vinculación con bases de datos de diversas fuentes.
#'
#'@return Los valores por defecto en \code{\link{get_geo}} son \code{codprov} para provincia y \code{coddepto} para departamentos, respectivamente.
#' Estos corresponden a la codificación de los escrutinios provisorios de elecciones nacionales y se etiquetaron como \code{'indra'} .
#' Se puede optar por la nomenclatura de \code{'indec'}, con la familia \code{\*_censo}, para ambos niveles, o la de \code{'iso'} con \code{\*_iso} ,
#' para el nivel provincial.
#'
#'@details Respecto el origen de los datos se puede consultar la documentación de
#' \href{https://www.iso.org/obp/ui/#iso:code:3166:AR}{\code{ISO 3166-2} - International Organization for Standardization} y
#' del \href{https://www.iso.org/obp/ui/#iso:code:3166:AR}{\emph{INDEC}}.
#'
#'@details \code{codprov} y \code{coddepto} son las codificaciones de las bases de datos de \emph{INDRA}, empresa encargada por
#' muchos años de la tarea del escrutinio provisorio y utilizados en \href{https://electorarg.github.io/polAr/}{polAr}.
#'
#'@param type la variante del código que se quiere definir para los mapas. Las opciones son \code{'indra'}, \code{'indec'} o \code{'iso'}.
#'
#'@param data data.frame obtenido con \code{\link{get_geo}}. Los mapas disponibles
#' se pueden chequear con \code{\link{show_arg_codes}}.
#'
#'
#'
#' @examples
#'
#' get_geo("ARGENTINA")
#'
#'
#' get_geo("ARGENTINA") %>%
#'    recode_polygon(type = "iso")
#'
#'
#'@export


recode_polygon<- function(data,
                       type = NULL){

  # Check parameters

  assertthat::assert_that(is.data.frame(data),
                          msg = glue::glue("{data} debe ser un 'data.frame' obtenido con la funcion get_geo()"))

  assertthat::assert_that('geometry' %in% names(data),
                          msg = glue::glue("{data} debe ser un 'data.frame' obtenido con la funcion get_geo()"))

  assertthat::assert_that(is.character(type),
                          msg = "type debe ser del tipo 'character'")


  assertthat::assert_that(length(type) == 1,
                            msg = glue::glue("{type} no es una opcion valida para recodificar grillas de provincias.
                                             Debe elegir una opcion entre 'indra', 'indec' o 'iso'"))

  assertthat::assert_that(type %in% c("indra", "indec", "iso"),
                            msg = glue::glue("{type} no es una opcion valida ('indra', 'indec', 'iso')"))



    if(type == "indec"){

      data %>%
        dplyr::left_join(geo_metadata, by = c("codprov", "coddepto")) %>%
        dplyr::select(provincia, dplyr::contains("censo"), geometry)


    } else if(type == "iso"){

      data %>%
        dplyr::left_join(geo_metadata, by = c("codprov", "coddepto"))%>%
        dplyr::select(provincia, dplyr::contains("iso"), geometry)


    } else{

      data %>%
        dplyr::left_join(geo_metadata, by = c("codprov", "coddepto")) %>%
        dplyr::select(provincia, codprov, coddepto, nomdepto = nomdepto_censo, geometry)

    }
}
