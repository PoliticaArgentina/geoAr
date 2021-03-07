#'Recodifica id de grillas asignados a  provincias o departamentos de Argentina
#'   (\emph{Recode Argentina's districs id in grids})
#'@description
#'
#'Función que permite re codificar etiquetas para utilización de grillas de Argentina y de departamentos para los 24 distritos
#'  sub nacionales. Ello permite hacer mas sencilla la vinculación con bases de datos de diversas fuentes.
#'
#'@return Los valores por defecto en \code{\link{get_grid}} son \code{codprov} para provincia y \code{coddepto} para departamentos, respectivamente.
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
#'@param type la variante del código que se quiere definir para la grilla. Las opciones son \code{'indra'}, \code{'indec'} o \code{'iso'}.
#'
#'@param data data.frame obtenido con \code{\link{get_grid}}. Las grillas disponibles
#' se pueden chequear con \code{\link{show_arg_codes}}.
#'
#'
#'
#' @examples
#'
#' get_grid("ARGENTINA")
#'
#'
#' get_grid("ARGENTINA") %>%
#'    recode_grid(type = "iso")
#'
#'
#'@export


recode_grid<- function(data,
                       type = NULL){

  # Check parameters

  assertthat::assert_that(is.data.frame(data),
                          msg = glue::glue("{data} debe ser un 'data.frame' obtenido con la funcion get_grid()"))

  assertthat::assert_that(dim(data)[2] == 5,
                          msg = glue::glue("{data} debe ser un 'data.frame' obtenido con la funcion get_grid()"))

  assertthat::assert_that(is.character(type),
                          msg = "type debe ser del tipo 'character'")




  if("CABA" %in% data$name){

    assertthat::assert_that(is.character(type),
                            msg = "type debe ser del tipo 'character'")


    assertthat::assert_that(length(type) == 1,
                            msg = glue::glue("{type} no es una opcion valida para recodificar grillas de provincias.
                                             Debe elegir una opcion entre 'indra', 'indec' o 'iso'"))

    assertthat::assert_that(type %in% c("indra", "indec", "iso"),
                            msg = glue::glue("{type} no es una opcion valida ('indra', 'indec', 'iso')"))


    full_codes <- geo_metadata %>%
      dplyr::select(codprov, codprov_censo, codprov_iso) %>%
      dplyr::distinct()

    if(type == "indec"){

      data %>%
        dplyr::left_join(full_codes, by = c("code" = "codprov")) %>%
        dplyr::select(2, 3, 5, code = codprov_censo)


    } else if(type == "iso"){

      data %>%
        dplyr::left_join(full_codes, by = c("code" = "codprov")) %>%
        dplyr::select(2, 3, 5, code = codprov_iso)


    } else{

      data

    }
  }else{

    assertthat::assert_that(is.character(type),
                            msg = "type debe ser del tipo 'character'")


    assertthat::assert_that(length(type) == 1,
                            msg = glue::glue("{type} no es una opcion valida para recodificar grillas de departamentos.
                                                    Debe elegir una opcion entre 'indra'o 'indec'"))

    assertthat::assert_that(type %in% c("indra", "indec"),
                            msg = glue::glue("{type} no es una opcion valida para recodificar grillas de departamentos.
                                                    Debe elegir una opcion entre 'indra'o 'indec'"))


    #### hack para filtrar grilla de deprtamento que se quiere recodear

    # Me traigo todos los id de depto de tdas las provincias y genero un id unico
    full_codes <- geo_metadata %>%
      dplyr::select(coddepto, nomdepto_censo,coddepto_censo, name_prov) %>%
      dplyr::mutate(nomdepto_censo = stringr::str_to_upper(nomdepto_censo),
                    id = paste0(coddepto,
                                stringr::str_remove_all(string = nomdepto_censo,
                                                        pattern = " "),
                                name_prov))


    # agrego a la base de grillas el mismo codigo de id de la metadata
    grillas_depto_id <- grillas_geofacet  %>%
      dplyr::bind_rows(.id = "name_prov") %>%
      tidyr::as_tibble() %>%
      dplyr::slice(25:dim(.)[1]) %>%
      dplyr::mutate(code = stringr::str_pad(code, 3, "left", 0),
                    id = paste0(code,
                                stringr::str_remove_all(string = name,
                                                        pattern = " "),
                                name_prov))

    # Creo filtro para seleccionar grilla correcta
    filtro_provincia <- data %>%
      dplyr::left_join(grillas_depto_id) %>%
      dplyr::left_join(full_codes)

    filtro_id <- filtro_provincia %>%
      dplyr::pull(id)

    # filtro la grilla de interes
    data <- grillas_depto_id %>%
      dplyr::filter(id %in% filtro_id) %>%
      dplyr::select(name, code, row, col)

    #######################################################################

    if(type == "indec"){

      data %>%
        dplyr::mutate(code = dplyr::case_when(
          code == filtro_provincia$coddepto ~ filtro_provincia$coddepto_censo
        ))


    } else{

      data

    }


  }
}
