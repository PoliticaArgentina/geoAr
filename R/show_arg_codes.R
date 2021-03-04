#' Diccionario de códigos identificadores de distritos
#' (\emph{geo ID's dictoniary})
#'
#'@description
#' Función que devuelve un \emph{data.frame} con códigos y equivalencias de identificación de unidades geográficas
#'
#'@param viewer Por default es \code{TRUE} y  muestra una tabla formateada en el \emph{Viewer} de \emph{RStudio}. Cuando \code{FALSE} imprime en consola.
#'
#'@param nivel  Un character que permite elegir opción para ver diccionario a nivel de \code{"provincias"} o de \code{"departamentos"}.
#'
#'
#' @examples
#'
#' show_arg_codes(viewer = FALSE)
#'
#' @export



show_arg_codes <- function(viewer = FALSE,
                           nivel  =  "provincias"){

  # Check parameters


  assertthat::assert_that(is.character(nivel),
                          msg = "nivel debe ser del tipo 'character'. Opciones 'provincias' (default) o 'departamentos'")

  assertthat::assert_that(nivel %in% c("provincias", "departamentos"),
                          msg = " 'provincias' o 'departamentos' son las unicas opciones validas")



  seleccion <-  if(nivel == "provincias"){

    geo_metadata %>%
      dplyr::select(id = name_prov, codprov, codprov_censo, codprov_iso, name_iso) %>%
      dplyr::distinct() %>%
      dplyr::add_row(.before = 1, id = "ARGENTINA", codprov = " ", codprov_censo = " ",
                     codprov_iso = "AR", name_iso = "Argentina")%>%
      dplyr::add_row(.after = 25, id = "AGLOMERADOS", codprov = " ", codprov_censo = " ",
                     codprov_iso = "AR", name_iso = "Aglomerados Urbanos - EPH (INDEC)")

  } else {

    geo_metadata %>%
      dplyr::select(id = name_prov, codprov, coddepto,
                    codprov_censo, coddepto_censo, nomdepto_censo) %>%
      dplyr::add_row(.before = 1, id = "ARGENTINA",
                     codprov = " ",
                     coddepto = " ",
                     codprov_censo = " ",
                     coddepto_censo = " ",
                     nomdepto_censo = " ")



  }



  if(viewer == FALSE){


    seleccion

  } else {


    selection <- dplyr::case_when(nivel == "provincias" ~ "Provincial",
                                  nivel == "departamentos" ~ "Departamental")

    seleccion %>%
      gt::gt()%>%
      gt::cols_label(id = gt::md(("ID grilla"))) %>%
      gt::tab_spanner(columns = dplyr::matches("censo"), label = "INDEC") %>%
      gt::tab_spanner(columns = dplyr::matches("iso"), label = "ISO") %>%
      gt::tab_spanner(columns = dplyr::matches("prov$|depto$"), label = "INDRA") %>%
      gt::tab_header(
        title = gt::md("**Diccionario de equivalencias para identificadores geo**"),
        subtitle = gt::md(glue::glue(("**Argentina - Nivel {selection}**")))) %>%
      gt::tab_source_note(
        source_note = gt::md("**Fuente:** geofaceteAR  - *https://electorarg.github.io/geofaceteAR*"))



  }



}
