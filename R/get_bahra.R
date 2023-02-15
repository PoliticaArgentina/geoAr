#' Descarga 'base total' de Asentamientos Humanos de la República Argentina (BAHRA)
#'
#' @param geo un character con el nombre del distrito que se quiere descargar (por defecto toda ARGENTINA)
#'Se puede chequear el id con \code{\link{show_arg_codes}}.
#'
#' @export
#'
#' @examples get_bahra()

get_bahra <- function(geo = "ARGENTINA"){

  ## Check for internet conection
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "Internet access was not detected. Please check your connection //
No se detecto acceso a internet. Por favor chequear la conexion.")

  assertthat::assert_that(geo %in% c("ARGENTINA", "CABA", "CATAMARCA", "CHACO", "CHUBUT", "CORDOBA", "CORRIENTES",
                                     "ENTRE RIOS", "FORMOSA", "JUJUY", "LA PAMPA", "LA RIOJA", "MENDOZA", "MISIONES",
                                     "NEUQUEN","BUENOS AIRES", "RIO NEGRO", "SALTA", "SANTA CRUZ", "SANTA FE", "SANTIAGO DEL ESTERO",
                                     "SAN JUAN", "SAN LUIS", "TIERRA DEL FUEGO", "TUCUMAN"),
                          msg = "no es un geo valido. Chequearlos con show_arg_codes()")


  # Check parameters


  url <- "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data_raw/base_total_bahra.geojson"



  # Set default value for try()

  default <- NULL

  df <- base::suppressWarnings(base::try(default <- sf::read_sf(url), silent = TRUE))

  if(is.null(default)){

    df <- base::message("Fail to download data. Source is not available // La fuente de datos no esta disponible")

  } else {

    df <- df %>%
      dplyr::mutate(cod_depto = stringr::str_sub(cod_depto, start = 3, end = 5)) %>%
      dplyr::rename(codprov_censo = "cod_pcia",
                    coddepto_censo = "cod_depto")

  }

    if(geo == "ARGENTINA") {

    df


  } else {

    ##############  PROVINCES MAPS ######################


    temp <- geoAr::show_arg_codes(viewer = FALSE) %>%
      dplyr::filter(id == geo) %>%
      dplyr::pull(codprov_censo)


    df <-   df %>%
      dplyr::filter(codprov_censo %in% temp)

  }

  # download message one per session hack
  if(base::getOption('descarga-bahra', TRUE)){


    message(glue::glue("Los datos fueron obtenidos del proyecto 'Base de Asentamientos Humanos de la Republica Argentina (BAHRA)'. La documentacion se encuetra disponible en http://www.bahra.gob.ar/"))

    options('ddescarga-bahra' = FALSE)




  return(df)



  }

}

