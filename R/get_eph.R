
#' Carga poligonos de los Aglomerados Urbanos correspondientes a la Encuesta Permanente de Hogares (INDEC)
#'
#' @param geo un character con el nombre del distrito que se quiere descargar.
#'Se puede chequear el id con \code{\link{show_arg_codes}}.
#'
#' @param simplified por defecto es TRUE y determina la descarga de una versión simplificada de las geometrias.
#'Con FALSE descarga la versión original de INDEC
#'
#' @param level por defecto devuelve a nivel `envolvente` pero puede descargarse a nivel `radios` y `entidades`
#'
#' @param centroid por defecto devuelve poligonos como geometry pero pueden descargarse puntos (centroides correspondientes al level especificado)
#' @return tibble con capa geografica correspondiente a alguna versión de geometrías utilizadas en la Encuesta Permanente de Hogares (EPH)
#' @export
#'
#' @examples
#' get_eph(geo = "TUCUMAN")
#'
get_eph <- function(geo = "ARGENTINA",
                    simplified = TRUE,
                    centroid = FALSE,
                    level = "envolventes"){

  ## Check for internet conection
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "Internet access was not detected. Please check your connection //
No se detecto acceso a internet. Por favor chequear la conexion.")


  # Check parameters

  assertthat::assert_that(!is.null(geo),
                          msg = "debe ingresar un geo valido. Chequear opciones con 'show_arg_codes()")

  assertthat::assert_that(is.character(geo),
                          msg = "geo debe ser del tipo 'character'. Chequear opciones con 'show_arg_codes()")

  assertthat::assert_that(level %in% c("entidades", "radios", "envolventes"),
                          msg = "level debe ser una de las siguientes opciones: 'entidades', 'radios' o 'envolventes'")

  assertthat::assert_that(geo %in% c("ARGENTINA", "CABA", "CATAMARCA", "CHACO", "CHUBUT", "CORDOBA", "CORRIENTES",
                                     "ENTRE RIOS", "FORMOSA", "JUJUY", "LA PAMPA", "LA RIOJA", "MENDOZA", "MISIONES",
                                     "NEUQUEN","BUENOS AIRES", "RIO NEGRO", "SALTA", "SANTA CRUZ", "SANTA FE", "SANTIAGO DEL ESTERO",
                                     "SAN JUAN", "SAN LUIS", "TIERRA DEL FUEGO", "TUCUMAN"),
                          msg = "no es un geo valido. Chequearlos con show_arg_codes()")


          url <- if(simplified == TRUE){

            if(level == "entidades"){

              "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data/aglos_simplified.geojson"

            }else if(level == "radios"){

              "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data/radios_eph_simplified.geojson"

            }else if(level == "envolventes"){

              "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data/aglos_envolventes_simplified.geojson"
            }



          } else if (simplified == FALSE){

            if(level == "entidades"){

            "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data_raw/entidades_eph.geojson"

            }else if(level == "radios"){

            "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data_raw/radios_eph.geojson"

            }else if(level == "envolventes"){

            "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data_raw/aglos_envolventes.geojson"

            }
          }

  # Set default value for try()

  default <- NULL

  df <- base::suppressWarnings(base::try(default <- sf::read_sf(url), silent = TRUE))

  if(is.null(default)){

    df <- base::message("Fail to download data. Source is not available // La fuente de datos no esta disponible")

  } else {

    df <- df

  }

  ############## ARG MAP###############


  if(geo == "ARGENTINA") {

      df


  } else {

    ##############  PROVINCES MAPS ######################


    temp <- geoAr::show_arg_codes(viewer = FALSE) %>%
      dplyr::filter(id == geo) %>%
      dplyr::pull(codprov_censo)


    df <-   df %>%
      dplyr::filter(codprov %in% temp)

  }


  if(centroid == FALSE){


    df

  }else{


    base::suppressWarnings(sf::st_centroid(df))


  }




  }

