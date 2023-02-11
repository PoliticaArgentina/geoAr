
#' Carga poligonos de los Aglomerados Urbanos correspondientes a la Encuesta Permanente de Hogares (INDEC)
#'
#' @param geo un character con el nombre del district que se quiere descargar.
#'Se pueden chequear el id con \code{\link{show_arg_codes}}.
#'
#' @param simplified por defecto es TRUE y determina la descarga de una versión simplificada de las geometrias.
#'Con FALSE descarga la versión original de INDEC
#' @export
#'
#' @examples
#' get_eph(geo = "TUCUMAN")
#'
get_eph <- function(geo = "ARGENTINA",
                    simplified = TRUE){

  ## Check for internet conection
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "Internet access was not detected. Please check your connection //
No se detecto acceso a internet. Por favor chequear la conexion.")


  # Check parameters

  assertthat::assert_that(!is.null(geo),
                          msg = "debe ingresar un geo valido. Chequear opciones con 'show_arg_codes()")

  assertthat::assert_that(is.character(geo),
                          msg = "geo debe ser del tipo 'character'. Chequear opciones con 'show_arg_codes()")

  assertthat::assert_that(geo %in% c("ARGENTINA", "CABA", "CATAMARCA", "CHACO", "CHUBUT", "CORDOBA", "CORRIENTES",
                                     "ENTRE RIOS", "FORMOSA", "JUJUY", "LA PAMPA", "LA RIOJA", "MENDOZA", "MISIONES",
                                     "NEUQUEN","BUENOS AIRES", "RIO NEGRO", "SALTA", "SANTA CRUZ", "SANTA FE", "SANTIAGO DEL ESTERO",
                                     "SAN JUAN", "SAN LUIS", "TIERRA DEL FUEGO", "TUCUMAN"),
                          msg = "no es un geo valido. Chequearlos con show_arg_codes()")


          url <- if(simplified == TRUE){

            "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data/aglos_simplified.geojson"

          } else if (simplified == FALSE){

            "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data_raw/entidades_eph.geojson"

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

  df






  }

