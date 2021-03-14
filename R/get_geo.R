#'Carga poligonos geográficos de distritos de Argentina
#'   (\emph{Load Argentina's districts geometries})
#'@description
#'Función que descarga (\emph{geometry}) para graficar con mapas
#'
#'
#' @examples
#'
#' get_geo("TUCUMAN")
#'
#'@param geo un character con el nombre del district que se quiere descargar. Se pueden chequear el id con \code{\link{show_arg_codes}}.
#'@param level parametro opcional para descargar geometrías a nivel 'departamento' cuando se solicita mapa nacional \code{get_geo(geo = "ARGNTINA", level = "departamento")}.
#'@param simplified por defecto es TRUE y determina la descarga de una versión simplificada de las geometrias. Con FALSE descarga la versión original de INDEC
#'@export

get_geo <- function(geo = NULL,
                    level = "departamento",
                    simplified = TRUE) {

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
                                     "NEUQUEN","PBA", "RIO NEGRO", "SALTA", "SANTA CRUZ", "SANTA FE", "SANTIAGO DEL ESTERO",
                                     "SAN JUAN", "SAN LUIS", "TIERRA DEL FUEGO", "TUCUMAN"),
                          msg = "no es un geo valido. Chequearlos con show_arg_codes()")

  # ARG MAP
  if(geo == "ARGENTINA") {

    assertthat::assert_that(level %in% c("departamento", "provincia"),
                            msg = "National geography can be downloaded only at 'departamento' or 'provincia' level" )

    if(level == "departamento"){

      if(simplified == TRUE){

        url <-  "https://github.com/electorArg/PolAr_Data/raw/master/geo/localidades_simplified.geojson"

      } else {

        url <-  "https://github.com/electorArg/PolAr_Data/raw/master/geo/localidades.geojson"

      }


      check <- httr::GET(url)

      httr::stop_for_status(x = check,
                            task = "Fail to download data. Source is not available // La fuente de datos no esta disponible")


    df <-   sf::read_sf(url)

    } else {

      if(simplified == TRUE){

      url <- "https://github.com/electorArg/PolAr_Data/raw/master/geo/provincias_simplified.geojson"


      } else {


        url <- "https://github.com/electorArg/PolAr_Data/raw/master/geo/provincias.geojson"

      }

      ## FAIL SAFELEY

      check <- httr::GET(url)

      httr::stop_for_status(x = check,
                            task = "Fail to download data. Source is not available // La fuente de datos no esta disponible")

      df <-   sf::read_sf(url)

      # PROVINCES MAPS

      }

    } else {

      assertthat::assert_that(level  == "departamento",
                              msg = "Provincial geography can be downloaded only at 'departamento' level" )

    if(simplified == TRUE){

      url <-  "https://github.com/electorArg/PolAr_Data/raw/master/geo/localidades_simplified.geojson"


      check <- httr::GET(url)

      httr::stop_for_status(x = check,
                            task = "Fail to download geo data. Source is not available // La fuente de datos geograficos no esta disponible")


      temp <- geoAr::show_arg_codes(viewer = FALSE) %>%
        dplyr::filter(id == geo) %>%
        dplyr::pull(codprov_censo)

      df <-   sf::read_sf(url) %>%
        dplyr::filter(codprov_censo %in% temp)



    } else {

      url <-  "https://github.com/electorArg/PolAr_Data/raw/master/geo/localidades.geojson"


    check <- httr::GET(url)

    httr::stop_for_status(x = check,
                          task = "Fail to download geo data. Source is not available // La fuente de datos geograficos no esta disponible")


    temp <- geoAr::show_arg_codes(viewer = FALSE) %>%
      dplyr::filter(id == geo) %>%
      dplyr::pull(codprov_censo)

    df <- sf::read_sf(url) %>%
      dplyr::filter(codprov_censo %in% temp)

   }

  }

  df

  }
