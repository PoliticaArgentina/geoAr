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
#'@param geo un character con el nombre del district que se quiere descargar.
#'Se pueden chequear el id con \code{\link{show_arg_codes}}.
#'@param level parametro opcional para descargar geometrías a nivel 'departamento' o 'censal'
#'cuando se solicita mapa nacional \code{get_geo(geo = "ARGNTINA", level = "departamento")}.
#'@param simplified por defecto es TRUE y determina la descarga de una versión simplificada de las geometrias.
#'Con FALSE descarga la versión original de INDEC
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
                                     "NEUQUEN","BUENOS AIRES", "RIO NEGRO", "SALTA", "SANTA CRUZ", "SANTA FE", "SANTIAGO DEL ESTERO",
                                     "SAN JUAN", "SAN LUIS", "TIERRA DEL FUEGO", "TUCUMAN"),
                          msg = "no es un geo valido. Chequearlos con show_arg_codes()")



  url <- if(level == "departamento" & simplified == TRUE){

    "https://github.com/politicaargentina/data_warehouse/raw/master/geoAr/data/localidades_simplified.geojson"

  } else if (level == "departamento" & simplified == FALSE){

    "https://github.com/politicaargentina/data_warehouse/raw/master/geoAr/data_raw/localidades.geojson"

  }  else if (level == "censal" & simplified == TRUE){

      "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data/radios_simplified.geojson"

  } else if (geo == "ARGENTINA" & level == "provincia" & simplified == TRUE){

    "https://github.com/politicaargentina/data_warehouse/raw/master/geoAr/data/provincias_simplified.geojson"

  } else if (geo == "ARGENTINA" & level == "departamento" & simplified == FALSE){

    "https://github.com/politicaargentina/data_warehouse/raw/master/geoAr/data_raw/provincias.geojson"

  } else if (level == "censal" & simplified == FALSE){

    link <- "https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data_raw/raw_radios_censales.zip"

    # Download file from URL

    # Create temfiles

    temp <- base::tempfile()

    temp2 <- base::tempfile()

    # Download .zip

    utils::download.file(url = link,  temp, quiet = TRUE)


    # Unzip files

    utils::unzip(zipfile = temp, exdir = temp2)

    # Select geojson file path
    (url <- base::list.files(temp2, pattern = "geojson$", full.names=TRUE))

  }






  # Set default value for try()

  default <- NULL

  df <- base::suppressWarnings(base::try(default <- sf::read_sf(url), silent = TRUE))

  if(is.null(default)){

    df <- base::message("Fail to download data. Source is not available // La fuente de datos no esta disponible")

  } else {

    df <- df

  }


  if("link" %in% names(df)){ # CENSUS TRACT DATA

    df <- df %>%
      dplyr::mutate(codprov_censo  = stringr::str_sub(string = link,
                                                      start = 1, end = 2),
                    coddepto_censo = stringr::str_sub(string = link,
                                                      start = 3, end = 5),
                    fraccion_censal = stringr::str_sub(string = link,
                                                       start = 6, end = 7),
                    radio_censal = stringr::str_sub(string = link,
                                                    start = 8, end = 9))



      }else{

        df <- df

        }

  ############## ARG MAP###############


  if(geo == "ARGENTINA") {

    assertthat::assert_that(level %in% c("departamento", "provincia", "censal"),
                            msg = "National geography can be downloaded only at 'censal', 'departamento' or 'provincia' level" )

    df


  } else {

    ##############  PROVINCES MAPS ######################

    assertthat::assert_that(level  %in% c("departamento", "censal"),
                            msg = "Provincial geography can be downloaded only at 'departamento' or 'censal' level" )


    temp <- geoAr::show_arg_codes(viewer = FALSE) %>%
      dplyr::filter(id == geo) %>%
      dplyr::pull(codprov_censo)


    df <-   df %>%
      dplyr::filter(codprov_censo %in% temp)

  }

  df

}
