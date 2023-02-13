
#' Carga poligonos de Censos de Población Históricos de Argentina
#'
#' @param censo un character con el id del año del censo del district que se quiere descargar.
#'Se pueden chequear el id con \code{\link{show_available_censos}}.
#'
#' @param simplified por defecto es TRUE y determina la descarga de una versión simplificada de las geometrias.
#'Con FALSE descarga la versión original de INDEC
#' @export
#'
#' @examples
#' get_censo(censo = "1947", simplified = TRUE)
#'
get_censo <- function(censo = NULL,
                      simplified = FALSE){

  ## Check for internet conection
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "Internet access was not detected. Please check your connection //
No se detecto acceso a internet. Por favor chequear la conexion.")



  # Check parameters

  assertthat::assert_that(!is.null(censo),
                          msg = "debe ingresar un id de censo valido. Chequear opciones con 'show_available_censos()")

  assertthat::assert_that(is.character(censo),
                          msg = "geo debe ser del tipo 'character'. Chequear opciones con 'show_available_censos()")

  assertthat::assert_that(censo %in% censos$censo,
                          msg = "no es un id de censo valido. Chequearlos con show_available_censos")


  url <- if(simplified == TRUE){

    glue::glue("https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data/censo{censo}_simplified.geojson")

  } else if (simplified == FALSE){

    glue::glue("https://github.com/PoliticaArgentina/data_warehouse/raw/master/geoAr/data_raw/censos/censo_{censo}.geojson")

  }

  # Set default value for try()

  default <- NULL

  df <- base::suppressWarnings(base::try(default <- sf::read_sf(url), silent = TRUE))

  if(is.null(default)){

    df <- base::message("Fail to download data. Source is not available // La fuente de datos no esta disponible")

  } else {

    df <- df


    # download message one per session hack
    if(base::getOption('descarga-censo', TRUE)){


      message(glue::glue("Los datos fueron obtenidos del proyecto 'Base cartografica de departamentos para los censos nacionales de la Republica Argentina desde 1869 a 2010' de Gonzalo Rodriguez y Pablo De Grande el {format(Sys.Date(), '%d %B de %Y')}. La documentacion se encuetra disponible en https://ri.conicet.gov.ar/handle/11336/149867"))

      options('descarga-censo' = FALSE)

    }


  }

  return(df)
}

