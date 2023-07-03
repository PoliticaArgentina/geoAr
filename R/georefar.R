
check_internet <- function(){
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "No se detecto acceso a internet. Por favor chequea tu conexion.")
}

check_status <- function(res){
  attempt::stop_if_not(.x = httr::status_code(res),
              .p = ~ .x == 200,
              msg = httr::message_for_status(res, "get data from API"))
}

base_url <- "http://apis.datos.gob.ar/georef/api/"

get_endpoint <- function(endpoint, args) {

  if (! assertthat::noNA(args)) {
    stop(c('Los parametros siguientes tienen NAs', names(args[is.na(args)])))
  }

  # Obtener el token de la variable de entorno
  token <- Sys.getenv("GEOREFAR_TOKEN")

  url <- paste0(base_url, endpoint)

  # Comprobar si el token está presente
  if (is.null(token) | token == "") {
    response <- httr::GET(url, query = args)
  } else {
    response <- httr::GET(url,
                          httr::add_headers(Authorization = paste("Bearer", token)),
                          query =  args)
  }

  parsed <- suppressMessages(jsonlite::fromJSON(httr::content(response, "text")))

  check_status(response)

  data <- parsed[[gsub(pattern = "-",replacement = "_", x = endpoint)]] %>%
    purrr::modify_if(is.null, list)

  if (length(data) == 0) {
    stop("La consulta devolvió una lista vacía", call. = F)
  }

  data %>%
    dplyr::as_tibble(.name_repair = function(x) {gsub(pattern = "\\$|\\.", replacement = "_", x = x)})
}


#' Obtener Calles
#'
#' Permite realizar búsquedas sobre el listado de vías de circulación.
#' Realiza la consulta GET al endpoint /calles de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param tipo text Tipo de calle. (Valores disponibles: calle, avenida, pasaje.)
#' @param provincia text Filtrar por nombre o ID de provincia.
#' @param departamento text Filtrar por nombre o ID de departamento.
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_calles
#'
#' @references [georef-ar-api/calles](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_calles)
#' @return Un Data Frame con el listado de Calles
#' @examples
#' \dontrun{
#' get_calles()
#' }

get_calles <- function(id = NULL, nombre = NULL, tipo = NULL, provincia = NULL, departamento = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, tipo = tipo, provincia = provincia, departamento = departamento, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  endpoint <- "calles"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)

}

#' Obtener Departamentos
#'
#' Permite realizar búsquedas sobre el listado de departamentos.
#' Realiza la consulta GET al endpoint /departamentos de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param provincia text Filtrar por nombre o ID de Provincia.
#' @param orden text Campo por el cual ordenar los resultados.  (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_departamentos
#'
#' @references [georef-ar-api/departamentos](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_departamentos)
#' @return Un Data Frame con el listado de Departamentos
#' @examples
#' \dontrun{
#' get_departamentos()
#' }

get_departamentos <- function(id = NULL, nombre = NULL, provincia = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  endpoint <- "departamentos"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)

}

#' Normalizacion de direcciones
#'
#' Permite normalizar una dirección utilizando el listado de vías de circulación.
#' Realiza la consulta GET al endpoint /direcciones de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param direccion text Requerido. Direccion a normalizar, debe contener altura separada por espacio. (Ej: Colon 127)
#' @param tipo text Tipo de calle. (Valores disponibles: calle, avenida, pasaje.)
#' @param provincia text Filtrar por nombre o ID de provincia.
#' @param departamento text Filtrar por nombre o ID de departamento.
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname normalizar_direccion
#'
#' @references [georef-ar-api/direcciones](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_direcciones)
#' @return Un Data Frame con el listado normalizado de de direcciones
#' @examples
#' \dontrun{
#' normalizar_direccion()
#' }

normalizar_direccion <- function(direccion, tipo = NULL, provincia = NULL, departamento = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(direccion = direccion, tipo = tipo, provincia = provincia, departamento = departamento, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  endpoint <- "direcciones"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)
}

#' Obtener Localidades
#'
#' Permite realizar búsquedas sobre el listado de localidades.
#' Realiza la consulta GET al endpoint /localidades de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param provincia text Filtrar por nombre o ID de Provincia.
#' @param departamento text Filtrar por nombre o ID de Departamento.
#' @param municipio text Filtrar por nombre o ID de Municipio.
#' @param orden text Campo por el cual ordenar los resultados (por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_localidades
#'
#' @references [georef-ar-api/localidades](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_localidades)
#' @return Un Data Frame con el listado de Localidades
#' @examples
#' \dontrun{
#' get_localidades()
#' }

get_localidades <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, municipio = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, municipio = municipio, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  endpoint <- "localidades"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)
}

#' Obtener Municipios
#'
#' Permite realizar búsquedas sobre el listado de municipios.
#' Realiza la consulta GET al endpoint /municipios de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param provincia text Filtrar por nombre o ID de Provincia.
#' @param departamento text Filtrar por nombre o ID de Departamento.
#' @param orden text Campo por el cual ordenar los resultados. (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_municipios
#'
#' @references [georef-ar-api/municipios](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_municipios)
#' @return Un Data Frame con el listado de Municipios
#' @examples
#' \dontrun{
#' get_municipios()
#' }

get_municipios <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  endpoint <- "municipios"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)
}

#' Obtener Provincias
#'
#' Permite realizar búsquedas sobre el listado de provincias.
#' Realiza la consulta GET al endpoint /provincias de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param orden text Campo por el cual ordenar los resultados. (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_provincias
#'
#' @references [georef-ar-api/provincias](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_provincias)
#' @return Un Data Frame con el listado de Provincias
#' @examples
#' \dontrun{
#' get_provincias(nombre = "Córdoba")
#' }

get_provincias <- function(id = NULL, nombre = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  endpoint <- "provincias"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)
}

#' Obtener Ubicacion
#'
#' Permite realizar una georreferenciación inversa para un punto, informando cuales unidades territoriales lo contienen.
#' Realiza la consulta GET al endpoint /ubicacion de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param lat numeric Latitud del punto, en forma de número real con grados decimales.
#' @param lon numeric Longitud del punto, en forma de número real con grados decimales.
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#'
#' @export
#' @rdname get_ubicacion
#'
#' @references [georef-ar-api/ubicacion](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_ubicacion)
#' @return Un Data Frame con las unidades territoriales que contienen el punto.
#' @examples
#' \dontrun{
#' get_ubicacion()
#' }

get_ubicacion <- function(lat, lon, aplanar = TRUE, campos = NULL){
  args <- list(lat = lat, lon = lon, aplanar = aplanar, campos = campos)

  endpoint <- "ubicacion"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)
}

#' Obtener Localidades Censales
#'
#' Permite realizar búsquedas sobre el listado de localidades censales.
#' Realiza la consulta GET al endpoint /localidades-censales de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param provincia text Filtrar por nombre o ID de Provincia.
#' @param departamento text Filtrar por nombre o ID de Departamento.
#' @param municipio text Filtrar por nombre o ID de Municipio.
#' @param orden text Campo por el cual ordenar los resultados (por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_localidades_censales
#'
#' @references [georef-ar-api/localidades-censales](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_localidades_censales)
#' @return Un Data Frame con el listado de Localidades Censales.
#' @examples
#' \dontrun{
#' get_localidades_censales()
#' }

get_localidades_censales <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, municipio = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, municipio = municipio, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  endpoint <- "localidades-censales"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)
}

#' Obtener Asentamientos de BAHRA
#'
#' Permite realizar búsquedas sobre el listado de asentamientos BAHRA.
#' Realiza la consulta GET al endpoint /asentamientos de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param provincia text Filtrar por nombre o ID de Provincia.
#' @param departamento text Filtrar por nombre o ID de Departamento.
#' @param municipio text Filtrar por nombre o ID de Municipio.
#' @param orden text Campo por el cual ordenar los resultados (por ID o nombre)
#' @param localidad_censal text Filtrar por nombre o ID de localidad censal. Se pueden especificar varios IDs separados por comas
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_asentamientos
#'
#' @references [georef-ar-api/asentamientos](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_asentamientos)
#' @return Un Data Frame con el listado de Asentamientos BAHRA.
#' @examples
#' \dontrun{
#' get_asentamientos()
#' }

get_asentamientos <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, municipio = NULL, localidad_censal = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, municipio = municipio, localidad_censal = localidad_censal, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  endpoint <- "asentamientos"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)
}
