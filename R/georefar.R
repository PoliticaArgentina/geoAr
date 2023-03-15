
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




#' Obtener Calles
#'
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
#' @return Un Data Frame con el listado de Calles
#' @examples
#' \dontrun{
#' get_calles()
#' }

get_calles <- function(id = NULL, nombre = NULL, tipo = NULL, provincia = NULL, departamento = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, tipo = tipo, provincia = provincia, departamento = departamento, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  check_internet()
  base_url_get_calles <- paste0(base_url, "calles")
  res <- httr::GET(base_url_get_calles,
                   query = purrr::compact(args))

  check_status(res)

  jsonlite::fromJSON(httr::content(res, "text"))$calles  %>%
    purrr::modify_if(is.null, list) %>%     dplyr::as_tibble() %>% suppressMessages()
}

#' Obtener Departamentos
#'
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param provincia text Filtrar por nombre o ID de Provincia.
#' @param orden text Campo por el cual ordenar los resultados.  (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#' @export
#' @rdname get_departamentos
#'
#' @return Un Data Frame con el listado de Departamentos
#' @examples
#' \dontrun{
#' get_departamentos()
#' }

get_departamentos <- function(id = NULL, nombre = NULL, provincia = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  check_internet()

  base_url_get_departamentos <- paste0(base_url, "departamentos")
  res <- httr::GET(base_url_get_departamentos,
                   query = purrr::compact(args))

  check_status(res)

  jsonlite::fromJSON(httr::content(res, "text"))$departamentos  %>%
    purrr::modify_if(is.null, list) %>%     dplyr::as_tibble() %>% suppressMessages()
}
#' Normalizacion de direcciones
#'
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
#' @return Un Data Frame con el listado normalizado de de direcciones
#' @examples
#' \dontrun{
#' normalizar_direccion()
#' }

normalizar_direccion <- function(direccion, tipo = NULL, provincia = NULL, departamento = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(direccion = direccion, tipo = tipo, provincia = provincia, departamento = departamento, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  check_internet()
  base_url_get_direcciones <- paste0(base_url, "direcciones")
  res <- httr::GET(base_url_get_direcciones,
             query = purrr::compact(args))

  check_status(res)

  jsonlite::fromJSON(httr::content(res, "text"))$direcciones %>%
    purrr::modify_if(is.null, list) %>%     dplyr::as_tibble() %>% suppressMessages()
}

#' Obtener Localidades
#'
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
#' @export
#' @rdname get_localidades
#'
#' @return Un Data Frame con el listado de Departamentos
#' @examples
#' \dontrun{
#' get_localidades()
#' }

get_localidades <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, municipio = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, municipio = municipio, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  check_internet()

  base_url_get_localidades <- paste0(base_url, "localidades")
  res <- httr::GET(base_url_get_localidades,
             query = purrr::compact(args))

  check_status(res)

  jsonlite::fromJSON(httr::content(res, "text"))$localidades  %>%
    purrr::modify_if(is.null, list) %>%     dplyr::as_tibble() %>% suppressMessages()
}
#' Obtener Municipios
#'
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param provincia text Filtrar por nombre o ID de Provincia.
#' @param departamento text Filtrar por nombre o ID de Departamento.
#' @param orden text Campo por el cual ordenar los resultados. (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#' @export
#' @rdname get_municipios
#'
#' @return Un Data Frame con el listado de Departamentos
#' @examples
#' \dontrun{
#' get_municipios()
#' }

get_municipios <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  check_internet()

  base_url_get_municipios <- paste0(base_url, "municipios")
  res <- httr::GET(base_url_get_municipios,
                   query = purrr::compact(args))

  check_status(res)

  jsonlite::fromJSON(httr::content(res, "text"))$municipios  %>%
    purrr::modify_if(is.null, list) %>%     dplyr::as_tibble() %>% suppressMessages()
}
#' Obtener Provincias
#'
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param orden text Campo por el cual ordenar los resultados. (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#' @export
#' @rdname get_provincias
#'
#' @return Un Data Frame con el listado de Provincias
#' @examples
#' \dontrun{
#' get_provincias(nombre = "Córdoba")
#' }

get_provincias <- function(id = NULL, nombre = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)
  #stop_if_all(args, is.null, "You need to specify at least one argument")

  check_internet()
  base_url_get_provincias <- paste0(base_url, "provincias")
  res <- httr::GET(base_url_get_provincias,
                   query = purrr::compact(args))

  check_status(res)

  jsonlite::fromJSON(httr::content(res, "text"))$provincias  %>%
    purrr::modify_if(is.null, list) %>%     dplyr::as_tibble() %>% suppressMessages()
}
#' Obtener Ubicacion
#'
#' @param lat numeric Latitud del punto, en forma de número real con grados decimales.
#' @param lon numeric Longitud del punto, en forma de número real con grados decimales.
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#'
#' @export
#' @rdname get_ubicacion
#'
#' @return Un Data Frame con el listado de Departamentos
#' @examples
#' \dontrun{
#' get_ubicacion()
#' }

get_ubicacion <- function(lat, lon, aplanar = TRUE, campos = NULL){
  args <- list(lat = lat, lon = lon, aplanar = aplanar, campos = campos)

  check_internet()

  base_url_get_ubicacion <- paste0(base_url, "ubicacion")
  res <- httr::GET(base_url_get_ubicacion,
                   query = purrr::compact(args))

  check_status(res)

  jsonlite::fromJSON(httr::content(res, "text"))$ubicacion %>%
    purrr::modify_if(is.null, list) %>%
    dplyr::as_tibble() %>% suppressMessages()
}
