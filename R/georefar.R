check_internet <- function(){
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "No se detecto acceso a internet. Por favor chequea tu conexion.")
}

check_status <- function(res){
  attempt::stop_if_not(.x = httr::status_code(res),
              .p = ~ .x == 200,
              msg = httr::message_for_status(res, "get data from API"))
}

base_url <- "https://apis.datos.gob.ar/georef/api/"

get_endpoint <- function(endpoint, args) {

  purrr::discard(args, is.null)

  if (! assertthat::noNA(args)) {
    stop(c('GET no admite NAs. Los parametros siguientes tienen NAs:', sapply(names(args[is.na(args)]),
                                                                              function(x) paste0(" ", x),
                                                                              USE.NAMES = F)))
  }

  # Obtener el token de la variable de entorno
  token <- Sys.getenv("GEOREFAR_TOKEN")

  url <- paste0(base_url, endpoint)

  # Comprobar si el token esta presente
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
    warning("La consulta devolvio una lista vacia", call. = F)
  }

  data %>%
    dplyr::as_tibble(.name_repair = function(x) {gsub(pattern = "\\$|\\.", replacement = "_", x = x)})
}



post_endpoint <- function(endpoint, queries_list) {
  # 'queries_list' should be a list of lists, where each inner list represents a single query's parameters.
  # For example: list(list(nombre = "Buenos Aires"), list(id = "06"))

  # Obtener el token de la variable de entorno
  token <- Sys.getenv("GEOREFAR_TOKEN")
  url <- paste0(base_url, endpoint)

  # The body needs to be a named list where the name is the endpoint
  # and the value is the list of queries.
  # e.g. list(provincias = list(list(nombre = "Tucuman"), list(id = "02")))
  body_data <- list()
  body_data[[endpoint]] <- queries_list

  # Convert to JSON
  json_body <- jsonlite::toJSON(body_data, auto_unbox = TRUE)

  # Comprobar si el token está presente
  if (is.null(token) || token == "") {
    response <- httr::POST(url,
                           body = json_body,
                           httr::content_type_json(),
                           encode = "raw" # Using raw as json_body is already a JSON string
    )
  } else {
    response <- httr::POST(url,
                           body = json_body,
                           httr::content_type_json(),
                           httr::add_headers(Authorization = paste("Bearer", token)),
                           encode = "raw"
    )
  }

  check_status(response) # Check HTTP status first

  # Parse the response content
  parsed_content <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = TRUE)

  # Bulk responses are expected to have a "resultados" field which is a list
  # Each element in "resultados" corresponds to one of the input queries
  if (!"resultados" %in% names(parsed_content)) {
    stop("La respuesta del POST no contiene el campo 'resultados' esperado.")
  }

  results_list <- parsed_content$resultados

  # Process each result, extracting the relevant data (e.g., 'provincias', 'departamentos')
  # The actual data is nested under a key that matches the endpoint name (plural form)
  # e.g. result$provincias, result$departamentos
  processed_results <- purrr::map_dfr(results_list, function(result) {
    if (length(result[[endpoint]]) == 0) {
      # If the specific endpoint's data is empty for this result part, return an empty tibble
      # with expected names if possible, or just an empty tibble.
      # This handles cases where a specific query in the bulk request had no matches.
      # Returning a tibble with 0 rows but correct column names is ideal if they can be inferred,
      # otherwise, an empty tibble and dplyr::bind_rows will handle it.
      return(dplyr::tibble())
    }
    # Convert the list of data items to a tibble
    # Need to handle cases where result[[endpoint]] might be a list of lists or a data frame
    if (is.data.frame(result[[endpoint]])) {
        dplyr::as_tibble(result[[endpoint]])
    } else if (is.list(result[[endpoint]])) {
        # If it's a list of lists (e.g., for single item results within the bulk)
        # This might need further refinement based on exact API response structure per endpoint
        dplyr::bind_rows(lapply(result[[endpoint]], dplyr::as_tibble))
    } else {
        dplyr::tibble() # Fallback
    }
  })


  # Clean column names
  if (ncol(processed_results) > 0) {
    processed_results <- processed_results %>%
      dplyr::rename_with(.fn = function(x) {gsub(pattern = "\\$|\\.", replacement = "_", x = x)})
  }
  
  if (nrow(processed_results) == 0 && length(queries_list) > 0) {
    warning("La consulta POST devolvio una lista vacia o no se pudieron procesar los resultados.", call. = F)
  }
  
  # The old code had a warning for empty rows, this is harder to check precisely with bulk.
  # The check above for nrow == 0 after processing is a general one.

  return(processed_results)
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

get_calles <- function(nombre = NULL, id = NULL, tipo = NULL, provincia = NULL, departamento = NULL, municipio = NULL, localidad_censal = NULL, categoria = NULL, max = NULL, inicio = NULL, aplanar = TRUE, campos = NULL, exacto = NULL){

  assertthat::assert_that(max <= 5000 || is.null(max), msg = "parametro 'max' debe ser menor a 5000 o NULL")
  assertthat::assert_that(max + inicio <= 10000 || is.null(max) || is.null(inicio), msg = "los parametros 'max' e 'inicio' deben sumar 10.000 o menos")

  args <- list(nombre = nombre, id = id, tipo = tipo, provincia = provincia, departamento = departamento, municipio = municipio, localidad_censal = localidad_censal, categoria = categoria, inicio = inicio, aplanar = aplanar, campos = campos, exacto = exacto, max = max)

  endpoint <- "calles"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)
}

#' Enviar Lote de Consultas de Calles (POST)
#'
#' Permite realizar múltiples búsquedas sobre el listado de vías de circulación en una sola llamada POST.
#' Realiza la consulta POST al endpoint /calles de georef-ar-api.
#'
#' @param queries_list Lista de listas. Cada lista interna debe contener los parámetros 
#'        para una consulta de calle individual.
#'        Parámetros válidos por consulta: nombre, id, tipo, provincia, departamento, municipio, localidad_censal, categoria, max, inicio, aplanar, campos, exacto.
#' @return Un Data Frame (tibble) con los resultados combinados de todas las consultas.
#' @export
#' @rdname post_calles_bulk
#'
#' @references [georef-ar-api/calles POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_calles)
#' @examples
#' \dontrun{
#' consultas_calles <- list(
#'   list(nombre = "SAN MARTIN", provincia = "BUENOS AIRES", max = 2),
#'   list(nombre = "SARMIENTO", provincia = "CORDOBA", max = 2)
#' )
#' resultados_calles <- post_calles_bulk(queries_list = consultas_calles)
#' print(resultados_calles)
#' }
post_calles_bulk <- function(queries_list) {
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas.")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' está vacía, no se realizará ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("id", "nombre", "tipo", "provincia", "departamento", "municipio", "localidad_censal", "categoria", "interseccion", "orden", "aplanar", "campos", "max", "inicio", "exacto")

  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'calles' contiene parámetro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Parámetros válidos son: ", paste(valid_params, collapse = ", "), "."))
    }

    current_max <- query$max
    current_inicio <- query$inicio
    if (!is.null(current_max) && current_max > 5000) {
      stop("En una de las consultas, el parámetro 'max' debe ser menor o igual a 5000.")
    }
    if (!is.null(current_max) && !is.null(current_inicio) && (current_max + current_inicio > 10000)) {
      stop("En una de las consultas, la suma de 'max' e 'inicio' debe ser 10,000 o menos.")
    }
  }

  endpoint <- "calles"
  check_internet()
  post_endpoint(endpoint = endpoint, queries_list = queries_list)
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

#' Enviar Lote de Consultas de Departamentos (POST)
#'
#' Permite realizar múltiples búsquedas sobre el listado de departamentos en una sola llamada POST.
#' Realiza la consulta POST al endpoint /departamentos de georef-ar-api.
#'
#' @param queries_list Lista de listas. Cada lista interna debe contener los parámetros 
#'        para una consulta de departamento individual (e.g., list(nombre = "Rosario"), list(provincia = "02")).
#'        Parámetros válidos por consulta: id, nombre, provincia, orden, aplanar, campos, max, exacto.
#' @return Un Data Frame (tibble) con los resultados combinados de todas las consultas.
#' @export
#' @rdname post_departamentos_bulk
#'
#' @references [georef-ar-api/departamentos POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_departamentos)
#' @examples
#' \dontrun{
#' consultas_deptos <- list(
#'   list(provincia = "22", nombre = "Ledesma"), 
#'   list(id = "14028")
#' )
#' resultados_deptos <- post_departamentos_bulk(queries_list = consultas_deptos)
#' print(resultados_deptos)
#' }
post_departamentos_bulk <- function(queries_list) {
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas.")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' está vacía, no se realizará ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("id", "nombre", "provincia", "interseccion", "orden", "aplanar", "campos", "max", "inicio", "exacto")
  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'departamentos' contiene parámetro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Parámetros válidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }

  endpoint <- "departamentos"
  check_internet()
  post_endpoint(endpoint = endpoint, queries_list = queries_list)
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

#' Normalizar un Lote de Direcciones (POST)
#'
#' Permite normalizar múltiples direcciones en una sola llamada POST.
#' Realiza la consulta POST al endpoint /direcciones de georef-ar-api.
#'
#' @param queries_list Lista de listas. Cada lista interna debe contener los parámetros 
#'        para una consulta de normalización de dirección individual.
#'        Parámetro requerido por consulta: 'direccion' (e.g., "AV SAN MARTIN 123").
#'        Otros parámetros válidos: tipo, provincia, departamento, aplanar, campos, max, exacto.
#' @return Un Data Frame (tibble) con los resultados combinados de todas las normalizaciones.
#' @export
#' @rdname post_direcciones_bulk
#'
#' @references [georef-ar-api/direcciones POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_direcciones)
#' @examples
#' \dontrun{
#' consultas_dir <- list(
#'   list(direccion = "MAIPU 100", provincia = "BUENOS AIRES"),
#'   list(direccion = "SANTA FE 2000, ROSARIO")
#' )
#' resultados_dir <- post_direcciones_bulk(queries_list = consultas_dir)
#' print(resultados_dir)
#' }
post_direcciones_bulk <- function(queries_list) {
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas.")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' está vacía, no se realizará ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("direccion", "provincia", "departamento", "localidad_censal", "localidad", "aplanar", "campos", "max", "inicio", "exacto")

  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    # Check for required 'direccion' field
    if (!"direccion" %in% names(query)) {
      stop(paste0("Consulta ", i, " en 'queries_list' para 'direcciones' debe contener un campo 'direccion'."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'direcciones' contiene parámetro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Parámetros válidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }

  endpoint <- "direcciones"
  check_internet()
  post_endpoint(endpoint = endpoint, queries_list = queries_list)
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

#' Enviar Lote de Consultas de Localidades (POST)
#'
#' Permite realizar múltiples búsquedas sobre el listado de localidades en una sola llamada POST.
#' Realiza la consulta POST al endpoint /localidades de georef-ar-api.
#'
#' @param queries_list Lista de listas. Cada lista interna debe contener los parámetros 
#'        para una consulta de localidad individual.
#'        Parámetros válidos por consulta: id, nombre, provincia, departamento, municipio, orden, aplanar, campos, max, exacto.
#' @return Un Data Frame (tibble) con los resultados combinados de todas las consultas.
#' @export
#' @rdname post_localidades_bulk
#'
#' @references [georef-ar-api/localidades POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_localidades)
#' @examples
#' \dontrun{
#' consultas_loc <- list(
#'   list(provincia = "Tucuman", departamento = "Capital"), 
#'   list(id = "22056140000")
#' )
#' resultados_loc <- post_localidades_bulk(queries_list = consultas_loc)
#' print(resultados_loc)
#' }
post_localidades_bulk <- function(queries_list) {
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas.")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' está vacía, no se realizará ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("id", "nombre", "provincia", "departamento", "municipio", "interseccion", "orden", "aplanar", "campos", "max", "inicio", "exacto")
  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'localidades' contiene parámetro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Parámetros válidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }

  endpoint <- "localidades"
  check_internet()
  post_endpoint(endpoint = endpoint, queries_list = queries_list)
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

#' Enviar Lote de Consultas de Municipios (POST)
#'
#' Permite realizar múltiples búsquedas sobre el listado de municipios en una sola llamada POST.
#' Realiza la consulta POST al endpoint /municipios de georef-ar-api.
#'
#' @param queries_list Lista de listas. Cada lista interna debe contener los parámetros 
#'        para una consulta de municipio individual.
#'        Parámetros válidos por consulta: id, nombre, provincia, departamento, orden, aplanar, campos, max, exacto.
#' @return Un Data Frame (tibble) con los resultados combinados de todas las consultas.
#' @export
#' @rdname post_municipios_bulk
#'
#' @references [georef-ar-api/municipios POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_municipios)
#' @examples
#' \dontrun{
#' consultas_muni <- list(
#'   list(provincia = "06", nombre = "La Plata"), 
#'   list(id = "540098")
#' )
#' resultados_muni <- post_municipios_bulk(queries_list = consultas_muni)
#' print(resultados_muni)
#' }
post_municipios_bulk <- function(queries_list) {
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas.")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' está vacía, no se realizará ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("id", "nombre", "provincia", "departamento", "interseccion", "orden", "aplanar", "campos", "max", "inicio", "exacto")
  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'municipios' contiene parámetro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Parámetros válidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }

  endpoint <- "municipios"
  check_internet()
  post_endpoint(endpoint = endpoint, queries_list = queries_list)
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
#' get_provincias(nombre = "Cordoba")
#' }

get_provincias <- function(id = NULL, nombre = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, orden = orden, aplanar = aplanar, campos = campos, max = max, exacto = exacto)

  endpoint <- "provincias"

  check_internet()

  get_endpoint(endpoint = endpoint, args = args)
}

#' Enviar Lote de Consultas de Provincias (POST)
#'
#' Permite realizar múltiples búsquedas sobre el listado de provincias en una sola llamada POST.
#' Realiza la consulta POST al endpoint /provincias de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#'
#' @param queries_list Lista de listas. Cada lista interna debe contener los parámetros 
#'        para una consulta de provincia individual (e.g., list(nombre = "Tucuman"), list(id = "06")).
#'        Parámetros válidos por consulta: id, nombre, orden, aplanar, campos, max, exacto.
#' @return Un Data Frame (tibble) con los resultados combinados de todas las consultas. 
#'         Las respuestas de la API para cada consulta en el lote se apilan.
#' @export
#' @rdname post_provincias_bulk
#'
#' @references [georef-ar-api/provincias POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_provincias)
#' @examples
#' \dontrun{
#' consultitas <- list(
#'   list(nombre = "santiago del estero"), 
#'   list(id = "82", campos = "id,nombre,centroide.lat,centroide.lon"),
#'   list(nombre = "tierra del fuego", campos = "completo")
#' )
#' resultados_provincias <- post_provincias_bulk(queries_list = consultitas)
#' print(resultados_provincias)
#' }
post_provincias_bulk <- function(queries_list) {
  # Validaciones básicas para queries_list
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas.")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' está vacía, no se realizará ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("id", "nombre", "interseccion", "orden", "aplanar", "campos", "max", "inicio", "exacto")
  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'provincias' contiene parámetro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Parámetros válidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }

  endpoint <- "provincias"
  check_internet()
  post_endpoint(endpoint = endpoint, queries_list = queries_list)
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

#' Georreferenciación Inversa para un Lote de Puntos (POST)
#'
#' Permite realizar georreferenciación inversa para múltiples puntos (lat, lon) en una sola llamada POST.
#' Realiza la consulta POST al endpoint /ubicacion de georef-ar-api.
#'
#' @param queries_list Lista de listas. Cada lista interna debe contener los parámetros 
#'        'lat' y 'lon' para un punto.
#'        Otros parámetros válidos por consulta: aplanar, campos.
#' @return Un Data Frame (tibble) con los resultados combinados de todas las georreferenciaciones.
#' @export
#' @rdname post_ubicacion_bulk
#'
#' @references [georef-ar-api/ubicacion POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_ubicacion)
#' @examples
#' \dontrun{
#' consultas_ubic <- list(
#'   list(lat = -34.6037, lon = -58.3816), # Buenos Aires
#'   list(lat = -32.9587, lon = -60.6393)  # Rosario
#' )
#' resultados_ubic <- post_ubicacion_bulk(queries_list = consultas_ubic)
#' print(resultados_ubic)
#' }
post_ubicacion_bulk <- function(queries_list) {
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas.")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' está vacía, no se realizará ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("lat", "lon", "aplanar", "campos")

  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    # Check for required 'lat' and 'lon' fields
    if (!all(c("lat", "lon") %in% names(query))) {
      stop(paste0("Consulta ", i, " en 'queries_list' para 'ubicacion' debe contener los campos 'lat' y 'lon'."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'ubicacion' contiene parámetro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Parámetros válidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }

  endpoint <- "ubicacion"
  check_internet()
  post_endpoint(endpoint = endpoint, queries_list = queries_list)
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

#' Enviar Lote de Consultas de Localidades Censales (POST)
#'
#' Permite realizar múltiples búsquedas sobre el listado de localidades censales en una sola llamada POST.
#' Realiza la consulta POST al endpoint /localidades-censales de georef-ar-api.
#'
#' @param queries_list Lista de listas. Cada lista interna debe contener los parámetros 
#'        para una consulta de localidad censal individual.
#'        Parámetros válidos por consulta: id, nombre, provincia, departamento, municipio, orden, aplanar, campos, max, exacto.
#' @return Un Data Frame (tibble) con los resultados combinados de todas las consultas.
#' @export
#' @rdname post_localidades_censales_bulk
#'
#' @references [georef-ar-api/localidades-censales POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_localidades_censales)
#' @examples
#' \dontrun{
#' consultas_loc_cen <- list(
#'   list(nombre = "AGUA DE ORO", provincia = "CÓRDOBA")
#' )
#' resultados_loc_cen <- post_localidades_censales_bulk(queries_list = consultas_loc_cen)
#' print(resultados_loc_cen)
#' }
post_localidades_censales_bulk <- function(queries_list) {
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas.")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' está vacía, no se realizará ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("id", "nombre", "provincia", "departamento", "municipio", "interseccion", "orden", "aplanar", "campos", "max", "inicio", "exacto")
  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'localidades-censales' contiene parámetro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Parámetros válidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }

  endpoint <- "localidades-censales"
  check_internet()
  post_endpoint(endpoint = endpoint, queries_list = queries_list)
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

#' Enviar Lote de Consultas de Asentamientos (POST)
#'
#' Permite realizar múltiples búsquedas sobre el listado de asentamientos BAHRA en una sola llamada POST.
#' Realiza la consulta POST al endpoint /asentamientos de georef-ar-api.
#'
#' @param queries_list Lista de listas. Cada lista interna debe contener los parámetros 
#'        para una consulta de asentamiento individual.
#'        Parámetros válidos por consulta: id, nombre, provincia, departamento, municipio, localidad_censal, orden, aplanar, campos, max, exacto.
#' @return Un Data Frame (tibble) con los resultados combinados de todas las consultas.
#' @export
#' @rdname post_asentamientos_bulk
#'
#' @references [georef-ar-api/asentamientos POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_asentamientos)
#' @examples
#' \dontrun{
#' consultas_asent <- list(
#'   list(provincia = "CHUBUT", nombre = "COSTA")
#' )
#' resultados_asent <- post_asentamientos_bulk(queries_list = consultas_asent)
#' print(resultados_asent)
#' }
post_asentamientos_bulk <- function(queries_list) {
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas.")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' está vacía, no se realizará ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("id", "nombre", "provincia", "departamento", "municipio", "localidad_censal", "interseccion", "orden", "aplanar", "campos", "max", "inicio", "exacto")
  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'asentamientos' contiene parámetro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Parámetros válidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }

  endpoint <- "asentamientos"
  check_internet()
  post_endpoint(endpoint = endpoint, queries_list = queries_list)
}

#' Descargar Datos Geográficos Completos
#'
#' Permite descargar listados completos de entidades geográficas en diversos formatos.
#' Accede al endpoint /{filename} de la georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta (aunque generalmente no es necesario para estos endpoints públicos).
#'
#' @param entidad Cadena de texto. La entidad geográfica a descargar. 
#'        Valores posibles: "provincias", "departamentos", "municipios", 
#'        "localidades", "localidades-censales", "asentamientos", "calles", "cuadras".
#' @param formato Cadena de texto. El formato deseado para el archivo. 
#'        Valores posibles: "csv", "json", "geojson", "ndjson".
#' @param path_to_save Cadena de texto opcional. Ruta completa (incluyendo nombre de archivo y extensión) 
#'        donde guardar el archivo descargado. Si es NULL (por defecto), la función devolverá el contenido 
#'        parseado (para json/geojson/ndjson) o un data frame (para csv).
#'        Si se especifica una ruta, la función guardará el archivo y devolverá la ruta del archivo guardado.
#' @return Dependiendo de 'path_to_save' y 'formato': 
#'         - Si 'path_to_save' se especifica: la ruta al archivo guardado (invisiblemente).
#'         - Si 'path_to_save' es NULL:
#'           - Para "csv": un data.frame.
#'           - Para "json", "geojson", "ndjson": una lista o estructura de R parseada desde JSON.
#'           - Si la descarga o parseo falla, genera un error.
#' @export
#' @rdname get_geodata_dump
#'
#' @references [georef-ar-api/descargas](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get__filename_)
#' @examples
#' \dontrun{
#'   # Obtener provincias en formato GeoJSON como objeto R
#'   provincias_geojson <- get_geodata_dump(entidad = "provincias", formato = "geojson")
#'
#'   # Guardar departamentos en formato CSV
#'   get_geodata_dump(entidad = "departamentos", formato = "csv", path_to_save = "deptos.csv")
#' }

get_geodata_dump <- function(entidad, formato, path_to_save = NULL) {

  check_internet()

  valid_entidades <- c("provincias", "departamentos", "municipios", 
                       "localidades", "localidades-censales", "asentamientos", 
                       "calles", "cuadras")
  valid_formatos <- c("csv", "json", "geojson", "ndjson")

  if (!entidad %in% valid_entidades) {
    stop(paste("Entidad no válida. Opciones disponibles:", paste(valid_entidades, collapse = ", ")))
  }
  if (!formato %in% valid_formatos) {
    stop(paste("Formato no válido. Opciones disponibles:", paste(valid_formatos, collapse = ", ")))
  }

  filename <- paste0(entidad, ".", formato)
  url <- paste0(base_url, filename)

  token <- Sys.getenv("GEOREFAR_TOKEN")

  if (is.null(token) || token == "") {
    response <- httr::GET(url)
  } else {
    response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", token)))
  }

  check_status(response) # Verifica que la respuesta sea 200 OK

  if (!is.null(path_to_save)) {
    assertthat::assert_that(is.character(path_to_save), length(path_to_save) == 1,
                            msg = "'path_to_save' debe ser una cadena de texto con la ruta del archivo.")
    tryCatch({
      writeBin(httr::content(response, "raw"), path_to_save)
      message(paste("Archivo guardado en:", path_to_save))
      return(invisible(path_to_save))
    }, error = function(e) {
      stop(paste("Error al guardar el archivo:", e$message))
    })
  } else {
    # Si no se guarda, intentar parsear según el formato
    content_text <- httr::content(response, "text", encoding = "UTF-8")
    if (formato == "csv") {
      return(utils::read.csv(text = content_text, stringsAsFactors = FALSE))
    } else if (formato %in% c("json", "geojson", "ndjson")) {
      return(jsonlite::fromJSON(content_text, flatten = TRUE))
    } else {
      # No debería llegar aquí por las validaciones previas, pero por si acaso
      warning("Formato no soportado para parseo directo, devolviendo contenido como texto.")
      return(content_text)
    }
  }
}
