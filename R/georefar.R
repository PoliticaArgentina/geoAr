#' @importFrom httr2 request req_url_query req_error req_auth_bearer_token resp_body_json resp_status resp_status_desc resp_url req_method req_body_json resp_body_raw resp_body_string resp_has_body resp_content_type req_perform req_perform_parallel
#' @importFrom promises then
NULL

check_internet <- function(){
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "No se detect\u00f3 acceso a internet. Por favor chequea tu conexi\u00f3n.")
}

#' Replace NULL Values with NA
#'
#' Recursively replaces NULL values with NA in a list or vector.
#' This function is useful for handling NULL values in API responses
#' before converting them to data frames.
#'
#' @param x A list, vector, or single value to process
#' @return The input with all NULL values replaced by NA
#' @keywords internal
replace_null_with_na <- function(x) {
  if (is.list(x)) {
    # If it's a list, apply the function recursively to each element
    lapply(x, replace_null_with_na)
  } else if (is.null(x)) {
    # If it's NULL, replace with NA
    NA
  } else {
    # If it's not a list and not NULL, return as is
    x
  }
}

base_url <- "https://apis.datos.gob.ar/georef/api/"

# Define a reusable error handler for httr2 requests
httr2_error_handler <- function(resp) {
  # Attempt to get more specific error details from the body if available and small
  error_body_text <- ""
  tryCatch({
    # Only try to read body if it's likely text and not too large
    if (httr2::resp_has_body(resp) && grepl("json|text|xml", httr2::resp_content_type(resp), ignore.case = TRUE)) {
        body_content <- httr2::resp_body_string(resp, encoding = "UTF-8")
        if (nchar(body_content) < 500) { # Limit body size in error to avoid overly long messages
            error_body_text <- paste0(" - API Response: ", body_content)
        }
    }
  }, error = function(e) {
    # Ignore errors during error body retrieval
  })

  stop(paste0("API request failed: ",
              httr2::resp_status_desc(resp), " (", httr2::resp_status(resp), "). ",
              "URL: ", httr2::resp_url(resp),
              error_body_text),
       call. = FALSE)
}

# API Limits for batch POST requests
GEOREFAR_API_MAX_QUERIES_PER_BATCH <- 1000
GEOREFAR_API_MAX_SUM_MAX_PARAM_PER_BATCH <- 5000

# Internal helper function to create batches of queries
create_query_batches <- function(all_queries,
                                 max_queries_per_batch = GEOREFAR_API_MAX_QUERIES_PER_BATCH,
                                 max_sum_of_a_param = GEOREFAR_API_MAX_SUM_MAX_PARAM_PER_BATCH,
                                 param_name_for_sum = NULL) {
  if (!is.list(all_queries) || length(all_queries) == 0) {
    return(list())
  }

  batches <- list()
  current_batch_queries <- list()
  current_query_count_in_batch <- 0
  current_sum_val_in_batch <- 0

  for (query_idx in seq_along(all_queries)) {
    query <- all_queries[[query_idx]]
    
    query_param_val <- 0
    # Calculate the value of the parameter to be summed (e.g., 'max') for the current query
    if (!is.null(param_name_for_sum) && !is.null(query[[param_name_for_sum]])) {
      val <- suppressWarnings(as.numeric(query[[param_name_for_sum]]))
      if (!is.na(val) && val > 0) {
        query_param_val <- val
      }
    }

    # Determine if this query can be added to the current batch
    can_add_to_current_batch <- TRUE
    
    # Check query count limit
    if ((current_query_count_in_batch + 1) > max_queries_per_batch) {
      can_add_to_current_batch <- FALSE
    }
    
    # Check sum of parameter limit (if applicable)
    if (can_add_to_current_batch && !is.null(max_sum_of_a_param) && !is.null(param_name_for_sum)) {
      if ((current_sum_val_in_batch + query_param_val) > max_sum_of_a_param && query_param_val > 0) {
        # This check is relevant only if the query itself contributes to the sum (param_value > 0).
        # A query with param_value = 0 (or param not present) doesn't affect the sum limit.
        can_add_to_current_batch <- FALSE
      }
    }

    # If current batch has queries AND this query cannot be added, finalize the current batch
    if (length(current_batch_queries) > 0 && !can_add_to_current_batch) {
      batches <- append(batches, list(current_batch_queries))
      # Reset for new batch
      current_batch_queries <- list()
      current_query_count_in_batch <- 0
      current_sum_val_in_batch <- 0
    }
    
    # Add query to current batch (which might be new or the existing one)
    current_batch_queries <- append(current_batch_queries, list(query))
    current_query_count_in_batch <- current_query_count_in_batch + 1
    if (!is.null(param_name_for_sum)) { # Only add to sum if param_name_for_sum is defined
      current_sum_val_in_batch <- current_sum_val_in_batch + query_param_val
    }
  }

  # Add the last batch if it contains any queries
  if (length(current_batch_queries) > 0) {
    batches <- append(batches, list(current_batch_queries))
  }
  
  return(batches)
}

# Internal helper to PREPARE a single POST batch httr2_request object
prepare_post_batch_request <- function(endpoint, single_batch_queries_list) {
  token <- Sys.getenv("GEOREFAR_TOKEN")
  url <- paste0(base_url, endpoint)

  body_data <- list()
  body_data[[endpoint]] <- single_batch_queries_list

  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = body_data, auto_unbox = TRUE) |>
    # httr2::req_error will convert responses with status != 200 to an R error,
    # which req_perform_parallel(on_error="return") will then return as that error object.
    # The httr2_error_handler provides a formatted message for such errors.
    httr2::req_error(is_error = ~ httr2::resp_status(.x) != 200, body = httr2_error_handler)
    
  if (!is.null(token) && token != "") {
    req <- req |> httr2::req_auth_bearer_token(token)
  }
  
  return(req) # Return the request object, not a promise
}

# Internal helper to process the httr2 response object of a single POST batch
process_single_post_response <- function(response_obj, endpoint, num_queries_in_this_batch) {
  # This function assumes response_obj is a successful httr2_response
  parsed_content <- httr2::resp_body_json(response_obj, flatten = TRUE)

  if (!"resultados" %in% names(parsed_content)) {
    url_info <- tryCatch(httr2::resp_url(response_obj), error = function(e) "unknown URL")
    stop(paste0("La respuesta del POST para el endpoint '", endpoint, "' (URL: ", url_info, ") no contiene el campo 'resultados' esperado."), call. = FALSE)
  }

  results_list_from_api <- parsed_content$resultados

  # The actual data items are expected to be in results_list_from_api[[endpoint]]
  actual_data_items_list <- lapply(results_list_from_api, function(x) x[[endpoint]])
  
  if (is.null(actual_data_items_list)) {
    warning(paste0("Expected data field '", endpoint, "' not found or is NULL within the 'resultados' field of the bulk API response. Available keys in 'resultados': ", paste(names(results_list_from_api), collapse=", ")), call. = FALSE)
    return(dplyr::tibble())
  }
  
  if (!is.list(actual_data_items_list)) {
      warning(paste0("Data for endpoint '", endpoint, "' within 'resultados' is not a list as expected. Type: ", class(actual_data_items_list)), call. = FALSE)
      return(dplyr::tibble())
  }
 
  processed_results <- purrr::map_dfr(actual_data_items_list, function(data_items_for_one_original_query) {
    

    if (is.null(data_items_for_one_original_query)) {
      return(dplyr::tibble())
    }
    if (is.data.frame(data_items_for_one_original_query)) {
      return(dplyr::as_tibble(data_items_for_one_original_query))
    } else if (is.list(data_items_for_one_original_query)) {
      if (length(data_items_for_one_original_query) == 0) {
        return(dplyr::tibble())
      }
      
      tryCatch({
        # Ensure that elements being bound are suitable for bind_rows (e.g., named lists or data.frames)
        # If data_items_for_one_original_query is a list of atomic vectors or unnamed lists, this might fail.
        # Assuming API returns list of objects (named lists) or list of data.frames.
        data_items_for_one_original_query <- replace_null_with_na(data_items_for_one_original_query)

        return(dplyr::bind_rows(lapply(data_items_for_one_original_query, as.data.frame)))
        
      }, error = function(e) {
        warning(paste0("Failed to bind rows for an item in '", endpoint, "' results. Item class: ", class(data_items_for_one_original_query), ". Error: ", e$message), call. = FALSE)
        return(dplyr::tibble()) 
      })
    } else {
      warning(paste0("Unexpected data type ('", class(data_items_for_one_original_query), "') for an item in '", endpoint, "' results. Skipping this item."), call. = FALSE)
      return(dplyr::tibble())
    }
  })


  if (ncol(processed_results) > 0) {
    # processed_results <- processed_results |>
    #   dplyr::rename_with(.fn = function(x) {gsub(pattern = "\\\\$|\\\\.", replacement = "_", x = x)})
  }
  
  if (nrow(processed_results) == 0 && num_queries_in_this_batch > 0) {
    # This warning applies to a single batch. The overall warning will be in the calling post_*_bulk function.
    warning(paste0("Una tanda de ", num_queries_in_this_batch, " consultas POST para '", endpoint, "' devolvi\\u00f3 una lista vac\\u00eda o no se pudieron procesar sus resultados."), call. = FALSE)
  }
  
  return(processed_results)
}

get_endpoint <- function(endpoint, args) {

  args_clean <- purrr::discard(args, is.null)

  if (!assertthat::noNA(args_clean)) {
    stop(c('GET no admite NAs. Los par\u00e1metros siguientes tienen NAs:', sapply(names(args_clean[is.na(args_clean)]),
                                                                              function(x) paste0(" ", x),
                                                                              USE.NAMES = F)))
  }

  token <- Sys.getenv("GEOREFAR_TOKEN")

  req <- httr2::request(paste0(base_url, endpoint)) |>
    httr2::req_url_query(!!!args_clean) |>
    httr2::req_error(is_error = ~ httr2::resp_status(.x) != 200, body = httr2_error_handler)

  if (!is.null(token) && token != "") {
    req <- req |> httr2::req_auth_bearer_token(token)
  }

  # Use req_perform() for synchronous behavior needed by get_endpoint
  response <- httr2::req_perform(req)
  
  parsed <- httr2::resp_body_json(response)

  data_list <- parsed[[gsub(pattern = "-", replacement = "_", x = endpoint)]]
  
  if (is.null(data_list)) {
    data_list <- list() # Ensure it's an empty list if the key wasn't found or was null
  }
  
  data <- data_list |>
    purrr::modify_if(is.null, list) # Convert NULL elements within the list to list() for as_tibble

  if (length(data) == 0) {
    warning("La consulta devolvi\u00f3 una lista vac\u00eda", call. = FALSE)
  }

  data |>
    dplyr::bind_rows() |>
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
#' @param municipio text Filtrar por nombre o ID de municipio.
#' @param localidad_censal text Filtrar por nombre o ID de localidad censal.
#' @param categoria text Filtrar por categoría de calle.
#' @param interseccion text Geometría GeoJSON utilizada para filtrar resultados por intersección espacial. Sólo se soportan polígonos y multipolígonos. Ejemplo: polygon((-58.431,-34.592),(-58.430,-34.590),(-58.428,-34.593),(-58.431,-34.592)).
#' @param orden text Campo por el cual ordenar los resultados. (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver. Debe ser menor o igual a 5000.
#' @param inicio integer Cantidad de resultados a omitir desde el principio. La suma de 'max' e 'inicio' no debe superar 10000.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_calles
#'
#' @references [georef-ar-api/calles](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_calles)
#' @return Un Data Frame con el listado de Calles
#' @examples
#' \dontrun{
#' get_calles(nombre = "Corrientes", provincia = "CIUDAD AUTONOMA DE BUENOS AIRES", max = 10)
#' }

get_calles <- function(nombre = NULL, id = NULL, tipo = NULL, provincia = NULL, departamento = NULL, municipio = NULL, localidad_censal = NULL, categoria = NULL, interseccion = NULL, orden = NULL, max = NULL, inicio = NULL, aplanar = TRUE, campos = NULL, exacto = NULL){

  assertthat::assert_that(max <= 5000 || is.null(max), msg = "par\u00e1metro 'max' debe ser menor a 5000 o NULL")
  assertthat::assert_that(max + inicio <= 10000 || is.null(max) || is.null(inicio), msg = "los par\u00e1metros 'max' e 'inicio' deben sumar 10.000 o menos")

  args <- list(nombre = nombre, id = id, tipo = tipo, provincia = provincia, departamento = departamento, municipio = municipio, localidad_censal = localidad_censal, categoria = categoria, interseccion = interseccion, orden = orden, inicio = inicio, aplanar = aplanar, campos = campos, exacto = exacto, max = max)

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
    warning("'queries_list' est\u00e1 vac\u00eda, no se realizar\u00e1 ninguna consulta.")
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
      stop(paste0("Consulta ", i, " en 'queries_list' para 'calles' contiene par\u00e1metro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Par\u00e1metros v\u00e1lidos son: ", paste(valid_params, collapse = ", "), ". Estos par\u00e1metros ser\u00e1n enviados a la API, pero podr\u00edan ser ignorados o causar un error si no son aceptados por el endpoint espec\u00edfico."))
    }
    current_max <- query$max
    current_inicio <- query$inicio
    if (!is.null(current_max) && (!is.numeric(current_max) || current_max < 0 || current_max > 5000)) {
      stop(paste0("En la consulta ", i, ", el par\u00e1metro 'max' debe ser un n\u00famero entre 0 y 5000."))
    }
    if (!is.null(current_inicio) && (!is.numeric(current_inicio) || current_inicio < 0)) {
        stop(paste0("En la consulta ", i, ", el par\u00e1metro 'inicio' debe ser un n\u00famero positivo."))
    }
    if (!is.null(current_max) && !is.null(current_inicio) && (current_max + current_inicio > 10000)) {
      stop(paste0("En la consulta ", i, ", la suma de 'max' e 'inicio' no debe superar 10,000."))
    }
  }

  endpoint <- "calles"
  check_internet()
  
  query_batches <- create_query_batches(queries_list, param_name_for_sum = "max")
  if (length(query_batches) == 0 && length(queries_list) > 0) {
      warning(paste0("No se pudieron crear lotes de consultas para '", endpoint, "', aunque la lista de consultas no estaba vac\u00eda."), call. = FALSE)
      return(dplyr::tibble())
  } else if (length(query_batches) == 0) {
      return(dplyr::tibble())
  }

  all_batch_requests <- list()
  for (batch_idx in seq_along(query_batches)) {
    current_batch <- query_batches[[batch_idx]]
    request_obj <- prepare_post_batch_request(endpoint = endpoint, single_batch_queries_list = current_batch)
    all_batch_requests <- append(all_batch_requests, list(request_obj))
  }

  responses_or_errors <- httr2::req_perform_parallel(all_batch_requests, on_error = "return")
  final_results_list_of_tibbles <- list()
  has_errors <- FALSE
  for (i in seq_along(responses_or_errors)) {
    item <- responses_or_errors[[i]]
    num_queries_in_this_batch <- length(query_batches[[i]])
    if (inherits(item, "httr2_response")) {
      processed_tibble <- process_single_post_response(response_obj = item, endpoint = endpoint, num_queries_in_this_batch = num_queries_in_this_batch)
      final_results_list_of_tibbles <- append(final_results_list_of_tibbles, list(processed_tibble))
    } else if (inherits(item, "error")) {
      has_errors <- TRUE
      warning(paste0("Error en el lote ", i, " para '", endpoint, "': ", conditionMessage(item)), call. = FALSE)
    } else {
      has_errors <- TRUE
      warning(paste0("Error desconocido o respuesta inesperada en el lote ", i, " para '", endpoint, "'. Clase del objeto: ", class(item)[1]), call. = FALSE)
    }
  }
  combined_results <- dplyr::bind_rows(final_results_list_of_tibbles)
  if (nrow(combined_results) == 0 && length(queries_list) > 0 && !has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) devolvi\u00f3 una lista vac\u00eda o no se pudieron procesar los resultados, aunque no se reportaron errores directos en los lotes."), call. = FALSE)
  } else if (nrow(combined_results) == 0 && length(queries_list) > 0 && has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) no produjo resultados y se encontraron errores en algunos lotes."), call. = FALSE)
  }
  return(combined_results)
}

#' Obtener Departamentos
#'
#' Permite realizar búsquedas sobre el listado de departamentos.
#' Realiza la consulta GET al endpoint /departamentos de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param provincia text Filtrar por nombre o ID de Provincia.
#' @param interseccion text Geometría GeoJSON utilizada para filtrar resultados por intersección espacial. Sólo se soportan polígonos y multipolígonos. Ejemplo: polygon((-58.431,-34.592),(-58.430,-34.590),(-58.428,-34.593),(-58.431,-34.592)).
#' @param orden text Campo por el cual ordenar los resultados.  (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver. La API limita a un máximo de 529 para este endpoint.
#' @param inicio integer Cantidad de resultados a omitir desde el principio.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_departamentos
#'
#' @references [georef-ar-api/departamentos](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_departamentos)
#' @return Un Data Frame con el listado de Departamentos
#' @examples
#' \dontrun{
#' get_departamentos(provincia = "06", max = 5)
#' }

get_departamentos <- function(id = NULL, nombre = NULL, provincia = NULL, interseccion = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, inicio = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, interseccion = interseccion, orden = orden, aplanar = aplanar, campos = campos, max = max, inicio = inicio, exacto = exacto)

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
    warning("'queries_list' est\u00e1 vac\u00eda, no se realizar\u00e1 ninguna consulta.")
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
      warning(paste0("Consulta ", i, " en 'queries_list' para 'departamentos' contiene par\u00e1metro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Par\u00e1metros v\u00e1lidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }
  endpoint <- "departamentos"
  check_internet()
  query_batches <- create_query_batches(queries_list, param_name_for_sum = "max")
  if (length(query_batches) == 0 && length(queries_list) > 0) {
      warning(paste0("No se pudieron crear lotes de consultas para '", endpoint, "', aunque la lista de consultas no estaba vac\u00eda."), call. = FALSE)
      return(dplyr::tibble())
  } else if (length(query_batches) == 0) {
      return(dplyr::tibble())
  }
  all_batch_requests <- list()
  for (batch_idx in seq_along(query_batches)) {
    current_batch <- query_batches[[batch_idx]]
    request_obj <- prepare_post_batch_request(endpoint = endpoint, single_batch_queries_list = current_batch)
    all_batch_requests <- append(all_batch_requests, list(request_obj))
  }
  responses_or_errors <- httr2::req_perform_parallel(all_batch_requests, on_error = "return")
  final_results_list_of_tibbles <- list()
  has_errors <- FALSE
  for (i in seq_along(responses_or_errors)) {
    item <- responses_or_errors[[i]]
    num_queries_in_this_batch <- length(query_batches[[i]])
    if (inherits(item, "httr2_response")) {
      processed_tibble <- process_single_post_response(response_obj = item, endpoint = endpoint, num_queries_in_this_batch = num_queries_in_this_batch)
      final_results_list_of_tibbles <- append(final_results_list_of_tibbles, list(processed_tibble))
    } else if (inherits(item, "error")) {
      has_errors <- TRUE
      warning(paste0("Error en el lote ", i, " para '", endpoint, "': ", conditionMessage(item)), call. = FALSE)
    } else {
      has_errors <- TRUE
      warning(paste0("Error desconocido o respuesta inesperada en el lote ", i, " para '", endpoint, "'. Clase del objeto: ", class(item)[1]), call. = FALSE)
    }
  }
  combined_results <- dplyr::bind_rows(final_results_list_of_tibbles)
  if (nrow(combined_results) == 0 && length(queries_list) > 0 && !has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) devolvi\u00f3 una lista vac\u00eda o no se pudieron procesar los resultados, aunque no se reportaron errores directos en los lotes."), call. = FALSE)
  } else if (nrow(combined_results) == 0 && length(queries_list) > 0 && has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) no produjo resultados y se encontraron errores en algunos lotes."), call. = FALSE)
  }
  return(combined_results)
}

#' Normalizacion de direcciones
#'
#' Permite normalizar una dirección utilizando el listado de vías de circulación.
#' Realiza la consulta GET al endpoint /direcciones de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param direccion text Requerido. Direccion a normalizar, debe contener altura separada por espacio. (Ej: Colon 127)
#' @param provincia text Filtrar por nombre o ID de provincia.
#' @param departamento text Filtrar por nombre o ID de departamento.
#' @param localidad_censal text Filtrar por nombre o ID de localidad censal.
#' @param localidad text Filtrar por nombre o ID de localidad.
#' @param orden text Campo por el cual ordenar los resultados. (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver. La API limita a un máximo de 10 para este endpoint.
#' @param inicio integer Cantidad de resultados a omitir desde el principio.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname normalizar_direccion
#'
#' @references [georef-ar-api/direcciones](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_direcciones)
#' @return Un Data Frame con el listado normalizado de de direcciones
#' @examples
#' \dontrun{
#' normalizar_direccion(direccion = "Corrientes 1200, Rosario")
#' normalizar_direccion(direccion = "SAN MARTIN 100", provincia = "02", max = 5)
#' }

normalizar_direccion <- function(direccion, provincia = NULL, departamento = NULL, localidad_censal = NULL, localidad = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, inicio = NULL, exacto = NULL){
  args <- list(direccion = direccion, provincia = provincia, departamento = departamento, localidad_censal = localidad_censal, localidad = localidad, orden = orden, aplanar = aplanar, campos = campos, max = max, inicio = inicio, exacto = exacto)

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
    warning("'queries_list' est\u00e1 vac\u00eda, no se realizar\u00e1 ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("direccion", "provincia", "departamento", "localidad_censal", "localidad", "aplanar", "campos", "max", "inicio", "exacto")
  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    if (!"direccion" %in% names(query)) {
      stop(paste0("Consulta ", i, " en 'queries_list' para 'direcciones' debe contener un campo 'direccion'."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'direcciones' contiene par\u00e1metro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Par\u00e1metros v\u00e1lidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }
  endpoint <- "direcciones"
  check_internet()
  query_batches <- create_query_batches(queries_list, param_name_for_sum = "max")
  if (length(query_batches) == 0 && length(queries_list) > 0) {
      warning(paste0("No se pudieron crear lotes de consultas para '", endpoint, "', aunque la lista de consultas no estaba vac\u00eda."), call. = FALSE)
      return(dplyr::tibble())
  } else if (length(query_batches) == 0) {
      return(dplyr::tibble())
  }
  all_batch_requests <- list()
  for (batch_idx in seq_along(query_batches)) {
    current_batch <- query_batches[[batch_idx]]
    request_obj <- prepare_post_batch_request(endpoint = endpoint, single_batch_queries_list = current_batch)
    all_batch_requests <- append(all_batch_requests, list(request_obj))
  }
  responses_or_errors <- httr2::req_perform_parallel(all_batch_requests, on_error = "return")
  final_results_list_of_tibbles <- list()
  has_errors <- FALSE
  for (i in seq_along(responses_or_errors)) {
    item <- responses_or_errors[[i]]
    num_queries_in_this_batch <- length(query_batches[[i]])
    if (inherits(item, "httr2_response")) {
      processed_tibble <- process_single_post_response(response_obj = item, endpoint = endpoint, num_queries_in_this_batch = num_queries_in_this_batch)
      final_results_list_of_tibbles <- append(final_results_list_of_tibbles, list(processed_tibble))
    } else if (inherits(item, "error")) {
      has_errors <- TRUE
      warning(paste0("Error en el lote ", i, " para '", endpoint, "': ", conditionMessage(item)), call. = FALSE)
    } else {
      has_errors <- TRUE
      warning(paste0("Error desconocido o respuesta inesperada en el lote ", i, " para '", endpoint, "'. Clase del objeto: ", class(item)[1]), call. = FALSE)
    }
  }
  combined_results <- dplyr::bind_rows(final_results_list_of_tibbles)
  if (nrow(combined_results) == 0 && length(queries_list) > 0 && !has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) devolvi\u00f3 una lista vac\u00eda o no se pudieron procesar los resultados, aunque no se reportaron errores directos en los lotes."), call. = FALSE)
  } else if (nrow(combined_results) == 0 && length(queries_list) > 0 && has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) no produjo resultados y se encontraron errores en algunos lotes."), call. = FALSE)
  }
  return(combined_results)
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
#' @param interseccion text Geometría GeoJSON utilizada para filtrar resultados por intersección espacial. Sólo se soportan polígonos y multipolígonos. Ejemplo: polygon((-58.431,-34.592),(-58.430,-34.590),(-58.428,-34.593),(-58.431,-34.592)).
#' @param orden text Campo por el cual ordenar los resultados (por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver. La API limita a un máximo de 2000 para este endpoint.
#' @param inicio integer Cantidad de resultados a omitir desde el principio.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_localidades
#'
#' @references [georef-ar-api/localidades](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_localidades)
#' @return Un Data Frame con el listado de Localidades
#' @examples
#' \dontrun{
#' get_localidades(nombre = "PALERMO", provincia = "CIUDAD AUTONOMA DE BUENOS AIRES")
#' }

get_localidades <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, municipio = NULL, interseccion = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, inicio = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, municipio = municipio, interseccion = interseccion, orden = orden, aplanar = aplanar, campos = campos, max = max, inicio = inicio, exacto = exacto)

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
    warning("'queries_list' est\u00e1 vac\u00eda, no se realizar\u00e1 ninguna consulta.")
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
      warning(paste0("Consulta ", i, " en 'queries_list' para 'localidades' contiene par\u00e1metro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Par\u00e1metros v\u00e1lidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }
  endpoint <- "localidades"
  check_internet()
  query_batches <- create_query_batches(queries_list, param_name_for_sum = "max")
  if (length(query_batches) == 0 && length(queries_list) > 0) {
      warning(paste0("No se pudieron crear lotes de consultas para '", endpoint, "', aunque la lista de consultas no estaba vac\u00eda."), call. = FALSE)
      return(dplyr::tibble())
  } else if (length(query_batches) == 0) {
      return(dplyr::tibble())
  }
  all_batch_requests <- list()
  for (batch_idx in seq_along(query_batches)) {
    current_batch <- query_batches[[batch_idx]]
    request_obj <- prepare_post_batch_request(endpoint = endpoint, single_batch_queries_list = current_batch)
    all_batch_requests <- append(all_batch_requests, list(request_obj))
  }
  responses_or_errors <- httr2::req_perform_parallel(all_batch_requests, on_error = "return")
  final_results_list_of_tibbles <- list()
  has_errors <- FALSE
  for (i in seq_along(responses_or_errors)) {
    item <- responses_or_errors[[i]]
    num_queries_in_this_batch <- length(query_batches[[i]])
    if (inherits(item, "httr2_response")) {
      processed_tibble <- process_single_post_response(response_obj = item, endpoint = endpoint, num_queries_in_this_batch = num_queries_in_this_batch)
      final_results_list_of_tibbles <- append(final_results_list_of_tibbles, list(processed_tibble))
    } else if (inherits(item, "error")) {
      has_errors <- TRUE
      warning(paste0("Error en el lote ", i, " para '", endpoint, "': ", conditionMessage(item)), call. = FALSE)
    } else {
      has_errors <- TRUE
      warning(paste0("Error desconocido o respuesta inesperada en el lote ", i, " para '", endpoint, "'. Clase del objeto: ", class(item)[1]), call. = FALSE)
    }
  }
  combined_results <- dplyr::bind_rows(final_results_list_of_tibbles)
  if (nrow(combined_results) == 0 && length(queries_list) > 0 && !has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) devolvi\u00f3 una lista vac\u00eda o no se pudieron procesar los resultados, aunque no se reportaron errores directos en los lotes."), call. = FALSE)
  } else if (nrow(combined_results) == 0 && length(queries_list) > 0 && has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) no produjo resultados y se encontraron errores en algunos lotes."), call. = FALSE)
  }
  return(combined_results)
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
#' @param interseccion text Geometría GeoJSON utilizada para filtrar resultados por intersección espacial. Sólo se soportan polígonos y multipolígonos. Ejemplo: polygon((-58.431,-34.592),(-58.430,-34.590),(-58.428,-34.593),(-58.431,-34.592)).
#' @param orden text Campo por el cual ordenar los resultados. (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver. La API limita a un máximo de 2000 para este endpoint.
#' @param inicio integer Cantidad de resultados a omitir desde el principio.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_municipios
#'
#' @references [georef-ar-api/municipios](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_municipios)
#' @return Un Data Frame con el listado de Municipios
#' @examples
#' \dontrun{
#' get_municipios(provincia = "cordoba", max = 10)
#' }

get_municipios <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, interseccion = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, inicio = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, interseccion = interseccion, orden = orden, aplanar = aplanar, campos = campos, max = max, inicio = inicio, exacto = exacto)

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
    warning("'queries_list' est\u00e1 vac\u00eda, no se realizar\u00e1 ninguna consulta.")
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
      warning(paste0("Consulta ", i, " en 'queries_list' para 'municipios' contiene par\u00e1metro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Par\u00e1metros v\u00e1lidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }
  endpoint <- "municipios"
  check_internet()
  query_batches <- create_query_batches(queries_list, param_name_for_sum = "max")
  if (length(query_batches) == 0 && length(queries_list) > 0) {
      warning(paste0("No se pudieron crear lotes de consultas para '", endpoint, "', aunque la lista de consultas no estaba vac\u00eda."), call. = FALSE)
      return(dplyr::tibble())
  } else if (length(query_batches) == 0) {
      return(dplyr::tibble())
  }
  all_batch_requests <- list()
  for (batch_idx in seq_along(query_batches)) {
    current_batch <- query_batches[[batch_idx]]
    request_obj <- prepare_post_batch_request(endpoint = endpoint, single_batch_queries_list = current_batch)
    all_batch_requests <- append(all_batch_requests, list(request_obj))
  }
  responses_or_errors <- httr2::req_perform_parallel(all_batch_requests, on_error = "return")
  final_results_list_of_tibbles <- list()
  has_errors <- FALSE
  for (i in seq_along(responses_or_errors)) {
    item <- responses_or_errors[[i]]
    num_queries_in_this_batch <- length(query_batches[[i]])
    if (inherits(item, "httr2_response")) {
      processed_tibble <- process_single_post_response(response_obj = item, endpoint = endpoint, num_queries_in_this_batch = num_queries_in_this_batch)
      final_results_list_of_tibbles <- append(final_results_list_of_tibbles, list(processed_tibble))
    } else if (inherits(item, "error")) {
      has_errors <- TRUE
      warning(paste0("Error en el lote ", i, " para '", endpoint, "': ", conditionMessage(item)), call. = FALSE)
    } else {
      has_errors <- TRUE
      warning(paste0("Error desconocido o respuesta inesperada en el lote ", i, " para '", endpoint, "'. Clase del objeto: ", class(item)[1]), call. = FALSE)
    }
  }
  combined_results <- dplyr::bind_rows(final_results_list_of_tibbles)
  if (nrow(combined_results) == 0 && length(queries_list) > 0 && !has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) devolvi\u00f3 una lista vac\u00eda o no se pudieron procesar los resultados, aunque no se reportaron errores directos en los lotes."), call. = FALSE)
  } else if (nrow(combined_results) == 0 && length(queries_list) > 0 && has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) no produjo resultados y se encontraron errores en algunos lotes."), call. = FALSE)
  }
  return(combined_results)
}

#' Obtener Provincias
#'
#' Permite realizar búsquedas sobre el listado de provincias.
#' Realiza la consulta GET al endpoint /provincias de georef-ar-api.
#' Si existe GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.
#' @param id text Filtrar por ID.
#' @param nombre text Filtrar por Nombre.
#' @param interseccion text Geometría GeoJSON utilizada para filtrar resultados por intersección espacial. Sólo se soportan polígonos y multipolígonos. Ejemplo: polygon((-58.431,-34.592),(-58.430,-34.590),(-58.428,-34.593),(-58.431,-34.592)).
#' @param orden text Campo por el cual ordenar los resultados. (Por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver. La API limita a un máximo de 24 para este endpoint.
#' @param inicio integer Cantidad de resultados a omitir desde el principio.
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

get_provincias <- function(id = NULL, nombre = NULL, interseccion = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, inicio = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, interseccion = interseccion, orden = orden, aplanar = aplanar, campos = campos, max = max, inicio = inicio, exacto = exacto)

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
  if (!is.list(queries_list) || !all(sapply(queries_list, is.list))) {
    stop("'queries_list' debe ser una lista de listas (cada elemento debe ser una lista representando una consulta).")
  }
  if (length(queries_list) == 0) {
    warning("'queries_list' est\u00e1 vac\u00eda, no se realizar\u00e1 ninguna consulta.")
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
      warning(paste0("Consulta ", i, " en 'queries_list' para 'provincias' contiene par\u00e1metro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Par\u00e1metros v\u00e1lidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }
  endpoint <- "provincias"
  check_internet()
  query_batches <- create_query_batches(queries_list, param_name_for_sum = "max")
  if (length(query_batches) == 0 && length(queries_list) > 0) {
      warning(paste0("No se pudieron crear lotes de consultas para '", endpoint, "', aunque la lista de consultas no estaba vac\u00eda."), call. = FALSE)
      return(dplyr::tibble())
  } else if (length(query_batches) == 0) {
      return(dplyr::tibble())
  }
  all_batch_requests <- list()
  for (batch_idx in seq_along(query_batches)) {
    current_batch <- query_batches[[batch_idx]]
    request_obj <- prepare_post_batch_request(endpoint = endpoint, single_batch_queries_list = current_batch)
    all_batch_requests <- append(all_batch_requests, list(request_obj))
  }
  responses_or_errors <- httr2::req_perform_parallel(all_batch_requests, on_error = "return")
  final_results_list_of_tibbles <- list()
  has_errors <- FALSE
  for (i in seq_along(responses_or_errors)) {
    item <- responses_or_errors[[i]]
    num_queries_in_this_batch <- length(query_batches[[i]])
    if (inherits(item, "httr2_response")) {
      processed_tibble <- process_single_post_response(response_obj = item, endpoint = endpoint, num_queries_in_this_batch = num_queries_in_this_batch)
      final_results_list_of_tibbles <- append(final_results_list_of_tibbles, list(processed_tibble))
    } else if (inherits(item, "error")) {
      has_errors <- TRUE
      warning(paste0("Error en el lote ", i, " para '", endpoint, "': ", conditionMessage(item)), call. = FALSE)
    } else {
      has_errors <- TRUE
      warning(paste0("Error desconocido o respuesta inesperada en el lote ", i, " para '", endpoint, "'. Clase del objeto: ", class(item)[1]), call. = FALSE)
    }
  }
  combined_results <- dplyr::bind_rows(final_results_list_of_tibbles)
  if (nrow(combined_results) == 0 && length(queries_list) > 0 && !has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) devolvi\u00f3 una lista vac\u00eda o no se pudieron procesar los resultados, aunque no se reportaron errores directos en los lotes."), call. = FALSE)
  } else if (nrow(combined_results) == 0 && length(queries_list) > 0 && has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) no produjo resultados y se encontraron errores en algunos lotes."), call. = FALSE)
  }
  return(combined_results)
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
    warning("'queries_list' est\u00e1 vac\u00eda, no se realizar\u00e1 ninguna consulta.")
    return(dplyr::tibble())
  }

  valid_params <- c("lat", "lon", "aplanar", "campos")
  for (i in seq_along(queries_list)) {
    query <- queries_list[[i]]
    if (!is.list(query)) {
      stop(paste0("Elemento ", i, " en 'queries_list' no es una lista. Cada consulta debe ser una lista."))
    }
    if (!all(c("lat", "lon") %in% names(query))) {
      stop(paste0("Consulta ", i, " en 'queries_list' para 'ubicacion' debe contener los campos 'lat' y 'lon'."))
    }
    param_names <- names(query)
    invalid_params <- setdiff(param_names, valid_params)
    if (length(invalid_params) > 0) {
      warning(paste0("Consulta ", i, " en 'queries_list' para 'ubicacion' contiene par\u00e1metro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Par\u00e1metros v\u00e1lidos son: ", paste(valid_params, collapse = ", "), "."))
    }
  }

  endpoint <- "ubicacion"
  check_internet()
  
  # 'ubicacion' endpoint does not use 'max', so sum limit is not applicable based on 'max'. 
  # Batching will be based on query count only.
  query_batches <- create_query_batches(queries_list, param_name_for_sum = NULL)

  if (length(query_batches) == 0 && length(queries_list) > 0) {
      warning(paste0("No se pudieron crear lotes de consultas para '", endpoint, "', aunque la lista de consultas no estaba vac\u00eda."), call. = FALSE)
      return(dplyr::tibble())
  } else if (length(query_batches) == 0) {
      return(dplyr::tibble())
  }

  all_batch_requests <- list()
  for (batch_idx in seq_along(query_batches)) {
    current_batch <- query_batches[[batch_idx]]
    request_obj <- prepare_post_batch_request(endpoint = endpoint, single_batch_queries_list = current_batch)
    all_batch_requests <- append(all_batch_requests, list(request_obj))
  }

  responses_or_errors <- httr2::req_perform_parallel(all_batch_requests, on_error = "return")

  final_results_list_of_tibbles <- list()
  has_errors <- FALSE
  for (i in seq_along(responses_or_errors)) {
    item <- responses_or_errors[[i]]
    num_queries_in_this_batch <- length(query_batches[[i]])
    if (inherits(item, "httr2_response")) {
      processed_tibble <- process_single_post_response(response_obj = item, endpoint = endpoint, num_queries_in_this_batch = num_queries_in_this_batch)
      final_results_list_of_tibbles <- append(final_results_list_of_tibbles, list(processed_tibble))
    } else if (inherits(item, "error")) {
      has_errors <- TRUE
      warning(paste0("Error en el lote ", i, " para '", endpoint, "': ", conditionMessage(item)), call. = FALSE)
    } else {
      has_errors <- TRUE
      warning(paste0("Error desconocido o respuesta inesperada en el lote ", i, " para '", endpoint, "'. Clase del objeto: ", class(item)[1]), call. = FALSE)
    }
  }
  
  combined_results <- dplyr::bind_rows(final_results_list_of_tibbles)
  if (nrow(combined_results) == 0 && length(queries_list) > 0 && !has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) devolvi\u00f3 una lista vac\u00eda o no se pudieron procesar los resultados, aunque no se reportaron errores directos en los lotes."), call. = FALSE)
  } else if (nrow(combined_results) == 0 && length(queries_list) > 0 && has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) no produjo resultados y se encontraron errores en algunos lotes."), call. = FALSE)
  }
  return(combined_results)
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
#' @param interseccion text Geometría GeoJSON utilizada para filtrar resultados por intersección espacial. Sólo se soportan polígonos y multipolígonos. Ejemplo: polygon((-58.431,-34.592),(-58.430,-34.590),(-58.428,-34.593),(-58.431,-34.592)).
#' @param orden text Campo por el cual ordenar los resultados (por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver. La API limita a un máximo de 5000 para este endpoint.
#' @param inicio integer Cantidad de resultados a omitir desde el principio.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_localidades_censales
#'
#' @references [georef-ar-api/localidades-censales](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_localidades_censales)
#' @return Un Data Frame con el listado de Localidades Censales.
#' @examples
#' \dontrun{
#' get_localidades_censales(nombre = "VILLA GENERAL BELGRANO")
#' }

get_localidades_censales <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, municipio = NULL, interseccion = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, inicio = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, municipio = municipio, interseccion = interseccion, orden = orden, aplanar = aplanar, campos = campos, max = max, inicio = inicio, exacto = exacto)

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
    warning("'queries_list' est\u00e1 vac\u00eda, no se realizar\u00e1 ninguna consulta.")
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
      warning(paste0("Consulta ", i, " en 'queries_list' para 'localidades-censales' contiene par\u00e1metro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Par\u00e1metros v\u00e1lidos son: ", paste(valid_params, collapse = ", "), "."))
    }
    # GET /localidades-censales has max=5000. POST might differ or use global batch limits.
  }

  endpoint <- "localidades-censales"
  check_internet()
  
  query_batches <- create_query_batches(queries_list, param_name_for_sum = "max")
  if (length(query_batches) == 0 && length(queries_list) > 0) {
      warning(paste0("No se pudieron crear lotes de consultas para '", endpoint, "', aunque la lista de consultas no estaba vac\u00eda."), call. = FALSE)
      return(dplyr::tibble())
  } else if (length(query_batches) == 0) {
      return(dplyr::tibble())
  }

  all_batch_requests <- list()
  for (batch_idx in seq_along(query_batches)) {
    current_batch <- query_batches[[batch_idx]]
    request_obj <- prepare_post_batch_request(endpoint = endpoint, single_batch_queries_list = current_batch)
    all_batch_requests <- append(all_batch_requests, list(request_obj))
  }

  responses_or_errors <- httr2::req_perform_parallel(all_batch_requests, on_error = "return")

  final_results_list_of_tibbles <- list()
  has_errors <- FALSE
  for (i in seq_along(responses_or_errors)) {
    item <- responses_or_errors[[i]]
    num_queries_in_this_batch <- length(query_batches[[i]])
    if (inherits(item, "httr2_response")) {
      processed_tibble <- process_single_post_response(response_obj = item, endpoint = endpoint, num_queries_in_this_batch = num_queries_in_this_batch)
      final_results_list_of_tibbles <- append(final_results_list_of_tibbles, list(processed_tibble))
    } else if (inherits(item, "error")) {
      has_errors <- TRUE
      warning(paste0("Error en el lote ", i, " para '", endpoint, "': ", conditionMessage(item)), call. = FALSE)
    } else {
      has_errors <- TRUE
      warning(paste0("Error desconocido o respuesta inesperada en el lote ", i, " para '", endpoint, "'. Clase del objeto: ", class(item)[1]), call. = FALSE)
    }
  }

  combined_results <- dplyr::bind_rows(final_results_list_of_tibbles)
  if (nrow(combined_results) == 0 && length(queries_list) > 0 && !has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) devolvi\u00f3 una lista vac\u00eda o no se pudieron procesar los resultados, aunque no se reportaron errores directos en los lotes."), call. = FALSE)
  } else if (nrow(combined_results) == 0 && length(queries_list) > 0 && has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) no produjo resultados y se encontraron errores en algunos lotes."), call. = FALSE)
  }
  return(combined_results)
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
#' @param localidad_censal text Filtrar por nombre o ID de localidad censal. Se pueden especificar varios IDs separados por comas
#' @param interseccion text Geometría GeoJSON utilizada para filtrar resultados por intersección espacial. Sólo se soportan polígonos y multipolígonos. Ejemplo: polygon((-58.431,-34.592),(-58.430,-34.590),(-58.428,-34.593),(-58.431,-34.592)).
#' @param orden text Campo por el cual ordenar los resultados (por ID o nombre)
#' @param aplanar boolean Cuando está presente, muestra el resultado JSON con una estructura plana.
#' @param campos text Campos a incluir en la respuesta separados por comas, sin espacios. Algunos campos siempre serán incluidos, incluso si no se agregaron en la lista. Para incluir campos de sub-entidades, separar los nombres con un punto, por ejemplo: provincia.id.
#' @param max integer Cantidad máxima de resultados a devolver. La API limita a un máximo de 5000 para este endpoint.
#' @param inicio integer Cantidad de resultados a omitir desde el principio.
#' @param exacto boolean Cuando está presente, se activa el modo de búsqueda por texto exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto (por ejemplo, nombre).
#'
#' @export
#' @rdname get_asentamientos
#'
#' @references [georef-ar-api/asentamientos](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_asentamientos)
#' @return Un Data Frame con el listado de Asentamientos BAHRA.
#' @examples
#' \dontrun{
#' get_asentamientos(provincia = "22", departamento = "007")
#' }

get_asentamientos <- function(id = NULL, nombre = NULL, provincia = NULL, departamento = NULL, municipio = NULL, localidad_censal = NULL, interseccion = NULL, orden = NULL, aplanar = TRUE, campos = NULL, max = NULL, inicio = NULL, exacto = NULL){
  args <- list(id = id, nombre = nombre, provincia = provincia, departamento = departamento, municipio = municipio, localidad_censal = localidad_censal, interseccion = interseccion, orden = orden, aplanar = aplanar, campos = campos, max = max, inicio = inicio, exacto = exacto)

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
    warning("'queries_list' est\u00e1 vac\u00eda, no se realizar\u00e1 ninguna consulta.")
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
      warning(paste0("Consulta ", i, " en 'queries_list' para 'asentamientos' contiene par\u00e1metro(s) no reconocido(s): ",
                     paste(invalid_params, collapse = ", "), ". ",
                     "Par\u00e1metros v\u00e1lidos son: ", paste(valid_params, collapse = ", "), "."))
    }
    # GET /asentamientos has max=5000. POST might differ or use global batch limits.
  }

  endpoint <- "asentamientos"
  check_internet()
  
  query_batches <- create_query_batches(queries_list, param_name_for_sum = "max")
  if (length(query_batches) == 0 && length(queries_list) > 0) {
      warning(paste0("No se pudieron crear lotes de consultas para '", endpoint, "', aunque la lista de consultas no estaba vac\u00eda."), call. = FALSE)
      return(dplyr::tibble())
  } else if (length(query_batches) == 0) {
      return(dplyr::tibble())
  }

  # List to store httr2_request objects for each batch
  all_batch_requests <- list()
  for (batch_idx in seq_along(query_batches)) {
    current_batch <- query_batches[[batch_idx]]
    # Prepare the request object for the current batch
    request_obj <- prepare_post_batch_request(endpoint = endpoint, 
                                              single_batch_queries_list = current_batch)
    all_batch_requests <- append(all_batch_requests, list(request_obj))
  }

  # Perform all batch requests in parallel
  # on_error = "return" ensures that errors are returned as objects in the list
  # instead of stopping the whole operation.
  responses_or_errors <- httr2::req_perform_parallel(all_batch_requests, on_error = "return")

  # Process results
  final_results_list_of_tibbles <- list()
  has_errors <- FALSE
  for (i in seq_along(responses_or_errors)) {
    item <- responses_or_errors[[i]]
    num_queries_in_this_batch <- length(query_batches[[i]])

    if (inherits(item, "httr2_response")) {
      # If req_error was set up correctly, non-200 status should have been converted to an R error object by httr2
      # So, if we get an httr2_response, it *should* be a successful one (status 200)
      # However, an extra check for status doesn't hurt, or rely on req_error having done its job.
      # For now, assume if it's an httr2_response, it's good to process based on req_error setup.
      processed_tibble <- process_single_post_response(response_obj = item, 
                                                       endpoint = endpoint, 
                                                       num_queries_in_this_batch = num_queries_in_this_batch)
      final_results_list_of_tibbles <- append(final_results_list_of_tibbles, list(processed_tibble))
    } else if (inherits(item, "error")) {
      # This is an error object, possibly from httr2_error_handler via req_error,
      # or a lower-level error from httr2/curl during the request that req_perform_parallel caught.
      has_errors <- TRUE
      warning(paste0("Error en el lote ", i, " para '", endpoint, "': ", conditionMessage(item)), call. = FALSE)
    } else {
      # Unexpected item type
      has_errors <- TRUE
      warning(paste0("Error desconocido o respuesta inesperada en el lote ", i, " para '", endpoint, "'. Clase del objeto: ", class(item)[1]), call. = FALSE)
    }
  }

  # Combine all tibbles from successfully processed batches
  combined_results <- dplyr::bind_rows(final_results_list_of_tibbles)

  if (nrow(combined_results) == 0 && length(queries_list) > 0 && !has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) devolvi\u00f3 una lista vac\u00eda o no se pudieron procesar los resultados, aunque no se reportaron errores directos en los lotes."), call. = FALSE)
  } else if (nrow(combined_results) == 0 && length(queries_list) > 0 && has_errors) {
    warning(paste0("La consulta POST completa para '", endpoint, "' (", length(queries_list)," consultas originales en ", length(query_batches)," lotes) no produjo resultados y se encontraron errores en algunos lotes."), call. = FALSE)
  }
  
  return(combined_results)
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
    stop(paste("Entidad no v\u00e1lida. Opciones disponibles:", paste(valid_entidades, collapse = ", ")))
  }
  if (!formato %in% valid_formatos) {
    stop(paste("Formato no v\u00e1lido. Opciones disponibles:", paste(valid_formatos, collapse = ", ")))
  }

  filename <- paste0(entidad, ".", formato)
  url <- paste0(base_url, filename)

  token <- Sys.getenv("GEOREFAR_TOKEN")

  req <- httr2::request(url) |>
    httr2::req_error(is_error = ~ httr2::resp_status(.x) != 200, body = httr2_error_handler)

  if (!is.null(token) && token != "") {
    req <- req |> httr2::req_auth_bearer_token(token)
  }

  # Use req_perform() for synchronous behavior
  response <- httr2::req_perform(req)

  if (!is.null(path_to_save)) {
    assertthat::assert_that(is.character(path_to_save), length(path_to_save) == 1,
                            msg = "'path_to_save' debe ser una cad\u00e1na de texto con la ruta del archivo.")
    tryCatch({
      writeBin(httr2::resp_body_raw(response), path_to_save)
      message(paste("Archivo guardado en:", path_to_save))
      return(invisible(path_to_save))
    }, error = function(e) {
      stop(paste("Error al guardar el archivo:", e$message))
    })
  } else {
    # Si no se guarda, intentar parsear según el formato
    # For JSON types, httr2::resp_body_json could be used directly.
    # For CSV, httr2::resp_body_string then read.csv is fine.
    # For consistency with original logic, use resp_body_string and then parse.
    content_text <- httr2::resp_body_string(response, encoding = "UTF-8")
    if (formato == "csv") {
      # httr2 can also directly parse CSVs with resp_body_csv(), but this is fine
      return(utils::read.csv(text = content_text, stringsAsFactors = FALSE))
    } else if (formato %in% c("json", "geojson", "ndjson")) {
      # httr2::resp_body_json(response, flatten = TRUE) could be used here too
      return(jsonlite::fromJSON(content_text, flatten = TRUE))
    } else {
      # This case should ideally not be reached due to prior format validation
      warning("Formato no soportado para parseo directo, devolviendo contenido como texto.", call. = FALSE)
      return(content_text)
    }
  }
}
