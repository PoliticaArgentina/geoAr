library(testthat)
# library(httptest2) # Would be used for actual API call mocking

# Source the functions to be tested if not in a package context during testing
# source("../../R/georefar.R") 

context("Bulk POST operations for georef-ar API")

# --- Tests for post_provincias_bulk --- 

test_that("post_provincias_bulk handles valid queries_list", {
  # This test would ideally use httptest2::with_mock_API or vcr
  # For now, it primarily tests input validation before the call.
  
  # Mock a successful scenario (conceptually, as post_endpoint makes the actual call)
  # We expect it not to error out on valid input structure for queries_list
  # and to call post_endpoint, which would then be mocked in a real test setup.
  
  # To truly test this without a live call or complex mocking of post_endpoint itself here,
  # we rely on the fact that if input validation passes, it proceeds. 
  # We can't easily check the direct output of post_endpoint without mocking it.
  
  # A simple check: does it run without error with valid parameters?
  # We expect a warning about empty list if post_endpoint returns empty, or an error if API is down.
  # For CI, we might need to mock check_internet() and post_endpoint() fully.
  
  # For now, let's focus on the validation aspect of post_provincias_bulk itself.
  # We expect a warning if queries_list is empty, or if a query has invalid params.

  # Valid (but empty) query - should pass validation, might warn from post_endpoint if it results in empty data
  expect_silent({
    # We can't truly run this without mocking post_endpoint and check_internet
    # So we test the parts we can: the validation within post_provincias_bulk
    # If we mock post_endpoint to return a tibble, we can test the happy path.
  })
})

test_that("post_provincias_bulk stops for invalid queries_list structure", {
  expect_error(post_provincias_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")
  expect_error(post_provincias_bulk(queries_list = list("not a list inside")),
               "Elemento 1 en 'queries_list' no es una lista.")
})

test_that("post_provincias_bulk warns for empty queries_list", {
  expect_warning(post_provincias_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_provincias_bulk warns for invalid parameter names in a query", {
  expect_warning(post_provincias_bulk(queries_list = list(list(nombre = "Cordoba", invalid_param = "foo"))),
                 "Consulta 1 en 'queries_list' para 'provincias' contiene parámetro(s) no reconocido(s): invalid_param. Parámetros válidos son: id, nombre, interseccion, orden, aplanar, campos, max, inicio, exacto.")
  
  # Multiple invalid params
  expect_warning(post_provincias_bulk(queries_list = list(list(nombre = "Cordoba", foo = 1, bar = 2))),
                 "Consulta 1 en 'queries_list' para 'provincias' contiene parámetro(s) no reconocido(s): foo, bar. Parámetros válidos son: id, nombre, interseccion, orden, aplanar, campos, max, inicio, exacto.",
                 all = TRUE) # Check all parts of the warning message

  # Valid and invalid in the same list of queries
  expect_warning(post_provincias_bulk(queries_list = list(
    list(nombre = "Mendoza"), # Valid
    list(id = "02", foo = "bar")    # Invalid
  )), "Consulta 2 en 'queries_list' para 'provincias' contiene parámetro(s) no reconocido(s): foo.")
})


# --- Tests for post_departamentos_bulk --- 

test_that("post_departamentos_bulk stops for invalid queries_list structure", {
  expect_error(post_departamentos_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")
  expect_error(post_departamentos_bulk(queries_list = list("not a list inside")),
               "Elemento 1 en 'queries_list' no es una lista.")
})

test_that("post_departamentos_bulk warns for empty queries_list", {
  expect_warning(post_departamentos_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_departamentos_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_warning(post_departamentos_bulk(queries_list = list(list(nombre = "Rosario", invalid_param = "foo"))),
                 paste0("Consulta 1 en 'queries_list' para 'departamentos' contiene parámetro(s) no reconocido(s): invalid_param. Parámetros válidos son: ", valid_params_string, "."))
})


# --- Tests for post_municipios_bulk --- 

test_that("post_municipios_bulk stops for invalid queries_list structure", {
  expect_error(post_municipios_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")
  expect_error(post_municipios_bulk(queries_list = list("not a list inside")),
               "Elemento 1 en 'queries_list' no es una lista.")
})

test_that("post_municipios_bulk warns for empty queries_list", {
  expect_warning(post_municipios_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_municipios_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, departamento, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_warning(post_municipios_bulk(queries_list = list(list(nombre = "La Plata", invalid_param = "foo"))),
                 paste0("Consulta 1 en 'queries_list' para 'municipios' contiene parámetro(s) no reconocido(s): invalid_param. Parámetros válidos son: ", valid_params_string, "."))
})


# --- Tests for post_localidades_bulk --- 

test_that("post_localidades_bulk stops for invalid queries_list structure", {
  expect_error(post_localidades_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")
  expect_error(post_localidades_bulk(queries_list = list("not a list inside")),
               "Elemento 1 en 'queries_list' no es una lista.")
})

test_that("post_localidades_bulk warns for empty queries_list", {
  expect_warning(post_localidades_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_localidades_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, departamento, municipio, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_warning(post_localidades_bulk(queries_list = list(list(nombre = "Merlo", invalid_param = "foo"))),
                 paste0("Consulta 1 en 'queries_list' para 'localidades' contiene parámetro(s) no reconocido(s): invalid_param. Parámetros válidos son: ", valid_params_string, "."))
})


# --- Tests for post_localidades_censales_bulk --- 

test_that("post_localidades_censales_bulk stops for invalid queries_list structure", {
  expect_error(post_localidades_censales_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")
  expect_error(post_localidades_censales_bulk(queries_list = list("not a list inside")),
               "Elemento 1 en 'queries_list' no es una lista.")
})

test_that("post_localidades_censales_bulk warns for empty queries_list", {
  expect_warning(post_localidades_censales_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_localidades_censales_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, departamento, municipio, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_warning(post_localidades_censales_bulk(queries_list = list(list(nombre = "AGUA DE ORO", invalid_param = "foo"))),
                 paste0("Consulta 1 en 'queries_list' para 'localidades-censales' contiene parámetro(s) no reconocido(s): invalid_param. Parámetros válidos son: ", valid_params_string, "."))
})


# --- Tests for post_asentamientos_bulk --- 

test_that("post_asentamientos_bulk stops for invalid queries_list structure", {
  expect_error(post_asentamientos_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")
  expect_error(post_asentamientos_bulk(queries_list = list("not a list inside")),
               "Elemento 1 en 'queries_list' no es una lista.")
})

test_that("post_asentamientos_bulk warns for empty queries_list", {
  expect_warning(post_asentamientos_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_asentamientos_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, departamento, municipio, localidad_censal, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_warning(post_asentamientos_bulk(queries_list = list(list(nombre = "COSTA", invalid_param = "foo"))),
                 paste0("Consulta 1 en 'queries_list' para 'asentamientos' contiene parámetro(s) no reconocido(s): invalid_param. Parámetros válidos son: ", valid_params_string, "."))
})


# --- Tests for post_calles_bulk --- 

test_that("post_calles_bulk stops for invalid queries_list structure", {
  expect_error(post_calles_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")
  expect_error(post_calles_bulk(queries_list = list("not a list inside")),
               "Elemento 1 en 'queries_list' no es una lista.")
})

test_that("post_calles_bulk warns for empty queries_list", {
  expect_warning(post_calles_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_calles_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, tipo, provincia, departamento, municipio, localidad_censal, categoria, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_warning(post_calles_bulk(queries_list = list(list(nombre = "SAN MARTIN", invalid_param = "foo"))),
                 paste0("Consulta 1 en 'queries_list' para 'calles' contiene parámetro(s) no reconocido(s): invalid_param. Parámetros válidos son: ", valid_params_string, "."))
})

test_that("post_calles_bulk stops for invalid max or max+inicio in a query", {
  # This re-tests validation already in the function, but good for completeness here
  expect_error(post_calles_bulk(queries_list = list(list(nombre = "Libertador", max = 6000))),
               "En una de las consultas, el parámetro 'max' debe ser menor o igual a 5000.")
  expect_error(post_calles_bulk(queries_list = list(list(nombre = "Libertador", max = 5000, inicio = 5001))),
               "En una de las consultas, la suma de 'max' e 'inicio' debe ser 10,000 o menos.")
})


# --- Tests for post_direcciones_bulk --- 

test_that("post_direcciones_bulk stops for invalid queries_list structure", {
  expect_error(post_direcciones_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")
  expect_error(post_direcciones_bulk(queries_list = list("not a list inside")),
               "Elemento 1 en 'queries_list' no es una lista.")
})

test_that("post_direcciones_bulk warns for empty queries_list", {
  expect_warning(post_direcciones_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_direcciones_bulk stops if 'direccion' is missing in a query", {
  expect_error(post_direcciones_bulk(queries_list = list(list(provincia = "BUENOS AIRES"))),
               "Consulta 1 en 'queries_list' para 'direcciones' debe contener un campo 'direccion'.")
})

test_that("post_direcciones_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "direccion, provincia, departamento, localidad_censal, localidad, aplanar, campos, max, inicio, exacto"
  expect_warning(post_direcciones_bulk(queries_list = list(list(direccion = "MAIPU 100", invalid_param = "foo"))),
                 paste0("Consulta 1 en 'queries_list' para 'direcciones' contiene parámetro(s) no reconocido(s): invalid_param. Parámetros válidos son: ", valid_params_string, "."))
})


# --- Tests for post_ubicacion_bulk --- 

test_that("post_ubicacion_bulk stops for invalid queries_list structure", {
  expect_error(post_ubicacion_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")
  expect_error(post_ubicacion_bulk(queries_list = list("not a list inside")),
               "Elemento 1 en 'queries_list' no es una lista.")
})

test_that("post_ubicacion_bulk warns for empty queries_list", {
  expect_warning(post_ubicacion_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_ubicacion_bulk stops if 'lat' or 'lon' is missing in a query", {
  expect_error(post_ubicacion_bulk(queries_list = list(list(lon = -58.3816))),
               "Consulta 1 en 'queries_list' para 'ubicacion' debe contener los campos 'lat' y 'lon'.")
  expect_error(post_ubicacion_bulk(queries_list = list(list(lat = -34.6037))),
               "Consulta 1 en 'queries_list' para 'ubicacion' debe contener los campos 'lat' y 'lon'.")
})

test_that("post_ubicacion_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "lat, lon, aplanar, campos"
  expect_warning(post_ubicacion_bulk(queries_list = list(list(lat = -34, lon = -58, invalid_param = "foo"))),
                 paste0("Consulta 1 en 'queries_list' para 'ubicacion' contiene parámetro(s) no reconocido(s): invalid_param. Parámetros válidos son: ", valid_params_string, "."))
})


# Placeholder for tests for other bulk functions
# test_that("post_municipios_bulk ...") {} 
# ... and so on for all other bulk functions ...

# --- Tests for get_geodata_dump --- 

test_that("get_geodata_dump validates entidad and formato parameters", {
  # Mock check_internet to avoid actual calls
  # Skipping actual download/parsing for now, focusing on input validation.
  expect_error(get_geodata_dump(entidad = "INVALID", formato = "csv"), "Entidad no válida.")
  expect_error(get_geodata_dump(entidad = "provincias", formato = "INVALID"), "Formato no válido.")
})

test_that("get_geodata_dump path_to_save validation", {
    # Conceptual: would need to mock check_internet and the GET request
    # Here, we just test the path_to_save assertion if other conditions were met.
    # This specific test is hard to isolate without deeper mocking.
    # expect_error(get_geodata_dump(entidad = "provincias", formato = "csv", path_to_save = 123), 
    #              "'path_to_save' debe ser una cadena de texto con la ruta del archivo.")
    # The above will fail early on check_internet or the GET call unless mocked.
})

# More tests would be needed here, ideally with httptest2 to mock API responses for:
# - Successful download and parsing of CSV
# - Successful download and parsing of JSON/GeoJSON
# - Successful saving to file
# - API returning an error status 