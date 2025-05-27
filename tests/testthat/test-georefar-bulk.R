library(testthat)
# library(httptest2) # Would be used for actual API call mocking

# Source the functions to be tested if not in a package context during testing
# source("../../R/georefar.R") 

context("Bulk POST operations for georef-ar API")

# --- Tests for post_provincias_bulk --- 


test_that("post_provincias_bulk stops for invalid queries_list structure", {
  expect_error(post_provincias_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")

  expect_error(post_provincias_bulk(queries_list = list("not a list inside")),
               "'queries_list' debe ser una lista de listas.")

  # Valid and invalid in the same list of queries
  expect_error(post_provincias_bulk(queries_list = list(
    list(nombre = "Mendoza"), # Valid
    list(id = "02", foo = "bar")    # Invalid
  )),
   "La consulta contiene parametros invalidos")
})

test_that("post_provincias_bulk warns for empty queries_list", {
  expect_warning(post_provincias_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})


# --- Tests for post_departamentos_bulk --- 

test_that("post_departamentos_bulk stops for invalid queries_list structure", {
  expect_error(post_departamentos_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")

  expect_error(post_departamentos_bulk(queries_list = list("not a list inside")),
               "'queries_list' debe ser una lista de listas.")

  # Valid and invalid in the same list of queries
  expect_error(
    expect_warning(
      post_departamentos_bulk(queries_list = list(
        list(nombre = "Valido Depto"), # Valid
        list(id = "02", foo = "bar")    # Invalid
      )),
      regexp = "Consulta 2 en 'queries_list' para 'departamentos' contiene parámetro\\(s\\) no reconocido\\(s\\): foo\\. Parámetros válidos son: id, nombre, provincia, interseccion, orden, aplanar, campos, max, inicio, exacto\\."
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})

test_that("post_departamentos_bulk warns for empty queries_list", {
  expect_warning(post_departamentos_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_departamentos_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_error(
    expect_warning(
      post_departamentos_bulk(queries_list = list(list(nombre = "Rosario", invalid_param = "foo"))),
      regexp = paste0("Consulta 1 en 'queries_list' para 'departamentos' contiene parámetro\\(s\\) no reconocido\\(s\\): invalid_param\\. Parámetros válidos son: ", valid_params_string, "\\.")
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})


# --- Tests for post_municipios_bulk --- 

test_that("post_municipios_bulk stops for invalid queries_list structure", {
  expect_error(post_municipios_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")

  expect_error(post_municipios_bulk(queries_list = list("not a list inside")),
               "'queries_list' debe ser una lista de listas.")

  # Valid and invalid in the same list of queries
  expect_error(
    expect_warning(
      post_municipios_bulk(queries_list = list(
        list(nombre = "Valido Municipio"), # Valid
        list(id = "02", foo = "bar")    # Invalid
      )),
      regexp = "Consulta 2 en 'queries_list' para 'municipios' contiene parámetro\\(s\\) no reconocido\\(s\\): foo\\. Parámetros válidos son: id, nombre, provincia, departamento, interseccion, orden, aplanar, campos, max, inicio, exacto\\."
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})

test_that("post_municipios_bulk warns for empty queries_list", {
  expect_warning(post_municipios_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_municipios_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, departamento, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_error(
    expect_warning(
      post_municipios_bulk(queries_list = list(list(nombre = "La Plata", invalid_param = "foo"))),
      regexp = paste0("Consulta 1 en 'queries_list' para 'municipios' contiene parámetro\\(s\\) no reconocido\\(s\\): invalid_param\\. Parámetros válidos son: ", valid_params_string, "\\.")
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})


# --- Tests for post_localidades_bulk --- 

test_that("post_localidades_bulk stops for invalid queries_list structure", {
  expect_error(post_localidades_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")

  expect_error(post_localidades_bulk(queries_list = list("not a list inside")),
               "'queries_list' debe ser una lista de listas.")

  # Valid and invalid in the same list of queries
  expect_error(
    expect_warning(
      post_localidades_bulk(queries_list = list(
        list(nombre = "Valida Localidad"), # Valid
        list(id = "02", foo = "bar")    # Invalid
      )),
      regexp = "Consulta 2 en 'queries_list' para 'localidades' contiene parámetro\\(s\\) no reconocido\\(s\\): foo\\. Parámetros válidos son: id, nombre, provincia, departamento, municipio, interseccion, orden, aplanar, campos, max, inicio, exacto\\."
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})

test_that("post_localidades_bulk warns for empty queries_list", {
  expect_warning(post_localidades_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_localidades_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, departamento, municipio, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_error(
    expect_warning(
      post_localidades_bulk(queries_list = list(list(nombre = "Merlo", invalid_param = "foo"))),
      regexp = paste0("Consulta 1 en 'queries_list' para 'localidades' contiene parámetro\\(s\\) no reconocido\\(s\\): invalid_param\\. Parámetros válidos son: ", valid_params_string, "\\.")
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})


# --- Tests for post_localidades_censales_bulk --- 

test_that("post_localidades_censales_bulk stops for invalid queries_list structure", {
  expect_error(post_localidades_censales_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")

  expect_error(post_localidades_censales_bulk(queries_list = list("not a list inside")),
               "'queries_list' debe ser una lista de listas.")

  # Valid and invalid in the same list of queries
  expect_error(
    expect_warning(
      post_localidades_censales_bulk(queries_list = list(
        list(nombre = "Valida Loc Censal"), # Valid
        list(id = "02", foo = "bar")    # Invalid
      )),
      regexp = "Consulta 2 en 'queries_list' para 'localidades-censales' contiene parámetro\\(s\\) no reconocido\\(s\\): foo\\. Parámetros válidos son: id, nombre, provincia, departamento, municipio, interseccion, orden, aplanar, campos, max, inicio, exacto\\."
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})

test_that("post_localidades_censales_bulk warns for empty queries_list", {
  expect_warning(post_localidades_censales_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_localidades_censales_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, departamento, municipio, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_error(
    expect_warning(
      post_localidades_censales_bulk(queries_list = list(list(nombre = "AGUA DE ORO", invalid_param = "foo"))),
      regexp = paste0("Consulta 1 en 'queries_list' para 'localidades-censales' contiene parámetro\\(s\\) no reconocido\\(s\\): invalid_param\\. Parámetros válidos son: ", valid_params_string, "\\.")
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})


# --- Tests for post_asentamientos_bulk --- 

test_that("post_asentamientos_bulk stops for invalid queries_list structure", {
  expect_error(post_asentamientos_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")

  expect_error(post_asentamientos_bulk(queries_list = list("not a list inside")),
               "'queries_list' debe ser una lista de listas.")

  # Valid and invalid in the same list of queries
  expect_error(
    expect_warning(
      post_asentamientos_bulk(queries_list = list(
        list(nombre = "Valido Asentamiento"), # Valid
        list(id = "02", foo = "bar")    # Invalid
      )),
      regexp = "Consulta 2 en 'queries_list' para 'asentamientos' contiene parámetro\\(s\\) no reconocido\\(s\\): foo\\. Parámetros válidos son: id, nombre, provincia, departamento, municipio, localidad_censal, interseccion, orden, aplanar, campos, max, inicio, exacto\\."
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})

test_that("post_asentamientos_bulk warns for empty queries_list", {
  expect_warning(post_asentamientos_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_asentamientos_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, provincia, departamento, municipio, localidad_censal, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_error(
    expect_warning(
      post_asentamientos_bulk(queries_list = list(list(nombre = "COSTA", invalid_param = "foo"))),
      regexp = paste0("Consulta 1 en 'queries_list' para 'asentamientos' contiene parámetro\\(s\\) no reconocido\\(s\\): invalid_param\\. Parámetros válidos son: ", valid_params_string, "\\.")
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})


# --- Tests for post_calles_bulk --- 

test_that("post_calles_bulk stops for invalid queries_list structure", {
  expect_error(post_calles_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")

  expect_error(post_calles_bulk(queries_list = list("not a list inside")),
               "'queries_list' debe ser una lista de listas.")

  # Valid and invalid in the same list of queries
  expect_error(
    expect_warning(
      post_calles_bulk(queries_list = list(
        list(nombre = "Valida Calle"), # Valid
        list(id = "02", foo = "bar")    # Invalid
      )),
      regexp = "Consulta 2 en 'queries_list' para 'calles' contiene parámetro\\(s\\) no reconocido\\(s\\): foo\\. Parámetros válidos son: id, nombre, tipo, provincia, departamento, municipio, localidad_censal, categoria, interseccion, orden, aplanar, campos, max, inicio, exacto\\."
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})

test_that("post_calles_bulk warns for empty queries_list", {
  expect_warning(post_calles_bulk(queries_list = list()),
                 "'queries_list' está vacía, no se realizará ninguna consulta.")
})

test_that("post_calles_bulk warns for invalid parameter names in a query", {
  valid_params_string <- "id, nombre, tipo, provincia, departamento, municipio, localidad_censal, categoria, interseccion, orden, aplanar, campos, max, inicio, exacto"
  expect_error(
    expect_warning(
      post_calles_bulk(queries_list = list(list(nombre = "SAN MARTIN", invalid_param = "foo"))),
      regexp = paste0("Consulta 1 en 'queries_list' para 'calles' contiene parámetro\\(s\\) no reconocido\\(s\\): invalid_param\\. Parámetros válidos son: ", valid_params_string, "\\.")
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
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
               "'queries_list' debe ser una lista de listas.")

  # Valid and invalid in the same list of queries
  expect_error(
    expect_warning(
      post_direcciones_bulk(queries_list = list(
        list(direccion = "CALLE VALIDA 123"), # Valid
        list(direccion = "OTRA CALLE 456", foo = "bar")    # Invalid
      )),
      regexp = "Consulta 2 en 'queries_list' para 'direcciones' contiene parámetro\\(s\\) no reconocido\\(s\\): foo\\. Parámetros válidos son: direccion, provincia, departamento, localidad_censal, localidad, aplanar, campos, max, inicio, exacto\\."
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
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
  expect_error(
    expect_warning(
      post_direcciones_bulk(queries_list = list(list(direccion = "MAIPU 100", invalid_param = "foo"))),
      regexp = paste0("Consulta 1 en 'queries_list' para 'direcciones' contiene parámetro\\(s\\) no reconocido\\(s\\): invalid_param\\. Parámetros válidos son: ", valid_params_string, "\\.")
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
})


# --- Tests for post_ubicacion_bulk --- 

test_that("post_ubicacion_bulk stops for invalid queries_list structure", {
  expect_error(post_ubicacion_bulk(queries_list = "not a list"),
               "'queries_list' debe ser una lista de listas.")

  expect_error(post_ubicacion_bulk(queries_list = list("not a list inside")),
               "'queries_list' debe ser una lista de listas.")

  # Valid and invalid in the same list of queries
  expect_error(
    expect_warning(
      post_ubicacion_bulk(queries_list = list(
        list(lat = -34.0, lon = -58.0), # Valid
        list(lat = -35.0, lon = -59.0, foo = "bar")    # Invalid
      )),
      regexp = "Consulta 2 en 'queries_list' para 'ubicacion' contiene parámetro\\(s\\) no reconocido\\(s\\): foo\\. Parámetros válidos son: lat, lon, aplanar, campos\\."
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
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
  expect_error(
    expect_warning(
      post_ubicacion_bulk(queries_list = list(list(lat = -34, lon = -58, invalid_param = "foo"))),
      regexp = paste0("Consulta 1 en 'queries_list' para 'ubicacion' contiene parámetro\\(s\\) no reconocido\\(s\\): invalid_param\\. Parámetros válidos son: ", valid_params_string, "\\.")
    ),
    regexp = "Test `~\\.x == 200` on `httr::status_code\\(res\\)` returned an error\\."
  )
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

