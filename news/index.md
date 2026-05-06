# Changelog

## geoAr 1.2.1

### Fix bug

Added lang = es,en to DESCRITPION file to solve Nu HTML checker problem
as requested by CRAN Team

## geoAr 1.2.0

### Major Refactoring & Performance Improvements

- **Switched to `httr2` for API Communication**: All functions
  interacting with the `georef-ar-api` (including `get_endpoint`, all
  `post_*_bulk` functions, and `get_geodata_dump`) have been refactored
  to use the `httr2` package instead of `httr`. This provides a more
  modern and flexible HTTP client interface.
- **Asynchronous Parallel Batch Requests**: The `post_*_bulk` functions
  now leverage `httr2` and `promises` to perform multiple batch requests
  to the API in parallel. This significantly improves performance when
  submitting a large number of queries (e.g., thousands of addresses for
  normalization) by:
  - Automatically splitting large query lists into smaller batches that
    respect the API’s limits (max 1000 queries per batch, sum of `max`
    parameters \<= 5000 per batch).
  - Executing these batches concurrently.
- **Enhanced Error Handling**: Implemented more robust error handling
  for API requests using `httr2`’s mechanisms, providing clearer error
  messages.
- **Dependency Changes**:
  - Removed `httr` from Imports.
  - Added `httr2 (>= 1.0.0)` and `promises (>= 1.2.0)` to Imports.

### Internal Changes

- Removed the internal `post_endpoint()` function; its logic is now
  integrated into new helper functions (`create_query_batches`,
  `execute_post_batch_promise`, `process_single_post_response`) that
  manage batching and parallel execution for POST requests.

## geoAr 1.1.0

### New Features

- Added a suite of `post_*_bulk()` functions
  ([`post_provincias_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_provincias_bulk.md),
  [`post_departamentos_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_departamentos_bulk.md),
  [`post_municipios_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_municipios_bulk.md),
  [`post_localidades_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_localidades_bulk.md),
  [`post_localidades_censales_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_localidades_censales_bulk.md),
  [`post_asentamientos_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_asentamientos_bulk.md),
  [`post_calles_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_calles_bulk.md),
  [`post_direcciones_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_direcciones_bulk.md),
  [`post_ubicacion_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_ubicacion_bulk.md))
  to allow for bulk queries to the corresponding `georef-ar-api`
  endpoints using the POST method. This enables sending multiple queries
  in a single API request.
- Each `post_*_bulk()` function includes enhanced input validation for
  the `queries_list` parameter, providing warnings for unrecognized
  parameter names within individual queries to guide correct usage.
- Added
  [`get_geodata_dump()`](https://politicaargentina.github.io/geoAr/reference/get_geodata_dump.md)
  function to download entire datasets (provincias, departamentos, etc.)
  in various formats (CSV, JSON, GeoJSON, NDJSON) directly from the API.
- The
  [`get_calles()`](https://politicaargentina.github.io/geoAr/reference/get_calles.md)
  function now exclusively handles GET requests; POST functionality for
  streets is managed by
  [`post_calles_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_calles_bulk.md).

### Fixes & Improvements

- The base API URL used by `R/georefar.R` functions has been updated
  from HTTP to HTTPS (`https://apis.datos.gob.ar/georef/api/`) for
  secure communication.
- Refactored the internal `post_endpoint()` function to correctly
  construct JSON bodies for POST requests and parse API responses,
  especially for bulk operations.
- Added comprehensive unit tests for input validation of all new
  `post_*_bulk()` functions and basic validation for
  [`get_geodata_dump()`](https://politicaargentina.github.io/geoAr/reference/get_geodata_dump.md).

## geoAr 1.0.0

CRAN release: 2024-03-03

Ready for CRAN re submission

## geoAr (development version) 0.0.1.5.0.0

- Fix [\#21](https://github.com/PoliticaArgentina/geoAr/issues/21) :
  `PKGNAME` tag

## geoAr 0.0.1.4.3.1

- Changes `stop` for `warning` in `get_endpoint()` internal function for
  `georefar` family functions.

## geoAr 0.0.1.4.3

Modify `georefar get_*` family functions:

- Add `TOKEN` workflow alternative (documented)
- Add new endpoints: `get_asentamientos` & `get_asentamientos`

## geoAr 0.0.1.4.2.1

Added [georefar](https://github.com/pdelboca/georefar) (R wrapper for
georef-ar API) `get_*` family functions

## geoAr 0.0.1.4.2

CRAN release: 2023-02-25

First CRAN version

## geoAr 0.0.1.4

[`get_bahra()`](https://politicaargentina.github.io/geoAr/reference/get_bahra.md)
function for a new data source: *Base de Asentamientos Humanos de la
República Argentina* (BAHRA)

## geoAr 0.0.1.3.1

Added `levels` (`envolvente`, `radios` and `entidades`) & `centroid`
(`FALSE/TRUE`) param to
[`get_eph()`](https://politicaargentina.github.io/geoAr/reference/get_eph.md)
function.

## geoAr 0.0.1.3

Added new features

- Geometry reconstruction for every CENSO (1869 - 2010). Example: Using
  `get_censo(censo = "1991", simplified = T)`.

- Permanent Household Survey (Encuesta Permanente de Hogares - EPH)
  Urban Aglomerations Geometries. Using
  `get_eph(geo = 'TUCUMAN', simplified = FALSE)` for example.

## geoAr 0.0.1.2

- Added census tract option as a parameter (CENSO 2010). Example
  `get_geo(geo = 'TUCUMAN', level = 'censal', simplified = FALSE)`

## geoAr 0.0.1.1

- Added a `NEWS.md` file to track changes to the package.
- Fix bug [\#1](https://github.com/PoliticaArgentina/geoAr/issues/1) in
  `get_geo("BUENOS AIRES")`
