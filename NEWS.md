# geoAr 1.1.0

## New Features

*   Added a suite of `post_*_bulk()` functions (`post_provincias_bulk()`, `post_departamentos_bulk()`, `post_municipios_bulk()`, `post_localidades_bulk()`, `post_localidades_censales_bulk()`, `post_asentamientos_bulk()`, `post_calles_bulk()`, `post_direcciones_bulk()`, `post_ubicacion_bulk()`) to allow for bulk queries to the corresponding `georef-ar-api` endpoints using the POST method. This enables sending multiple queries in a single API request.
*   Each `post_*_bulk()` function includes enhanced input validation for the `queries_list` parameter, providing warnings for unrecognized parameter names within individual queries to guide correct usage.
*   Added `get_geodata_dump()` function to download entire datasets (provincias, departamentos, etc.) in various formats (CSV, JSON, GeoJSON, NDJSON) directly from the API.
*   The `get_calles()` function now exclusively handles GET requests; POST functionality for streets is managed by `post_calles_bulk()`.

## Fixes & Improvements

*   The base API URL used by `R/georefar.R` functions has been updated from HTTP to HTTPS (`https://apis.datos.gob.ar/georef/api/`) for secure communication.
*   Refactored the internal `post_endpoint()` function to correctly construct JSON bodies for POST requests and parse API responses, especially for bulk operations.
*   Added comprehensive unit tests for input validation of all new `post_*_bulk()` functions and basic validation for `get_geodata_dump()`.


# geoAr 1.0.0

Ready for CRAN re submission


# geoAr (development version) 0.0.1.5.0.0

- Fix #21 : `PKGNAME` tag


# geoAr 0.0.1.4.3.1

- Changes `stop` for `warning` in `get_endpoint()` internal function for `georefar` family functions. 

# geoAr 0.0.1.4.3

Modify `georefar get_*` family functions:

 - Add  `TOKEN` workflow alternative (documented)
 - Add new endpoints:  `get_asentamientos ` & `get_asentamientos`

# geoAr 0.0.1.4.2.1

Added [georefar](https://github.com/pdelboca/georefar) (R wrapper for georef-ar API) `get_*` family functions 

# geoAr 0.0.1.4.2

First CRAN version

# geoAr 0.0.1.4

`get_bahra()` function for a new data source: _Base de Asentamientos Humanos de la República Argentina_ (BAHRA) 


# geoAr 0.0.1.3.1

Added `levels` (`envolvente`, `radios` and `entidades`) & `centroid` (`FALSE/TRUE`)  param to `get_eph()` function. 


# geoAr 0.0.1.3

Added new features

* Geometry reconstruction for every CENSO (1869 - 2010). Example: Using `get_censo(censo = "1991", simplified = T)`.


* Permanent Household Survey (Encuesta Permanente de Hogares - EPH) Urban Aglomerations Geometries. Using `get_eph(geo = 'TUCUMAN', simplified = FALSE)` for example.

# geoAr 0.0.1.2

* Added census tract option as a parameter (CENSO 2010). Example `get_geo(geo = 'TUCUMAN', level = 'censal', simplified = FALSE)`


# geoAr 0.0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Fix bug #1 in   `get_geo("BUENOS AIRES")`
