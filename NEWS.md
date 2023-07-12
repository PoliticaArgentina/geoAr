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
