# geoAr 0.0.1.3.1

Added `levels` (`envolvente`, `radios` and `entidades`) param to `get_eph()` function. 


# geoAr 0.0.1.3

Added new features

* Geometry reconstruction for every CENSO (1869 - 2010). Example: Using `get_censo(censo = "1991", simplified = T)`.


* Permanent Household Survey (Encuesta Permanente de Hogares - EPH) Urban Aglomerations Geometries. Using `get_eph(geo = 'TUCUMAN', simplified = FALSE)` for example.

# geoAr 0.0.1.2

* Added census tract option as a parameter (CENSO 2010). Example `get_geo(geo = 'TUCUMAN', level = 'censal', simplified = FALSE)`


# geoAr 0.0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Fix bug #1 in   `get_geo("BUENOS AIRES")`
