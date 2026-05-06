# Package index

## Geometries

Functions for exploring and downloading geo data.

### Explore

- [`show_available_censos()`](https://politicaargentina.github.io/geoAr/reference/show_available_censos.md)
  :

  Geometrías de CENSOS (*geo ID's dictoniary*)

- [`show_arg_codes()`](https://politicaargentina.github.io/geoAr/reference/show_arg_codes.md)
  :

  Diccionario de códigos identificadores de distritos (*geo ID's
  dictoniary*)

### Download

- [`get_geo()`](https://politicaargentina.github.io/geoAr/reference/get_geo.md)
  :

  Carga poligonos geográficos de distritos de Argentina (*Load
  Argentina's districts geometries*)

- [`get_eph()`](https://politicaargentina.github.io/geoAr/reference/get_eph.md)
  : Carga poligonos de los Aglomerados Urbanos correspondientes a la
  Encuesta Permanente de Hogares (INDEC)

- [`get_bahra()`](https://politicaargentina.github.io/geoAr/reference/get_bahra.md)
  : Descarga 'base total' de Asentamientos Humanos de la República
  Argentina (BAHRA)

- [`get_censo()`](https://politicaargentina.github.io/geoAr/reference/get_censo.md)
  : Carga poligonos de Censos de Población Históricos de Argentina

### georef-ar API

- [`get_provincias()`](https://politicaargentina.github.io/geoAr/reference/get_provincias.md)
  : Obtener Provincias
- [`get_calles()`](https://politicaargentina.github.io/geoAr/reference/get_calles.md)
  : Obtener Calles
- [`get_departamentos()`](https://politicaargentina.github.io/geoAr/reference/get_departamentos.md)
  : Obtener Departamentos
- [`get_localidades()`](https://politicaargentina.github.io/geoAr/reference/get_localidades.md)
  : Obtener Localidades
- [`get_municipios()`](https://politicaargentina.github.io/geoAr/reference/get_municipios.md)
  : Obtener Municipios
- [`get_ubicacion()`](https://politicaargentina.github.io/geoAr/reference/get_ubicacion.md)
  : Obtener Ubicacion
- [`get_asentamientos()`](https://politicaargentina.github.io/geoAr/reference/get_asentamientos.md)
  : Obtener Asentamientos de BAHRA
- [`get_localidades_censales()`](https://politicaargentina.github.io/geoAr/reference/get_localidades_censales.md)
  : Obtener Localidades Censales
- [`normalizar_direccion()`](https://politicaargentina.github.io/geoAr/reference/normalizar_direccion.md)
  : Normalizacion de direcciones
- [`get_geodata_dump()`](https://politicaargentina.github.io/geoAr/reference/get_geodata_dump.md)
  : Descargar Datos Geográficos Completos
- [`post_asentamientos_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_asentamientos_bulk.md)
  : Enviar Lote de Consultas de Asentamientos (POST)
- [`post_calles_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_calles_bulk.md)
  : Enviar Lote de Consultas de Calles (POST)
- [`post_departamentos_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_departamentos_bulk.md)
  : Enviar Lote de Consultas de Departamentos (POST)
- [`post_direcciones_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_direcciones_bulk.md)
  : Normalizar un Lote de Direcciones (POST)
- [`post_localidades_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_localidades_bulk.md)
  : Enviar Lote de Consultas de Localidades (POST)
- [`post_localidades_censales_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_localidades_censales_bulk.md)
  : Enviar Lote de Consultas de Localidades Censales (POST)
- [`post_municipios_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_municipios_bulk.md)
  : Enviar Lote de Consultas de Municipios (POST)
- [`post_provincias_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_provincias_bulk.md)
  : Enviar Lote de Consultas de Provincias (POST)
- [`post_ubicacion_bulk()`](https://politicaargentina.github.io/geoAr/reference/post_ubicacion_bulk.md)
  : Georreferenciación Inversa para un Lote de Puntos (POST)

## Grids

Functions to work with ‘{geofacet}’ (‘ggplot2’ facet grids as maps)

- [`get_grid()`](https://politicaargentina.github.io/geoAr/reference/get_grid.md)
  :

  Carga grillas de districts de Argentina (*Load grids of districts of
  Argentina*)

- [`recode_grid()`](https://politicaargentina.github.io/geoAr/reference/recode_grid.md)
  :

  Recodifica id de grillas asignados a provincias o departamentos de
  Argentina (*Recode Argentina's districs id in grids*)

## Data

Helper data

- [`censos`](https://politicaargentina.github.io/geoAr/reference/censos.md)
  : Un archivo de datos que contiene el listado de las geometrías para
  los censos disponibles

- [`geo_metadata`](https://politicaargentina.github.io/geoAr/reference/geo_metadata.md)
  :

  Un archivo de datos que contiene identificadores geográficos para los
  departamentos de Argentina y sus provincias (*A data file containing
  geographic identifiers for the departments of Argentina and their
  provinces*)

- [`grillas_geofacet`](https://politicaargentina.github.io/geoAr/reference/grillas_geofacet.md)
  : Base de datos de grillas

## Other

- [`addArgTiles()`](https://politicaargentina.github.io/geoAr/reference/addArgTiles.md)
  :

  Agrega capa de Argentina para mapa interactivo (*Load Argentina Tiles
  Template*)

- [`add_geo_codes()`](https://politicaargentina.github.io/geoAr/reference/add_geo_codes.md)
  :

  Agrega columnas con id geográficos asignados a poligonos de provincias
  o departamentos de Argentina (*Augment Argentina's districts polygons
  id*)

- [`geoAr-package`](https://politicaargentina.github.io/geoAr/reference/geoAr.md)
  [`geoAr`](https://politicaargentina.github.io/geoAr/reference/geoAr.md)
  :

  `geoAr` package
