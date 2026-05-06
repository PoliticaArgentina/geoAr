# Obtener Municipios

Permite realizar búsquedas sobre el listado de municipios. Realiza la
consulta GET al endpoint /municipios de georef-ar-api. Si existe
GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.

## Usage

``` r
get_municipios(
  id = NULL,
  nombre = NULL,
  provincia = NULL,
  departamento = NULL,
  interseccion = NULL,
  orden = NULL,
  aplanar = TRUE,
  campos = NULL,
  max = NULL,
  inicio = NULL,
  exacto = NULL
)
```

## Arguments

- id:

  text Filtrar por ID.

- nombre:

  text Filtrar por Nombre.

- provincia:

  text Filtrar por nombre o ID de Provincia.

- departamento:

  text Filtrar por nombre o ID de Departamento.

- interseccion:

  text Geometría GeoJSON utilizada para filtrar resultados por
  intersección espacial. Sólo se soportan polígonos y multipolígonos.
  Ejemplo:
  polygon((-58.431,-34.592),(-58.430,-34.590),(-58.428,-34.593),(-58.431,-34.592)).

- orden:

  text Campo por el cual ordenar los resultados. (Por ID o nombre)

- aplanar:

  boolean Cuando está presente, muestra el resultado JSON con una
  estructura plana.

- campos:

  text Campos a incluir en la respuesta separados por comas, sin
  espacios. Algunos campos siempre serán incluidos, incluso si no se
  agregaron en la lista. Para incluir campos de sub-entidades, separar
  los nombres con un punto, por ejemplo: provincia.id.

- max:

  integer Cantidad máxima de resultados a devolver. La API limita a un
  máximo de 2000 para este endpoint.

- inicio:

  integer Cantidad de resultados a omitir desde el principio.

- exacto:

  boolean Cuando está presente, se activa el modo de búsqueda por texto
  exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto
  (por ejemplo, nombre).

## Value

Un Data Frame con el listado de Municipios

## References

[georef-ar-api/municipios](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_municipios)

## Examples

``` r
if (FALSE) { # \dontrun{
get_municipios(provincia = "cordoba", max = 10)
} # }
```
