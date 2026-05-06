# Obtener Calles

Permite realizar búsquedas sobre el listado de vías de circulación.
Realiza la consulta GET al endpoint /calles de georef-ar-api. Si existe
GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.

## Usage

``` r
get_calles(
  nombre = NULL,
  id = NULL,
  tipo = NULL,
  provincia = NULL,
  departamento = NULL,
  municipio = NULL,
  localidad_censal = NULL,
  categoria = NULL,
  interseccion = NULL,
  orden = NULL,
  max = NULL,
  inicio = NULL,
  aplanar = TRUE,
  campos = NULL,
  exacto = NULL
)
```

## Arguments

- nombre:

  text Filtrar por Nombre.

- id:

  text Filtrar por ID.

- tipo:

  text Tipo de calle. (Valores disponibles: calle, avenida, pasaje.)

- provincia:

  text Filtrar por nombre o ID de provincia.

- departamento:

  text Filtrar por nombre o ID de departamento.

- municipio:

  text Filtrar por nombre o ID de municipio.

- localidad_censal:

  text Filtrar por nombre o ID de localidad censal.

- categoria:

  text Filtrar por categoría de calle.

- interseccion:

  text Geometría GeoJSON utilizada para filtrar resultados por
  intersección espacial. Sólo se soportan polígonos y multipolígonos.
  Ejemplo:
  polygon((-58.431,-34.592),(-58.430,-34.590),(-58.428,-34.593),(-58.431,-34.592)).

- orden:

  text Campo por el cual ordenar los resultados. (Por ID o nombre)

- max:

  integer Cantidad máxima de resultados a devolver. Debe ser menor o
  igual a 5000.

- inicio:

  integer Cantidad de resultados a omitir desde el principio. La suma de
  'max' e 'inicio' no debe superar 10000.

- aplanar:

  boolean Cuando está presente, muestra el resultado JSON con una
  estructura plana.

- campos:

  text Campos a incluir en la respuesta separados por comas, sin
  espacios. Algunos campos siempre serán incluidos, incluso si no se
  agregaron en la lista. Para incluir campos de sub-entidades, separar
  los nombres con un punto, por ejemplo: provincia.id.

- exacto:

  boolean Cuando está presente, se activa el modo de búsqueda por texto
  exacto. Sólo tiene efecto cuando se usan campos de búsqueda por texto
  (por ejemplo, nombre).

## Value

Un Data Frame con el listado de Calles

## References

[georef-ar-api/calles](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_calles)

## Examples

``` r
if (FALSE) { # \dontrun{
get_calles(nombre = "Corrientes", provincia = "CIUDAD AUTONOMA DE BUENOS AIRES", max = 10)
} # }
```
