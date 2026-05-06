# Enviar Lote de Consultas de Provincias (POST)

Permite realizar múltiples búsquedas sobre el listado de provincias en
una sola llamada POST. Realiza la consulta POST al endpoint /provincias
de georef-ar-api. Si existe GEOREFAR_TOKEN en el Renviron lo usará para
hacer la consulta.

## Usage

``` r
post_provincias_bulk(queries_list)
```

## Arguments

- queries_list:

  Lista de listas. Cada lista interna debe contener los parámetros para
  una consulta de provincia individual (e.g., list(nombre = "Tucuman"),
  list(id = "06")). Parámetros válidos por consulta: id, nombre, orden,
  aplanar, campos, max, exacto.

## Value

Un Data Frame (tibble) con los resultados combinados de todas las
consultas. Las respuestas de la API para cada consulta en el lote se
apilan.

## References

[georef-ar-api/provincias
POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_provincias)

## Examples

``` r
if (FALSE) { # \dontrun{
consultitas <- list(
  list(nombre = "santiago del estero"),
  list(id = "82", campos = "id,nombre,centroide.lat,centroide.lon"),
  list(nombre = "tierra del fuego", campos = "completo")
)
resultados_provincias <- post_provincias_bulk(queries_list = consultitas)
print(resultados_provincias)
} # }
```
