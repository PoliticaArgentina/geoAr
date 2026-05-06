# Enviar Lote de Consultas de Municipios (POST)

Permite realizar múltiples búsquedas sobre el listado de municipios en
una sola llamada POST. Realiza la consulta POST al endpoint /municipios
de georef-ar-api.

## Usage

``` r
post_municipios_bulk(queries_list)
```

## Arguments

- queries_list:

  Lista de listas. Cada lista interna debe contener los parámetros para
  una consulta de municipio individual. Parámetros válidos por consulta:
  id, nombre, provincia, departamento, orden, aplanar, campos, max,
  exacto.

## Value

Un Data Frame (tibble) con los resultados combinados de todas las
consultas.

## References

[georef-ar-api/municipios
POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_municipios)

## Examples

``` r
if (FALSE) { # \dontrun{
consultas_muni <- list(
  list(provincia = "06", nombre = "La Plata"),
  list(id = "540098")
)
resultados_muni <- post_municipios_bulk(queries_list = consultas_muni)
print(resultados_muni)
} # }
```
