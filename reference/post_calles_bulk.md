# Enviar Lote de Consultas de Calles (POST)

Permite realizar múltiples búsquedas sobre el listado de vías de
circulación en una sola llamada POST. Realiza la consulta POST al
endpoint /calles de georef-ar-api.

## Usage

``` r
post_calles_bulk(queries_list)
```

## Arguments

- queries_list:

  Lista de listas. Cada lista interna debe contener los parámetros para
  una consulta de calle individual. Parámetros válidos por consulta:
  nombre, id, tipo, provincia, departamento, municipio,
  localidad_censal, categoria, max, inicio, aplanar, campos, exacto.

## Value

Un Data Frame (tibble) con los resultados combinados de todas las
consultas.

## References

[georef-ar-api/calles
POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_calles)

## Examples

``` r
if (FALSE) { # \dontrun{
consultas_calles <- list(
  list(nombre = "SAN MARTIN", provincia = "BUENOS AIRES", max = 2),
  list(nombre = "SARMIENTO", provincia = "CORDOBA", max = 2)
)
resultados_calles <- post_calles_bulk(queries_list = consultas_calles)
print(resultados_calles)
} # }
```
