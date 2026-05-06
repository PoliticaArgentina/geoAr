# Enviar Lote de Consultas de Localidades (POST)

Permite realizar múltiples búsquedas sobre el listado de localidades en
una sola llamada POST. Realiza la consulta POST al endpoint /localidades
de georef-ar-api.

## Usage

``` r
post_localidades_bulk(queries_list)
```

## Arguments

- queries_list:

  Lista de listas. Cada lista interna debe contener los parámetros para
  una consulta de localidad individual. Parámetros válidos por consulta:
  id, nombre, provincia, departamento, municipio, orden, aplanar,
  campos, max, exacto.

## Value

Un Data Frame (tibble) con los resultados combinados de todas las
consultas.

## References

[georef-ar-api/localidades
POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_localidades)

## Examples

``` r
if (FALSE) { # \dontrun{
consultas_loc <- list(
  list(provincia = "Tucuman", departamento = "Capital"),
  list(id = "22056140000")
)
resultados_loc <- post_localidades_bulk(queries_list = consultas_loc)
print(resultados_loc)
} # }
```
