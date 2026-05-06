# Enviar Lote de Consultas de Localidades Censales (POST)

Permite realizar múltiples búsquedas sobre el listado de localidades
censales en una sola llamada POST. Realiza la consulta POST al endpoint
/localidades-censales de georef-ar-api.

## Usage

``` r
post_localidades_censales_bulk(queries_list)
```

## Arguments

- queries_list:

  Lista de listas. Cada lista interna debe contener los parámetros para
  una consulta de localidad censal individual. Parámetros válidos por
  consulta: id, nombre, provincia, departamento, municipio, orden,
  aplanar, campos, max, exacto.

## Value

Un Data Frame (tibble) con los resultados combinados de todas las
consultas.

## References

[georef-ar-api/localidades-censales
POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_localidades_censales)

## Examples

``` r
if (FALSE) { # \dontrun{
consultas_loc_cen <- list(
  list(nombre = "AGUA DE ORO", provincia = "CÓRDOBA")
)
resultados_loc_cen <- post_localidades_censales_bulk(queries_list = consultas_loc_cen)
print(resultados_loc_cen)
} # }
```
