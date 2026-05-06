# Enviar Lote de Consultas de Asentamientos (POST)

Permite realizar múltiples búsquedas sobre el listado de asentamientos
BAHRA en una sola llamada POST. Realiza la consulta POST al endpoint
/asentamientos de georef-ar-api.

## Usage

``` r
post_asentamientos_bulk(queries_list)
```

## Arguments

- queries_list:

  Lista de listas. Cada lista interna debe contener los parámetros para
  una consulta de asentamiento individual. Parámetros válidos por
  consulta: id, nombre, provincia, departamento, municipio,
  localidad_censal, orden, aplanar, campos, max, exacto.

## Value

Un Data Frame (tibble) con los resultados combinados de todas las
consultas.

## References

[georef-ar-api/asentamientos
POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_asentamientos)

## Examples

``` r
if (FALSE) { # \dontrun{
consultas_asent <- list(
  list(provincia = "CHUBUT", nombre = "COSTA")
)
resultados_asent <- post_asentamientos_bulk(queries_list = consultas_asent)
print(resultados_asent)
} # }
```
