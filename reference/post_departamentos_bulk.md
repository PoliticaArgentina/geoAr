# Enviar Lote de Consultas de Departamentos (POST)

Permite realizar múltiples búsquedas sobre el listado de departamentos
en una sola llamada POST. Realiza la consulta POST al endpoint
/departamentos de georef-ar-api.

## Usage

``` r
post_departamentos_bulk(queries_list)
```

## Arguments

- queries_list:

  Lista de listas. Cada lista interna debe contener los parámetros para
  una consulta de departamento individual (e.g., list(nombre =
  "Rosario"), list(provincia = "02")). Parámetros válidos por consulta:
  id, nombre, provincia, orden, aplanar, campos, max, exacto.

## Value

Un Data Frame (tibble) con los resultados combinados de todas las
consultas.

## References

[georef-ar-api/departamentos
POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_departamentos)

## Examples

``` r
if (FALSE) { # \dontrun{
consultas_deptos <- list(
  list(provincia = "22", nombre = "Ledesma"),
  list(id = "14028")
)
resultados_deptos <- post_departamentos_bulk(queries_list = consultas_deptos)
print(resultados_deptos)
} # }
```
