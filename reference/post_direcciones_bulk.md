# Normalizar un Lote de Direcciones (POST)

Permite normalizar múltiples direcciones en una sola llamada POST.
Realiza la consulta POST al endpoint /direcciones de georef-ar-api.

## Usage

``` r
post_direcciones_bulk(queries_list)
```

## Arguments

- queries_list:

  Lista de listas. Cada lista interna debe contener los parámetros para
  una consulta de normalización de dirección individual. Parámetro
  requerido por consulta: 'direccion' (e.g., "AV SAN MARTIN 123"). Otros
  parámetros válidos: tipo, provincia, departamento, aplanar, campos,
  max, exacto.

## Value

Un Data Frame (tibble) con los resultados combinados de todas las
normalizaciones.

## References

[georef-ar-api/direcciones
POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_direcciones)

## Examples

``` r
if (FALSE) { # \dontrun{
consultas_dir <- list(
  list(direccion = "MAIPU 100", provincia = "BUENOS AIRES"),
  list(direccion = "SANTA FE 2000, ROSARIO")
)
resultados_dir <- post_direcciones_bulk(queries_list = consultas_dir)
print(resultados_dir)
} # }
```
