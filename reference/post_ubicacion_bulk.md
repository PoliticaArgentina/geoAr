# Georreferenciación Inversa para un Lote de Puntos (POST)

Permite realizar georreferenciación inversa para múltiples puntos (lat,
lon) en una sola llamada POST. Realiza la consulta POST al endpoint
/ubicacion de georef-ar-api.

## Usage

``` r
post_ubicacion_bulk(queries_list)
```

## Arguments

- queries_list:

  Lista de listas. Cada lista interna debe contener los parámetros 'lat'
  y 'lon' para un punto. Otros parámetros válidos por consulta: aplanar,
  campos.

## Value

Un Data Frame (tibble) con los resultados combinados de todas las
georreferenciaciones.

## References

[georef-ar-api/ubicacion
POST](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/post_ubicacion)

## Examples

``` r
if (FALSE) { # \dontrun{
consultas_ubic <- list(
  list(lat = -34.6037, lon = -58.3816), # Buenos Aires
  list(lat = -32.9587, lon = -60.6393)  # Rosario
)
resultados_ubic <- post_ubicacion_bulk(queries_list = consultas_ubic)
print(resultados_ubic)
} # }
```
