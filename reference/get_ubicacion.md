# Obtener Ubicacion

Permite realizar una georreferenciación inversa para un punto,
informando cuales unidades territoriales lo contienen. Realiza la
consulta GET al endpoint /ubicacion de georef-ar-api. Si existe
GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta.

## Usage

``` r
get_ubicacion(lat, lon, aplanar = TRUE, campos = NULL)
```

## Arguments

- lat:

  numeric Latitud del punto, en forma de número real con grados
  decimales.

- lon:

  numeric Longitud del punto, en forma de número real con grados
  decimales.

- aplanar:

  boolean Cuando está presente, muestra el resultado JSON con una
  estructura plana.

- campos:

  text Campos a incluir en la respuesta separados por comas, sin
  espacios. Algunos campos siempre serán incluidos, incluso si no se
  agregaron en la lista. Para incluir campos de sub-entidades, separar
  los nombres con un punto, por ejemplo: provincia.id.

## Value

Un Data Frame con las unidades territoriales que contienen el punto.

## References

[georef-ar-api/ubicacion](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get_ubicacion)

## Examples

``` r
if (FALSE) { # \dontrun{
get_ubicacion()
} # }
```
