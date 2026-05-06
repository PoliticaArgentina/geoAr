# Carga grillas de districts de Argentina (*Load grids of districts of Argentina*)

Función que descarga grillas (*facet*) para acomodarlas como si fueran
mapas de provincias y deparamentos de Argentina.

## Usage

``` r
get_grid(district = NULL)
```

## Arguments

- district:

  un character con el nombre del district que se quiere descargar.
  Disponibles grillas para Argentina y para las 24 provincias. Se pueden
  chequear los parametros con
  [`show_arg_codes`](https://politicaargentina.github.io/geoAr/reference/show_arg_codes.md).

## Value

tibble con datos correspondientes a diseño de grillas para utilizar con
libreria `geofacet`

## Examples

``` r

get_grid("TUCUMAN")
#> Adding missing grouping variables: `name_provincia`
#>    name_provincia            name row col code
#> 1         TUCUMAN       BURRUYACU   1   4  013
#> 2         TUCUMAN         CAPITAL   2   4  001
#> 3         TUCUMAN     CHICLIGASTA   4   2  005
#> 4         TUCUMAN       CRUZ ALTA   2   5  012
#> 5         TUCUMAN        FAMAILLA   3   3  003
#> 6         TUCUMAN        GRANEROS   5   4  009
#> 7         TUCUMAN JUAN B. ALBERDI   5   3  007
#> 8         TUCUMAN        LA COCHA   6   3  008
#> 9         TUCUMAN          LEALES   3   5  011
#> 10        TUCUMAN           LULES   3   4  002
#> 11        TUCUMAN        MONTEROS   3   2  004
#> 12        TUCUMAN       RIO CHICO   4   3  006
#> 13        TUCUMAN          SIMOCA   4   4  010
#> 14        TUCUMAN  TAFI DEL VALLE   2   1  017
#> 15        TUCUMAN      TAFI VIEJO   2   2  016
#> 16        TUCUMAN         TRANCAS   1   3  014
#> 17        TUCUMAN     YERBA BUENA   2   3  015
```
