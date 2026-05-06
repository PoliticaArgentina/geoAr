# Geometrías de CENSOS (*geo ID's dictoniary*)

Función que devuelve un *data.frame* con listado de las geometrías de
los CENSOS disponible

## Usage

``` r
show_available_censos()
```

## Value

tibble con información auxiliar para descarga de CENSOS históricos con
[`get_censo`](https://politicaargentina.github.io/geoAr/reference/get_censo.md)

## Examples

``` r
show_available_censos()
#> # A tibble: 10 × 1
#>    censo
#>    <int>
#>  1  1869
#>  2  1895
#>  3  1914
#>  4  1947
#>  5  1960
#>  6  1970
#>  7  1980
#>  8  1991
#>  9  2001
#> 10  2010
```
