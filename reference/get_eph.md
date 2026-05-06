# Carga poligonos de los Aglomerados Urbanos correspondientes a la Encuesta Permanente de Hogares (INDEC)

Carga poligonos de los Aglomerados Urbanos correspondientes a la
Encuesta Permanente de Hogares (INDEC)

## Usage

``` r
get_eph(
  geo = "ARGENTINA",
  simplified = TRUE,
  centroid = FALSE,
  level = "envolventes"
)
```

## Arguments

- geo:

  un character con el nombre del distrito que se quiere descargar. Se
  puede chequear el id con
  [`show_arg_codes`](https://politicaargentina.github.io/geoAr/reference/show_arg_codes.md).

- simplified:

  por defecto es TRUE y determina la descarga de una versión
  simplificada de las geometrias. Con FALSE descarga la versión original
  de INDEC

- centroid:

  por defecto devuelve poligonos como geometry pero pueden descargarse
  puntos (centroides correspondientes al level especificado)

- level:

  por defecto devuelve a nivel `envolvente` pero puede descargarse a
  nivel `radios` y `entidades`

## Value

tibble con capa geografica correspondiente a alguna versión de
geometrías utilizadas en la Encuesta Permanente de Hogares (EPH)

## Examples

``` r
get_eph(geo = "TUCUMAN")
#> Simple feature collection with 5 features and 11 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 3562896 ymin: 7024181 xmax: 3588786 ymax: 7046155
#> Projected CRS: POSGAR 94 / Argentina 3
#> # A tibble: 5 × 12
#>      id eph_codagl eph_aglome        codaglo aglomerado codprov nomprov coddepto
#> * <int> <chr>      <chr>             <chr>   <chr>      <chr>   <chr>   <chr>   
#> 1    54 29         Gran Tucumán - T… 0000    ""         90      Tucumán 014,063…
#> 2    55 29         Gran Tucumán - T… 0006    "Gran San… 90      Tucumán 014,063…
#> 3    56 29         Gran Tucumán - T… 0081    "Tafí Vie… 90      Tucumán 105     
#> 4    57 29         Gran Tucumán - T… 0362    "Villa Ca… 90      Tucumán 119     
#> 5    58 29         Gran Tucumán - T… 7016    "Barrio P… 90      Tucumán 105     
#> # ℹ 4 more variables: localidade <chr>, entidades <chr>, the_geom <chr>,
#> #   geometry <MULTIPOLYGON [m]>
```
