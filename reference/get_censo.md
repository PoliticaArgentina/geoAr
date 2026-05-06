# Carga poligonos de Censos de Población Históricos de Argentina

Carga poligonos de Censos de Población Históricos de Argentina

## Usage

``` r
get_censo(censo = NULL, simplified = FALSE)
```

## Arguments

- censo:

  un character con el id del año del censo del district que se quiere
  descargar. Se pueden chequear el id con
  [`show_available_censos`](https://politicaargentina.github.io/geoAr/reference/show_available_censos.md).

- simplified:

  por defecto es TRUE y determina la descarga de una versión
  simplificada de las geometrias. Con FALSE descarga la versión original
  de INDEC

## Value

tibble con capa geografica correspondiente a geometrías de un censo
histórico

## Examples

``` r
get_censo(censo = "1947", simplified = TRUE)
#> Los datos fueron obtenidos del proyecto 'Base cartografica de departamentos para los censos nacionales de la Republica Argentina desde 1869 a 2010' de Gonzalo Rodriguez y Pablo De Grande el 06 May de 2026. La documentacion se encuetra disponible en https://ri.conicet.gov.ar/handle/11336/149867
#> Simple feature collection with 488 features and 4 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -73.5708 ymin: -55.05456 xmax: -53.64224 ymax: -21.78092
#> Geodetic CRS:  WGS 84
#> # A tibble: 488 × 5
#>    prov_cod depto      link  terr1947                                   geometry
#>    <chr>    <chr>      <chr> <chr>                            <MULTIPOLYGON [°]>
#>  1 02       SECCION 01 02001 02       (((-58.42705 -34.66459, -58.41681 -34.659…
#>  2 02       SECCION 02 02002 02       (((-58.38828 -34.66039, -58.39117 -34.627…
#>  3 02       SECCION 03 02003 02       (((-58.37473 -34.63785, -58.36806 -34.624…
#>  4 02       SECCION 04 02004 02       (((-58.35734 -34.64413, -58.35276 -34.633…
#>  5 02       SECCION 05 02005 02       (((-58.44443 -34.62187, -58.44651 -34.607…
#>  6 02       SECCION 06 02006 02       (((-58.43886 -34.63526, -58.41596 -34.631…
#>  7 02       SECCION 07 02007 02       (((-58.41848 -34.61102, -58.41726 -34.597…
#>  8 02       SECCION 08 02008 02       (((-58.41596 -34.63138, -58.39117 -34.627…
#>  9 02       SECCION 09 02009 02       (((-58.40351 -34.61884, -58.40597 -34.610…
#> 10 02       SECCION 10 02010 02       (((-58.40351 -34.61884, -58.3917 -34.6181…
#> # ℹ 478 more rows
```
