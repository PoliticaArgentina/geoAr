# Carga poligonos geográficos de distritos de Argentina (*Load Argentina's districts geometries*)

Función que descarga (*geometry*) para graficar con mapas

## Usage

``` r
get_geo(geo = NULL, level = "departamento", simplified = TRUE)
```

## Arguments

- geo:

  un character con el nombre del district que se quiere descargar. Se
  pueden chequear el id con
  [`show_arg_codes`](https://politicaargentina.github.io/geoAr/reference/show_arg_codes.md).

- level:

  parametro opcional para descargar geometrías a nivel 'departamento' o
  'censal' cuando se solicita mapa nacional
  `get_geo(geo = "ARGNTINA", level = "departamento")`.

- simplified:

  por defecto es TRUE y determina la descarga de una versión
  simplificada de las geometrias. Con FALSE descarga la versión original
  de INDEC

## Value

tibble con capa geografica de Argentina o distrito seleccionado (a
diferentes niveles de agregación) correspondientes al CENSO 2010 - INDEC

## Examples

``` r

get_geo("TUCUMAN")
#> Simple feature collection with 17 features and 2 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -66.18101 ymin: -28.01575 xmax: -64.48315 ymax: -26.06037
#> Geodetic CRS:  WGS 84
#> # A tibble: 17 × 3
#>    codprov_censo coddepto_censo                                         geometry
#>  * <chr>         <chr>                                        <MULTIPOLYGON [°]>
#>  1 90            007            (((-64.49919 -26.23353, -64.49857 -26.26325, -6…
#>  2 90            014            (((-65.13782 -26.74975, -65.10095 -26.75771, -6…
#>  3 90            021            (((-65.94729 -27.08444, -65.93885 -27.0989, -65…
#>  4 90            028            (((-65.62733 -26.87134, -65.62598 -26.84177, -6…
#>  5 90            035            (((-65.47532 -27.53838, -65.43503 -27.5425, -65…
#>  6 90            042            (((-65.51768 -27.54528, -65.53685 -27.62278, -6…
#>  7 90            049            (((-65.51348 -27.62978, -65.52091 -27.66553, -6…
#>  8 90            056            (((-65.12933 -27.02488, -65.12589 -27.01659, -6…
#>  9 90            063            (((-65.43583 -26.83751, -65.38124 -26.84196, -6…
#> 10 90            070            (((-65.86853 -26.99852, -65.85796 -27.01388, -6…
#> 11 90            077            (((-65.93481 -27.39544, -65.89887 -27.3702, -65…
#> 12 90            084            (((-65.16293 -26.82289, -65.18562 -26.86204, -6…
#> 13 90            091            (((-65.33499 -27.20478, -65.33462 -27.21948, -6…
#> 14 90            098            (((-66.08361 -26.23547, -66.06621 -26.23792, -6…
#> 15 90            105            (((-65.70111 -26.52317, -65.65078 -26.5263, -65…
#> 16 90            112            (((-65.52483 -26.09667, -65.4682 -26.09253, -65…
#> 17 90            119            (((-65.27025 -26.84087, -65.28587 -26.83549, -6…
```
