# Agrega columnas con id geográficos asignados a poligonos de provincias o departamentos de Argentina (*Augment Argentina's districts polygons id*)

Función que permite agregar columnas con ids geográficos para
utilización de mapas de Argentina y de departamentos para los 24
distritos sub nacionales para una más sencilla la vinculación con bases
de datos de diversas fuentes.

## Usage

``` r
add_geo_codes(data)
```

## Arguments

- data:

  data.frame obtenido con
  [`get_geo`](https://politicaargentina.github.io/geoAr/reference/get_geo.md).
  Los mapas disponibles se pueden chequear con
  [`show_arg_codes`](https://politicaargentina.github.io/geoAr/reference/show_arg_codes.md).

## Value

Los valores por defecto en
[`get_geo`](https://politicaargentina.github.io/geoAr/reference/get_geo.md)
son `codprov_censo` para provincia y `coddepto_censo` para
departamentos, respectivamente. Estos corresponden a la codificación de
INDEC. Con el agregado usando `add_geo_codes` se puede optar por las
nomenclaturas de `'indra'` - correspondiente a los escrutinios
provisorios de elecciones nacionales, con las variantes `codprov` y
`coddepto`, o la de `'iso'` con `\*_iso` , estas últimas hasta el nivel
provincial.

## Details

Respecto el origen de los datos se puede consultar la documentación de
[`ISO 3166-2` - International Organization for
Standardization](https://www.iso.org/obp/ui/#iso:code:3166:AR) y del
[*INDEC*](https://www.iso.org/obp/ui/#iso:code:3166:AR).

`codprov` y `coddepto` son las codificaciones de las bases de datos de
*INDRA*, empresa encargada por muchos años de la tarea del escrutinio
provisorio y utilizados en [polAr](https://electorarg.github.io/polAr/).

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


get_geo("TUCUMAN") %>%
   add_geo_codes()
#> Simple feature collection with 17 features and 8 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -66.18101 ymin: -28.01575 xmax: -64.48315 ymax: -26.06037
#> Geodetic CRS:  WGS 84
#> # A tibble: 17 × 9
#>    codprov_censo coddepto_censo codprov coddepto nomdepto_censo  name_prov
#>    <chr>         <chr>          <chr>   <chr>    <chr>           <chr>    
#>  1 90            007            23      013      BURRUYACU       TUCUMAN  
#>  2 90            014            23      012      CRUZ ALTA       TUCUMAN  
#>  3 90            021            23      005      CHICLIGASTA     TUCUMAN  
#>  4 90            028            23      003      FAMAILLA        TUCUMAN  
#>  5 90            035            23      009      GRANEROS        TUCUMAN  
#>  6 90            042            23      007      JUAN B. ALBERDI TUCUMAN  
#>  7 90            049            23      008      LA COCHA        TUCUMAN  
#>  8 90            056            23      011      LEALES          TUCUMAN  
#>  9 90            063            23      002      LULES           TUCUMAN  
#> 10 90            070            23      004      MONTEROS        TUCUMAN  
#> 11 90            077            23      006      RIO CHICO       TUCUMAN  
#> 12 90            084            23      001      CAPITAL         TUCUMAN  
#> 13 90            091            23      010      SIMOCA          TUCUMAN  
#> 14 90            098            23      017      TAFI DEL VALLE  TUCUMAN  
#> 15 90            105            23      016      TAFI VIEJO      TUCUMAN  
#> 16 90            112            23      014      TRANCAS         TUCUMAN  
#> 17 90            119            23      015      YERBA BUENA     TUCUMAN  
#> # ℹ 3 more variables: codprov_iso <chr>, name_iso <chr>,
#> #   geometry <MULTIPOLYGON [°]>

```
