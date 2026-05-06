# Descarga 'base total' de Asentamientos Humanos de la República Argentina (BAHRA)

Descarga 'base total' de Asentamientos Humanos de la República Argentina
(BAHRA)

## Usage

``` r
get_bahra(geo = "ARGENTINA")
```

## Arguments

- geo:

  un character con el nombre del distrito que se quiere descargar (por
  defecto toda ARGENTINA) Se puede chequear el id con
  [`show_arg_codes`](https://politicaargentina.github.io/geoAr/reference/show_arg_codes.md).

## Value

tibble con datos de BAHRA

## Examples

``` r
get_bahra()
#> Los datos fueron obtenidos del proyecto 'Base de Asentamientos Humanos de la Republica Argentina (BAHRA)'. La documentacion se encuetra disponible en http://www.bahra.gob.ar/
#> Simple feature collection with 14673 features and 17 fields
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: -73.02492 ymin: -77.87551 xmax: -27.70879 ymax: -21.78255
#> Geodetic CRS:  WGS 84
#> # A tibble: 14,673 × 18
#>       id codprov_censo nom_pcia    coddepto_censo nom_depto cod_ase nombre tipo 
#>    <dbl> <chr>         <chr>       <chr>          <chr>     <chr>   <chr>  <chr>
#>  1   780 06            Buenos Air… 021            Alberti   06021A… Palan… Para…
#>  2   851 06            Buenos Air… 049            Azul      06049A… Vicen… Para…
#>  3   446 14            Córdoba     014            Capital   140140… La Fl… Enti…
#>  4   572 50            Mendoza     028            Guaymall… 500280… Berme… Enti…
#>  5  1324 06            Buenos Air… 231            Daireaux  06231A… Luro   Para…
#>  6  1519 06            Buenos Air… 343            General … 06343A… La So… Para…
#>  7  2267 06            Buenos Air… 686            Rojas     066860… Rojas  Loca…
#>  8  2509 06            Buenos Air… 798            Tapalqué  06798A… Altona Para…
#>  9  2608 06            Buenos Air… 833            Tres Arr… 06833A… La Cu… Para…
#> 10  5445 22            Chaco       028            Chacabuco 22028A… Pampa… Para…
#> # ℹ 14,663 more rows
#> # ℹ 10 more variables: cod_aglo <chr>, nom_aglo <chr>, cod_agl <chr>,
#> #   nom_agl <chr>, lat_gd <chr>, long_gd <chr>, lat_gs <chr>, long_gs <chr>,
#> #   fuente <chr>, geometry <MULTIPOINT [°]>
```
