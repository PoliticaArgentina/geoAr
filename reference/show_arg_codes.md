# Diccionario de códigos identificadores de distritos (*geo ID's dictoniary*)

Función que devuelve un *data.frame* con códigos y equivalencias de
identificación de unidades geográficas

## Usage

``` r
show_arg_codes(viewer = FALSE, nivel = "provincias")
```

## Arguments

- viewer:

  Por default es `TRUE` y muestra una tabla formateada en el *Viewer* de
  *RStudio*. Cuando `FALSE` imprime en consola.

- nivel:

  Un character que permite elegir opción para ver diccionario a nivel de
  `"provincias"` o de `"departamentos"`.

## Value

tibble con códigos geográficos correspondientes a distritos de Argentina
descargados con
[`get_geo`](https://politicaargentina.github.io/geoAr/reference/get_geo.md)

## Examples

``` r

show_arg_codes(viewer = FALSE)
#> # A tibble: 26 × 5
#>    id           codprov codprov_censo codprov_iso name_iso                      
#>    <chr>        <chr>   <chr>         <chr>       <chr>                         
#>  1 ARGENTINA    " "     " "           AR          Argentina                     
#>  2 CABA         "01"    "02"          AR-C        Ciudad Autónoma de Buenos Air…
#>  3 BUENOS AIRES "02"    "06"          AR-B        Buenos Aires                  
#>  4 CATAMARCA    "03"    "10"          AR-K        Catamarca                     
#>  5 CORDOBA      "04"    "14"          AR-X        Córdoba                       
#>  6 CORRIENTES   "05"    "18"          AR-W        Corrientes                    
#>  7 CHACO        "06"    "22"          AR-H        Chaco                         
#>  8 CHUBUT       "07"    "26"          AR-U        Chubut                        
#>  9 ENTRE RIOS   "08"    "30"          AR-E        Entre Ríos                    
#> 10 FORMOSA      "09"    "34"          AR-P        Formosa                       
#> # ℹ 16 more rows
```
