# Recodifica id de grillas asignados a provincias o departamentos de Argentina (*Recode Argentina's districs id in grids*)

Función que permite re codificar etiquetas para utilización de grillas
de Argentina y de departamentos para los 24 distritos sub nacionales.
Ello permite hacer mas sencilla la vinculación con bases de datos de
diversas fuentes.

## Usage

``` r
recode_grid(data, type = NULL)
```

## Arguments

- data:

  data.frame obtenido con
  [`get_grid`](https://politicaargentina.github.io/geoAr/reference/get_grid.md).
  Las grillas disponibles se pueden chequear con
  [`show_arg_codes`](https://politicaargentina.github.io/geoAr/reference/show_arg_codes.md).

- type:

  la variante del código que se quiere definir para la grilla. Las
  opciones son `'indra'`, `'indec'` o `'iso'`.

## Value

Los valores por defecto en
[`get_grid`](https://politicaargentina.github.io/geoAr/reference/get_grid.md)
son `codprov` para provincia y `coddepto` para departamentos,
respectivamente. Estos corresponden a la codificación de los escrutinios
provisorios de elecciones nacionales y se etiquetaron como `'indra'` .
Se puede optar por la nomenclatura de `'indec'`, con la familia
`\*_censo`, para ambos niveles, o la de `'iso'` con `\*_iso` , para el
nivel provincial.

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

get_grid("ARGENTINA")
#> Adding missing grouping variables: `name_provincia`
#>    name_provincia                name row col code
#> 1       ARGENTINA               JUJUY   1   1   10
#> 2       ARGENTINA             FORMOSA   2   3   09
#> 3       ARGENTINA            MISIONES   2   5   14
#> 4       ARGENTINA               SALTA   2   1   17
#> 5       ARGENTINA             TUCUMAN   2   2   23
#> 6       ARGENTINA           CATAMARCA   3   1   03
#> 7       ARGENTINA               CHACO   3   3   06
#> 8       ARGENTINA          CORRIENTES   3   4   05
#> 9       ARGENTINA SANTIAGO DEL ESTERO   3   2   22
#> 10      ARGENTINA             CORDOBA   4   2   04
#> 11      ARGENTINA          ENTRE RIOS   4   4   08
#> 12      ARGENTINA            LA RIOJA   4   1   12
#> 13      ARGENTINA            SANTA FE   4   3   21
#> 14      ARGENTINA        BUENOS AIRES   5   3   02
#> 15      ARGENTINA                CABA   5   4   01
#> 16      ARGENTINA            SAN JUAN   5   1   18
#> 17      ARGENTINA            SAN LUIS   5   2   19
#> 18      ARGENTINA            LA PAMPA   6   2   11
#> 19      ARGENTINA             MENDOZA   6   1   13
#> 20      ARGENTINA             NEUQUEN   7   1   15
#> 21      ARGENTINA           RIO NEGRO   7   2   16
#> 22      ARGENTINA              CHUBUT   8   1   07
#> 23      ARGENTINA          SANTA CRUZ   9   1   20
#> 24      ARGENTINA    TIERRA DEL FUEGO  10   1   24


get_grid("ARGENTINA") %>%
   recode_grid(type = "iso")
#> Adding missing grouping variables: `name_provincia`
#>                   name row col code
#> 1                JUJUY   1   1 AR-Y
#> 2              FORMOSA   2   3 AR-P
#> 3             MISIONES   2   5 AR-N
#> 4                SALTA   2   1 AR-A
#> 5              TUCUMAN   2   2 AR-T
#> 6            CATAMARCA   3   1 AR-K
#> 7                CHACO   3   3 AR-H
#> 8           CORRIENTES   3   4 AR-W
#> 9  SANTIAGO DEL ESTERO   3   2 AR-G
#> 10             CORDOBA   4   2 AR-X
#> 11          ENTRE RIOS   4   4 AR-E
#> 12            LA RIOJA   4   1 AR-F
#> 13            SANTA FE   4   3 AR-S
#> 14        BUENOS AIRES   5   3 AR-B
#> 15                CABA   5   4 AR-C
#> 16            SAN JUAN   5   1 AR-J
#> 17            SAN LUIS   5   2 AR-D
#> 18            LA PAMPA   6   2 AR-L
#> 19             MENDOZA   6   1 AR-M
#> 20             NEUQUEN   7   1 AR-Q
#> 21           RIO NEGRO   7   2 AR-R
#> 22              CHUBUT   8   1 AR-U
#> 23          SANTA CRUZ   9   1 AR-Z
#> 24    TIERRA DEL FUEGO  10   1 AR-V

```
