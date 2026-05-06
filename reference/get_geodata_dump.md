# Descargar Datos Geográficos Completos

Permite descargar listados completos de entidades geográficas en
diversos formatos. Accede al endpoint de la georef-ar-api. Si existe
GEOREFAR_TOKEN en el Renviron lo usará para hacer la consulta (aunque
generalmente no es necesario para estos endpoints públicos).

## Usage

``` r
get_geodata_dump(entidad, formato, path_to_save = NULL)
```

## Arguments

- entidad:

  Cadena de texto. La entidad geográfica a descargar. Valores posibles:
  "provincias", "departamentos", "municipios", "localidades",
  "localidades-censales", "asentamientos", "calles", "cuadras".

- formato:

  Cadena de texto. El formato deseado para el archivo. Valores posibles:
  "csv", "json", "geojson", "ndjson".

- path_to_save:

  Cadena de texto opcional. Ruta completa (incluyendo nombre de archivo
  y extensión) donde guardar el archivo descargado. Si es NULL (por
  defecto), la función devolverá el contenido parseado (para
  json/geojson/ndjson) o un data frame (para csv). Si se especifica una
  ruta, la función guardará el archivo y devolverá la ruta del archivo
  guardado.

## Value

Dependiendo de 'path_to_save' y 'formato': - Si 'path_to_save' se
especifica: la ruta al archivo guardado (invisiblemente). - Si
'path_to_save' es NULL: - Para "csv": un data.frame. - Para "json",
"geojson", "ndjson": una lista o estructura de R parseada desde JSON. -
Si la descarga o parseo falla, genera un error.

## References

[georef-ar-api/descargas](https://datosgobar.github.io/georef-ar-api/open-api/#/Recursos/get__filename_)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Obtener provincias en formato GeoJSON como objeto R
  provincias_geojson <- get_geodata_dump(entidad = "provincias", formato = "geojson")

  # Guardar departamentos en formato CSV
  get_geodata_dump(entidad = "departamentos", formato = "csv", path_to_save = "deptos.csv")
} # }
```
