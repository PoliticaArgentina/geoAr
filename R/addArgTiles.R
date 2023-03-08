#'Agrega capa de Argentina para mapa interactivo
#'   (\emph{Load Argentina Tiles Template})
#'@description
#'Función que descarga capa base de Argentina para mapa interactivo creado con la libreria \code{leaflet}.
#'
#'@param data debe ser un objeto con class "leaflet" "htmlwidget".
#'@return Capa base para mapas interactivos
#'@details  El objetivo es agregar referencias de la base sobre la que se visualizan geometrías de Argentina. Se pueden agregar capas como polígonos descargandolos con \code{\link{get_geo}}.
#' Los geometrías disponibles se pueden chequear con \code{\link{show_arg_codes}}.

#' @examples
#'
#'  get_geo("TUCUMAN") %>%
#'  leaflet::leaflet() %>%
#'  leaflet::addPolygons() %>%
#'  addArgTiles()
#'
#'
#'@export


addArgTiles <- function(data){

  data %>%
    leaflet::addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png")

}
