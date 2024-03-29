% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_arg_codes.R
\name{show_arg_codes}
\alias{show_arg_codes}
\title{Diccionario de códigos identificadores de distritos
(\emph{geo ID's dictoniary})}
\usage{
show_arg_codes(viewer = FALSE, nivel = "provincias")
}
\arguments{
\item{viewer}{Por default es \code{TRUE} y  muestra una tabla formateada en el \emph{Viewer} de \emph{RStudio}. Cuando \code{FALSE} imprime en consola.}

\item{nivel}{Un character que permite elegir opción para ver diccionario a nivel de \code{"provincias"} o de \code{"departamentos"}.}
}
\value{
tibble con códigos geográficos correspondientes a distritos de Argentina descargados con \code{\link{get_geo}}
}
\description{
Función que devuelve un \emph{data.frame} con códigos y equivalencias de identificación de unidades geográficas
}
\examples{

show_arg_codes(viewer = FALSE)

}
