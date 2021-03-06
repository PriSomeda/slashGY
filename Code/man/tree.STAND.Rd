% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree.STAND.r
\name{tree.STAND}
\alias{tree.STAND}
\title{Calculates the basal area, number of trees and mean quadratic diameter for a plot based on tree-level data.}
\usage{
tree.STAND(DBH, AREA)
}
\arguments{
\item{DBH}{Vector of diameter at breast height (DBH, in). Must be complete}

\item{AREA}{Numeric value of the size of the plot (ft2).}
}
\value{
A list containing the parameters:
\itemize{
\item \code{BA} Basal Area (ft2/acre).
\item \code{N}  Number of trees per acre.
\item \code{QD} Mean Quadratic Diameter (in).
}
}
\description{
\code{tree.STAND} Calculates the basal area, number of trees and mean quadratic diameter for a plot based on tree-level data.
}
\examples{
# Example - Stand information from inventory data
DBH <- treedata$DBH[!is.na(treedata$DBH)]
tree.STAND(DBH=DBH, AREA=3240)
}
\author{
Priscila Someda-Dias, Salvador A. Gezan
}
