#' Data from inventory at tree-level to test
#'
#' A dataset containing the measurements at tree-level of 60 trees in the same plot.
#' The age of meadurement is 5 years and plot area is 3240 square meters.
#'
#' @format A data frame with 60 observations and 5 variables:
#' \describe{
#' \itemize{
#' \item \code{PLOTID} Unique plot identification. For a single plot, all trees should have the same PLOTID.
#' \item \code{TREEID} Unique tree identification.
#' \item \code{DBH} Diameter at breast height (DBH, inches). Must have the same size and order as TREEID.
#' \item \code{HT} Total height (feet). Must be of the same size and order as TREEID.
#' \item \code{OBS} Aditional information about the tree.
#' }
#'}
#' @source \url{http://www.sfrc.ufl.edu/CFGRP/}

"treedata"
