#' Calculate dominant height for a plot based on tree-level data.
#'
#' \code{tree.HDOM25} Calculate the dominant height for a plot based on tree-level data based on the top 25th percentile
#' of the tree heights. The provided vector of heights should be complete without missing data.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param HT Vector of tree heights (ft) for a given plot (must be complete).
#'
#' @return A value with Dominant Height (HDOM, ft) for the plot.
#'
#' @examples
#' HT <- treedata$HT[!is.na(treedata$HT)]
#' tree.HDOM25(HT=HT)


tree.HDOM25  <-  function(HT){

  if(sum(is.na(HT))==0){

    HT <- HT[order(-HT)]
    H75 <- as.vector(stats::quantile(HT, 0.75))
    HDOM <- mean(HT[HT>=H75])

  } else {

    stop("Error: Not enought height measurements to fit model.")

  }

  return(HDOM=HDOM)

}

