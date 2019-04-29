#' Calculate dominant height for a plot based on tree-level data.
#'
#' \code{tree.HDOM} Calculate the dominant height for a plot based on tree-level data based on the top 25th percentile
#' of the tree heights. The provided vector of heights should be complete without missing data.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param HT Vector of tree heights (ft) for a given plot (must be complete).
#' @param DBH  Vector of diameter at breast height (DBH, in). Must be complete and have the same size and order of HT.
#' @param AREA Numeric value of area of the inventory plot (ft2).
#'
#' @return A value with Dominant Height (HDOM, ft) for the plot.
#'
#' @examples
#' treedata <- subset(treedata, is.na(DBH)==FALSE)
#' HT <- treedata$HT
#' DBH <- treedata$DBH
#' AREA <- 3240
#' tree.HDOM(HT=HT, DBH=DBH, AREA=AREA)


tree.HDOM  <-  function(HT,DBH,AREA){
                  if(sum(is.na(HT))==0 & sum(is.na(DBH))==0){
                    if(length(HT)==length(DBH)){
                      aux <- data.frame(DBH=DBH,HT=HT,EF=rep(1/(AREA/43560), length(HT)))
                    }
                    aux <- aux[order(-aux$DBH),]
                    i <- 1
                    trees <- aux$EF[1]
                    HDOM <- aux$HT[1]

                    while(trees<40){
                      trees <- sum(aux$EF[1:i])
                      wi <- c(rep(1,i-1),(1-((trees-40)/40)))/sum(c(rep(1,i-1),(1-((trees-40)/40))))
                      HDOM <- stats::weighted.mean(aux$HT[1:i],wi)
                      i <- i+1
                     }
                  } else {
                      #stop("Error: Missing information")
                      HDOM <- NA
                  }
            return(HDOM=HDOM)
          }

