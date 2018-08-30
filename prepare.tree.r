#' Checks and prepares tree-level data from a single plot and calculates some stand-level parameters.
#'
#' \code{prepare.tree} Checks and prepares tree-level data from a single plot and then calculates stand-level
#' parameters such as basal area, number of trees per hectarea and dominant height. The provided vector of total heights
#' can have missing information. If there are missing trees, there are two methods to use: 1) Estimates heights according
#' to a parametrized DBH-height model, or 2) Estimates heights by fitting a simple DBH-height model that requires at least
#' 10 measurements. Missing values are indentified as 'NA'.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param TREEID    Vector of unique tree identification.
#' @param DBH       Vector of diameter at breast height (DBH, in). Must be complete and have the same size and order as TREEID.
#' @param HT        Vector of total height (ft). Must be of the same size and order as TREEID.
#' @param AREA      Numeric value of area of the inventory plot (ft2).
#' @param AGE       Numeric value of stand age (years). Required if method = 1.
#' @param methodHT  Numeric value that identifies the method to estimate missing heights. 1: parametrized DBH-height model,
#' 2: fits a simple DBH-height model from available measurements. Default method = 2.
#'
#' @return A list containing the following:
#' \itemize{
#' \item \code{BA}   Basal Area (ft2/acre).
#' \item \code{N}    Number of trees per acre.
#' \item \code{HDOM} Dominant Height (ft).
#' \item \code{tree.table} Data frame with all tree data an observed heights (for the ones provided) and estimated heights
#' (for those missing). The data frame contains the columns: TREEID, DBH and HT.
#' }
#'
#' @seealso
#' \code{\link{tree.HT}}
#'
#' @examples
#' # Example - Stand-level information from inventory data
#' treedata <- subset(treedata, is.na(DBH)==FALSE)
#' TREEID <- treedata$TREEID
#' DBH <- treedata$DBH
#' HT <- treedata$HT
#' prepare.tree(TREEID=TREEID, DBH=DBH, HT=HT, AREA=3240, AGE=5, methodHT=2)


prepare.tree <- function(TREEID=NA, DBH=NA, HT=NA, AREA=NA, AGE=NA, methodHT=2){

  # Check if all requeriments are meet
  if(sum(is.na(TREEID))==0 && sum(is.na(DBH))==0 |            # TREEID and DBH must be complete
     length(TREEID)==length(DBH) && length(DBH)==length(HT)){ # TREEID, DBH and HT must have the same size

    BA <- tree.STAND(DBH, AREA)$BA
    N <- tree.STAND(DBH, AREA)$N

    if(sum(is.na(HT))!=0){
      if(methodHT==1){
        HT <- round(tree.HT(DBH=DBH, HT=HT, AREA=AREA, AGE=AGE, BA=BA, N=N, methodHT=methodHT)$HTFIN,2)
      } else {
        if(methodHT==2 && (length(HT)-sum(is.na(HT))>=10)){
          HT <- round(tree.HT(DBH=DBH, HT=HT, AREA=AREA, AGE=AGE, methodHT=methodHT)$HTFIN,2)
        } else {
          stop("Error: Not enough tree height measurements to fit model.")
        }
      }
    }


    HDOM <- tree.HDOM(HT=HT,DBH=DBH,AREA=AREA)

  } else {
    stop("Error: Violation of data requeriments. Check input.")
  }

  tree.table<-data.frame(TREEID,DBH,HT)
  return(list(BA=BA,N=N, HDOM=HDOM, tree.table=tree.table))

}
