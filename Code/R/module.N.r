#' Predicts the number of trees (mortality) at next age based on stand-level informtion.
#'
#' \code{module.N} Estimates the number of trees (mortality) from initial age (AGE0) to next age (AGE1) based on stand level
#' information based on the equation:
#' N1 = N0 x  exp(-(-c1 -c2 x Z) x ((AGE1^c3)-(AGE0^c3)))
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param N0         Numeric value of number of trees per hectare at age 0.
#' @param AGE0       Numeric value of intial stand age or age 0 (years).
#' @param AGE1       Numeric value of final stand age or age 1 for prediction (years).
#' @param THINNING   If TRUE then thinning happened. Default is FALSE.
#' @param AGET       Optional numeric value of stand age (in years) where thinning is planned.
#'
#' @return A value with the number of trees per hectare at age 1 (N1).
#'
#' @references
#' Gonzalez-Benecke et al. (2010) - Forest management effects on in situ and ex situ slash pine forest carbon balance
#' Forest Ecology and Management, 260(5), 795-805; https://doi.org/10.1016/j.foreco.2010.05.038
#'
#' @examples
#' module.N(N0=376, AGE0=12, AGE1=21)$N1  # Whitout thinning
#' module.N(N0=376, AGE0=12, AGE1=31, THINNING=TRUE, AGET=18)$N1  # Whitout thinning


module.N  <-  function(N0=NA, AGE0=NA, AGE1=NA, THINNING=FALSE, AGET=NA){

  c1 <- -0.0041
  c2 <- -0.0019
  c3 <- 1.345
  
  Zt <- 0

  # 1. If all informations are provided
  if(is.na(N0)==F && is.na(AGE0)==F && is.na(AGE1)==F){

    if(THINNING==TRUE && is.na(AGET)==T |
       THINNING==TRUE && AGET<=AGE0) {
      Zt <- 1
    }

    N1 <- N0*exp((c1+c2*Zt)*((AGE1^c3)-(AGE0^c3)))

  } else {
    stop("Warning - Please provide information required.")

  }

  return(list(N1=N1))

}
