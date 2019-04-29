#' Calculates dominant height for a plot.
#'
#' \code{stand.SITE} Calculates dominant height, based on site index (at reference age 25 year) and
#' stand age for a plot.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param HDOM      Numeric value of mean Dominant Height (ft).
#' @param SI        Numeric value of Site Index (ft) (Dominant Height of the plot at age 25 years).
#' @param AGE       Numeric value of stand Age (years).
#'
#' @return A list containing the parameters:
#' \itemize{
#' \item \code{HDOM} Dominant Height (ft).
#' \item \code{SI} Site Index (ft).
#' }
#'
#' @references
#' Refit for CFGRP FSBPdata of Chapman & Richards ()
#'  Hdom1 = Hdom2 x ((1-exp(b x Age1))/(1-exp(b x Age2))) ^ c
#' Anamorphic site index equation: Hdom = a x S x (1-exp(b x Age)) ^ c
#'
#'
#' @examples
#' # Example 1 - No fertilization
#' stand.SITE(SI=70,AGE=18)$HDOM
#'
#' # Example 2 - With fertilization
#' stand.SITE(SI=70,AGE=18,T2=1, AGEFERT=15)$HDOM
#'
#' # Example 3 - Site Index
#' stand.SITE(HDOM=54,AGE=18)$SI


stand.SITE  <-  function(HDOM=NA, SI=NA, AGE=NA){

  # Dominant Height Prediction
  # UNI only
  a1 <- 1.143349
  a2 <- -0.1015468
  a3 <- 1.6284317

  # UNI + IMP
  #a1 <- 1.143921
  #a2 <- -0.102330
  #a3 <- 1.668200

  # 1. If all informations are provided
  if(is.na(HDOM)==F && is.na(SI)==F && is.na(AGE)==F){
    stop("Warning - No parameteres to estimated. You already have all information.")
  }

  # 2. If HDOM is missing
  else if(is.na(HDOM)==T && is.na(SI)==F && is.na(AGE)==F){
    HDOM <- a1*SI*(1-exp(a2*AGE))^a3
  }

  # 3. If SI is missing and there is no fertilization
  else if(is.na(HDOM)==F && is.na(SI)==T && is.na(AGE)==F && is.na(AGEFERT)==T){
    #SI <- (HDOM)/(a1*(1-exp(a2*AGE))^a3)
    SI <- (HDOM)/(a1*(1-exp(a2*AGE))^a3)
  }


  # 4. If the minimum informations are provided
  else if(is.na(AGE)==F | is.na(HDOM)==T && is.na(AGEFERT)==T ){
    stop("Warning - Please provide information required.")
  }

  return(list(HDOM=HDOM, SI=SI))

}
