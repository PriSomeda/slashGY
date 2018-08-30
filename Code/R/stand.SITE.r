#' Calculates dominant height for a plot.
#'
#' \code{stand.SITE} Calculates dominant height, based on site index (at reference age 25 year) and
#' stand age for a plot considering silvicultural managements (i.e. site preparation, fertilization).
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param HDOM      Numeric value of mean Dominant Height (ft).
#' @param SI        Numeric value of Site Index (ft) (Dominant Height of the plot at age 25 years).
#' @param AGE       Numeric value of stand Age (years).
#' @param Z1        If fertilized at planting then Z1 = 1. Default is 0.
#' @param Z2        If bedded (site preparation) then Z2 = 1. Default is 0.
#' @param Z3        If herbicide was used then Z3 = 1. Default is 0.
#' @param T1        If only Nitrogen fertilization at AGEFERT then T1 = 1. Default is 0.
#' @param T2        If N and P fertilization at AGEFERT then T2 = 1. Default is 0.
#' @param AGEFERT   Numeric value of stand age (in years) where fertilization is planned. Required if T1=1 or T2=1.
#'
#' @return A list containing the parameters:
#' \itemize{
#' \item \code{HDOM} Dominant Height (ft).
#' \item \code{SI} Site Index (ft).
#' }
#'
#' @references
#' Gonzalez-Benecke et al. (2010) - Forest management effects on in situ and ex situ slash pine forest carbon balance
#' Forest Ecology and Management, 260(5), 795-805; https://doi.org/10.1016/j.foreco.2010.05.038
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


stand.SITE  <-  function(HDOM=NA, SI=NA, AGE=NA, Z1=0, Z2=0, Z3=0, T1=0, T2=0, AGEFERT=NA){

  # Dominant Height Prediction
  a1 <- 1.3679
  a2 <- -0.07345
  a3 <- 1.804
  a4 <- 0.678
  a5 <- 0.546
  a6 <- 1.395
  a7 <- -0.412
  a8 <- -0.0691

  # Response to Fertilization
  b1<- 0.375379
  b2 <- 0.652099
  b3 <- 0.154525
  b4 <- 0.100104


  # 1. If all informations are provided
  if(is.na(HDOM)==F && is.na(SI)==F && is.na(AGE)==F){
    stop("Warning - No parameteres to estimated. You already have all information.")
  }

  # 2. If HDOM is missing and there is no fertilization
  else if(is.na(HDOM)==T && is.na(SI)==F && is.na(AGE)==F && is.na(AGEFERT)==T){
    HDOM <- a1*SI*(1-exp(a2*AGE))^a3 + (a4*Z1 + a5*Z2 + a6*Z3 + a7*Z1*Z3)*AGE*exp(a8*AGE)
  }

  # 3. If HDOM is missing and there is fertilization
  else if(is.na(HDOM)==T && is.na(SI)==F && is.na(AGE)==F && is.na(AGEFERT)==F){
    HDOM <- a1*SI*(1-exp(a2*AGE))^a3 + (a4*Z1 + a5*Z2 + a6*Z3 + a7*Z1*Z3)*AGE*exp(a8*AGE)
    HDOM <- HDOM + (b1*T1 + b2*T2)*(AGE-AGEFERT)*exp(-(AGE-AGEFERT)*(b3*T1 + b4*T2))
  }

  # 4. If SI is missing and there is no fertilization
  else if(is.na(HDOM)==F && is.na(SI)==T && is.na(AGE)==F && is.na(AGEFERT)==T){
    #SI <- (HDOM)/(a1*(1-exp(a2*AGE))^a3)
    SI <- (HDOM - (a4*Z1 + a5*Z2 + a6*Z3 + a7*Z1*Z3)*AGE*exp(a8*AGE))/(a1*(1-exp(a2*AGE))^a3)
  }


  # 4. If the minimum informations are provided
  else if(is.na(AGE)==F | is.na(HDOM)==T && is.na(AGEFERT)==T ){
    stop("Warning - Please provide information required.")
  }
  
  return(list(HDOM=HDOM, SI=SI))

}
