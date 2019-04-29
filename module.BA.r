#' Predicts and projects stand basal area based on stand-level information.
#'
#' \code{module.BA} Predicts and projects stand basal area based on number of trees per acre and
#' dominant height at the stand-level considering silvicultural information.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param N0          Numeric value of number of trees per acre at age 0 (or initial age).
#' @param HDOM0       Numeric value of Dominant Height (feet) at age 0.
#' @param AGE0        Numeric value of initial stand age (years).
#' @param Z1          If fertilized at planting then Z1 = 1. Default is 0.
#' @param Z2          If bedded (site preparation) then Z2 = 1. Default is 0.
#' @param Z3          If herbicide was used then Z3 = 1. Default is 0.
#' @param ZB          If burned was used then ZB = 1. Default is 0.
#' @param T1          If only Nitrogen fertilization at AGEFERT then T1 = 1. Default is 0.
#' @param T2          If N and P fertilization at AGEFERT then T2 = 1. Default is 0.
#' @param AGEFERT     Numeric value of stand age (in years) where fertilization is planned. Required if T1=1 or T2=1.
#' @param S1          If soil is Goup C then S1 = 1. Default is 0.
#' @param S2          If soil is Goup D then S2 = 1. Default is 0.
#' @param projection  If TRUE then model projection from provided BA0 is executed for a 1 year increment. Default: FALSE.
#' @param BA0         Numeric value of Basal Area (ft2/acre) at age 0 (required for model projection).
#' @param N1          Numeric value of number of trees per acre at age 1 for projection.
#' @param HDOM1       Numeric value of Dominant Height (ft) at age 1 for projection.
#'
#' @return A list containing:
#' \itemize{
#' \item \code{BA0} Predicted Basal Area at age 0 (ft2/acre).
#' \item \code{BA1} Projected Basal Area at age 1 (ft2/acre).
#' }
#'
#' @references
#' Gonzalez-Benecke et al. (2010) - Forest management effects on in situ and ex situ slash pine forest carbon balance
#' Forest Ecology and Management, 260(5), 795-805; https://doi.org/10.1016/j.foreco.2010.05.038
#'
#' @examples
#' # Example 1 - Predicting BA
#' module.BA(N0=693, HDOM0=50.00, AGE0=12, Z1=0, Z2=0, Z3=0, ZB=0, T1=0, T2=0, S1=0, S2=0)$BA0
#' module.BA(N0=683.93, HDOM0=54.61, AGE0=13, Z1=0, Z2=0, Z3=0, ZB=0, T1=0, T2=0, S1=0, S2=0)$BA0
#' module.BA(N0=674.74, HDOM0=59.05, AGE0=14, Z1=0, Z2=0, Z3=0, ZB=0, T1=0, T2=0, S1=0, S2=0)$BA0
#'
#' # Example 2 - Projecting BA
#' SI <- stand.SITE(HDOM=50, AGE=12)$SI
#' HDOM1 <-stand.SITE(SI=SI, AGE=13)$HDOM
#' HDOM4 <-stand.SITE(SI=SI, AGE=14)$HDOM
#'
#' N1 <- module.N(N0=693, AGE0=12, AGE1=13)$N1
#' N2 <- module.N(N0=683.93, AGE0=13, AGE1=14)$N1
#'
#' module.BA(N0=493, HDOM0=41.57, AGE0=12, projection=TRUE, BA0=51.74, N1=N1, HDOM1=HDOM1)$BA1
#' module.BA(N0=493, HDOM0=41.57, AGE0=12, projection=TRUE, BA0=51.74, N1=N1, HDOM1=HDOM1)$BA0
#'
#' module.BA(N0=693, HDOM0=50, AGE0=12, projection=TRUE, BA0=58.753, N1=683.93, HDOM1=54.61)$BA1
#' module.BA(N0=683.93, HDOM0=54.61, AGE0=13, projection=TRUE, BA0=64.52, N1=674.74, HDOM1=59.05)$BA1


module.BA  <-  function(N0=NA, HDOM0=NA, AGE0=NA, Z1=0, Z2=0, Z3=0, ZB=0,
                        T1=0, T2=0, AGEFERT=NA, S1=0, S2=0, projection=FALSE, BA0=NA, N1=NA, HDOM1=NA){


  # Paper
  # (BA1) =	EXP(a1 + a2/Age)*Hdom^(a3+a4/Age)*N^(a5+a6/Age)+(a7*Z1+a8*Z3)*Age*EXP(a9*Age))
  a1 = -3.394
  a2 = -35.668
  a3 = 1.336
  a4 = 6.205
  a5 = 0.366
  a6 = 3.155
  a7 = 0.436
  a8 = 2.134
  a9 = -0.0960

  #a1 = -3.394
  #a2 = -34.0570
  #a3 = 1.2410
  #a4 = 4.8130
  #a5 = 0.3180
  #a6 = 3.3050
  #a7 = 0.4190
  #a8 = 2.7630
  #a9 = -0.0960



  # RBA =	(b1*T1 + b2*T2)*(Age-Agef)*exp(-(age-Agef)*(b3*T1 + b4*T2 - b5*S1 - b6*/S2))
  b1 <- 1.12915
  b2 <- 1.68711
  b3 <- 0.09793
  b4 <- 0.073688
  b5 <- -0.046995
  b6 <- -0.055385

  # Alternative
  # ln(BA)=c0+(c1/AGE)+c2*ln(N)+c3*ln(HDOM)+c4*ln(N)/AGE + c5*ln(HDOM)/AGE
  # Logan 2005
  #  c0 <- -3.2101
  #  c1 <- -22.1226
  #  c2 <- 0.4064
  #  c3 <- 1.3302
  #  c4 <- 2.6972
  #  c5 <- 1.8881

  # Pienaar et al. 1996
  # c0 <- -4.807
  # c1 <- –26.273
  # c2 <- 0.527
  # c3 <- 1.512
  # c4 <- 2.497
  # c5 <- - 4.129


  if(is.na(N0)==T | is.na(HDOM0)==T | is.na(AGE0)==T){
    stop("Warning - Please provide information required.")
  }

  # Prediction
  else if(projection==FALSE){

    # Prediction and there is no fertilization
    if(is.na(AGEFERT)==T){
      BA0 <- 0.9162284*(exp(a1+a2/AGE0)*HDOM0^(a3+a4/AGE0)*N0^(a5+a6/AGE0)+(a7*Z1+a8*Z3)*AGE0*exp(a9*AGE0))
    }

    # Prediction and there is fertilization
    else if(is.na(AGEFERT)==F){
      BA0 <- 0.9162284*(exp(a1+a2/AGE0)*HDOM0^(a3+a4/AGE0)*N0^(a5+a6/AGE0)+(a7*Z1+a8*Z3)*AGE0*exp(a9*AGE0))
      BA0 <- BA0 + (b1*T1 + b2*T2)*(AGE0-AGEFERT)*exp(-(AGE0-AGEFERT)*(b3*T1 + b4*T2 - b5*S1 - b6*S2))
    }
    BA1 <- NA

  }

  # Projection
  else if (projection==TRUE){

    if(is.na(BA0)==T | is.na(N1)==T | is.na(HDOM1)==T){
      print("Warning - Please provide information required for projection.")
    }

    deltaN <- (1/N0)*(N1-N0)
    deltaHdom <- (1/HDOM0)*(HDOM1-HDOM0)
    deltaNx <- (N0^(a5+a6/AGE0))*(((a5+a6/AGE0)*deltaN)+(log(N0)*(-a6/(AGE0^2))))
    deltaHdomx <-(HDOM0^(a3+a4/AGE0))*(((a3+a4/AGE0)*deltaHdom)+(log(HDOM0)*(-a4/(AGE0^2))))
    part1 <- (exp(a1+a2/AGE0))*(((HDOM0^(a3+a4/AGE0))*deltaNx)+((N0^(a5+a6/AGE0))*deltaHdomx))+
             ((N0^(a5+a6/AGE0))*(HDOM0^(a3+a4/AGE0))*(exp(a1+a2/AGE0))*(-a2/(AGE0^2)))
    part2 <- ((a7*Z1+a8*Z3)*(exp(a9*AGE0)))*(a9*AGE0+1)
    deltaBA <- part1 + part2
    BA1 <- BA0+deltaBA*0.9162284

  }

  return(list(BA0=BA0, BA1=BA1))

}
