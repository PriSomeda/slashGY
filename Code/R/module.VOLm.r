#' Calculates the stand-level merchantable volume based on stand-level information.
#'
#' \code{module.VOLm} Calculates the stand-level merchantable volume inside and outside bark from a plot
#' based on the equation:  VOLm = VOL x exp(m1 x ((t/QD)^m2) + m3 x (N^m4) x (d/QD)^m5)
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param N       Numeric value of number of trees per acre.
#' @param QD      Numeric value of mean Quadratic Diameter (in).
#' @param t       Numeric value top stem diameter outside bark for merchantability limit (in).
#' @param d       Numeric value of a DBH threshold limit for merchantable trees (in).
#' @param VOL_OB  Total stand-level volume outside bark (ft3/acre).
#' @param VOL_IB  Total stand-level volume inside bark (ft3/acre).
#'
#' @return A list containing the parameters:
#' \itemize{
#' \item \code{VOLm_OB} Merchantable stand-level volume outside bark (ft3/acre).
#' \item \code{VOLm_IB} Merchantable stand-level volume inside bark (ft3/acre).
#' }
#'
#' @seealso
#' \code{\link{module.VOL}}
#'
#' @references
#' Gonzalez-Benecke et al. (2010) - Forest management effects on in situ and ex situ slash pine forest carbon balance
#' Forest Ecology and Management, 260(5), 795-805; https://doi.org/10.1016/j.foreco.2010.05.038
#'
#' @examples
#' VOL_OB<-module.VOL(HDOM=46, BA=100, N=376, AGE=12)$VOL_OB
#' VOL_IB<-module.VOL(HDOM=46, BA=100, N=376, AGE=12)$VOL_IB
#' module.VOLm(N=376, QD=7, t=1, d=1.96, VOL_OB=VOL_OB, VOL_IB=VOL_IB)


module.VOLm  <-  function(N=NA, QD=NA, t=NA, d=NA, VOL_OB=NA, VOL_IB=NA){

  # VOLm = VOL*exp(m1*((t/QD)^m2)+m3*(N^m4)*(d/QD)^m5)

  # Ouside bark estimated parameters
  m1_OB <- -0.52
  m2_OB <- 3.84
  m3_OB <- 0.69
  m4_OB <- -0.12
  m5_OB <- 5.72

  # Inside bark estimated parameters
  m1_IB <- -0.52
  m2_IB <- 3.84
  m3_IB <- 0.69
  m4_IB <- -0.12
  m5_IB <- 5.72

  # 1. If all informations are provided
  if(is.na(N)==F && is.na(QD)==F && is.na(t)==F &&
     is.na(d)==F && is.na(VOL_OB)==F && is.na(VOL_IB)==F){

    VOLm_OB <- VOL_OB*exp(m1_OB*((t/QD)^m2_OB)+m3_OB*(N^m4_OB)*(d/QD)^m5_OB)
    VOLm_IB <- VOL_IB*exp(m1_IB*((t/QD)^m2_IB)+m3_IB*(N^m4_IB)*(d/QD)^m5_IB)

  }

  # 2. If only volume outside of bark is provided
  else if(is.na(N)==F && is.na(QD)==F && is.na(t)==F &&
          is.na(d)==F && is.na(VOL_OB)==F && is.na(VOL_IB)==T){

    VOLm_OB <- VOL_OB*exp(m1_OB*((t/QD)^m2_OB)+m3_OB*(N^m4_OB)*(d/QD)^m5_OB)
    VOLm_IB <- NA
    print("Warning - Only total volume outside bark provided.")

  }

  # 3. If only volume inside of bark is provided
  else if(is.na(N)==F && is.na(QD)==F && is.na(t)==F &&
          is.na(d)==F && is.na(VOL_OB)==T && is.na(VOL_IB)==F){

    VOLm_IB <- VOL_IB*exp(m1_IB*((t/QD)^m2_IB)+m3_IB*(N^m4_IB)*(d/QD)^m5_IB)
    VOLm_OB <- NA
    print("Warning - Only total volume inside bark provided.")

  }

  # 4. If any informations is missing
  else {
    stop("Please provide information required for calculating merchantable volume.")
  }

  return(list(VOLm_OB=VOLm_OB, VOLm_IB=VOLm_IB))

}
