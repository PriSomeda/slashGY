#' Calculates the total stand-level volume based on stand-level information.
#'
#' \code{module.VOL} Calculates the total stand-level volume inside and outside bark for
#' a Slash pine stands based a selected method.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param HDOM      Numeric value of Site Index (ft) (Dominant Height of the plot at age 25 years).
#' @param BA        Numeric value of Basal Area of the plot (ft2/acre).
#' @param N         Numeric value of number of trees per acre.
#' @param AGE       Numeric value of Age of the plot (in years).
#' @param methodVOL Numeric value that identifies the method to compute the total stand-level.\itemize{
#' \item 1: parametrized DBH-height model that requires HDOM, N, BA and AGE.
#' \item 2: parametrized DBH-height model that requires only HDOM and BA.
#' }
#' Default method = 1.
#'
#' @return A list containing the parameters:
#' \itemize{
#'  \item \code{VOL_OB} Total stand-level volume outside bark (ft3/acre).
#'  \item \code{VOL_IB} Total stand-level volume inside bark (ft3/acre).
#' }
#'
#' @references
#' Gonzalez-Benecke et al. (2010) - Forest management effects on in situ and ex situ slash pine forest carbon balance
#' Forest Ecology and Management, 260(5), 795-805; https://doi.org/10.1016/j.foreco.2010.05.038
#'
#' @examples
#' # Example 1 - Method 1
#' module.VOL(HDOM=46, BA=100, N=376, AGE=12, methodVOL=1)
#'
#' # Example 2 - Method 2
#' module.VOL(HDOM=46, BA=100, N=376, AGE=12, methodVOL=2)
#'
#' # Example 3 - Method 3
#' module.VOL(HDOM=46, BA=100, N=376, AGE=12, methodVOL=3)


module.VOL  <-  function(HDOM=NA, BA=NA, N=NA, AGE=NA, methodVOL=1){

  # methodVOL = 1
  # Paper - Yin et al. (1998)
  # VOB = (HDOM^a1)*(N^(a2-a3/AGE))*(BA^(a4-a5/AGE))
  a1 = 0.820
  a2 = -0.017
  a3 = -0.320
  a4 = 1.016
  a5 = 0.501

  # Excel: VIB =	VOB * (b1 + b2*ln(Dq))
  b1 = 0.4389
  b2 = 0.1815

  # methodVOL = 2
  # Excel without thinning - Logan 2005
  # VOL/WT = b0*(HDOM^b1)*(N^b2)*(BA^(b3+b4/AGE))

  # IB
  b0_OB = 1.694334
  b1_OB = 0.669564
  b2_OB = -0.09417
  b3_OB = 1.125543
  b4_OB = -0.07003

  # OB
  b0_IB = 0.699864
  b1_IB = 0.829137
  b2_IB = -0.10023
  b3_IB = 1.137847
  b4_IB = -0.08739

  # methodVOL = 3
  # Excel - Volume removed
  # VOB = exp(a)*(Hdom^b)*(BA^c)
  a = -0.173000
  b = 0.836000
  c = 1.018000


  if(methodVOL==1 && is.na(HDOM)==F && is.na(BA)==F && is.na(N)==F && is.na(AGE)==F){

    VOL_OB <- (HDOM^a1)*(N^(a2-a3/AGE))*(BA^(a4-a5/AGE)) # Paper
    QD <- stand.STAND(BA=BA, N=N)$QD
    VOL_IB <-	VOL_OB*(b1+b2*log(QD))	# Not in the paper

  }

  else if(methodVOL==2 && is.na(HDOM)==F && is.na(BA)==F && is.na(N)==F && is.na(AGE)==F){

    VOL_OB <- b0_OB*(HDOM^b1_OB)*(N^b2_OB)*(BA^(b3_OB+b4_OB/AGE))
    VOL_IB <- b0_IB*(HDOM^b1_IB)*(N^b2_IB)*(BA^(b3_IB+b4_IB/AGE))

  }


  else if(methodVOL==3 && is.na(HDOM)==F && is.na(BA)==F && is.na(N)==F){
    VOL_OB <- exp(a)*(HDOM^b)*(BA^c)
    QD <- stand.STAND(BA=BA, N=N)$QD
    VOL_IB <-	VOL_OB*(b1+b2*log(QD))	# Not in the paper

  }
  else {

    VOL_OB <- NA
    VOL_IB <- NA
    stop("Warning - Please provide information required.")

  }

  return(list(VOL_OB=VOL_OB, VOL_IB=VOL_IB))

}
