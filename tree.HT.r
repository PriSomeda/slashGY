#' Estimates the total height of trees for a single plot.
#'
#' \code{tree.HT} Estimates the total height of trees that have missing height from tree-level data.
#' For the missing trees there are two methods to use: 1) Estimates heights according
#' to a parametrized DBH-height model, or 2) Estimates heights by fitting a simple DBH-height model that requires at least
#' 10 measurements. Missing values are indentified as 'NA'.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param DBH       Vector of diameter at breast height (DBH, in). Must be complete and have the same size and order as TREEID.
#' @param HT        Vector of total height (ft). Must be of the same size and order as TREEID.
#' @param AREA      Numeric value of area of the inventory plot (ft2).
#' @param AGE       Numeric value of stand age (years). Required if methodHT = 1.
#' @param BA        Numeric value of Basal Area (ft2/acre). Required if methodHT = 1.
#' @param N         Numeric value of number of trees per acre. Required if methodHT = 1.
#' @param methodHT  Numeric value that identifies the method to estimate missing heights. 1: parametrized DBH-height model
#' that requires DBH, BA and AGE, 2: fits a simple DBH-height model from available measurements using the equation:
#' ln(HT) = b0 + b1/DBH. Default methodHT = 2.
#'
#' @return A list containing the following:
#' \itemize{
#' \item \code{HTFIN} A vector of final tree heigths (ft), replacing the missing values for estimated heigths and retaining the observed heights.
#' \item \code{r2} A value with the coefficient of determination from the fitting the DBH-height model when methodHT = 2.
#' }
#'
#' @references
#' Gonzalez-Benecke et al. (2014) - IParameterization of the 3-PG model for Pinus elliottii stands using alternative methods to estimate fertility rating, biomass
#' partitioning and canopy closure. Forest Ecology and Management, 327(1): 55–75; https://doi.org/10.1016/j.foreco.2014.04.030
#'
#' @examples
#' # Example 1 - Method 1 - Parametrized DBH-height model
#' DBH <- c(9.3,11.1,15.5,9,14.8,27.3,11.4,6.6,12.6,17.5,6.3,7.2,11.5,13.6,7.3,12,11.9,8.1,7.6,5)
#' HT <- c(11.8,12.3,NA,NA,15.3,18,12,NA,14.5,NA,NA,NA,NA,NA,10.3,14.6,NA,NA,NA,NA)
#' tree.HT(DBH=DBH, HT=HT, AREA=200, AGE=47, methodHT=1)
#'
#' # Example 2 - Method 2 - Simple DBH-height model
#' DBH <- c(9.3,11.1,15.5,9,14.8,27.3,11.4,6.6,12.6,17.5,6.3,7.2,11.5,13.6,7.3,12,11.9,8.1,7.6,5)
#' HT <- c(11.8,12.3,NA,NA,15.3,18,12,NA,14.5,NA,NA,NA,NA,NA,10.3,14.6,NA,NA,NA,NA)
#' tree.HT(DBH=DBH, HT=HT, methodHT=2)


tree.HT  <-  function(DBH, HT, AREA=NA, AGE=NA, BA=NA, N=NA, methodHT=2){

  if(length(DBH)==length(HT) & sum(is.na(DBH))==0){

    if(methodHT==1){

      if(is.na(AGE)==T | is.na(AREA)==T && is.na(BA)==T){
        stop("Warning - Incomplete information. Please check input.")
      }

      if(is.na(AGE)==F){

        if(is.na(AREA)==F & is.na(BA)==T){
          BA <- tree.STAND(DBH=DBH, AREA=AREA)$BA
        }

        if(is.na(AREA)==F & is.na(N)==T){
          N <- tree.STAND(DBH=DBH, AREA=AREA)$N
        }

        a1 <- 1.37
        a2 <- 1.0738
        a3 <- -5.4379
        a4 <- -0.8544
        a5 <- 0.545
        a6 <- -0.0218
        a7 <- 0.2243

        aux <- data.frame(DBH=DBH, HT=HT, HTEST=NA, HTFIN=NA)
        aux$HTEST <- (a1 + exp(a2 + a3*DBH^a4 + a5*log(AGE) + a6*log(N) + a7*log(BA)))

        #BA_m <- 0.229568*BA
        #N_m <- 2.47105*N

        #aux <- data.frame(DBH=2.54*DBH, HT=HT, HTEST=NA, HTFIN=NA)
        #aux$HTEST <- 3.28084*(a1 + exp(a2 + a3*DBH^a4 + a5*log(AGE) + a6*log(N_m) + a7*log(BA_m)))

        aux$HTFIN <- ifelse(is.na(aux$HT),aux$HTEST,aux$HT)
        r2 <- NA

      }

    }

    if(methodHT==2){

      aux <- data.frame(DBH=DBH, HT=HT)
      aux2 <-subset(aux, HT!='NA')
      model <- stats::lm(log(aux2$HT)~I(1/aux2$DBH),na.action='na.omit')
      r2 <- summary(model)$r.squared
      HTEST <- exp(model$coefficients[1]+model$coefficients[2]/DBH)
      aux$HTFIN <- ifelse(is.na(aux$HT),HTEST,aux$HT)

    }

  } else {

    stop("Warning - Incomplete information. Please check input.")

  }

  return(list(HTFIN=aux$HTFIN, r2=r2))

}
