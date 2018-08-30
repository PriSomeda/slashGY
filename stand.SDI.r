#' Calculates relative stand density index for Slash pine (\emph{Pinus ellioti})
#'
#' \code{stand.SDI} Calculates the relative stand density index (RSDI, \%) for Slash in a single plot using the
#' expression: RSDI = 100xNx(QD/10)^1.605/SDImax. For Slash pine (\emph{Pinus ellioti}) SDImax is 400 trees/acre.
#' All parameters are required to compute the RSDI.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param N   Numeric value of the number of trees per acre.
#' @param QD  Numeric value of mean quadratic diameter (in).
#'
#' @return A value for the relative stand density index (RSDI, \%).
#'
#' @examples
#' stand.SDI(QD=7, N=376)


stand.SDI <- function(N=NA, QD=NA){

  # If the minimum informations are provided
  if(is.na(N)==T && is.na(QD)==T |
     is.na(N)==F && is.na(QD)==T |
     is.na(N)==T && is.na(QD)==F){
    stop("Warning - Please provide information required.")
  }

  SDImax <- 400  # trees/acre for slash
  SDI <- N*(QD/10)^1.605
  SDIR <- 100*SDI/SDImax

  return(SDIR=SDIR)
}
