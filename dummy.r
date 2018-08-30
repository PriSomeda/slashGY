#' Convert information as the right dummy variable numbers
#'
#' \code{dummy} Convert logical information or selectin to the right values as
#' categories that will be use in the G&Y Model.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param PFERT         TRUE if fertilized at planting. Default is FALSE.
#' @param BEDDED        TRUE if bedded (site preparation). Default is FALSE.
#' @param HERB          TRUE if herbicide was used. Default is FALSE.
#' @param BURN          TRUE if burned was used. Default is FALSE.
#' @param TYPEFERT      Type of fertilization at AGEFERT. N: only Nitrogen. NP: N and P. Default is NA.
#' @param AGEFERT       Numeric value of stand age (in years) where fertilization is planned. Required if definied TYPEFERT.
#' @param SOIL          Soil type. C: Group C. D: Group D. Default is NA.
#'
#' @return A list containing:
#' \itemize{
#' \item \code{Z1}Dummy variable if fertilized at planting then Z1 = 1.
#' \item \code{Z2}Dummy variable if bedded (site preparation) then Z2 = 1.
#' \item \code{Z3}Dummy variable if herbicide was used then Z3 = 1.
#' \item \code{ZB}Dummy variable if burned was used then ZB = 1.
#' \item \code{T1}Dummy variable if only Nitrogen fertilization at AGEFERT then T1 = 1.
#' \item \code{T2}Dummy variable if N and P fertilization at AGEFERT then T2 = 1.
#' \item \code{AGEFERT}Numeric value of stand age (in years) where fertilization is planned.
#' \item \code{S1}Dummy variable if soil is Goup C then S1 = 1.
#' \item \code{S2}Dummy variable if soil is Goup D then S2 = 1.
#' }
#'
#' @references
#' Gonzalez-Benecke et al. (2010) - Forest management effects on in situ and ex situ slash pine forest carbon balance
#' Forest Ecology and Management, 260(5), 795-805; https://doi.org/10.1016/j.foreco.2010.05.038
#'
#' @examples
#' dummy(PFERT=TRUE, BEDDED=TRUE, HERB=FALSE, BURN=FALSE, TYPEFERT='NA', AGEFERT=NA, SOIL='C')
#' dummy(PFERT=FALSE, BEDDED=FALSE, HERB=FALSE, BURN=FALSE, TYPEFERT='NP', AGEFERT=15, SOIL='NA')

dummy  <-  function(PFERT=FALSE, BEDDED=FALSE, HERB=FALSE, BURN=FALSE,
                    TYPEFERT='NA', AGEFERT=NA, SOIL='NA'){

  Z1 <- 0
  Z2 <- 0
  Z3 <- 0
  ZB <- 0
  T1 <- 0
  T2 <- 0
  S1 <- 0
  S2 <- 0

  if(PFERT==TRUE)     {  Z1 <- 1 }
  if(BEDDED==TRUE)    {  Z2 <- 1 }
  if(HERB==TRUE)      {  Z3 <- 1 }
  if(BURN==TRUE)      {  Zb <- 1 }
  if(TYPEFERT=='N')   {  T1 <- 1 }
  else if(TYPEFERT=='NP')  {  T2 <- 1 }
  if(SOIL=='C')       {  S1 <- 1 }
  else if(SOIL=='D')       {  S2 <- 1 }
  else if(SOIL!='NA' && is.na(AGEFERT)==T |
          TYPEFERT=='NA' && is.na(AGEFERT)==F |
          TYPEFERT!='NA' && is.na(AGEFERT)==T){
    stop("Warning - Please provide information required.")
  }

  return(list(Z1=Z1, Z2=Z2, Z3=Z3, ZB=ZB, T1=T1, T2=T2,
              AGEFERT=AGEFERT, S1=S1, S2=S2))
}

