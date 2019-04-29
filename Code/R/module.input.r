#' Module of input tree- or stand-level data to prepare it for further simulations.
#'
#' \code{module.input} Prepares tree- or stand-level data from a single plot, checks and completes missing values, and
#' calculates several stand-level parameters including total volume. It also reads required information for further
#' simulations including simulation age and details of future thinning. Some information is only traspassed to other modules.
#' Note that form tree-level data individual tree (complete or incomplete) information is required.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param TYPE      Character for type of input data. PLOT: stand-level data information, TREE: tree-level information. Default is PLOT
#' @param TREEDATA  Data frame with tree-level information with columns: PLOTID, TREEID, DBH, HT (these should be identical names).
#' @param AREA      Numeric value of size of the inventory plot (ft2). Required for TYPE='TREE'.
#' @param SI        Numeric value of Site Index (ft) (Dominant Height of the plot at age 50 years).
#' @param HDOM0     Numeric value of Dominant Height (ft) at initial age (or age 0).
#' @param AGE0      Numeric value of initial stand age or age 0 (years).
#' @param BA0       Numeric value of Basal Area (ft2/ha) at age 0 (required for model projection).
#' @param N0        Numeric value of number of trees per hectare at age 0.
#' @param AGEF      Numeric value of final stand age (in years) of simulation. Default is 50.
#' @param PFERT     If TRUE then fertilized at planting. Default is FALSE.
#' @param BEDDED    If TRUE then bedded (site preparation). Default is FALSE.
#' @param HERB      If TRUE then herbicide was used. Default is FALSE.
#' @param BURN      If TRUE then burned was used. Default is FALSE.
#' @param TYPEFERT  Type of fertilization at AGEFERT. N: only Nitrogen. NP: N and P. Default is NA.
#' @param AGEFERT   Numeric value of stand age (in years) where fertilization is planned. Required if definied TYPEFERT.
#' @param SOIL      Type of Soil. C: Group C. D: Group D. Default is NA.
#' @param THINNING  If TRUE then a thinning is implemented according to AGET and NT. Default is FALSE.
#' @param AGET      Numeric value of stand age (in years) where thinning is planned.
#' @param NT        Numeric value of trees to be removed when thinning at age AGET.
#' @param t         Numeric value top stem diameter outside bark for merchantability limit (in).
#' @param d         Numeric value of a DBH threshold limit for merchantable trees (in).
#' @param method    Numeric value that identifies the method to estimate missing heights from TYPE='TREE'.
#' 1: parametrized DBH-height model that requires DBH, BA and AGE, 2: fits a simple DBH-height model from
#' available measurements using the equation: ln(Ht) = b0 + b1/DBH. Default method=2.
#'
#' @return A list containing the following:
#' \itemize{
#' \item \code{SI}       Site Index (ft).
#' \item \code{AGE0}     Initial stand age or age 0 (years).
#' \item \code{HDOM0}    Dominant Height (ft) at initial age (or age 0).
#' \item \code{BA0}      Basal Area (ft2/ha) at age 0.
#' \item \code{N0}       Number of trees per hectare at age 0.
#' \item \code{QD}       Mean quadratic diameter (in) at age 0.
#' \item \code{SDIR0}    Relative stand density index (\%) at age 0.
#' \item \code{VOL_OB0}  Total stand-level volume outside bark (ft3/ha) at age 0.
#' \item \code{VOL_IB0}  Total stand-level volume inside bark (ft3/ha) at age 0.
#' \item \code{VOLm_OB0} Merchantable stand-level volume outside bark (ft3/ha) at age 0.
#' \item \code{VOLm_IB0} Merchantable stand-level volume inside bark (ft3/ha) at age 0.
#' \item \code{AGEF}     Final stand age (in years) of simulation.
#' \item \code{Z1}       Dummy variable if fertilized at planting then Z1 = 1.
#' \item \code{Z2}       Dummy variable if bedded (site preparation) then Z2 = 1.
#' \item \code{Z3}       Dummy variable if herbicide was used then Z3 = 1.
#' \item \code{ZB}       Dummy variable if burned was used then ZB = 1.
#' \item \code{T1}       Dummy variable if only Nitrogen fertilization at AGEFERT then T1 = 1.
#' \item \code{T2}       Dummy variable if N and P fertilization at AGEFERT then T2 = 1.
#' \item \code{AGEFERT}  Numeric value of stand age (in years) where fertilization is planned.
#' \item \code{S1}       Dummy variable if soil is Goup C then S1 = 1.
#' \item \code{S2}       Dummy variable if soil is Goup D then S2 = 1.
#' \item \code{THINNING} Logical that indicates if thinning is implemented according to AGET and NT.
#' \item \code{AGET}     Stand age (in years) where thinning is planned.
#' \item \code{NT}       Number of trees to be removed when thinning at age AGET.
#' \item \code{t}        Top stem diameter outside bark for merchantability limit (in).
#' \item \code{d}        DBH threshold limit for merchantable trees (in).
#' \item \code{method}   Selection of the method to estimate missing heights from TYPE='TREE'.
#' }
#'
#' @examples
#' # Example 1 - Input stand-level data
#' module.input(TYPE='PLOT', BA0=93, SI=70, AGE0=18, N0=282, AGEF=19)
#' module.input(TYPE='PLOT', BA0=93, HDOM0=54, AGE0=18, N0=282, AGEF=19)
#' module.input(TYPE='PLOT', HDOM0=54, AGE0=18, N0=282, AGEF=19) # BA obtained by prediction
#'
#' # Example 2 - Input with individual tree data
#' treedata <- subset(treedata, is.na(DBH)==FALSE)
#' module.input(TYPE='TREE', TREEDATA=treedata, AREA=3240, AGE0=5, AGEF=21)


module.input <-  function(TYPE='PLOT', TREEDATA=NA, AREA=NA, SI=NA,
                          HDOM0=NA, AGE0=NA, BA0=NA, N0=NA, AGEF=50,
                          PFERT=FALSE, BEDDED=FALSE, HERB=FALSE, BURN=FALSE,
                          TYPEFERT='NA', AGEFERT=NA, SOIL='NA',
                          THINNING=FALSE, AGET=NA, NT=NA, t=1, d=1.95, method=2){

  # Dummy variables
  dummy <- dummy(PFERT=PFERT, BEDDED=BEDDED, HERB=HERB, BURN=BURN,
                 TYPEFERT=TYPEFERT, AGEFERT=AGEFERT, SOIL=SOIL)
  Z1 <- dummy$Z1
  Z2 <- dummy$Z2
  Z3 <- dummy$Z3
  ZB <- dummy$ZB
  T1 <- dummy$T1
  T2 <- dummy$T2
  AGEFERT <- dummy$AGEFERT
  S1 <- dummy$S1
  S2 <- dummy$S2

  # Gathering stand-level information.
  if(TYPE=='TREE'){

    tree <- prepare.tree(TREEID=TREEDATA$TREEID, DBH=TREEDATA$DBH, HT=TREEDATA$HT, AREA=AREA, AGE=AGE0)
    BA0 <- tree$BA
    N0 <- tree$N
    HDOM0 <- tree$HDOM

    if(is.na(SI)==T){
      SI <- stand.SITE(HDOM=HDOM0,AGE=AGE0)$SI    # Missing SI
    }
   # else if(is.na(AGE0)==T){
   #    AGE0 <- stand.SITE(HDOM=HDOM0,AGE=AGE0, Z1=Z1, Z2=Z2, Z3=Z3, T1=T1, T2=T2, AGEFERT=AGEFERT)$AGE     # Missing AGE
   #  }

    # If SI is missing and AGE is provided.
    else if(is.na(SI)==T &&  is.na(AGE0)==F){
      SI <- tree$SI
    }

    # If AGE is missing and SI is provided.
    else if(is.na(SI)==F &&  is.na(AGE0)==T){
      AGE0 <- tree$AGE
    }

    # Completing QD and Volume
    QD0 <- stand.STAND(BA=BA0, N=N0)$QD   # Missing QD (this is always missing)
    SDIR0 <- stand.SDI(QD=QD0, N=N0)

    VOL_OB0 <- module.VOL(HDOM=HDOM0, BA=BA0, N=N0, AGE=AGE0)$VOL_OB
    VOL_IB0 <- module.VOL(HDOM=HDOM0, BA=BA0, N=N0, AGE=AGE0)$VOL_IB
    VOLm_OB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_OB
    VOLm_IB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_IB

  }

  # Completing stand-level information
  else if(TYPE=='PLOT'){

    # Completing Site information
    if(is.na(HDOM0)==T){
      HDOM0 <- stand.SITE(SI=SI, AGE=AGE0)$HDOM     # Missing HDOM
    }
    else if(is.na(SI)==T){
      SI <- stand.SITE(HDOM=HDOM0,AGE=AGE0)$SI    # Missing SI
    }

    # Predicting BA0 by model (if not provided)
    if(is.na(BA0)==T){
      BA0 <- module.BA(N0=N0, HDOM0=HDOM0, AGE0=AGE0, Z1=Z1, Z2=Z2, Z3=Z3, ZB=ZB,
                       T1=T1, T2=T2, AGEFERT=AGEFERT, S1=S1, S2=S2, projection=FALSE)$BA0
    }

    # Completing QD and Volume
    QD0 <- stand.STAND(BA=BA0, N=N0)$QD   # Missing QD (this is always missing)
    SDIR0 <- stand.SDI(QD=QD0, N=N0)

    VOL_OB0 <- module.VOL(HDOM=HDOM0, BA=BA0, N=N0, AGE=AGE0)$VOL_OB
    VOL_IB0 <- module.VOL(HDOM=HDOM0, BA=BA0, N=N0, AGE=AGE0)$VOL_IB
    VOLm_OB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_OB
    VOLm_IB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_IB

  }

  return(list(SI=SI, HDOM0=HDOM0, AGE0=AGE0, BA0=BA0, N0=N0, QD0=QD0, SDIR0=SDIR0,
              VOL_OB0=VOL_OB0, VOL_IB0=VOL_IB0, VOLm_OB0=VOLm_OB0,VOLm_IB0=VOLm_IB0,
              AGEF=AGEF, Z1=Z1, Z2=Z2, Z3=Z3, ZB=ZB, T1=T1, T2=T2, AGEFERT=AGEFERT,
              S1=S1, S2=S2, THINNING=THINNING, AGET=AGET, NT=NT, t=t, d=d, method=method))

}
