#' Module that performs the simulations by projections at the desired age starting from initial conditions.
#'
#' \code{module.simulation} Performs model projections of a stand to a final age based on information provided
#' by the input module that contains:  SI, AGE0, HDOM0, BA0, N0, QD0, SDIR0, VOL_0B0, VOL_IB0,
#' VOLm_0B0, VOLm_IB0, AGEF, THINNING, AGET, BAR, t, d and method.
#' Provides with a data frame with all relevant information for simulations from AGE0 to AGEF and thinning (if requested).
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param stand  List with information originated from \code{\link{module.input}} containing:
#'               SI, AGE0, HDOM0, BA0, N0, QD0, SDIR0, VOL_0B0, VOL_IB0, VOLm_0B0, VOLm_IB0,
#'               AGEF, THINNING, AGET, BAR, t, d, method.
#'
#' @return A list containing a data frame (sim.stand) containing the columns below with simulations from
#' AGE0 to AGEF and a logical value indicating if thinning was requested.
#' \itemize{
#' \item \code{AGE}     Stand age form simulations from AGE0 to AGEF (years).
#' \item \code{HDOM}    Dominant Height (ft).
#' \item \code{SI}      Site Index (ft).
#' \item \code{BA}      Basal Area (ft2/ha).
#' \item \code{N}       Number of trees per hectare.
#' \item \code{QD}      Mean quadratic diameter (in).
#' \item \code{SDIR}    Relative stand density index (\%).
#' \item \code{VOL_OB}  Total stand-level volume outside bark (ft3/ha).
#' \item \code{VOL_IB}  Total stand-level volume inside bark (ft3/ha).
#' \item \code{VOLm_OB} Merchantable stand-level volume outside bark (ft3/ha).
#' \item \code{VOLm_IB} Merchantable stand-level volume inside bark (ft3/ha).
#' \item \code{CI}      Competition Index (required for THINNING=TRUE).
#' \item \code{BAU}     Basal Area (ft2/ha) of unthinned counterpart (required for THINNING=TRUE).
#' \item \code{NU}      Number of trees per hectare of unthinned counterpart (required for THINNING=TRUE).
#' }
#'
#' @references
#' Gonzalez-Benecke et al. (2012) - Modeling Survival, Yield, Volume Partitioning and Their Response to Thinning for Longleaf Pine Plantations.
#' Forests, 3(4), 1104-1132; doi:10.3390/f3041104
#'
#' @seealso
#' \code{\link{module.input}}
#'
#' @examples
#' # Example 1 - Input from plot-level data (not thinning)
#' sim1 <- module.input(TYPE='PLOT', BA0=93, HDOM0=54, AGE0=18, N0=282, AGEF=19)
#' module.simulation(stand=sim1)
#'
#' # Example 2 - Input from tree-level data
#' treedata <- subset(treedata, is.na(DBH)==FALSE)
#' sim2 <- module.input(TYPE='TREE', TREEDATA=treedata, AREA=3240, AGE0=5, AGEF=21)
#' module.simulation(stand=sim2)
#'
#' # Example 3 - Input from plot-level data (with thinning)
#' sim3 <- module.input(TYPE='PLOT', BA0=93, SI=70, AGE0=18, N0=282, AGEF=25,
#'                      THINNING=TRUE, AGET=22, NT=70)
#' sims <- module.simulation(stand=sim3)$sim.stand
#' sims
#' plot(sims$AGE,sims$BAU,type="l",xlim=c(15,30),ylim=c(0,10000),col=2,
#'      xlab='Age (years)', ylab='Basal Area (feet2/acre)')
#' par(new=TRUE)
#' plot(sims$AGE,sims$BA,type="l",xlim=c(15,30),ylim=c(0,10000),col=1,
#'      xlab='Age (years)', ylab='Basal Area (feet2/acre)')
#' par(new=FALSE)


module.simulation <- function(stand=NULL){

  # Errors with Age
  if(is.na(stand$AGEF)){
    print('There is no final age for simulation ')
  }  else if(stand$AGE0 >= stand$AGEF){
    stop('The Final age of simulation should be larger than the Initial Age')
  }

  HDOM0 <- stand$HDOM0
  SI <- stand$SI
  AGE0 <- stand$AGE0
  BA0 <- stand$BA0
  N0 <- stand$N0
  QD0 <- stand$QD0
  SDIR0 <- stand$SDIR0
  VOL_OB0 <- stand$VOL_OB0
  VOL_IB0 <- stand$VOL_IB0
  VOLm_OB0 <- stand$VOLm_OB0
  VOLm_IB0 <- stand$VOLm_IB0
  AGEF <- stand$AGEF
  Z1 <- stand$Z1
  Z2 <- stand$Z2
  Z3 <- stand$Z3
  ZB <- stand$ZB
  T1 <- stand$T1
  T2 <- stand$T2
  AGEFERT <- stand$AGEFERT
  S1 <- stand$S1
  S2 <- stand$S2
  THINNING <- stand$THINNING
  AGET <- stand$AGET
  NT <- stand$NT
  t <- stand$t
  d <- stand$d

  # Stop if any is missing
  if(is.na(HDOM0)==T | is.na(SI)==T | is.na(AGE0)==T | is.na(BA0)==T |
     is.na(N0)==T | is.na(QD0)==T | is.na(SDIR0)==T | is.na(VOL_OB0)==T | is.na(VOL_IB0)==T |
     is.na(AGEF)==T | is.na(t)==T | is.na(d)==T) {
    stop("Warning - Please provide information required for simulation")
  }

  # Creating vectors for table
  sim.stand <- data.frame(AGE = AGE0,
                          HDOM = HDOM0,
                          SI = SI,
                          BA = BA0,
                          N = N0,
                          QD = QD0,
                          SDIR = SDIR0,
                          VOL_OB = VOL_OB0,
                          VOL_IB = VOL_IB0,
                          VOLm_OB = VOLm_OB0,
                          VOLm_IB = VOLm_IB0,
                          CI = 0,
                          BAU = BA0,
                          NU = N0
  )


  # No thinning
  if(!isTRUE(THINNING)){

    # Yearly simulations
    for(y in (AGE0 + 1):AGEF){

      HDOM1 <- stand.SITE(SI=SI,AGE=y)$HDOM
      N1 <- module.N(N0=N0, AGE0=y-1, AGE1=y, THINNING=THINNING, AGET=AGET)$N1
      BA1 <- module.BA(N0=N0, HDOM0=HDOM0, AGE0=y, Z1=Z1, Z2=Z2, Z3=Z3, ZB=ZB, T1=T1, T2=T2,
                       AGEFERT=AGEFERT, S1=S1, S2=S2,projection=TRUE, BA0=BA0, N1=N1, HDOM1=HDOM1)$BA1 # With projection

      QD1 <- stand.STAND(BA=BA1, N=N1)$QD
      SDIR1 <- stand.SDI(QD=QD1, N=N1)

      VOL_OB1 <- module.VOL(HDOM=HDOM1, BA=BA1, N=N1, AGE=y)$VOL_OB
      VOL_IB1 <-  module.VOL(HDOM=HDOM1, BA=BA1, N=N1, AGE=y)$VOL_IB
      VOLm_OB1 <- module.VOLm(N=N1, QD=QD1, t=t, d=d, VOL_OB=VOL_OB1, VOL_IB=VOL_IB1)$VOLm_OB
      VOLm_IB1 <- module.VOLm(N=N1, QD=QD1, t=t, d=d, VOL_OB=VOL_OB1, VOL_IB=VOL_IB1)$VOLm_IB

      # Adds simulation results to table  # in the same order as data frame above!
      sim.stand <- rbind(sim.stand, c(y, HDOM1, SI, BA1, N1, QD1, SDIR1, VOL_OB1,VOL_IB1,VOLm_OB1,VOLm_IB1, 0, BA1, N1))

      # Variable replacement
      HDOM0 <- HDOM1
      BA0 <- BA1
      N0 <- N1
      QD0 <- QD1
      SDIR0 <- SDIR1
      VOL_OB0 <- VOL_OB1
      VOL_IB0 <- VOL_IB1

    }

  }


  # Thinning at age AGET
  if(isTRUE(THINNING)){

    temp.plot<-stand
    temp.plot$THINNING <- FALSE
    temp.plot$AGEF <- AGET

    sim.stand <- module.simulation(stand=temp.plot)$sim.stand

    # State after thinning
    n <- nrow(sim.stand)
    #HDOM0 <- sim.stand$HDOM[n]
    #BA0 <- sim.stand$BA[n]*(1-BAR)  # Residual Basal Area (after thinning)
    #QD0 <- sim.stand$QD[n]
    #N0 <- stand.STAND(BA=BA0, QD=QD0)$N     # Residual number of trees (after RANDOM thinning)

    HDOM0 <- sim.stand$HDOM[n]

    QD0 <- sim.stand$QD[n]
    N0 <- stand.STAND(BA=BA0, QD=QD0)$N       # Residual number of trees (after RANDOM thinning)
    SDIR0 <- stand.SDI(QD=QD0, N=N0)

    BAU0 <- sim.stand$BA[n] # Unthinned counterpart BA
    NU0 <- sim.stand$N[n]   # Unthinned counterpart N
    CI0 <- (BAU0 - BA0)/BAU0

    BA0 <- BAU0*(NT/NU0)^1.2248      # Residual Basal Area (after thinning)

    VOL_OB0 <- module.VOL(HDOM=HDOM0,N=N0, BA=BA0, AGE=AGET)$VOL_OB
    VOL_IB0 <- module.VOL(HDOM=HDOM0,N=N0, BA=BA0, AGE=AGET)$VOL_IB
    VOLm_OB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_OB
    VOLm_IB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_IB



    sim.stand <- rbind(sim.stand, c(AGET, HDOM0, SI, BA0, N0, QD0, SDIR0, VOL_OB0,VOL_IB0, VOLm_OB0, VOLm_IB0, CI0, BAU0, NU0))

    # Yearly simulations
    for(y in (AGET + 1):AGEF){

      # Competition Index estimated parameter
      d1 <- -0.093

      HDOM1 <- stand.SITE(SI=SI,AGE=y)$HDOM

      NU1 <- module.N(N0=NU0, AGE0=y-1, AGE1=y, THINNING=THINNING, AGET=AGET)$N1 # It uses u. status
      BAU1 <- module.BA(N0=NU0, HDOM0=HDOM0, AGE0=y-1, Z1=Z1, Z2=Z2, Z3=Z3, ZB=ZB, T1=T1, T2=T2,
                        AGEFERT=AGEFERT, S1=S1, S2=S2, projection=TRUE, BA0=BAU0, N1=NU1, HDOM1=HDOM1)$BA1 # With projection

      CI1 <- CI0*exp(d1*1)  # Maybe there is another model
      BA1 <- BAU1*(1-CI1)

      N1 <- module.N(N0=N0, AGE0=y-1, AGE1=y)$N1   # It uses current status
      QD1 <- stand.STAND(BA=BA1, N=N1)$QD
      SDIR1 <- stand.SDI(QD=QD1, N=N1)

      VOL_OB1 <- module.VOL(HDOM=HDOM1, N=N1, BA=BA1, AGE=y)$VOL_OB
      VOL_IB1 <- module.VOL(HDOM=HDOM1, N=N1, BA=BA1, AGE=y)$VOL_IB
      VOLm_OB1 <- module.VOLm(N=N1, QD=QD1, t=t, d=d, VOL_OB=VOL_OB1, VOL_IB=VOL_IB1)$VOLm_OB
      VOLm_IB1 <- module.VOLm(N=N1, QD=QD1, t=t, d=d, VOL_OB=VOL_OB1, VOL_IB=VOL_IB1)$VOLm_IB

      # Adds simulation results to table  # in the same order as dataframe above!
      sim.stand <- rbind(sim.stand, c(y, HDOM1, SI, BA1, N1, QD1, SDIR0, VOL_OB1,VOL_IB1,VOLm_OB1,VOLm_IB1, CI1, BAU1, NU1))

      # Variable replacement
      HDOM0 <- HDOM1
      BA0 <- BA1
      BAU0 <- BAU1
      CI0 <- CI1
      NU0 <- NU1
      N0 <- N1
      QD0 <- QD1
      SDIR0 <- SDIR1
      VOL_OB0 <- VOL_OB1
      VOL_IB0 <- VOL_IB1

    }

  }

  sim.stand <- round(sim.stand,3)
  return(list(sim.stand=sim.stand, THINNING=stand$THINNING))

}
