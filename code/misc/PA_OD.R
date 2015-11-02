setwd("~/tci")
source("code/thirdparty/omx.r")

get_factor <- function(period.factors, pr, md, direction, colname) {
  # mode (md) filter is not being used for now
  period.factors[(period.factors$purpose==pr) & (period.factors$direction==direction), colname]
}

# For a given trip purpose, 
# PA.mtx * pa.peak + t(PA.mtx) * ap.peak = OD_peak.mtx, and
# PA.mtx * pa.offPeak + t(PA.mtx) * ap.offPeak = OD_offPeak.mtx
# thus, PA.mtx = (OD_peak.mtx - OD_offPeak.mtx * ap.peak / ap.offPeak) / (pa.peak - pa.offPeak * ap.peak / ap.offPeak)

INPUT_DIR <- 'data/Corvallis2011'
period.factors <- read.csv(file.path(INPUT_DIR, 'TDM/period_factors.csv'), sep=",", header=T)
pr <- 'hbw'; md <- 'driveAlone';
pa.peak <- get_factor(period.factors, pr, md, 'pa', 'peak')
pa.offPeak <- get_factor(period.factors, pr, md, 'pa', 'offPeak')
ap.peak <- get_factor(period.factors, pr, md, 'ap', 'peak')
ap.offPeak <- get_factor(period.factors, pr, md, 'ap', 'offPeak')

## works if starting off with PA
  Zi.nx <- 3
  PA <- matrix(rnorm(Zi.nx * Zi.nx, mean=10), nrow=Zi.nx, ncol=Zi.nx)
  OD_peak <- PA * pa.peak + t(PA) * ap.peak
  OD_offPeak <- PA * pa.offPeak + t(PA) * ap.offPeak
  
  stopifnot(all.equal(OD_peak, (-ap.peak*OD_offPeak - t(OD_offPeak)*pa.peak + t(OD_peak)*pa.offPeak)/(ap.peak + pa.offPeak + pa.peak - 1)))
  #OD_peak = (-ap.peak*OD_offPeak - t(OD_offPeak)*pa.peak + t(OD_peak)*pa.offPeak)/(ap.peak + pa.offPeak + pa.peak - 1)
  #or OD_offPeak = (-ap_pk*OD_peak - t(OD_offPeak)*pa_pk - OD_peak*pa_op - OD_peak*pa_pk + OD_peak + t(OD_peak)*pa_op)/ap_pk
  
  #reverse to get PA
  ratio <- ap.peak / ap.offPeak
  PA.revs <- (OD_peak - OD_offPeak * ratio) / (pa.peak - pa.offPeak * ratio)
  all.equal(PA, PA.revs)
  
  OD_peak.ax <- PA.revs * pa.peak + t(PA.revs) * ap.peak
  OD_offPeak.ax <- PA.revs * pa.offPeak + t(PA.revs) * ap.offPeak
  
  all.equal(OD_peak, OD_peak.ax); all.equal(OD_offPeak, OD_offPeak.ax)

## doesn't if starting off with OD
  Zi.nx <- 3
  OD_peak <- matrix(rnorm(Zi.nx * Zi.nx, mean=10), nrow=Zi.nx, ncol=Zi.nx)
  OD_offPeak <- matrix(rnorm(Zi.nx * Zi.nx, mean=10), nrow=Zi.nx, ncol=Zi.nx)
  OD_peak.expected <- (-ap.peak*OD_offPeak - t(OD_offPeak)*pa.peak + t(OD_peak)*pa.offPeak)/(ap.peak + pa.offPeak + pa.peak - 1)
  stopifnot(all.equal(OD_peak, OD_peak.expected))  # "OD_peak and OD_offPeak don't seem to come from the same PA"
  
  # now reverse to get PA
  ratio <- ap.peak / ap.offPeak
  PA <- (OD_peak - OD_offPeak * ratio) / (pa.peak - pa.offPeak * ratio)
  
  #PA <- matrix(rnorm(Zi.nx * Zi.nx, mean=10), nrow=Zi.nx, ncol=Zi.nx)
  #PA[1,1] <- 0
  
  OD_peak.ax <- PA * pa.peak + t(PA) * ap.peak
  OD_offPeak.ax <- PA * pa.offPeak + t(PA) * ap.offPeak
  
  all.equal(OD_peak, OD_peak.ax)
  all.equal(OD_offPeak, OD_offPeak.ax)

## OD_peak and OD_offPeak must satisfy certain condition
  #pa.peak + ap.Peak + pa.offPeak + ap.offPeak = 1 and
  #PA * pa.peak + t(PA) * ap.peak = OD_peak, and
  #PA * pa.offPeak + t(PA) * ap.offPeak = OD_offPeak
  # ie
  #ap: (-od_op*pa_pk + od_pk*pa_op)/(ap_pk*pa_op + pa_pk*(ap_pk + pa_op + pa_pk - 1)),
  #pa: (ap_pk*od_op + od_pk*(ap_pk + pa_op + pa_pk - 1))/(ap_pk*pa_op + pa_pk*(ap_pk + pa_op + pa_pk - 1))
  #and pa = t(ap)
  # (ap_pk*od_op + od_pk*(ap_pk + pa_op + pa_pk - 1))/(ap_pk*pa_op + pa_pk*(ap_pk + pa_op + pa_pk - 1)) = \
  # (-t(od_op)*pa_pk + t(od_pk) * pa_op) / (ap_pk*pa_op + pa_pk*(ap_pk + pa_op + pa_pk - 1))
  # od_pk = [(-ap_pk*od_op - od_op_T*pa_pk + od_pk_T*pa_op)/(ap_pk + pa_op + pa_pk - 1)]
  
##sympy code
# from sympy import *
# pa, ap, od_pk, od_op, od_pk_T, od_op_T= symbols('pa ap od_pk od_op od_pk_T od_op_T')
# pa_pk, pa_op, ap_pk = symbols('pa_pk pa_op ap_pk')
# 
# #od_pk = pa*pa_pk+ap*ap_pk-od_pk and 
# #od_op =  pa*pa_op+ap*(1-pa_pk-ap_pk-pa_op)
# solve((pa*pa_pk+ap*ap_pk-od_pk, pa*pa_op+ap*(1-pa_pk-ap_pk-pa_op)-od_op), (pa, ap))
# 
# #{ap: (-od_op*pa_pk + od_pk*pa_op)/(ap_pk*pa_op + pa_pk*(ap_pk + pa_op + pa_pk - 1)),
# # pa: (ap_pk*od_op + od_pk*(ap_pk + pa_op + pa_pk - 1))/(ap_pk*pa_op + pa_pk*(ap_pk + pa_op + pa_pk - 1))}
# 
# #ap = pa.T
# solve((-od_op*pa_pk + od_pk*pa_op)/(ap_pk*pa_op + pa_pk*(ap_pk + pa_op + pa_pk - 1)) - (ap_pk*od_op_T + od_pk_T*(ap_pk + pa_op + pa_pk - 1))/(ap_pk*pa_op + pa_pk*(ap_pk + pa_op + pa_pk - 1)), od_pk)
# 
# #od_pk = (ap_pk*od_op_T + ap_pk*od_pk_T + od_op*pa_pk + od_pk_T*pa_op + od_pk_T*pa_pk - od_pk_T)/pa_op
