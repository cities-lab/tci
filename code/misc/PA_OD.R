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

## works for a hypotheitcal matrix
  Zi.nx <- 10
  PA <- matrix(rnorm(Zi.nx * Zi.nx, mean=10), nrow=Zi.nx, ncol=Zi.nx)
  PA[1,1] <- NA
  
  OD_peak <- PA * pa.peak + t(PA) * ap.peak
  OD_offPeak <- PA * pa.offPeak + t(PA) * ap.offPeak
  
  # now reverse to get PA
  ratio <- ap.peak / ap.offPeak
  PA.revs <- (OD_peak - OD_offPeak * ratio) / (pa.peak - pa.offPeak * ratio)

  all.equal(PA, PA.revs)

## however it doesn't work for the Corvallis trips matrix
  ic <- 'lowInc'
  path.tdm <- file.path(INPUT_DIR, "TDM")
  file.trips <- file.path(path.tdm, "PrIcMdTpTrips.omx")
  
  pktrips.name <- paste0(pr, ic, "peak", md, "Trips")
  OD_peak <- readMatrixOMX(file.trips, pktrips.name)
  optrips.name <- paste0(pr, ic, "peak", md, "Trips")
  OD_offPeak <- readMatrixOMX(file.trips, optrips.name)
  
  ratio <- ap.peak / ap.offPeak
  PA.revs <- (OD_peak - OD_offPeak * ratio) / (pa.peak - pa.offPeak * ratio)
  
  OD_peak.ax <- PA.revs * pa.peak + t(PA.revs) * ap.peak
  OD_offPeak.ax <- PA.revs * pa.offPeak + t(PA.revs) * ap.offPeak
  
  all.equal(OD_peak, OD_peak.ax)
  all.equal(OD_peak, OD_offPeak.ax)

