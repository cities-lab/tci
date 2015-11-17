#
require(reshape2)
require(data.table)
require(tidyr)
source("code/thirdparty/omx.r")

## Define functions
  # Since we need to attribute travel cost to home locations of HB trips (Productions),
  # we need to reverse-engineer from OD trips to get PA trips:
  # For a given trip purpose, 
  # PA.mtx * pa.peak + t(PA.mtx) * ap.peak = OD_peak.mtx, and
  # PA.mtx * pa.offPeak + t(PA.mtx) * ap.offPeak = OD_offPeak.mtx
  # thus, PA.mtx = (OD_peak.mtx - OD_offPeak.mtx * ap.peak / ap.offPeak) / (pa.peak - pa.offPeak * ap.peak / ap.offPeak)
  # TODO: extend the function to work with more than peak/offpeak periods.
  
  get_factor <- function(factors.df, pr, md, direction, colname) {
    # mode (md) filter is not being used for now
    factors.df[(factors.df$purpose==pr) & (factors.df$direction==direction), colname]
  }
  
  get_PA_from_OD <- function(factors.df, pr, md, OD_peak.mtx, OD_offPeak.mtx) {
    pa.peak <- get_factor(factors.df, pr, md, 'pa', 'peak')
    pa.offPeak <- get_factor(factors.df, pr, md, 'pa', 'offPeak')
    ap.peak <- get_factor(factors.df, pr, md, 'ap', 'peak')
    ap.offPeak <- get_factor(factors.df, pr, md, 'ap', 'offPeak')
    ratio <- ap.peak / ap.offPeak
    (OD_peak.mtx - OD_offPeak.mtx * ratio) / (pa.peak - pa.offPeak * ratio)
  }

  # efficiently set data.table values 
  set_dt <- function(dt, idx.start, df) {
    n <- nrow(df)
    for (j in 1:ncol(df)) {
      #stopifnot(names(dt)[j] == names(df)[j])
      dt.j <- which(names(dt) == names(df)[j])
      set(dt, idx.start:(idx.start+n-1), dt.j, df[, j])
    }
  }
  # flatten a skim/trip matrix to a data.frame
  flatten_mtx <- function(mtx) {
    df_mtx <- as.data.frame(mtx, stringsAsFactors=FALSE)
    df_mtx$htaz <- as.integer(rownames(df_mtx))
    gather(df_mtx, ataz, value, -htaz)
  }

## computing

  #default switches
  if (!exists("ttime.in.minutes")) ttime.in.minutes <- TRUE
  if (!exists("tdist.vary.by.tp"))  tdist.vary.by.tp <- FALSE
  if (!exists("get.PA.from.OD"))  get.PA.from.OD <- FALSE
  
# Start Computing
  ttime.convertor <- ifelse(ttime.in.minutes, 60, 1) #convert ttime to hours
  Zi.n <- length(Zi)
  
  #init a data.table to stop intermediate results (for efficiency)
  tcost.n <- length(Ic) * length(Pr) * length(Zi) * length(Zi)
  tcost.trip.dt <- data.table(htaz=rep(-1L, tcost.n),  #home (production) taz
                            ic='',
                            #hhs=0,
                            pr='',
                            ataz='',               #attraction taz
                            trips=0L,
                            tcost.sum=0.0)
  
  idx.start <- 1
  for (ic in Ic) {
    trips.pr <- list()
    tcost.pr <- list()
    for (pr in Pr) {
      #matrix for centers/travel market baskets
      #basket <- define_basket() #return a Zi.n * Zi.n matrix with 1/0
      basket <- dummy_basket(Zi)
      
      for (md in Md) {
        trips.tp <- list()
        tcost.tp <- list()
        unitcost <- unitcosts[names(MODE)==md, ] #name or id
        
        if (!tdist.vary.by.tp) {
          tdist.name <- paste0(md, "Distance")
          tdist.mtx <- readMatrixOMX(file.tdist, tdist.name)
        }
               
        for (tp in Tp) {
          trips.name <- paste0(pr, ic, tp, md, "Trips")
          trips.mtx <- readMatrixOMX(file.trips, trips.name)
          
          ttime.name <- paste0(tp, md, "Time")
          ttime.mtx <- readMatrixOMX(file.ttime, ttime.name)
          
          if (tdist.vary.by.tp) {
            tdist.name <- paste0(tp, md, "Distance") # if tdist.vary.by.tp
            tdist.mtx <- readMatrixOMX(file.tdist, tdist.name)
          }
          
          # those may be a concern
          #stopifnot(sum(is.na(ttime.mtx[trips.mtx > 0]))==0) # if there are trips with NA travel time
          #stopifnot(sum(is.na(tdist.mtx[trips.mtx > 0.03]))==0) # if there are trips with NA travel distance
          ## fill NAs as they will mess up calculation, e.g. 1 * NA = NA
          trips.mtx[is.na(trips.mtx)] <- 0
          ttime.mtx[is.na(ttime.mtx)] <- 0
          tdist.mtx[is.na(tdist.mtx)] <- 0
          
          trips.tp[[tp]] <- trips.mtx * basket
          # total tcost of all trips made in period tp
          tcost.tp[[tp]] <- with(unitcost, constant + VOT * ttime.mtx / ttime.convertor + mcpm * tdist.mtx) * trips.tp[[tp]]
        }
        
        # total tcost of all trips using mode md in all periods, reversed to PA matrix
        #stopifnot()
        if (get.PA.from.OD) {
          .trips.md <- get_PA_from_OD(period.factors, pr, md, trips.tp[["peak"]], trips.tp[["offPeak"]])
          .tcost.md <- get_PA_from_OD(period.factors, pr, md, tcost.tp[["peak"]], tcost.tp[["offPeak"]])
        } else {
        # naive combination
          .trips.md <- trips.tp[["peak"]] + trips.tp[["offPeak"]]
          .tcost.md <- tcost.tp[["peak"]] + tcost.tp[["offPeak"]]
        }
        # total tcost of all trips for the purpose of pr with the same production TAZ
        trips.pr[[pr]] <- if (exists(pr, where=trips.pr)) trips.pr[[pr]] + .trips.md else .trips.md
        tcost.pr[[pr]] <- if (exists(pr, where=tcost.pr)) tcost.pr[[pr]] + .tcost.md else .tcost.md
      }
      
      tcost.df <- cbind(flatten_mtx(trips.pr[[pr]]), 
                        tcost.sum=flatten_mtx(tcost.pr[[pr]])$value, 
                        ic=ic, pr=pr)
      
      tcost.df <- tcost.df %>%
        rename(trips=value) %>%
        filter(trips > 0) %>%
        na.omit() %>%
        select(htaz, ic, pr, ataz, trips, tcost.sum)
      
      set_dt(tcost.trip.dt, idx.start, tcost.df)
      idx.start <- idx.start + nrow(tcost.df)
    }
  }

  # truncate unused rows from tcost.trip.dt
  tail(tcost.trip.dt)
  tcost.trip <- tcost.trip.dt %>%
    as.data.frame()  %>%
    filter(htaz>0) %>%
    mutate(ic = factor(ic, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc"))) %>%
    mutate(pr = factor(pr, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other")))
  
  hhs.ZiIc <- hhs.ZiIc %>%
    mutate(ic = factor(ic, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc")))
#Summarize tcost 
# TODO: consider using data.table syntax for efficiency
  ## tcost per trip (averge tcost over trips)
  tcost_trip.htaz.pr.ic <- tcost.trip %>%
    group_by(htaz, ic, pr) %>%
    summarize(tcost=sum(tcost.sum) / sum(trips))

  tcost_trip.pr.ic <- tcost.trip %>%
    group_by(ic, pr) %>%
    summarize(tcost=sum(tcost.sum) / sum(trips))
  
  tcost_trip.pr <- tcost.trip %>%
    group_by(pr) %>%
    summarize(tcost=sum(tcost.sum) / sum(trips))
  
  tcost_trip.ic <- tcost.trip %>%
    group_by(ic) %>%
    summarize(tcost=sum(tcost.sum) / sum(trips))
  
  tcost_trip.all <- with(tcost.trip, sum(tcost.sum) / sum(trips))
  
  ## tcost per household (averge tcost over hhs)
  hhs.ic <- hhs.ZiIc %>%
    group_by(ic) %>%
    summarize(hhs=sum(hhs))

  hhs.htaz <- hhs.ZiIc %>%
    group_by(htaz) %>%
    summarize(hhs=sum(hhs))
  
  tcost.htaz.pr.ic <- tcost.trip %>%
    group_by(htaz, pr, ic) %>%
    summarize(tcost.sum=sum(tcost.sum)) %>%
    left_join(hhs.ZiIc) %>%
    mutate(tcost=tcost.sum/hhs)

  tcost.htaz.ic <- tcost.trip %>%
    group_by(htaz, ic) %>%
    summarize(tcost.sum=sum(tcost.sum)) %>%
    left_join(hhs.ZiIc) %>%
    mutate(tcost=tcost.sum/hhs)

  tcost.htaz.pr <- tcost.trip %>%
    group_by(htaz, pr) %>%
    summarize(tcost.sum=sum(tcost.sum)) %>%
    left_join(hhs.htaz) %>%
    mutate(tcost=tcost.sum/hhs)
  
  tcost.pr.ic <- tcost.trip %>%
    group_by(pr, ic) %>%
    summarize(tcost.sum=sum(tcost.sum)) %>%
    left_join(hhs.ic) %>%
    mutate(tcost=tcost.sum/hhs)
  
  # TODO: What the denominator should be?
  sum.hhs <- sum(hhs.ZiIc$hhs)
  tcost.pr <- tcost.trip %>%
    group_by(pr) %>%
    summarize(tcost.sum=sum(tcost.sum)) %>%
    mutate_(tcost= interp(~tcost.sum/hhs, hhs = as.name("sum.hhs")))
  
  tcost.ic <- tcost.trip %>%
    group_by(ic) %>%
    summarize(tcost.sum=sum(tcost.sum)) %>%
    left_join(hhs.ic) %>%
    mutate(tcost=tcost.sum/hhs)
  
  tcost.all <- sum(tcost.trip$tcost.sum) / sum(hhs.ZiIc$hhs)
  
  # by geography
  hhs.htaz <- hhs.ZiIc %>%
    group_by(htaz) %>%
    summarize(hhs=sum(hhs))
  
  tcost.htaz <- tcost.trip %>%
    group_by(htaz) %>%
    summarize(tcost.sum=sum(tcost.sum)) %>%
    left_join(hhs.htaz) %>%
    mutate(tcost=tcost.sum/hhs)

  if (exists('districts')) {
    tcost.trip <- tcost.trip %>%
      left_join(districts, by=c("htaz"="TAZ"))
  
    hhs.district.ic <- hhs.ZiIc %>%
      left_join(districts, by = c("htaz" = "TAZ")) %>%
      group_by(DISTRICT, ic) %>%
      summarize(hhs = sum(hhs))
    
    hhs.district <- hhs.ZiIc %>%
      left_join(districts, by = c("htaz" = "TAZ")) %>%
      group_by(DISTRICT) %>%
      summarize(hhs = sum(hhs))
    
    tcost.distr.pr.ic <- tcost.trip %>%
      group_by(DISTRICT, pr, ic) %>%
      summarize(tcost.sum = sum(tcost.sum)) %>%
      left_join(hhs.district.ic) %>%
      mutate(tcost = sum(tcost.sum) / hhs)
    
    tcost.distr.pr  <- tcost.trip %>%
      group_by(DISTRICT, pr) %>%
      summarize(tcost.sum = sum(tcost.sum)) %>%
      left_join(hhs.district) %>%
      mutate(tcost = sum(tcost.sum) / hhs)
    
    tcost.distr.ic <- tcost.trip %>%
      group_by(DISTRICT, ic) %>%
      summarize(tcost.sum = sum(tcost.sum)) %>%
      left_join(hhs.district.ic) %>%
      mutate(tcost = sum(tcost.sum) / hhs)
    
    tcost.distr <- tcost.trip %>%
      group_by(DISTRICT) %>%
      summarize(tcost.sum = sum(tcost.sum)) %>%
      left_join(hhs.district) %>%
      mutate(tcost = sum(tcost.sum) / hhs)
  }