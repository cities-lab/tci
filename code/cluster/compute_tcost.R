# This script calculates minimal travel time and weighted average travel time to centers, 
# converts travel time into monetary cost and combines cost into arrays. 

# define a constant for converting time to cost 
cost.conversion <- hourly.wage/60 

# load taz index of centers 
if (!in.memory(paste(Pr, "ci", sep="")))
  load(file.path(INTERMEDIATE_DIR, "centers.RData"))

# loaded in 4_cal_md_trips_probs.R
# Calculate travel time by bike and walk from trip distance
#td_bw.df <- read.csv(file.path(INPUT_DIR, 'TDM/mf202.csv'), header=FALSE)
#td_bw.mx <- matrix(trip_distance.bw[,3], nrow=2162, byrow=TRUE)
#bikeTime <- td_bw.mx*60/bike.speed
#walkTime <- td_bw.mx*60/walk.speed

#Begin iteration by trip purpose
#-------------------------------  
for (pr in Pr) {
  # Begin iteration by income group
  for (ic in Ic) {
    # Begin iteration by time period
    for ( tp in Tp){
      TTimecost.ZiMdCm <- array(0, dim=(c(length(Zi), length(Md), length(Cm))), 
                                   dimnames=list(Zi,Md,Cm))
      
      # Begin iteration by mode 
      for (md in Md ) {
        # load trip matrices
        TripsMdObjName <- paste(pr, ic, md, "trips", sep="")
        if (! in.memory(TripsMdObjName)) {
          TripsMd <- readMatrixOMX(file.path(INTERMEDIATE_DIR, "ModeTrips.omx"), TripsMdObjName)
          assign(TripsMdObjName, TripsMd)
        }
        
        if ((md == "bike")|(md=="walk")) {
          # load travel time for bike and walk 
          ##TODO: how were these data processed? why bike & walk travel vary by purpose?
          TTimeObjName <- paste(md, "Time", sep="")
          # End calculate travel time for walk and bike 
        } else {          
          # load travel time for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
          TTimeObjName <- paste(md, tp, "Time", sep = "")
        }
        
        if (! in.memory(TripsMdObjName)) {
          TTime.mx <- readMatrixOMX(file.path(INPUT_DIR, "TDM/Skims.omx"), TTimeObjName)
          assign(TTimeObjName, TTime.mx)
        }        
                
        for (cm in Cm) {
          func <- get(paste(cm, '_tt', sep=""))
          TTime <- func(Centers$TAZ, TTime.mx, TripsMd)
          TCost <- TTime * modecosttrans.Md[md] * cost.conversion
          paste("min", pr, ic, md, "time", sep="")
          
          TTimecost.ZiMdCm[, md, cm] <- TCost
        }
        
      } # End loop through mode
      
      if (SAVE.INTERMEDIARIES) {
        intm.path <- file.path(INTERMEDIATE_DIR, 'costs')
        dir.create(intm.path, recursive = TRUE, showWarnings = FALSE)
        obj.name <- paste(pr, ic, tp, "TimeCost.ZiMdCm", sep="")
        assign(obj.name, TTimecost.ZiMdCm)
        intm.file <- file.path(intm.path, paste(obj.name, ".RData", sep=""))
        save(list=c(obj.name), file=intm.file)
      }
      
    } # End loop through time period
  } # End loop through income group 
} # End loop through purpose

