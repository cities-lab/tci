# This script calculates minimal travel time and weighted average travel time to centers, 
# converts travel time into monetary cost and combines cost into arrays. 

# set setting
source("code/cluster/settings.R")
# define a constant for converting time to cost 
cost.conversion <- hourly.wage/60 

# define operation cost per mile by mode
modedistancecost.Md <- c(driveAlone = 59.2, drivePass=59.2, pass=59.2, busWalk=101.0, parkAndRideBus=101.0, bike=0, walk=0)

# define hourly wage by income group
wage.Ic <- c(lowInc = 10, midInc = 20, highInc =30)

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
  CentersObjName <- paste(pr, "ci", sep="")
  Centers <- get(CentersObjName); rm(CentersObjName)
  
  # Begin iteration by income group
  for (ic in Ic) {
    # Begin iteration by time period
    for ( tp in Tp){
      TTimecost.ZiMdCm <- array(0, dim=(c(length(Zi), length(Md), length(Cm))), 
                                dimnames=list(Zi,Md,Cm))
      
      TOperationcost.ZiMdCm <- array(0, dim=(c(length(Zi), length(Md), length(Cm))), 
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
          TDistanceObjName <- paste(md, "Distance", sep="")
          
          # End calculate travel time for walk and bike 
        } else {          
          # load travel time for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
          TTimeObjName <- paste(md, tp, "Time", sep = "")
          TDistanceObjName <- paste(md, tp, "Distance", sep = "")
        }
        
        if (! in.memory(TripsMdObjName)) {
          TTime.mx <- readMatrixOMX(file.path(INPUT_DIR, "TDM/Skims.omx"), TTimeObjName)
          assign(TTimeObjName, TTime.mx)
        }     
        
        if (! in.memory(TDistanceObjName)) {
          TDistance.mx <- readMatrixOMX(file.path(INPUT_DIR, "TDM/DistanceSkims.omx"), TDistanceObjName)
          assign(TDistanceObjName, TDistance.mx)
        } 
        
        for (cm in Cm) {
          func <- get(paste(cm, '_tt', sep=""))
          TTime <- func(Centers$TAZ, TTime.mx, TripsMd)
          TCost <- TTime * modecosttrans.Md[md] * wage.Ic[ic]/60
          paste("min", pr, ic, md, "time", sep="")
          
          TDistance <- func(Centers$TAZ, TDistance.mx, TripsMd)
          TOperationcost <- TDistance*modedistancecost.Md[md]/(100*wage.Ic[ic])
          
          TTimecost.ZiMdCm[, md, cm] <- TCost
          TOperationcost.ZiMdCm[, md, cm] <- TOperationcost
          
        }
        
      } # End loop through mode
      
      Fullcost.ZiMdCm <- TTimecost.ZiMdCm + TOperationcost.ZiMdCm
      
      if (SAVE.INTERMEDIARIES) {
        intm.path <- file.path(INTERMEDIATE_DIR, 'costs')
        dir.create(intm.path, recursive = TRUE, showWarnings = FALSE)
        obj.name <- paste(pr, ic, tp, "FullCost.ZiMdCm", sep="")
        assign(obj.name, Fullcost.ZiMdCm)
        intm.file <- file.path(intm.path, paste(obj.name, ".RData", sep=""))
        save(list=c(obj.name), file=intm.file)
      }
      
    } # End loop through time period
  } # End loop through income group 
} # End loop through purpose