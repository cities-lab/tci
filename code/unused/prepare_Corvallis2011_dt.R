# TO BE merged with prepare_data.R
require(dplyr)
require(tidyr)
require(maptools)
require(tools)
require(rhdf5)
require(data.table)

#setwd("~/tci") ## now set in code/settings.R
source("code/thirdparty/omx.r")

omx <- F

dt.set <- function(dt, idx.start, df) {
  n <- nrow(df)
  for (j in 1:ncol(df)) {
    stopifnot(names(dt)[j] == names(df)[j])
    set(dt, idx.start:(idx.start+n-1), j, df[, j])
  }
}


#default settings
source("code/settings.R")
source("code/cluster/settings.R")
#  Inc levels
#Ic <- c("lowInc", "midInc", "highInc")
# Trip Purpose
#Pr <- c("hbw", "hbs", "hbr", "hbo")
# Define the travel modes
#Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")
# Define time period 
Tp <- c("peak", "offPeak")

# modes used for distance skims
Md.td <- c("auto", "auto", "auto", "tran", "tran", "bike", "walk")
names(Md.td) <- Md

INPUT_DIR <- "data/raw/CALM_2010_090115_CVM_wlk_df"
OUTPUT_DIR <- "data/Corvallis/shp"
path.shp <- file.path(INPUT_DIR, "TAZ")
taz.id_name <- "TAZ"

dir.create(file.path(OUTPUT_DIR), recursive = TRUE, showWarnings = FALSE)
files.shp <- Sys.glob(file.path(path.shp, "TAZ*"))
file.copy(files.shp, OUTPUT_DIR)
#taz.shpfile <- files.shp[sapply(files.shp, file_ext) == "shp"]

# Init Zi
mtx.file <- file.path(INPUT_DIR, "modec/hbw/lowInc/peakTrips.RData")
load(mtx.file)
mtx.obj <- get(file_path_sans_ext(basename(mtx.file)))
Zi <- dimnames(mtx.obj)[[1]]
Zi.max <- length(Zi)

#Load trips by Trips by purpose, income & mode & tod
OUTPUT_DIR = "data/Corvallis/TDM"
dir.create(file.path(OUTPUT_DIR), recursive = TRUE, showWarnings = FALSE)

if (omx) {
  out.file <- file.path(OUTPUT_DIR, "PrIcMdTpTrips.omx")
  createFileOMX(out.file, Zi.max, Zi.max, Replace=TRUE)
} else {
  trips.n <- length(Ic) * length(Pr) * length(Md) * length(Tp) * length(Zi) * length(Zi)
  trips_dt <- data.table(ic='',
                         pr='',
                         md='',
                         tp='',
                         from.taz=rep('',trips.n), 
                         to.taz=rep('',trips.n),
                         trips=rep(0,trips.n))
}
idx.start <- 1

for (pr in Pr) {
  for (ic in Ic){
    for (tp in Tp) {
      # Trips by purpose, income & mode & tod
      obj.name <- paste0(tp, "Trips")
      file.name <- file.path(INPUT_DIR, "modec", pr, ic, paste0(obj.name, ".RData"))
      load(file.name)
      obj.data <- get(obj.name)
      
      for (md in Md) {
        print(paste(pr, ic, tp, md, sep="->"))
        if (omx) {
          mtx.name <- paste0(pr, ic, tp, md, "Trips")
          mtx.descr <- paste("Trips for", pr, ic, tp, md, sep=" ")
          writeMatrixOMX(out.file, obj.data[,,md], mtx.name, Description=mtx.descr)          
        } else {
          obj.mdf <- as.data.frame(obj.data[,,md], stringsAsFactors=FALSE)
          obj.mdf$ic <- ic
          obj.mdf$pr <- pr
          obj.mdf$md <- md
          obj.mdf$tp <- tp
          obj.mdf$from.taz <- rownames(obj.mdf)
          trips.df <- gather(obj.mdf, to.taz, trips, -ic, -pr, -md, -tp, -from.taz)
          
          dt.set(trips_dt, idx.start, trips.df)
          idx.start <- idx.start + nrow(trips.df)
        }
      }
      #rm(list=c(obj.name, file.name, obj.data, mtx.name, mtx.descr))
    }
  }
}
save(trips_dt, file=file.path(OUTPUT_DIR, "trips_dt.rda"))


#Load travel time skims
path.skims <- file.path(INPUT_DIR, "inputs/RData/")
if (F) {
  out.file <- file.path(OUTPUT_DIR, "travelTimeSkims.omx")
  createFileOMX(out.file, Zi.n, Zi.n, Replace=TRUE)
}

ttime.n <- length(Md) * length(Tp) * length(Zi) * length(Zi)
ttime_dt <- data.table(md='',
                       tp='',
                       from.taz=rep('',trips.n), 
                       to.taz=rep('',trips.n),
                       ttime=rep(0,trips.n))
idx.start <- 1

for (md in Md){
  #certain modes or components of a mode may not differetiate b/w peak and offPeak
  #special handling of bikeTime file as the file name doesn't follow the same naming convention for other modes.
  obj.name.suffix1 <- ifelse(md=="bike", paste0(md, "Time"), paste0("Time", md))
  file.ttcomps <- Sys.glob(file.path(path.skims, paste0("*", obj.name.suffix1, ".RData")))
  for (tp in Tp){
    obj.name.suffix2 <- paste0("Time", tp, md)
    file.ttcomps_tp <- Sys.glob(file.path(path.skims, paste0("*", obj.name.suffix2, ".RData")))
    file.ttcomps_tp <- c(file.ttcomps, file.ttcomps_tp)
    
    ttskims <- NULL
    for (file.ttcomp in file.ttcomps_tp){
      obj.name <- file_path_sans_ext(basename(file.ttcomp))
      load(file.ttcomp)
      obj.data <- get(obj.name)
      ttskims <- if(is.null(ttskims)) obj.data else ttskims+obj.data
    }
    #mtx.name <- paste0(tp, md, "Time")
    #mtx.descr <- paste(tp, "Travel Time Skims for", md, sep=" ")
    #writeMatrixOMX(out.file, ttskims, mtx.name, Description=mtx.descr)
    
    obj.mdf <- as.data.frame(ttskims, stringsAsFactors=FALSE)
    obj.mdf$md <- md
    obj.mdf$tp <- tp
    obj.mdf$from.taz <- rownames(obj.mdf)
    ttime.df <- gather(obj.mdf, to.taz, time, -md, -tp, -from.taz)
    
    dt.set(ttime_dt, idx.start, ttime.df)
    idx.start <- idx.start + nrow(ttime.df)      
    
    #rm(list=c(obj.name, ttskims, mtx.name, mtx.descr))
  }
}

#file.remove(file.bikeTime_new)

#load travel distance skim
#travel distance could potentially differ by time period?
path.skims <- file.path(INPUT_DIR, "inputs/RData/")
if (F) {
  out.file <- file.path(OUTPUT_DIR, "travelDistanceSkims.omx")
  createFileOMX(out.file, Zi.n, Zi.n, Replace=TRUE)
}

tdist.n <- length(Md) * length(Zi) * length(Zi)
tdist_dt <- data.table(md='',
                       from.taz=rep('',trips.n), 
                       to.taz=rep('',trips.n),
                       ttime=rep(0,trips.n))
idx.start <- 1

for (md in Md){
  obj.name <- paste0(Md.tdist[[md]], "Dist")
  file.name <- file.path(path.skims, paste0(obj.name, ".RData"))
  load(file.name)
  obj.data <- get(obj.name)
  #mtx.name <- paste0(md, "Distance")
  #mtx.descr <- paste("Travel distance skims for", md, sep=" ")
  #writeMatrixOMX(out.file, obj.data, mtx.name, Description=mtx.descr)
  
  obj.mdf <- as.data.frame(obj.data, stringsAsFactors=FALSE)
  obj.mdf$md <- md
  obj.mdf$from.taz <- rownames(obj.mdf)
  tdist.df <- gather(obj.mdf, to.taz, time, -md, -tp, -from.taz)
  
  dt.set(tdist_dt, idx.start, tdist.df)
  idx.start <- idx.start + nrow(tdist.df)
}