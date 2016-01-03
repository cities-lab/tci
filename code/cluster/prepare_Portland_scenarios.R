# load required package
require(dplyr)
require(rgeos)
require(rgdal)
source("code/thirdparty/omx.r")

Zi.n <- length(Zi)

# read four counties blockgroup level LEHD data, with a tazindex column identifying
# the taz id the block group is in 
lehd <- read.csv(file.path(INPUT_DIR, "LEHD.csv"), header=TRUE)

emp.by.taz <- lehd %>%
  dplyr::rename(TAZ = tazindex) %>%
  group_by(TAZ) %>%
  summarise(tot.emp=sum(C000,  na.rm=TRUE),
            gvt.emp=sum(CNS20, na.rm=TRUE),
            ret.emp=sum(CNS07, CNS18, na.rm=TRUE),
            fin.emp=sum(CNS10, CNS11, CNS13, na.rm=TRUE),
            svc.emp=sum(CNS09, CNS12, CNS14, CNS15, CNS16, CNS17, CNS19, na.rm=TRUE)
  )

# get number of Household in each taz
hh.by.taz <- read.csv(file.path(INPUT_DIR, "HIA.csv"), header=TRUE)
hh.by.taz$HHold <- rowSums(hh.by.taz[, 2:64]) 
hh.by.taz <- hh.by.taz %>%
  dplyr::select(taz, HHold) %>%
  mutate(HHold=round(HHold)) %>%
  rename(TAZ=taz)

# get park acres in each taz
ParkAcres <- read.csv(file.path(INPUT_DIR, "ParkAcres.csv"), header=TRUE)

# Merge Employment and HHold, ParkAcres, area of TAZ
EmpHHold <- left_join(emp.by.taz, hh.by.taz, by="TAZ")
EmpHHoldParkAcre <- left_join(EmpHHold, ParkAcres, by="TAZ")

SQFT.KM2 <- 0.092903/1000000 # define a constant for converting sqft to sqkm

#taz <- readOGR(dsn=file.path(INPUT_DIR, "shp"), layer = "TAZ")
                         #proj4string=CRS("+init=epsg:2913"))


taz.data <- taz@data %>% 
  dplyr::select_("TAZ", area.sqft=taz.area) %>%
  mutate(seq=1:n(), area.km2 = area.sqft * SQFT.KM2)

# Merge Area and EmpHHoldParkAcre
AllData <- left_join(taz.data, EmpHHoldParkAcre,  by="TAZ")

# Calculate densities
AllData <- AllData %>% 
  mutate(non.ret = tot.emp - ret.emp,
         non.retsvcgvt = non.ret - svc.emp - gvt.emp,
         st.hbw = tot.emp,
         st.hbs = ret.emp + .008396 * non.ret + .022126*HHold,
         st.hbr = tot.emp + 1.278 * HHold + 4.6833 *ParkAcres,
         st.hbo = 0.2393 * HHold + ret.emp + 0.6419 * svc.emp + 0.6109 * gvt.emp + 0.06802 * non.retsvcgvt,
         den.hbw = st.hbw / area.km2,
         den.hbs = st.hbs / area.km2,
         den.hbr = st.hbr / area.km2,
         den.hbo = st.hbo / area.km2)

# re-order by the original row order (seq), as shp file depends on the order
AllData <- AllData %>% 
  arrange(seq) %>% 
  dplyr::select(TAZ, area.km2, st.hbw, den.hbw, st.hbs, den.hbs, st.hbr, den.hbr, st.hbo, den.hbo)

taz@data <- left_join(taz@data, AllData, by="TAZ")
# eliminate TAZ with null value (external TAZs)
taz <- taz[!is.na(taz@data$den.hbw), ]

if (SAVE.INTERMEDIARIES) {
  out.shp <- "tazden"
  unlink(file.path(INTERMEDIATE_DIR, paste(out.shp, ".*", sep="")))
  writeOGR(taz, INTERMEDIATE_DIR, out.shp, driver="ESRI Shapefile")
}

basket.args$taz <- taz

# compute mode probabilities to get trips by mode, pr, ic
#source("compute_md_prob_trips.R")

## apply time of day factors when trip tables by pr, ic, tod, and mode are not available
file.tod <- file.path(INPUT_DIR, 'TDM/time_of_day.csv')
tod <- read.table(file.tod, header = TRUE, sep=",")

tod.pr.md <- tod %>%
  group_by(purpose, mode) %>%
  mutate(peak.factor = AM2 + PM2, offpeak.factor = 1 - peak.factor)

file.tptrips <- file.path(dirname(file.trips), "PrIcTpMdODTrips.omx")

if (!file.exists(file.tptrips)) {
  createFileOMX(file.tptrips, Zi.n, Zi.n, Replace=TRUE)
  
  for (ic in Ic) {
    for (pr in Pr) {
      for (md in Md) {
        if (md %in% c('bike', 'walk')) {
          peak.factor = .5
        } else {
          peak.factor <- tod.pr.md %>% 
            ungroup() %>%
            filter(purpose==pr, mode==md, direction=='p-a') %>% 
            select(peak.factor)
          
          stopifnot(nrow(peak.factor)==1)
          peak.factor <- as.numeric(peak.factor)
        }
        
        offpeak.factor <- 1 - peak.factor
        
        trips.name <- paste0(pr, ic, md, "trips")
        trips.mtx <- readMatrixOMX(file.trips, trips.name)
        
        tp <- "peak"  
        peaktrips.mtx <- trips.mtx * peak.factor
        mtx.name <- paste0(pr, ic, tp, md, "Trips")
        mtx.descr <- paste("Trips for", pr, ic, tp, md, sep = " ")
        writeMatrixOMX(file.tptrips, peaktrips.mtx, mtx.name, Description = mtx.descr)      
        
        tp <- "offpeak"
        offpeaktrips.mtx <- trips.mtx * offpeak.factor
        mtx.name <- paste0(pr, ic, tp, md, "Trips")
        mtx.descr <- paste("Trips for", pr, ic, tp, md, sep = " ")
        writeMatrixOMX(file.tptrips, peaktrips.mtx, mtx.name, Description = mtx.descr)
      }
    }
  }
}

file.trips <- file.tptrips

file.ttime <- file.path(path.tdm, "travelTimeSkims.omx")
#createFileOMX(file.tptrips, Zi.n, Zi.n, Replace=TRUE)

#rename travel time skims from MdTpTime to TpMdTime
#handle bike & walk peak/offpeak
for (md in Md) {
  for (tp in Tp) {
    if (md %in% c('bike', 'walk')) {
      ttime.name <- paste0(md, "Time")
    } else {
      ttime.name <- paste0(md, tp, "Time")
    }
    
    ttime.mtx <- readMatrixOMX(file.ttime, ttime.name)
    
    ttime.name_new <- paste0(tp, md, "Time")
    
    if (!(ttime.name_new %in% listOMX(file.ttime)$Matrices$name)) {
      ttime.descr <- paste(tp, "travel time skims for", md, sep = " ")
      writeMatrixOMX(file.ttime, ttime.mtx, ttime.name_new, Description = ttime.descr)
    }
  }
}

file.tdist <- file.path(path.tdm, "travelDistanceSkims.omx")

#rename travel distance skims from MdTpTime to TpMdTime
#handle bike & walk peak/offpeak
for (md in Md) {
  for (tp in Tp) {
    if (md %in% c('bike', 'walk')) {
      tdist.name <- paste0(md, "Distance")
    } else {
      tdist.name <- paste0(md, tp, "Distance")
    }
    
    tdist.mtx <- readMatrixOMX(file.tdist, tdist.name)
    
    tdist.name_new <- paste0(tp, md, "Distance")
    
    if (!(tdist.name_new %in% listOMX(file.tdist)$Matrices$name)) {
      tdist.descr <- paste(tp, "travel distance skims for", md, sep = " ")
      writeMatrixOMX(file.tdist, tdist.mtx, tdist.name_new, Description = tdist.descr)
    }
  }
}

