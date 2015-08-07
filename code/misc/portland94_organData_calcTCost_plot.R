# This script organizes Portland_94 data
# There are three parts: part1 loads data file; part2 generate linedk trips; part3 calculate trip cost 
require(dplyr)
require(magrittr)

# Settings
  # Set workspace
  setwd("~/tci")
  var_list.0 <- ls()

  ## settings
  INPUT_DIR <- 'data/'
  OUTPUT_DIR <- 'output/OHAS/portland_94'
  dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
  # whether to save intermediate results
  SAVE.INTERMEDIARIES <- TRUE
  INTERMEDIATE_DIR <- "output/intermediate/OHAS/portland_94"
  dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)
  
  # make names for household income groups, trip purpose and calculation method
  IcNames <- c("Low Income", "Mid Income", "High Income")
  Ic <- c("lowInc", "midInc", "highInc")
  names(IcNames) <- Ic
  
  PrNames <- c("Work", "Shopping", "Recreation", "Other")
  Pr <- c("hbw", "hbs", "hbr", "hbo")
  names(PrNames) <- Pr
  
  CmNames <- c("mintcost", "avgtcost", "maxtcost")
  Cm <- c("min", "avg", "max")
  names(CmNames) <- Cm

# Part1 load data files 
    # load activity 1 data 
    act1<- read.table(file.path(INPUT_DIR, "portland_94/act1.txt"), header=FALSE,  sep=",", as.is=TRUE)
  
    # change column names
    # colname is from http://www.surveyarchive.org/sda/Portland/doc/hcbk.htm
    
    act1.colnames <- c("PHASE", "STRATUM", "SAMPN", "PERNO", "DAYNO", "ACTNO", "ACT1", "OLOC", "THERE",
                           "ACTBEG", "AMPMS", "ACTEND", "AMPME", "TRPSTHEN", "MODE", "OMODE", "VEHAVAIL", 
                           "PAY4CAR", "HOWMUCH", "TIME1", "NOPARTY", "VEHPARTY", "PARTYPAY", "HOWMPP", 
                           "TIME2", "ROUTE1", "BOARDWHR", "ACCESSSTOP", "EGRESTOP", "PAYHOW", "SUBSIDWHO1", 
                           "TRANSFER", "TRANSWHE", "TRANSAG", "HOWMANY", "VEHWHICH", "DRIVER", "NUMINVEH",
                           "PRKWHERE", "PRKPAY", "PRKFARE", "TIME3", "SUBSIDWHO2", "USUBSCOST", "TIME4",
                           "TRIPBEG", "AMPMTB", "TRIPEND", "AMPMTE", "TRIPHRS", "TRIPMIN", "MODCHG", "CHGWHERE", 
                           "CHG2FRM", "HALFMILE")
  
    names(act1.colnames) <- as.character(c(1:55))
    colnames(act1) <- act1.colnames
    #act1 <- transform(act1, OLOC=as.character(OLOC))
    
    # Read geocode data 
    geocode <- read.table(file.path(INPUT_DIR, "portland_94/geocode.raw"), header=FALSE, sep=",")  
    
    colnames(geocode) <- c("UID", "XCORD", "YCORD", "CASE_ID", "FREQ", "RTZ", "SID", "TOTEMP94", "RETEMP94")
    
#     geocode <- geocode[order(geocode$UID), ] 
#     geocode <- transform(geocode, UID=as.character(UID))
#     
#     geocode <- geocode %>% 
#       mutate(SAMPN=substr(UID, 1,6), PERNO=substr(UID, 7, 7), 
#              DAYNO=substr(UID, 8, 8), ACTNO=substr(UID, 9,10)) %>%
#       
#     geocode <- transform(geocode, SAMPN=as.numeric(SAMPN), PERNO=as.numeric(PERNO), 
#                          DYANO=as.numeric(DAYNO), ACTNO=as.numeric(ACTNO))

    # Merge with act1
    #TODO: use left_join (dplyr)
    #TODO: stringr, str_pad
    #require(stringr)
    act1 %<>%
      mutate(#UID.c = paste(SAMPN, PERNO, DAYNO, str_pad(ACTNO, 2, pad="0"), sep=""),
             #UID = as.numeric(UID.c)
             UID = SAMPN * 10^4 + PERNO * 10^3 + DAYNO * 10^2 + ACTNO
             ) %>% 
      left_join(geocode)
    
    
    #act1 <- merge(act1, geocode, by=c("SAMPN", "PERNO", "DAYNO", "ACTNO"), all.x=TRUE)

   # load activity 2 data
    act2 <- read.table(file.path(INPUT_DIR, "portland_94/act2.txt"), header=FALSE,  sep=",")
    act2.colnames <- c("PHASE", "SAMPN", "PERNO", "DAYNO", "ACTNO", "OLOC", "ADDTYP") 
    names(act2.colnames) <- as.character(c(1:7))
    colnames(act2) <- act2.colnames
  
  # load household data
    hh <- read.table(file.path(INPUT_DIR, "portland_94/hh.txt"), header=FALSE,  sep=",")
    hh[9, 28] <- 0 #  hh[9, 28] input error
   
    
    hh.colnames <- c("PHASE", "INTWNUMB", "SAMPN", "CITY", "ZIP", "SAMPTYPE", "STRATUM", "HHSIZE",
                     "PHONES", "PARTYLIN", "CARPHONE", "VEHICLES", "OWNHOME", "YRSHOME", "HOMWTYPE",
                     "OLDAREA", "INCOME", "TRAVELD", "TRAVELD2", "DAY1", "DAY2", "DAY1ACT", "DAY2ACT", 
                     "TOTLACT", "DAY1TRIP", "DAY2TRIP", "TOTLTRIP", "HALFMILE")
    names(hh.colnames) <- as.character(c(1:28))
    colnames(hh) <- hh.colnames
  
  # load person data
    per <- read.table(file.path(INPUT_DIR, "portland_94/per.txt"), header=FALSE,  sep=",")
    per.colnames <- c("PHASE", "SAMPN", "PERNO", "RELATION", "GENDER", "AGE", "RACE", "HOMELANG",
                      "OTHLANG", "SPEAKENG", "LICENSED", "EMPLOYED", "WORKERS", "OCCUPAT", "INDUSTRY",
                      "WORKHOME", "HRSHOME", "SUBPARK", "SHIFTWRK", "PAY2PARK", "COST2PRK", "DRIVE", 
                      "CARPOOL", "TRANSIT", "OTHER", "NOWORK", "YRSWORK", "TWOJOBS", "LASTJOB", "STUDENT", 
                      "HHSTU", "STULEVEL", "SCHOOL", "SCHCITY", "SCHDRIVE", "SCHPOOL", "SCHBUS", "SCHOTHER", 
                      "NOSCHOOL", "HANDICAP", "HANDIOTH")
    names(per.colnames) <- as.character(c(1:41))
    colnames(per) <- per.colnames  
  
  # load recrited household file 
    rhh <- read.table(file.path(INPUT_DIR, "portland_94/rhh.txt"), header=FALSE,  sep=",")
  
    # is second column "comp" ?? only two values, while there are 16 values in codebook for "COMP"
    table(rhh[ , 2])
    # rhh does not include STATE column?? In th codebook, STATE column is between forth column and fifth column
    table(rhh[,4])
    table(rhh[,5])
    
    # sixed column includes value "8", which does not appear in code book
    table(rhh[,6])
  
    # second column is named "COMP"
    rhh.colnames <- c("PHASE", "COMP", "SAMPO", "CITY",  "ZIP", "SAMPTYPE", "STRATUM", "HHSIZE", "PHONES", 
                      "PARTYLIN", "CARPHONE", "VEHICLES", "OWNHOME", "YRSHOME", "HOMETYPE", "OLDAREA", "INCOME", 
                      "HALFMILE")
    names(rhh.colnames) <- as.character(c(1:18))
    colnames(rhh) <- rhh.colnames
  
  
  # load recruited person file data
    rper <- read.table(file.path(INPUT_DIR, "portland_94/rper.txt"), header=FALSE,  sep=",")
    rper.colnames <- c("PHASE", "COMP", "SAMPN", "PERNO", "RELATION", "GENDER", "AGE", "RACE", "HOMELANG",
                       "OTHLANG", "SPEAKENG", "LICENSED", "EMPLOYED", "WORKHRS", "OCCUPAT", "INDUSTRY", 
                       "WORKHOME", "HRSHOME", "SUBPARK", "SHIFTWRK", "PAY2PARK", "COST2PPK", "DRIVE", "CARPOOL", 
                       "TRANSIT", "OTHER", "NOWORK", "YRSWORK", "TWOJOBS", "LASTJOB", "STUDENT", "HHSTU", 
                       "STULEVEL", "SCHOOL", "SCHCITY", "SCHDRIVE", "SCHPOOL", "SCHBUS", "SCHOTHER", "NOSCHOOL",
                       "HANDICAP", "HANDIOTH")
    names(rper.colnames) <- as.character(c(1:42))
    colnames(rper) <- rper.colnames
  
  # load vehicle file 
    veh <- read.table(file.path(INPUT_DIR, "portland_94/veh.txt"), header=FALSE,  sep=",")
    veh.colnames <- c("PHASE", "SAMPN", "VEHNUMBER", "VEHOWNER", "YEAR", "ACQUIRED", "REPLACE", "MAKE", "MODEL", 
                      "CLASS", "TPYE", "FUEL", "BEGOD", "ENDOD", "MILES")
    names(veh.colnames) <- as.character(c(1:15))
    colnames(veh) <- veh.colnames
  
  # load data
    require(foreign)
    data<- read.dbf(file.path(INPUT_DIR, "portland_94/DATA94.DBF"), as.is=FALSE)

# Part2 generates linked trips
  
  # Defnine functions
    # identify trip purpose 
 
    identifyTripPurpose <- function (df) {
      
      workLabels = c("Work","WorkRelated")
      othLabels  = c("Meals","PersonalBus", "Escort", "SocialRec","Other")
      shopLabels = c("Shopping")
      recLabels  = c("Recreation")
      schLabels  = c("School")
      
      ACT1.f
      ACT1
      
      notWorkLabels = c(othLabels,shopLabels,recLabels,schLabels)
      
      df$TripPurpose=ifelse(df$AGGACT %in% workLabels & df$LastHOME==1 & df$HOME!=1,"HBW","NA")
      df$TripPurpose=ifelse(df$AGGACT %in% othLabels  & df$LastHOME==1 & df$HOME!=1,"HBO",df$TripPurpose)
      df$TripPurpose=ifelse(df$AGGACT %in% shopLabels & df$LastHOME==1 & df$HOME!=1,"HBShp",df$TripPurpose)
      df$TripPurpose=ifelse(df$AGGACT %in% recLabels  & df$LastHOME==1 & df$HOME!=1,"HBRec",df$TripPurpose)
      df$TripPurpose=ifelse(df$AGGACT %in% schLabels  & df$LastHOME==1 & df$HOME!=1,"HBSch",df$TripPurpose)
      
      df$TripPurpose=ifelse(df$HOME==1 & df$LastAGGACT %in% workLabels & df$LastHOME!=1,"HBW",df$TripPurpose)
      df$TripPurpose=ifelse(df$HOME==1 & df$LastAGGACT %in% othLabels & df$LastHOME!=1,"HBO",df$TripPurpose)
      df$TripPurpose=ifelse(df$HOME==1 & df$LastAGGACT %in% shopLabels & df$LastHOME!=1,"HBShp",df$TripPurpose)
      df$TripPurpose=ifelse(df$HOME==1 & df$LastAGGACT %in% recLabels & df$LastHOME!=1,"HBRec",df$TripPurpose)
      df$TripPurpose=ifelse(df$HOME==1 & df$LastAGGACT %in% schLabels & df$LastHOME!=1,"HBSch",df$TripPurpose)
      
      return(df)
    }
    
    
#     # add This,Next,Last to each activity for activity, TAZ, and mode
#     # Change for portland_94: PLANO--ACTNO; delete city; DEP_HR--ACTBEG; DEP_MIN--AMPMS; delete routeDistance 
#     addThisNextLast = function(df) {
#       
#       #This Activity,TAZ (X and Y),Mode
#       df$ThisAGGACT = df$AGGACT
#       #df$ThisXCORD = df$XCORD
#       #df$ThisYCORD = df$YCORD
#       df$ThisMODE = df$MODE
#       
#       #Last (Previous) Activity, TAZ, Mode, activity number
#       lastACTNO = match(df$ACTNO -1,df$ACTNO)
#       if(length(df$AGGACT) > 1) {
#         df$LastAGGACT = df$AGGACT[lastACTNO]
#         df$LastXCORD = df$XCORD[lastACTNO]
#         df$LastYCORD = df$YCORD[lastACTNO]
#         df$LastMODE = df$MODE[lastACTNO]
#         df$LastACTNO = df$ACTNO[lastACTNO]
#         df$LastOLOC = df$OLOC[lastACTNO]
#         df$LastHOME = df$HOME[lastACTNO]
#         df$LastRTZ = df$RTZ[lastACTNO]
#         df$LastACTBEG = df$ACTBEG[lastACTNO]
#         df$LastAMPMS = df$AMPMS[lastACTNO]
#       } else {
#         df$LastAGGACT = NA
#         df$LastXCORD = NA
#         df$LastYCORD = NA
#         df$LastMODE = NA
#         df$LastACTNO = NA
#         df$LastOLOC = NA
#         df$LastHOME = NA
#         df$LastRTZ = NA
#         df$LastACTBEG = NA
#         df$LastAMPMS = df$AMPMS[lastACTNO]
#       }
#       
#       #Next Activity, TAZ, Mode
#       nextACTNO = match(df$ACTNO +1,df$ACTNO)
#       
#       if(length(df$AGGACT) > 1) {
#         df$NextAGGACT = df$AGGACT[nextACTNO]
#         df$NextXCORD = df$XCORD[nextACTNO]
#         df$NextYCORD = df$YCORD[nextACTNO]
#         df$NextMODE = df$MODE[nextACTNO]
#         
#       } else {
#         df$NextAGGACT = NA
#         df$NextXCORD = NA
#         df$NextYCORD = NA
#         df$NextMODE = NA
#         
#       }
#       
#       return(df)
#     }
   
   
  # Generate Linked trips 

    # # Add AGGACT and AGGACTCODE for act1 
    #ACT1 <- sort(unique(act1$ACT1)) 
    levels = c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 31, 32, 33, 41, 42, 43, 44, 45, 51, 52, 53, 54, 55, 56, 90, 91)
    AGGACT <- c("Meals", "Work", "Work-related", "Shopping (general)", "Shopping (major)", 
                "Personal services", "Medical care", "Professional services", "Household or personal business", 
                "Household maintenance", "Household obligations", "Pick-Up/Drop-Off passengers", "Visiting", 
                "Casual entertaining", "Formal entertaining", "School", "Culture", "Religion/Civil Services", 
                "Civic", "Volunteer work", "Amusements (at-home)", "Amusements (out-of-home)", "Hobbies", 
                "Exercise/Athletics", "Rest and relaxation", "Spectator athletic events", "Incidental trip", "Tag along trip")

    act1$ACT1.f <- factor(act1$ACT1, levels=levels, labels=AGGACT)
    
    # Use doParallel and foreach packages to generate linkedTrip
#     library(doParallel)
#     clusternumber = 8
#     cluster = makeCluster(clusternumber)
#     registerDoParallel(cluster)
#     require(foreach)
#     
    # Identify if the location is at home or not
    act1 %<>%
      mutate(
        HOME = ifelse(OLOC %in% c("HOME", "RESIDENCE") | as.integer(ACT1) == 51, 1, 0))
    
    # act1$NCHAROLOC <- nchar(act1$OLOC)
    
    # act1$HOME <- ifelse((act1$OLOC=="HOME"|act1$OLOC=="RESIDENCE"|act1$ACT1==51) & act1$NCHAROLOC, 1, 0)
    
    
    act1 %<>% 
      filter(DAYNO==1) %>%
      arrange(SAMPN, PERNO, ACTNO) %>%
      group_by(SAMPN, PERNO) %>%
      mutate(LASTHOME=lag(HOME),
             LASTAGGACT=lag(ACT1.f),
             LASTOLOC=lag(OLOC),
             LastRTZ=lag(RTZ)
             ) %>%
      select(SAMPN, PERNO, ACTNO, HOME, LASTHOME, ACT1.f, LASTAGGACT, OLOC, LASTOLOC) 
     
        

    
   
    # identify trip purpose
    linkedTrip <- identifyTripPurpose(act1)
    
    
    linkedTrip %>%
      mutate(TRPDUR=TRIPHRS *60+TRIPMIN,
             HHWGT=1)
    
#     # Calculate TRPDUR
#     linkedTrip$TRPDUR <- linkedTrip$TRIPHRS *60 + linkedTrip$TRIPMIN
#     
#     # Hypothesis wll HHWGT is 1 
#     linkedTrip$HHWGT <- 1
    
    # Identify trip route distance for linked trip
    # Source omx functions
    source("code/thirdparty/omx.r")
    require(rhdf5)
    
    # Load trip distance matrix
    load(file.path(INPUT_DIR, "emme1994MfNames.RData"))
    omx.file <- file.path(INPUT_DIR,"emme1994.omx")
    tdist <- readMatrixOMX(omx.file, "tdist")
    
    #depends on how many rows with LASTRTZ == NA
    linkedTrip %>%
      left_join(hhtaz)
      mutate(LASTRTZ=ifelse(is.na(LASTRTZ), HTAZ, LASTRTZ))
    
    linkedTrip$DistanceRoute <- with(linkedTrip, tdist[cbind(LASTRTZ, RTZ)] * 5280)

#     linkedTrip$DistanceRoute <- NA
#     
#     for (i in c(1:nrow(linkedTrip))) {
#       if (length(tdist[linkedTrip[i, "LastRTZ"], linkedTrip[i, "RTZ"]]) < 1) {
#         
#         linkedTrip[i, "DistanceRoute"] <- NA
#       } else {
#         
#         linkedTrip[i, "DistanceRoute"] <- tdist[linkedTrip[i, "LastRTZ"], linkedTrip[i, "RTZ"]]*5280
#         
#       }
#       
#     }

    # save results
    input_file <- file.path(INPUT_DIR, "portland_94.RData")
    save(act1, act2, hh, per, rhh, rper, veh, linkedTrip, file=input_file)

# Part3 calculate travel cost 
   # load(file.path(INPUT_DIR, "portland_94.RData"))
  
  # Define functions to calculate tcost 
    source("code/OHAS/functions.R")    
    
  # calculate trip cost 
  # Load required packages
    require(maptools)
    require(rgeos)
    require(rgdal)
    require(dplyr)
    
    # prepare data -> a data.frame for linked trips with columns
    # SAMPN, HHWGT,  HTAZ, inc.level, TripPurpose, MODE, tripdur.hours, tripdist.miles
  
    tcost.trip <- linkedTrip %>% 
      select(SAMPN, HHWGT, PERNO, TripPurpose, MODE, TRPDUR, DistanceRoute) %>%
      mutate(TripPurpose = tolower(TripPurpose),
             TripPurpose=ifelse(TripPurpose=="hbshp", "hbs", TripPurpose),
             TripPurpose=ifelse(TripPurpose=="hbrec", "hbr", TripPurpose)
             #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
             #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
      ) %>%
      filter( TripPurpose %in% c("hbw", "hbs", "hbr", "hbo")) %>%
      mutate(tripdur.hours=TRPDUR/60,
             tripdist.miles=DistanceRoute/5280
      )
    
    # reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
    # low <- (1,5); median <- (6,10); high <- 11:13
    
    # Identify household TAZ and district id
    hhtaz.1 <- linkedTrip %>%
      filter(OLOC=="HOME"| OLOC=="RESIDENCE") %>%
      arrange(SAMPN, OLOC) %>%
      group_by(SAMPN) %>%
      summarize(HTAZ=RTZ)
    
    
    hhtaz.2 <- linkedTrip %>%
      filter(OLOC=="HOME"|OLOC=="RESIDENCE") %>%  # ACT1 == 51 is not needed to identify household TAZ after checking data
      arrange(OLOC) %>%                           # Prefer OLOC == "HOME" to identify household TAZ
      dplyr::select(SAMPN, OLOC, XCORD, YCORD) %>%
      filter(!is.na(XCORD)) %>%
      filter(!duplicated(SAMPN))
    
    
    TAZPoly1994.shapefile <- file.path(INPUT_DIR, "taz1260/taz1260.shp")
    TAZPoly1994 <- readShapePoly(TAZPoly1994.shapefile, proj4string=CRS("+init=epsg:2913"))
    
    spdf = SpatialPointsDataFrame(hhtaz[, c('XCORD', 'YCORD')], 
                                  hhtaz, 
                                  proj4string=CRS("+init=epsg:2913"))
    
    hhtaz$HTAZ <- over(spdf, TAZPoly1994)[,"TAZ"]
    
    districtsPoly.shapefile <- file.path(INPUT_DIR, "shp/districts.shp")
    districtsPoly <- readShapePoly(districtsPoly.shapefile, proj4string=CRS("+init=epsg:2913"))
    hhtaz$district.id <- over(spdf, districtsPoly)[,"DISTRICT"]
    
    
    
    hh.metro <- hh %>% 
      mutate(inc.level=cut(INCOME,
                           breaks=c(1, 6, 11, 13.5),
                           labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                           include.lowest=T, right=F)
             ) %>%
      left_join(hhtaz, by="SAMPN") %>%
      dplyr::select(SAMPN, inc.level, HHSIZE, INCOME, HTAZ, district.id) %>%
      rename(HHSIZ=HHSIZE) %>%
      as.data.frame() 
    
    
    tcost.trip <- tcost.trip %>%
      left_join(hh.metro, by="SAMPN")
    

  # Define unit cost 
    # settings that are common to all methods
    
    # unit travel costs by mode
    # unit travel costs can be differentiated by income 
    # by adding an "inc.level" column to the data.frame
    # with value "lowInc", "midInc", "highInc"
    
    # this configuration converts travel costs to $
    hourly.wage <- 24.77
    MODE <- c(1:8) # 1:10 and 97 are coded in OHAS; 1:2 and 21:25 are coded for TDM
    MdNames <- c("other", "walk", "bicycle", "schol bus", "public bus", "MAX", "personal vehicle", "non-personal vehicle") 
    
    names(MODE) <- MdNames
    
    
    VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.5, 0.5) * hourly.wage
    
    
    # distance-based monetary cost per mile
    # http://www.portlandfacts.com/cost_of_transit_&_cars.html
    # http://portlandtaxi.net/rates.php
    # mcpm <- c(29.6, 0, 0, 101.0, 101.0, 138.0, 0, 59.2, 59.2) / 100
    # mcpm <- rep(0, length(MODE))
    
    ## time-equivalent monetary cost per mile, which can be specific to income group
    mcpm <- c(29.6, 0, 0, 0, 101.0, 138.0, 59.2, 59.2) / (100 * hourly.wage)
    
    unitcosts <- data.frame(MODE, VOT, mcpm)


  # Source scripts to calculate tcost
    source("code/OHAS/compute_tcost.R")
    source("code/OHAS/plot_tcost.R") 
    
##clean up
  var_list.1 <- ls()
  rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
  rm(var_list.1)
    
      
      
      
   
    