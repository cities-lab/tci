# This script organizes Portland_94 data
# There are two parts: part1 loads data file; part2 generate linedk trips

# Part1 load data files 
  # load activity 1 data 
    act1<- read.table("data/portland_94/act1.txt", header=FALSE,  sep=",")
  
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
    
  # Add x, y coordinate to act1 
    xycord <- read.csv("data/portland_94/1994_ax_ay.csv", header=TRUE,  sep=",")
    colnames(xycord) <- c("SAMPN", "DAYNO", "PERNO", "ACTNO", "XCORD", "YCORD")
    act1 <- merge(act1, xycord, by=c("SAMPN", "DAYNO", "PERNO", "ACTNO"), all.x=TRUE)
  
  # load activity 2 data
    act2 <- read.table("data/portland_94/act2.txt", header=FALSE,  sep=",")
    act2.colnames <- c("PHASE", "SAMPN", "PERNO", "DAYNO", "ACTNO", "OLOC", "ADDTYP") 
    names(act2.colnames) <- as.character(c(1:7))
    colnames(act2) <- act2.colnames
  
  # load household data
    hh <- read.table("data/portland_94/hh.txt", header=FALSE,  sep=",")
    hh[9, 28] <- 0 #  hh[9, 28] input error
   
    
    hh.colnames <- c("PHASE", "INTWNUMB", "SAMPN", "CITY", "ZIP", "SAMPTYPE", "STRATUM", "HHSIZE",
                     "PHONES", "PARTYLIN", "CARPHONE", "VEHICLES", "OWNHOME", "YRSHOME", "HOMWTYPE",
                     "OLDAREA", "INCOME", "TRAVELD", "TRAVELD2", "DAY1", "DAY2", "DAY1ACT", "DAY2ACT", 
                     "TOTLACT", "DAY1TRIP", "DAY2TRIP", "TOTLTRIP", "HALFMILE")
    names(hh.colnames) <- as.character(c(1:28))
    colnames(hh) <- hh.colnames
  
  # load person data
    per <- read.table("data/portland_94/per.txt", header=FALSE,  sep=",")
    per.colnames <- c("PHASE", "SAMPN", "PERNO", "RELATION", "GENDER", "AGE", "RACE", "HOMELANG",
                      "OTHLANG", "SPEAKENG", "LICENSED", "EMPLOYED", "WORKERS", "OCCUPAT", "INDUSTRY",
                      "WORKHOME", "HRSHOME", "SUBPARK", "SHIFTWRK", "PAY2PARK", "COST2PRK", "DRIVE", 
                      "CARPOOL", "TRANSIT", "OTHER", "NOWORK", "YRSWORK", "TWOJOBS", "LASTJOB", "STUDENT", 
                      "HHSTU", "STULEVEL", "SCHOOL", "SCHCITY", "SCHDRIVE", "SCHPOOL", "SCHBUS", "SCHOTHER", 
                      "NOSCHOOL", "HANDICAP", "HANDIOTH")
    names(per.colnames) <- as.character(c(1:41))
    colnames(per) <- per.colnames  
  
  # load recrited household file 
    rhh <- read.table("data/portland_94/rhh.txt", header=FALSE,  sep=",")
  
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
    rper <- read.table("data/portland_94/rper.txt", header=FALSE,  sep=",")
    rper.colnames <- c("PHASE", "COMP", "SAMPN", "PERNO", "RELATION", "GENDER", "AGE", "RACE", "HOMELANG",
                       "OTHLANG", "SPEAKENG", "LICENSED", "EMPLOYED", "WORKHRS", "OCCUPAT", "INDUSTRY", 
                       "WORKHOME", "HRSHOME", "SUBPARK", "SHIFTWRK", "PAY2PARK", "COST2PPK", "DRIVE", "CARPOOL", 
                       "TRANSIT", "OTHER", "NOWORK", "YRSWORK", "TWOJOBS", "LASTJOB", "STUDENT", "HHSTU", 
                       "STULEVEL", "SCHOOL", "SCHCITY", "SCHDRIVE", "SCHPOOL", "SCHBUS", "SCHOTHER", "NOSCHOOL",
                       "HANDICAP", "HANDIOTH")
    names(rper.colnames) <- as.character(c(1:42))
    colnames(rper) <- rper.colnames
  
  # load vehicle file 
    veh <- read.table("data/portland_94/veh.txt", header=FALSE,  sep=",")
    veh.colnames <- c("PHASE", "SAMPN", "VEHNUMBER", "VEHOWNER", "YEAR", "ACQUIRED", "REPLACE", "MAKE", "MODEL", 
                      "CLASS", "TPYE", "FUEL", "BEGOD", "ENDOD", "MILES")
    names(veh.colnames) <- as.character(c(1:15))
    colnames(veh) <- veh.colnames
  
  # load data
    require(foreign)
    data<- read.dbf("data/portland_94/DATA94.DBF", as.is=FALSE)

# Part2 generates linked trips
    
    # Define functions for generating linked trips 
    #distance function - lat/long
    distance = function(x1,x2,y1,y2) { 
      
      dLat = (y2-y1)/180*pi
      dLon = (x2-x1)/180*pi
      a = sin(dLat/2) * sin(dLat/2) + cos(y1/180*pi) * cos(y2/180*pi) * sin(dLon/2) * sin(dLon/2)
      c = 2 * atan2(sqrt(a), sqrt(1-a))
      d = 3958.82 * c; #(earth radius in miles) * c
      return(d * 5280) #distance feet
    }
    
    # identify trip purpose 
    
    identifyTripPurpose <- function (df) {
      
      workLabels = c("Work","WorkRelated")
      othLabels  = c("Meals","PersonalBus", "Escort", "SocialRec","Other")
      shopLabels = c("Shopping")
      recLabels  = c("Recreation")
      schLabels  = c("School")
      notWorkLabels = c(othLabels,shopLabels,recLabels,schLabels)
      
      df$TripPurpose=ifelse(df$AGGACT %in% workLabels & df$LastOLOC=="HOME","hbw","NA")
      df$TripPurpose=ifelse(df$AGGACT %in% othLabels  & df$LastOLOC=="HOME","hbo",df$TripPurpose)
      df$TripPurpose=ifelse(df$AGGACT %in% shopLabels & df$LastOLOC=="HOME","hbshp",df$TripPurpose)
      df$TripPurpose=ifelse(df$AGGACT %in% recLabels  & df$LastOLOC=="HOME","hbrec",df$TripPurpose)
      df$TripPurpose=ifelse(df$AGGACT %in% schLabels  & df$LastOLOC=="HOME","hbsch",df$TripPurpose)
      
      df$TripPurpose=ifelse(df$OLOC=="HOME" & df$LastAGGACT %in% workLabels,"hbw",df$TripPurpose)
      df$TripPurpose=ifelse(df$OLOC=="HOME" & df$LastAGGACT %in% othLabels,"hbo",df$TripPurpose)
      df$TripPurpose=ifelse(df$OLOC=="HOME" & df$LastAGGACT %in% shopLabels,"hbshp",df$TripPurpose)
      df$TripPurpose=ifelse(df$OLOC=="HOME" & df$LastAGGACT %in% recLabels,"hbrec",df$TripPurpose)
      df$TripPurpose=ifelse(df$OLOC=="HOME" & df$LastAGGACT %in% schLabels,"hbsch",df$TripPurpose)
      
      return(df)
    }
    
    
    # add This,Next,Last to each activity for activity, TAZ, and mode
    # Change for portland_94: PLANO--ACTNO; delete city; DEP_HR--ACTBEG; DEP_MIN--AMPMS; delete routeDistance 
    addThisNextLast = function(df) {
      
      #This Activity,TAZ (X and Y),Mode
      df$ThisAGGACT = df$AGGACT
      df$ThisXCORD = df$XCORD
      df$ThisYCORD = df$YCORD
      df$ThisMODE = df$MODE
      
      #Last (Previous) Activity, TAZ, Mode, activity number
      lastACTNO = match(df$ACTNO -1,df$ACTNO)
      if(length(df$AGGACT) > 1) {
        df$LastAGGACT = df$AGGACT[lastACTNO]
        df$LastXCORD = df$XCORD[lastACTNO]
        df$LastYCORD = df$YCORD[lastACTNO]
        df$LastMODE = df$MODE[lastACTNO]
        df$LastACTNO = df$ACTNO[lastACTNO]
        df$LastOLOC = df$OLOC[lastACTNO]
        
        df$LastACTBEG = df$ACTBEG[lastACTNO]
        df$LastAMPMS = df$AMPMS[lastACTNO]
      } else {
        df$LastAGGACT = NA
        df$LastXCORD = NA
        df$LastYCORD = NA
        df$LastMODE = NA
        df$LastACTNO = NA
        df$LastOLOC = NA
        
        df$LastACTBEG = NA
        df$LastAMPMS = df$AMPMS[lastACTNO]
      }
      
      #Next Activity, TAZ, Mode
      nextACTNO = match(df$ACTNO +1,df$ACTNO)
      
      if(length(df$AGGACT) > 1) {
        df$NextAGGACT = df$AGGACT[nextACTNO]
        df$NextXCORD = df$XCORD[nextACTNO]
        df$NextYCORD = df$YCORD[nextACTNO]
        df$NextMODE = df$MODE[nextACTNO]
        
      } else {
        df$NextAGGACT = NA
        df$NextXCORD = NA
        df$NextYCORD = NA
        df$NextMODE = NA
        
      }
      
      #Trip straight line distance (ft)
      df$Distance = distance(df$LastXCORD, df$ThisXCORD, df$LastYCORD, df$ThisYCORD)
      
      return(df)
    }
    
    # Generate Linked trips 

    # # Add AGGACT and AGGACTCODE for act1 
    ACT1 <- sort(unique(act1$ACT1)) 
    
    AGGACT <- c("Meals", "Work", "WorkRealted", "Shopping", "Shopping", "PersonalBus", "PersonalBus", "PersonalBus",
                "PersonalBus", "PersonalBus", "PersonalBus", "Escort", "PersonalBus", "Recreation", "Recreation",
                "School", "SocialRec", "SocialRec", "SocialRec", "SocialRec", "Recreation", "Recreation", 
                "Recreation", "Recreation", "Recreation", "Recreation", "Other", "Other")
    AGGACTCODE <-  c("M", "W", "WR", "Shp", "Shp", "PB", "PB", "PB",
                     "PB", "PB", "PB", "E", "PB", "R", "R",
                     "Sch", "SR", "SR", "SR", "SR", "R", "R", 
                     "R", "R", "R", "R", "O", "O")
    
    AGGACT.df <- data.frame(ACT1, AGGACT, AGGACTCODE)
    
    act1 <- merge(act1, AGGACT.df, by="ACT1", all.x=TRUE)
    act1 <- transform(act1, AGGACT=as.character(AGGACT), AGGACTCODE=as.character(AGGACTCODE))
    
    # Generate linked trips
    # Use doParallel and foreach packages to generate linkedTrip
    library(doParallel)
    clusternumber = 2
    cluster = makeCluster(clusternumber)
    registerDoParallel(cluster)
    require(foreach)
    
    act1 <- act1[order(act1$SAMPN,act1$DAYNO, act1$PERNO, act1$ACTNO), ]
    act_hh = by(act1, act1$SAMPN, function(x) {x})
    act_out = foreach(i=act_hh, .combine="rbind") %dopar% {
      
      # flag to indicate if activity output data frame created 
      createdActDF = FALSE 
      
      dayno <- unique(i$DAYNO)
      
      for (j in dayno) {
        
        act2 <- i[which(i$DAYNO==j), ]
        perno <- unique(act2$PERNO)
        
        for (k in perno) {
          act3 <- act2[which(act2$PERNO==k), ]
          act4 <- addThisNextLast(act3)
          # act6 <- identifyTripPurpose(act5)
          
          if(createdActDF) {
            actReturn = rbind(actReturn, act4)
          } else {
            actReturn = act4
            createdActDF = TRUE 
            
          }
          
          # End loop for perno
        }
        
        # End loop for dayno
      }
      return(actReturn)
      
      # End loop for sampn
    }
    
    
   
    # identify trip purpose
    linkedTrip <- identifyTripPurpose(act_out)
    
    # save results
    save(act1, act2, hh, per, rhh, rper, veh, linkedTrip, file="data/portland_94.RData")
