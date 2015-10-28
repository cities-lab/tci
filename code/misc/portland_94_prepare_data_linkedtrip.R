

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
  # Load data
  load("data/portland_94.RData")
  
  # # Add AGGACT and AGGACTCODE
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
  act1 <- act1[order(act1$SAMPN,act1$DAYNO, act1$PERNO, act1$ACTNO), ]
  sampn <- unique(act1$SAMPN)
  
  # flag to indicate if activity output data frame created 
  createdActDF = FALSE 
  
  for (i in sampn) {
    
    act2 <- act1[which(act1$SAMPN==i), ]
    dayno <- unique(act2$DAYNO)

    for (j in dayno) {
      
      act3 <- act2[which(act2$DAYNO==j), ]
      perno <- unique(act3$PERNO)
      
      for (k in perno) {
        act4 <- act3[which(act3$PERNO==k), ]
        act5 <- addThisNextLast(act4)
       
        if(createdActDF) {
          actReturn = rbind(actReturn, act5)
        } else {
          actReturn = act5
          createdActDF = TRUE 
          
        }
        
        # End loop for perno
      }
      
      # End loop for dayno
    }
    
    # End loop for sampn
  }
  
  # Use doParallel and foreach packages to generate linkedTrip
  
  # distribute process
  
  library(doParallel)
  clusternumber = 8
  cluster = makeCluster(clusternumber)
  registerDoParallel(cluster)
  require(foreach)
  
  act1 <- act1[order(act1$SAMPN,act1$DAYNO, act1$PERNO, act1$ACTNO), ]
  # 
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

  linkedTrip <- identifyTripPurpose(actReturn)
  
  save(act1, act2, hh, per, rhh, rper, veh, linkedTrip, file="data/portland_94.RData")
  
  