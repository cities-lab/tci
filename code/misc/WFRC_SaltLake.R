# This script orgainze WFRC data 

# Load required packages
  require(gdata)
  require(dplyr)
  
  
# Settings
  # Set workspace
  setwd("~/tci")
  var_list.0 <- ls()
  
  ## settings
  INPUT_DIR <- 'data/'
  OUTPUT_DIR <- 'output/WFRC_SaltLake'
  dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
  # whether to save intermediate results
  SAVE.INTERMEDIARIES <- TRUE
  INTERMEDIATE_DIR <- "output/intermediate/WFRC_SaltLake"
  dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)
  
  
  # Define names for household income groups, trip purpose and calculation method
  IcNames <- c("Low Income", "Mid Income", "High Income")
  Ic <- c("lowInc", "midInc", "highInc")
  names(IcNames) <- Ic
  
  PrNames <- c("Work", "Shopping", "Recreation", "Other")
  Pr <- c("hbw", "hbs", "hbr", "hbo")
  names(PrNames) <- Pr
  
  CmNames <- c("mintcost", "avgtcost", "maxtcost")
  Cm <- c("min", "avg", "max")
  names(CmNames) <- Cm  
  
# Load data
  # trip <- read.xls("data/WFRC/HouseholdDiary_TripData_clean.xls", sheet=1, header=TRUE)
  # trip <- read.xls("data/WFRC/HouseholdDiary_TripData_clean.xlsx", sheet=1, header=TRUE)
  trip  <- read.csv("data/WFRC/HouseholdDiary_TripData_clean.csv", header=TRUE, sep=",", as.is = TRUE)
  hh <-  read.csv("data/WFRC/HouseholdDiary_HouseholdData.csv", header=TRUE, sep=",", as.is = TRUE)
 
  # reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
  # hh_income: Total household income; password: household ID
  
  hh.inc <- hh %>% 
          mutate(inc.level=cut(hh_income,
                         breaks=c(1, 3, 5, 10),
                         labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                         include.lowest=T, right=F)) %>%
          group_by(password) %>%
          summarise(hh_income=first(hh_income),
                    inc.level=first(inc.level), 
                    hhsize=first(hhsize))
  

  # identify household taz 
  # oTAZ: origin taz; ptaz, production taz; dtaz: Destination TAZ; ataz: Attraction TAZ; 
  # oloc: trip origin; dLoc: Trip destination
  trip.oloc <- trip %>% 
               group_by(oLoc) %>%
               summarise(count=n()) %>%
               arrange(desc(count)) %>%
               as.data.frame() 
  # There are different spelling for home: Home, home, HOME, HOme, 
    
  # Identify household TAZ
  
  hhtaz <- trip %>% 
            mutate(oLoc=tolower(oLoc))%>% 
            filter(oLoc=="home") %>% 
            group_by(password) %>%
            summarise(HTAZ=first(oTAZ))
  
  
  # Identify trip purpose
  # trip_purpose_num: Trip purpose category, numeric
  # trip_purpose: Trip purpose category
  # o_purpose: Trip origin primary purpose
  # o_purpose3: Trip origin primary purpose 3 cat: Work/Home/Other
  # d_purpose: Trip destination primary purpose, numeric
  # d_purpose3: Trip destination primary purpose 3 cat: Work/Home/Other
  
  levels <- sort(unique(trip$o_purpose))
  purposes <- c("Go home", "Go to primary workplace", "Go shopping (e.g., grocery store, mall)", 
                "Conduct personal business (e.g., doctor, banking, post office)", "Drop off/pick up someone else", 
                "Make a quick stop (e.g., ATM, drive-thru, fast-food, coffee)", "Go to restaurant to eat out/get take out", 
                "Go to other work-related location (e.g., meeting, sales call, delivery)", 
                "Attend social/recreational event (e.g., movies, visit friends/family)", "Go to school/child care", 
                "Go to gym or go for exercise (e.g., go for a walk/jog)", "Go to religious/community/volunteer activity", "Other")
  
  trip$o_purpose.f <- factor(trip$o_purpose, levels=levels, labels=purposes)
  trip$d_purpose.f <- factor(trip$d_purpose, levels=levels, labels=purposes)
  
  
# Defin unit cost 
  # main_mode: Primary mode used on trip
  hourly.wage <- 60
  MODE <- sort(unique(trip$main_mode))
  MdNames <- c("Auto/truck/motorcycle", "Transit", "Walked/wheelchair", "Bicycle", "Other", "School bus (only in independent child trips)") 
  names(MODE) <- MdNames
  
  VOT <- rep(1, length(MODE)) * hourly.wage
  mcpm <- c(59.2, 101.0, 0, 0, 29.6, 0)/(100 * 24.77)
  
  unitcosts <- data.frame(MODE, VOT, mcpm)
  

# 
  # trip_distance: Trip distance Google, assumes mode is auto (miles)
  # District
  # o_airsage_dist: Origin airsage district; p_airsage_dist: Production airsage district; 
  # d_airsage_dist: Destination airsage district; a_airsage_dist: Attraction airsage district
  # weight: Household weight
  
  summary(trip$trip_distance)
  
  # Trip duration 
  table(trip$trip_duration)
  summary(trip$trip_duration)
  
  trip <- trip %>% 
          left_join(hh.inc) %>%
          left_join(hhtaz) %>%
          mutate(tripdist.miles=trip_distance,
                 tripdur.hours=trip_duration/60,
                 TripPurpose=trip_purpose,
                 district.id=o_airsage_dist,
                 HHWGT=weight,
                 MODE=main_mode, 
                 SAMPN=password,
                 PERNO=memberID,
                 TRIPNO=tripID,
                 HHSIZ=hhsize,
                 INCOME=hh_income
                        )
  # Reclassify trip purpose 
  # HBRb includes trip purposes: 1 and 4; HBRb trips are reclasssified as HBO
  # 1: "Go home"
  # 4: "Conduct personal business (e.g., doctor, banking, post office)"
  

  # HBO includes 8 trip purposes; trip purposes for 9 and 11 are reclassified as HBR
  # 1: Go home; 
  # 5: Drop off/pick up someone else;  
  # 6: Make a quick stop (e.g., ATM, drive-thru, fast-food, coffee);
  # 7: Go to restaurant to eat out/get take out 
  # 9: Attend social/recreational event (e.g., movies, visit friends/family); 
  # 11: Go to gym or go for exercise (e.g., go for a walk/jog);
  # 12: Go to religious/community/volunteer activity   
  # 13: other 
  
  trip[which(trip$trip_purpose=="HBO" & trip$o_purpose %in% c(9, 11)), "trip_purpose"] <- "HBR"
  trip[which(trip$trip_purpose=="HBO" & trip$d_purpose %in% c(9, 11)), "trip_purpose"] <- "HBR"
  
  tcost.trip <- trip  %>%
    mutate(TripPurpose = tolower(TripPurpose),
           TripPurpose=ifelse(TripPurpose=="hbshp", "hbs", TripPurpose),
           TripPurpose=ifelse(TripPurpose=="hbo" & (o_purpose %in% c(9, 11)|d_purpose %in% c(9, 11)), "hbr", TripPurpose),
           TripPurpose=ifelse(TripPurpose=="hbrb", "hbo", TripPurpose)
           #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
           #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
    ) %>%
    filter(TripPurpose %in% c("hbw", "hbs", "hbr", "hbo")) %>%
    dplyr::select(SAMPN, PERNO, TRIPNO, HTAZ, INCOME, inc.level, district.id,
                  HHWGT, HHSIZ, MODE, TripPurpose, tripdur.hours, tripdist.miles)

# Source scripts  
  source("code/OHAS/functions.R")
  source("code/OHAS/compute_tcost.R")
  source("code/OHAS/plot_tcost.R")
  
  
##clean up
  var_list.1 <- ls()
  rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
  rm(var_list.1) 
      
    
    
    
    
    
  
  
  
  
  
  
  
  
        
          
  
