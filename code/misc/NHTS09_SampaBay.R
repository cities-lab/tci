# This script organizes NHTS2009 in Tampa Bay data

# Load required packages
  require(dplyr)

# Settings
# Set workspace
  setwd("~/tci")
  var_list.0 <- ls()

## settings
  INPUT_DIR <- 'data/'
  OUTPUT_DIR <- 'output/NHTS09_TampaBay'
  dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
# whether to save intermediate results
  SAVE.INTERMEDIARIES <- TRUE
  INTERMEDIATE_DIR <- "output/intermediate/NHTS09_TampaBay"
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

# load data 
  day <- read.csv("data/NHTS/DAYV2PUB.CSV", header=TRUE, sep=",") 

# Travel day trip file records each trip 
# ID variables: HOUSEID, PERSONID, TDTRPNUM 

# Identify Tampa Bay trips 
  day <- day %>% 
         filter(HHC_MSA==8280) # 8280 = Tampa--St. Petersburg--Clearwater, FL; 15238 trips 


# TRIPPURP field identify home-based purpose types

# reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
# HHFAMINC: Total household income 

  day <- day %>% 
         mutate(inc.level=cut(HHFAMINC,
                         breaks=c(1, 6, 11, 18),
                         labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                         include.lowest=T, right=F)) 


# Trip distance 
# TRPMILES Calculated Trip distance converted into miles: -9 = Not ascertained; -8 = Don't know; -7 = Refused; -1 = Appropriate skip; 0-9000
# Filter unkown trip distance rows 
  day <- day %>% 
         filter(TRPMILES >= 0)

# Trip duration 
# TRVLCMIN (Calculated travel time) is calculacted based on ENDTIME (Trip END time in military) and STRTTIME (Trip START time in military)
# Filter unkown trip duration rows: -9 = Not ascertained; 0-1439
  day <- day %>% 
         filter(TRVLCMIN >= 0)

# Filter weekends
# TDWKND TD trip was on weekend: 01 = Yes; 02 = No
  day <- day %>%
         filter(TDWKND==2)

# Calculatr trip cost 
# Assume district.id is 1; HHWGT is 1;  HTAZ is 1
  day <- day %>%
         mutate(tripdur.hours=TRVLCMIN/60,
                district.id=1,
                HHWGT=1, 
                HTAZ=1) %>%
         dplyr::rename(tripdist.miles=TRPMILES,
                       MODE=TRPTRANS, 
                       HHSIZ=HHSIZE,
                       INCOME=HHFAMINC,
                       SAMPN=HOUSEID,
                       PERNO=PERSONID,
                       TRIPNO=TDTRPNUM, 
                       TripPurpose=TRIPPURP)
    
  
  tcost.trip <- day  %>%
    mutate(TripPurpose = tolower(TripPurpose),
           TripPurpose=ifelse(TripPurpose=="hbshop", "hbs", TripPurpose),
           TripPurpose=ifelse(TripPurpose=="hbsocrec", "hbr", TripPurpose)
           #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
           #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
    ) %>%
    filter(TripPurpose %in% c("hbw", "hbs", "hbr", "hbo")) %>%
    dplyr::select(SAMPN, PERNO, TRIPNO, INCOME, inc.level, district.id,
                   HHSIZ, HHWGT, HTAZ, MODE, TripPurpose, tripdur.hours, tripdist.miles) 
  
  
# Define unist cost 
  # this configuration converts travel costs to $
  hourly.wage <- 60
  MODE <-sort(unique(tcost.trip$MODE)) 
  MdNames <- c("Don't know", "Refused", "Appropriate skip", "Car", "Van", "SUV", "Pickup truck", "Other truck", "Motorcycle", "Light electric veh (golf cart)", 
               "Local public bus", "School bus", "Charter/tour bus", "Shuttle bus", "Taxicab", "Bicycle", "Walk", "Other")
  
  names(MODE) <- MdNames
  
  VOT <- rep(1, length(MODE)) * hourly.wage
  
  # Motorcycle 59.2 
  # RV mode 
  mcpm <- c(NA, NA, NA, 59.2, 59.2, 59.2, 59.2, 59.2, 29.6, 29.6, 101.0, 0, 101.0, 0, 260.0, 0, 0, 29.6)/(100 * 24.77)
  
  # rep(1, length(MODE))
  unitcosts <- data.frame(MODE, VOT, mcpm)

# Source scripts  
  source("code/OHAS/functions.R")
  source("code/OHAS/compute_tcost.R")
  source("code/OHAS/plot_tcost.R")
  
  
##clean up
  var_list.1 <- ls()
  rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
  rm(var_list.1) 