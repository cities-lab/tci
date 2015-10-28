# This script organizes Tampa Bay 1996 data

# Load required packages
require(dplyr)

# Settings
# Set workspace
setwd("~/tci")
var_list.0 <- ls()

## settings
  INPUT_DIR <- 'data/'
  OUTPUT_DIR <- 'output/Survey/TampaBay_96/Minutes'
  dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
# whether to save intermediate results
  SAVE.INTERMEDIARIES <- TRUE
  INTERMEDIATE_DIR <- "output/intermediate/tampabay_96"
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


# Define unit cost 
  # tripmode 
  # Private vehicle -1
  # Bus - 2
  # Walk/Bike - 3
  # Other - 4
  
  # this configuration converts travel costs to time
  hourly.wage <- 60
  MODE <- c(1:4) 
  MdNames <- c("Private vehicle", "Bus", "Walk/Bike", "Other")
  names(MODE) <- MdNames
  
  # VOT <- c(0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.35)
  VOT <- rep(1, length(MODE)) * hourly.wage
  mcpm <- c(59.2, 101.0, 0, 29.6)/(100 * 24.77)
  
  unitcosts <- data.frame(MODE, VOT, mcpm)



# Load data
  trip <- read.table("data/TampaBay1996/Tampa_Bay_Region_Household_Travel_Survey_Data_1996.dat", header=FALSE, sep="\t")
  
  trip.colnames <- c("hhldid", "hhldsize", "hvehicle", "du_type", "hhincome", 
                     "memberid", "hhmemid", "age", "emp_stat", "mem_lic", 
                     "res_stat", "weight", "exp_wght", "trip_num",  "tripmode", 
                     "trip_occ", "tripdate", "trip_day", "beg_hour", "beg_min", 
                     "end_hour", "end_min", "origin", "destinat", "beg_area", "end_area")
  
  
  colnames(trip) <- trip.colnames
  
  
  # Add label for act1 
  levels = c(1:9)
  actlables <- c("Home", "School", "Daycare", "Recreation", "Shop/Errand", "Friend", "Work", "Other", "Medical")
  
  trip$origin.f <- factor(trip$origin, levels=levels, labels=actlables)
  trip$destinat.f <- factor(trip$destinat, levels=levels, labels=actlables)
  
  # identify trip purpose 
    # origin：origin of the trip；destinat: detination of the trip
    # Home - 1; School - 2; Daycare - 3; Recreation - 4; Shop/Errand - 5; Friend - 6; Work - 7; Other – 8; Medical – 9; 
    
    # Identify trip purpose 
    workLabels = c("Work")
    othLabels  = c("Daycare", "Friend", "Other", "Medical" )
    shopLabels = c("Shop/Errand")
    recLabels  = c("Recreation" )
    schLabels  = c("School")
    
    # do not consider went for a drive, travels for living
    # pick-up/drop-off someone at work, pick-up/drop-off someone at school, pick-up/drop-off someone at other
    
    trip$TripPurpose=ifelse(trip$destinat.f %in% workLabels & trip$origin==1,"HBW","NA")
    trip$TripPurpose=ifelse(trip$destinat.f %in% othLabels  & trip$origin==1,"HBO",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destinat.f %in% shopLabels & trip$origin==1,"HBShp",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destinat.f %in% recLabels  & trip$origin==1,"HBRec",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destinat.f %in% schLabels  & trip$origin==1,"HBSch",trip$TripPurpose)
    
    trip$TripPurpose=ifelse(trip$destinat==1 & trip$origin.f %in% workLabels,"HBW",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destinat==1 & trip$origin.f %in% othLabels,"HBO",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destinat==1 & trip$origin.f %in% shopLabels,"HBShp",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destinat==1 & trip$origin.f %in% recLabels,"HBRec",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destinat==1 & trip$origin.f %in% schLabels,"HBSch",trip$TripPurpose)
    
    
    # identify household taz
    # There is no taz information in the file
    # 
    
    
    
    # reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
    trip <- trip %>% 
      mutate(inc.level=cut(hhincome,
                           breaks=c(1, 3, 5, 6),
                           labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                           include.lowest=T, right=F)) 
    
     
    # Assumption: tripdist.miles is o;district.id is 1; 
    # HHWGT is exp_qgt: Weight combined with population expansion factor
    # Rename colnames to correspond following calculation
    trip <- trip %>% 
      mutate(tripdur.hours = (end_hour+end_min/60) - (beg_hour+beg_min/60),
             tripdist.miles=0,
             district.id=1
             ) %>%            
      dplyr::rename(SAMPN=hhldid, 
                    PERNO=memberid,
                    TRIPNO=trip_num,
                    HHWGT=exp_wght,
                    MODE=tripmode,
                    HHSIZ=hhldsize, 
                    INCOME=hhincome
                    )
    
    
      tcost.trip <- trip  %>%
      mutate(TripPurpose = tolower(TripPurpose),
             TripPurpose=ifelse(TripPurpose=="hbshp", "hbs", TripPurpose),
             TripPurpose=ifelse(TripPurpose=="hbrec", "hbr", TripPurpose)
             #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
             #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
      ) %>%
      filter(TripPurpose %in% c("hbw", "hbs", "hbr", "hbo")) %>%
      dplyr::select(SAMPN, PERNO, TRIPNO,INCOME, inc.level, district.id,
                    HHWGT, HHSIZ, MODE, TripPurpose, tripdur.hours, tripdist.miles)
    
    
    # Source scripts  
#     source("code/functions.R")
#     source("code/OHAS/compute_tcost.R")
#     source("code/OHAS/plot_tcost.R")
    
    ##clean up
    var_list.1 <- ls()
    rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
    rm(var_list.1)  
    