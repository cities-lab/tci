# This script organizes Salt Lake 93 data

# Load required packages
require(dplyr)

# Settings
# Set workspace
setwd("~/tci")
var_list.0 <- ls()

## settings
INPUT_DIR <- 'data/'
OUTPUT_DIR <- 'output/Survey/saltlake_93/Minutes'
dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
# whether to save intermediate results
SAVE.INTERMEDIARIES <- TRUE
INTERMEDIATE_DIR <- "output/intermediate/saltlake_93"
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
# Mode 
#  Value    Label
#     1    walk
#     2    driver
#     3    passenger
#     4    UTA bus
#     5    other public transit/taxi
#     6    school bus
#     7    motorcycle
#     8    bicycle
#     9    other

  # this configuration converts travel costs to time
  hourly.wage <- 60
  MODE <- c(1:9) 
  MdNames <- c("walk", "driver", "passenger", "UTA bus", "other public transit/taxi", "school bus", "motorcycle", "bicycle", "other")
  names(MODE) <- MdNames
  
  # VOT <- c(0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.35)
  VOT <- rep(1, length(MODE)) * hourly.wage
  mcpm <- c(0, 59.2, 59.2, 101.0, 260.0, 0, 29.6, 0, 29.6)/(100 * 24.77)
  
  unitcosts <- data.frame(MODE, VOT, mcpm)



# Load data 
  
  # Load activity 
  # id (1) Household ID Number;  pernum (2) Person Number
  activity <- read.csv(file.path(INPUT_DIR, "saltlake_93/activity.csv"), header=TRUE, sep=",")
  
  # Load cover
  cover <- read.csv(file.path(INPUT_DIR, "saltlake_93/cover.csv"), header=TRUE, sep=",")

  # Load hhld
  hhld <- read.csv(file.path(INPUT_DIR, "saltlake_93/hhld.csv"), header=TRUE, sep=",")
 
  # Load hhpertrip
  hhpertrip <- read.csv(file.path(INPUT_DIR, "saltlake_93/hhpertrip.csv"), header=TRUE, sep=",")

  # Load person
  person <- read.csv(file.path(INPUT_DIR, "saltlake_93/person.csv"), header=TRUE, sep=",")

  # Load recruit
  recruit <- read.csv(file.path(INPUT_DIR, "saltlake_93/recruit.csv"), header=TRUE, sep=",")

  # Load trip
  trip <- read.csv(file.path(INPUT_DIR, "saltlake_93/trip.csv"), header=TRUE, sep=",")


  # Use Trip file to identify linkedtrips        
  
  # check trip field 
  #   starttrv (8) Start Travel Time (24hr clock)
  #   endtrav (9) End Travel Time (24hr clock)
  #   origtaz (20) Origin Traffic Analysis Zone
  #   desttaz (21) Destination Traffic Analysis Zone
  #   day (41) Day of Week Survey was Completed 
  #   totalhh (48) Total Number in HH (regardless of age)
  #   destplce (16) Place Name of Destination Activity

    # Add labels for activity 
    levels <- sort(unique(trip$destpurp))
    actlabels <- c("at home activities", "pick-up/drop-off someone at work", "pick-up/drop-off someone at school", 
                   "pick-up/drop-off someone at other", "work", "work-related", "school", "childcare(daycare/after school care)",
                   "shopping", "social/recreation/church/eat out", "banking/personal business", "out of town/leaving the area",
                   "went for a drive", "travels for a living")
    
    trip$origpurp.f <- factor(trip$origpurp, levels=levels, labels=actlabels)
    trip$destpurp.f <- factor(trip$destpurp, levels=levels, labels=actlabels)
  
    # identify whether origin/destination place is home or not
    # origpurp value 1 means at home activities; destpurp value 1 means at home activities
    trip <- trip %>% 
            mutate(orighome = ifelse(origplce=="HOME"|origpurp==1, 1, 0), 
                   desthome = ifelse(destplce=="HOME"|destpurp==1, 1, 0), 
                   starthour=trunc(starttrv/100),
                   startmin=starttrv-starthour*100,
                   endhour=trunc(endtrav/100),
                   endmin=endtrav-endhour*100) %>%
            dplyr::select(id, pernum, tripnum, origplce, destplce, origpurp, origpurp.f, destpurp, 
                          destpurp.f, origtaz, desttaz, orighome, desthome, starthour, startmin, 
                          endhour, endmin, purpose, totalhh, mainmode, income)
    
    
    
    # Identify trip purpose 
    workLabels = c("work", "work-related", "travels for a living")
    othLabels  = c("at home activities", "pick-up/drop-off someone at work", "pick-up/drop-off someone at school",
                   "pick-up/drop-off someone at other", "childcare(daycare/after school care)", 
                   "banking/personal business", "out of town/leaving the area")
    shopLabels = c("shopping")
    recLabels  = c("social/recreation/church/eat out")
    schLabels  = c("school")
    
    # There are 11 trips for  "went for a drive" 
    
    
    trip$TripPurpose=ifelse(trip$destpurp.f %in% workLabels & trip$orighome==1 & trip$desthome!=1,"HBW","NA")
    trip$TripPurpose=ifelse(trip$destpurp.f %in% othLabels  & trip$orighome==1 & trip$desthome!=1,"HBO",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destpurp.f %in% shopLabels & trip$orighome==1 & trip$desthome!=1,"HBShp",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destpurp.f %in% recLabels  & trip$orighome==1 & trip$desthome!=1,"HBRec",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$destpurp.f %in% schLabels  & trip$orighome==1 & trip$desthome!=1,"HBSch",trip$TripPurpose)
    
    trip$TripPurpose=ifelse(trip$desthome==1 & trip$origpurp.f %in% workLabels & trip$orighome!=1,"HBW",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$desthome==1 & trip$origpurp.f %in% othLabels & trip$orighome!=1,"HBO",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$desthome==1 & trip$origpurp.f %in% shopLabels & trip$orighome!=1,"HBShp",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$desthome==1 & trip$origpurp.f %in% recLabels & trip$orighome!=1,"HBRec",trip$TripPurpose)
    trip$TripPurpose=ifelse(trip$desthome==1 & trip$origpurp.f %in% schLabels & trip$orighome!=1,"HBSch",trip$TripPurpose)
   
    
    # Check HBRec trips and orgin trip purpose; The results work well and most correspond to origin purpose 
    
    # idendity household taz  
    htaz.orig <- trip %>% 
            filter(orighome==1) %>%
            group_by(id) %>%
            summarise(htaz=first(origtaz))
    
    htaz <- trip %>% 
      filter(desthome==1) %>%
      group_by(id) %>%
      summarise(htaz=first(desttaz)) %>%
      full_join(htaz.orig)

    trip <- trip %>% 
            left_join(htaz)
    
    
    #reclassify income categories (low income: $0- $20,000; mid income: $20,001 - $50,000; high income: $50,001 or more; NA: refused)
    trip <- trip %>% 
            mutate(inc.level=cut(income,
                   breaks=c(1, 3, 6, 11),
                   labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                   include.lowest=T, right=F)) 
    
    # Assumption: tripdist.miles is o;district.id is 1; HHWGT is 1 
    # Rename colnames to correspond following calculation
    trip <- trip %>% 
                  mutate(tripdur.hours = (endhour+endmin/60) - (starthour+startmin/60),
                  tripdist.miles=0,
                  district.id=1,
                  HHWGT=1) %>%            
                  dplyr::rename(MODE=mainmode,
                                SAMPN=id, 
                                HTAZ=htaz,
                                HHSIZ=totalhh, 
                                INCOME=income,
                                PERNO=pernum,
                                TRIPNO=tripnum)
      
    tcost.trip <- trip  %>%
                  mutate(TripPurpose = tolower(TripPurpose),
                         TripPurpose=ifelse(TripPurpose=="hbshp", "hbs", TripPurpose),
                         TripPurpose=ifelse(TripPurpose=="hbrec", "hbr", TripPurpose)
                         #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
                         #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
                  ) %>%
                  filter(TripPurpose %in% c("hbw", "hbs", "hbr", "hbo")) %>%
                  dplyr::select(SAMPN, PERNO, TRIPNO, HTAZ, INCOME, inc.level, district.id,
                                HHWGT, HHSIZ, MODE, TripPurpose, tripdur.hours, tripdist.miles)
                  
# Source scripts  
    source("code/functions.R")
    source("code/OHAS/compute_tcost.R")
    source("code/OHAS/plot_tcost.R")
    
    
##clean up
  var_list.1 <- ls()
  rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
  rm(var_list.1)  
    