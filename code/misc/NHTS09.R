# This script uses NHTS2009 to calculate trip cost for MSAs

# Load required packages
  require(dplyr)

# Settings
  # Set workspace
  setwd("~/tci")
  var_list.0 <- ls()
  INPUT_DIR <- 'data/'
  
  # Define unit costs and data source for generating output and intermediate directory and unit costs data frame 
 
  data.source <- "NHTS09"
  unit.name <- "minutes" 
  #unit.name <- "dollars"
  source("code/settings.R")
  unitcosts <- NHTS09.unitcosts.list[[unit.name]]
   
  # Survey area
  # HHC_MSA: CMSA FIPS code for HH address
  # 6442 = Portland--Salem, OR--WA; 1637 trips
  # 8280 = Tampa--St. Petersburg--Clearwater, FL
  # 7160 = Salt Lake City--Ogden, UT
  
  # Calculate three cities 
  # HHC_MSAs <- c(Portland=6442, TampaBay=8280, SaltLakeCity=7160)  
  

  HHC_MSAs <- c("Atlanta, GA"= 0520, "Austin--San Marcos, TX" = 0640, "Boston--Worcester--Lawrence, MA--NH--ME--CT"= 1122,
      "Buffalo--Niagara Falls, NY"=1280, "Charlotte--Gastonia--Rock Hill, NC--SC" = 1520, 
      "Chicago--Gary--Kenosha, IL--IN--WI"= 1602, "Cincinnati--Hamilton, OH--KY--IN"=1642, "Cleveland--Akron, OH"=1692, 
      "Columbus, OH"=1840, "Dallas--Fort Worth, TX"=1922, "Denver--Boulder--Greeley, CO"=2082, 
      "Detroit--Ann Arbor--Flint, MI"=2162, "Grand Rapids--Muskegon--Holland, MI"=3000, 
      "Greensboro--Winston-Salem--High Point, NC"=3120,  "Hartford, CT"= 3280, "Houston--Galveston--Brazoria, TX"=3362, 
      "Indianapolis, IN"=3480, "Jacksonville, FL" = 3600, "Kansas City, MO--KS"=3760, "Las Vegas, NV--AZ" = 4120, 
      "Los Angeles--Riverside--Orange County, CA"=4472, "Louisville, KY--IN"=4520, "Memphis, TN--AR--MS"=4920, 
      "Miami--Fort Lauderdale, FL"=4992, "Milwaukee--Racine, WI"=5082, "Minneapolis--St. Paul, MN--WI"=5120, 
      "Nashville, TN"=5360, "New Orleans, LA"=5560, "New York--Northern New Jersey--Long Island,NY--NJ--CT--PA"=5602, 
      "Norfolk--Virginia Beach--Newport News, VA--NC" = 5720, "Oklahoma City, OK"=5880, "Orlando, FL"=5960,
      "Philadelphia--Wilmington--Atlantic City,PA--NJ--DE--MD"= 6162, "Phoenix--Mesa, AZ"=6200, "Pittsburgh, PA"=6280, 
      "Portland--Salem, OR--WA"=6442, "Providence--Fall River--Warwick, RI--MA"=6480, "Raleigh--Durham--Chapel Hill, NC"=6640, 
      "Rochester, NY" = 6840, "Sacramento--Yolo, CA"=6922, "St. Louis, MO--IL"=7040, 
      "Salt Lake City--Ogden, UT"=7160, "San Antonio, TX"= 7240, "San Diego, CA"= 7320, 
      "San Francisco--Oakland--San Jose, CA"=7362, "Seattle--Tacoma--Bremerton, WA"= 7602,
      "8280 = Tampa--St. Petersburg--Clearwater, FL" = 8280, 
      "Washington--Baltimore, DC--MD--VA--WV"=8872, "West Palm Beach--Boca Raton, FL"=8960
    )  
  
  
  
#   # make names for household income groups, trip purpose and calculation method
#   IcNames <- c("Low Income", "Mid Income", "High Income")
#   Ic <- c("lowInc", "midInc", "highInc")
#   names(IcNames) <- Ic
#   
#   PrNames <- c("Work", "Shopping", "Recreation", "Other")
#   Pr <- c("hbw", "hbs", "hbr", "hbo")
#   names(PrNames) <- Pr
#   
#   CmNames <- c("mintcost", "avgtcost", "maxtcost")
#   Cm <- c("min", "avg", "max")
#   names(CmNames) <- Cm

  
# Road and process data 
  # Load national household survey data 
  day <- read.csv("data/NHTS/DAYV2PUB.CSV", header=TRUE, sep=",") 
  
  # Select survey areas 
  day <- day %>% 
    filter(HHC_MSA %in% HHC_MSAs) %>%
    # Filter unkown trip distance rows  
    # TRPMILES Calculated Trip distance converted into miles: -9 = Not ascertained; -8 = Don't know; -7 = Refused; -1 = Appropriate skip; 0-9000
    filter(TRPMILES >= 0) %>%
    # Filter unkown trip duration rows: -9 = Not ascertained; 0-1439
    # TRVLCMIN (Calculated travel time) is calculacted based on ENDTIME (Trip END time in military) and STRTTIME (Trip START time in military)
    filter(TRVLCMIN >= 0) %>%
    # Filter surveyday 
    # TDWKND TD trip was on weekend: 01 = Yes; 02 = No
    filter(TDWKND==2)

  # reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
  # HHFAMINC: Total household income
  day <- day %>% 
    mutate(inc.level=cut(HHFAMINC,
                         breaks=c(1, 6, 11, 18),
                         labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                         include.lowest=T, right=F)) 
  
  # Reformat data for TCI survey-based approach
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
                      TripPurpose=TRIPPURP)%>%
        dplyr::select(HHC_MSA, SAMPN, PERNO, TRIPNO, INCOME, inc.level, district.id,
                    HHSIZ, HHWGT, HTAZ, MODE, TripPurpose, tripdur.hours, tripdist.miles)

  # Filter home-based trips
  # TRIPPURP field identifies home-based purpose types
  day <- day  %>%
      mutate(TripPurpose = tolower(TripPurpose),
             TripPurpose=ifelse(TripPurpose=="hbshop", "hbs", TripPurpose),
             TripPurpose=ifelse(TripPurpose=="hbsocrec", "hbr", TripPurpose)
             #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
             #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
      ) %>%
      filter(TripPurpose %in% c("hbw", "hbs", "hbr", "hbo"))
    
    
# # Define unist cost 
#     # this configuration converts travel costs to $
#     hourly.wage <- 60
#     # hourly.wage <- 24.77
#     MODE <-sort(unique(day$MODE)) 
#     
#     MdNames <- c("Don't know", "Refused", "Appropriate skip", "Car", "Van", "SUV", "Pickup truck", "Other truck", "Motorcycle", "Light electric veh (golf cart)", 
#                  "Local public bus", "Commuter bus", "School bus", "Charter/tour bus", "Shuttle bus", "Taxicab", "Bicycle", "Walk", "Other")
#     
#     names(MODE) <- MdNames
#     
#      VOT <- c(NA, NA, NA, rep(1, (length(MODE) -3))) * hourly.wage
#     # VOT <- c(NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5) * hourly.wage
#     
#     mcpm <- c(NA, NA, NA, 59.2, 59.2, 59.2, 59.2, 59.2, 29.6, 29.6, 101.0, 101.0, 0, 101.0, 0, 260.0, 0, 0, 29.6)/(100 * 24.77)
#     # mcpm <- c(NA, NA, NA, 59.2, 59.2, 59.2, 59.2, 59.2, 29.6, 29.6, 101.0, 101.0, 0, 101.0, 0, 260.0, 0, 0, 29.6)/100
#     
#     length(VOT)
#     length(mcpm)
#     # rep(1, length(MODE))
#     unitcosts <- data.frame(MODE, VOT, mcpm)

# Calculate and plot trip cost for Portland and Tampa Bay

    for (msa.id in HHC_MSAs) {
      
      # Set output directory 
      OUTPUT_DIR <- file.path("output/Survey/NHTS09", names(HHC_MSAs)[HHC_MSAs==msa.id], unit.name)
      dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
      
      # Select survery area
      tcost.trip <- day %>% 
                    filter(HHC_MSA==msa.id)
      
      if (nrow(tcost.trip) > 0) {
        # Source scripts
        source("code/OHAS/functions.R")
        source("code/OHAS/compute_tcost.R")
        source("code/OHAS/plot_tcost.R")
      }
    } 
  
##clean up
  var_list.1 <- ls()
  rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
  rm(var_list.1) 
  