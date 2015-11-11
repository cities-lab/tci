# This script uses NHTS2009 to calculate trip cost for MSAs

# Load required packages
  require(dplyr)
  require(ggplot2)
  
# Settings
  # Set workspace
  setwd("~/tci")
  var_list.0 <- ls()
  
  method.name <- 'survey'
  project.name <- 'NHTS'
  year <- '2009'

## settings
  source("code/settings.R")

  # Define unist cost for NHTS 2009 survey data 
  # mode
  MODE <- c(-8, -7, -1, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 14, 19, 22, 23, 97)
  MdNames <- c("Don't know", "Refused", "Appropriate skip", "Car", "Van", "SUV", "Pickup truck", "Other truck", "Motorcycle", "Light electric veh (golf cart)", 
               "Local public bus", "Commuter bus", "School bus", "Charter/tour bus", "Shuttle bus", "Taxicab", "Bicycle", "Walk", "Other")
  names(MODE) <- MdNames
  
  constant <- rep(0, length(MODE))
  
  # unit costs by minutes
  VOT <- c(NA, NA, NA, rep(1, (length(MODE) -3))) * minutes.per.hour
  mcpm <- c(NA, NA, NA, 59.2, 59.2, 59.2, 59.2, 59.2, 29.6, 29.6, 101.0, 101.0, 0, 101.0, 0, 260.0, 0, 0, 29.6) * minutes.per.cent
  unitcosts.minutes <- data.frame(MODE, constant, VOT, mcpm, unit.name = "minutes")
  
  # unit costs by dollars 
  hourly.wage <- 24.77
  VOT <- c(NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5) * hourly.wage
  mcpm <- c(NA, NA, NA, 59.2, 59.2, 59.2, 59.2, 59.2, 29.6, 29.6, 101.0, 101.0, 0, 101.0, 0, 260.0, 0, 0, 29.6) / cents.per.dollar
  
  unitcosts.dollars <- data.frame(MODE, constant, VOT, mcpm, unit.name = "dollars")
  
  unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)  
  unitcosts <- unitcosts.list[[unit.name]]
  
## path settings (using default settings in code/settings.R)
  #INPUT_DIR <- 'data/NHTS09'
  
  # Define unit costs and data source for generating output and intermediate directory and unit costs data frame 
  #OUTPUT_DIR <- file.path("output/NHTS09", unit.name)
  #dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)

## load and prepare data   
  # Survey area
  # HHC_MSA: CMSA FIPS code for HH address
  # 6442 = Portland--Salem, OR--WA; 1637 trips
  # 8280 = Tampa--St. Petersburg--Clearwater, FL
  # 7160 = Salt Lake City--Ogden, UT
  
  # Calculate three cities 
  # HHC_MSAs <- c(Portland=6442, TampaBay=8280, SaltLakeCity=7160)  

  msa.names <- read.table(file.path(INPUT_DIR, "MSA_names.tab"), header=T, sep="\t", stringsAsFactors = F, colClasses="character")
  HHC_MSAs <- msa.names$FIPS
  
  # Load national household survey data 
  day <- read.csv(file.path(INPUT_DIR, "DAYV2PUB.CSV"), header=TRUE, sep=",", stringsAsFactors = F) 
  
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
                         labels=c("lowInc", "midInc", "highInc"),
                         include.lowest=T, right=F)) 
  
  # Reformat data for TCI survey-based approach
  # Hack district.id to do what we want: HHC_MSA; 
  # HTAZ is 1
  # since HHWGT is missing, equal weight of 1 is used;  
  day <- day %>%
        mutate(tripdur.hours=TRVLCMIN/60,
               district.id=HHC_MSA,
               HTAZ=1,
               HHWGT=1) %>%
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
  tcost.trip <- day  %>%
      mutate(TripPurpose = tolower(TripPurpose),
             TripPurpose=ifelse(TripPurpose=="hbshop", "hbs", TripPurpose),
             TripPurpose=ifelse(TripPurpose=="hbsocrec", "hbr", TripPurpose)
             #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
             #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
      ) %>%
      filter(TripPurpose %in% c("hbw", "hbs", "hbr", "hbo"))
    
## Calculate and plot trip cost
  source("code/functions.R")
  source("code/survey/compute.R")
  #source("code/survey/plot.R")  

## post-process and plot    
  summary.table <- function(tcost.df) {
    tcost.df %>%
      group_by(short.name) %>%
      summarise(tcost.25percentile = quantile(tcost, 0.25), 
                tcost.mean=mean(tcost),
                tcost.median=median(tcost),
                tcost.75percentile = quantile(tcost, 0.75)) %>%
      as.data.frame()
  }
  
  tcost.hh <- tcost.hh  %>% 
    dplyr::rename(FIPS=district.id) %>%
    left_join(msa.names)
  
  # Generate travel cost summary table
  tcost.allhh.summary <- summary.table(tcost.hh)
  tcost.allhh.summary$unit.name <- unit.name
  
  output_file <- file.path(OUTPUT_DIR, "tcost_allhh_summary.csv")
  write.csv(tcost.allhh.summary, file=output_file, row.names=FALSE)
  
  tcost.lowInchh.summary <- summary.table(tcost.hh %>% filter(inc.level=="lowInc"))
  tcost.lowInchh.summary$unit.name <- unit.name
  
  output_file <- file.path(OUTPUT_DIR, "tcost_lowInchh_summary.csv")
  write.csv(tcost.lowInchh.summary, file=output_file, row.names=FALSE)
  
  do_boxplot <- function(tcost.df, unit.name, ymax=200) {
    ggplot(tcost.df, aes(x=reorder(short.name, tcost, FUN=median, na.rm=T), y=tcost)) +
    geom_boxplot() + labs(y=paste0("Generalized Travel Costs (", unit.name, ")")) + 
    xlab("MSAs") + 
    ylim(0, 200) + 
    coord_flip() +
    ggtitle("Total travel cost by MSA") +
    theme(plot.title = element_text(face="bold", size=12, vjust=1), legend.position="none") 
  }
  
  bplot <- do_boxplot(tcost.hh, unit.name)
  output_file <- file.path(OUTPUT_DIR, "boxp.allhh.png")
  ggsave(bplot, file= output_file)
  
  bplot <- do_boxplot(tcost.hh %>% filter(inc.level=="lowInc"), unit.name)
  output_file <- file.path(OUTPUT_DIR, "boxp.lowInchh.png")
  ggsave(bplot, file= output_file)
  
##clean up
  if (CLEAN.UP) clean.up(var_list.0)
