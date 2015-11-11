# Part1 load data files 
  # load activity 1 data 
  act1<- read.table(file.path(INPUT_DIR, "act1.txt"), header=FALSE,  sep=",", as.is=TRUE)
  
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
  
  colnames(act1) <- act1.colnames
  
  # Read geocode data 
  geocode <- read.table(file.path(INPUT_DIR, "geocode.raw"), header=FALSE, sep=",")  
  
  colnames(geocode) <- c("UID", "XCORD", "YCORD", "CASE_ID", "FREQ", "RTZ", "SID", "TOTEMP94", "RETEMP94")
  
  
  act1 <- act1 %>%
    mutate(UID = SAMPN * 10^4 + PERNO * 10^3 + DAYNO * 10^2 + ACTNO
    ) %>% 
    left_join(geocode)
  
  # load activity 2 data
  act2 <- read.table(file.path(INPUT_DIR, "act2.txt"), header=FALSE,  sep=",")
  act2.colnames <- c("PHASE", "SAMPN", "PERNO", "DAYNO", "ACTNO", "OLOC", "ADDTYP") 
  names(act2.colnames) <- as.character(c(1:7))
  colnames(act2) <- act2.colnames
  
  # load household data
  hh <- read.table(file.path(INPUT_DIR, "hh.txt"), header=FALSE,  sep=",")
  hh[9, 28] <- 0 #  hh[9, 28] input error
  
  
  hh.colnames <- c("PHASE", "INTWNUMB", "SAMPN", "CITY", "ZIP", "SAMPTYPE", "STRATUM", "HHSIZE",
                   "PHONES", "PARTYLIN", "CARPHONE", "VEHICLES", "OWNHOME", "YRSHOME", "HOMWTYPE",
                   "OLDAREA", "INCOME", "TRAVELD", "TRAVELD2", "DAY1", "DAY2", "DAY1ACT", "DAY2ACT", 
                   "TOTLACT", "DAY1TRIP", "DAY2TRIP", "TOTLTRIP", "HALFMILE")
  
  colnames(hh) <- hh.colnames
  
  # load person data
  per <- read.table(file.path(INPUT_DIR, "per.txt"), header=FALSE,  sep=",")
  per.colnames <- c("PHASE", "SAMPN", "PERNO", "RELATION", "GENDER", "AGE", "RACE", "HOMELANG",
                    "OTHLANG", "SPEAKENG", "LICENSED", "EMPLOYED", "WORKERS", "OCCUPAT", "INDUSTRY",
                    "WORKHOME", "HRSHOME", "SUBPARK", "SHIFTWRK", "PAY2PARK", "COST2PRK", "DRIVE", 
                    "CARPOOL", "TRANSIT", "OTHER", "NOWORK", "YRSWORK", "TWOJOBS", "LASTJOB", "STUDENT", 
                    "HHSTU", "STULEVEL", "SCHOOL", "SCHCITY", "SCHDRIVE", "SCHPOOL", "SCHBUS", "SCHOTHER", 
                    "NOSCHOOL", "HANDICAP", "HANDIOTH")
  
  colnames(per) <- per.colnames  
  
  # load recrited household file 
  rhh <- read.table(file.path(INPUT_DIR, "rhh.txt"), header=FALSE,  sep=",")
  
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
  
  colnames(rhh) <- rhh.colnames
  
  
  # load recruited person file data
  rper <- read.table(file.path(INPUT_DIR, "rper.txt"), header=FALSE,  sep=",")
  rper.colnames <- c("PHASE", "COMP", "SAMPN", "PERNO", "RELATION", "GENDER", "AGE", "RACE", "HOMELANG",
                     "OTHLANG", "SPEAKENG", "LICENSED", "EMPLOYED", "WORKHRS", "OCCUPAT", "INDUSTRY", 
                     "WORKHOME", "HRSHOME", "SUBPARK", "SHIFTWRK", "PAY2PARK", "COST2PPK", "DRIVE", "CARPOOL", 
                     "TRANSIT", "OTHER", "NOWORK", "YRSWORK", "TWOJOBS", "LASTJOB", "STUDENT", "HHSTU", 
                     "STULEVEL", "SCHOOL", "SCHCITY", "SCHDRIVE", "SCHPOOL", "SCHBUS", "SCHOTHER", "NOSCHOOL",
                     "HANDICAP", "HANDIOTH")
  
  colnames(rper) <- rper.colnames
  
  # load vehicle file 
  veh <- read.table(file.path(INPUT_DIR, "veh.txt"), header=FALSE,  sep=",")
  veh.colnames <- c("PHASE", "SAMPN", "VEHNUMBER", "VEHOWNER", "YEAR", "ACQUIRED", "REPLACE", "MAKE", "MODEL", 
                    "CLASS", "TPYE", "FUEL", "BEGOD", "ENDOD", "MILES")
  
  colnames(veh) <- veh.colnames

  # load data
  require(foreign)
  data<- read.dbf(file.path(INPUT_DIR, "DATA94.DBF"), as.is=FALSE)


# Part2 generates linked trips

  # Add label for act1 
  levels = c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 31, 32, 33, 41, 42, 43, 44, 45, 51, 52, 53, 54, 55, 56, 90, 91)
  AGGACT <- c("Meals", "Work", "Work-related", "Shopping (general)", "Shopping (major)", 
              "Personal services", "Medical care", "Professional services", "Household or personal business", 
              "Household maintenance", "Household obligations", "Pick-Up/Drop-Off passengers", "Visiting", 
              "Casual entertaining", "Formal entertaining", "School", "Culture", "Religion/Civil Services", 
              "Civic", "Volunteer work", "Amusements (at-home)", "Amusements (out-of-home)", "Hobbies", 
              "Exercise/Athletics", "Rest and relaxation", "Spectator athletic events", "Incidental trip", "Tag along trip")
  
  act1$ACT1.f <- factor(act1$ACT1, levels=levels, labels=AGGACT)


  # Identify if the location is at home or not
  act1 <- act1 %>%
    mutate(HOME = ifelse(OLOC %in% c("HOME", "RESIDENCE") | as.integer(ACT1) == 51, 1, 0))
  
  # Identify activity TAZ
  # Do not use RTZ from geocode because some RTZ value is zero
  
  act1.xy <- act1 %>% 
             dplyr::select(SAMPN, DAYNO, PERNO, ACTNO, XCORD, YCORD) %>% 
             filter(!is.na(XCORD))
  
  
  TAZPoly1994 <- readShapePoly(TAZPoly1994.shpfile, proj4string=CRS("+init=epsg:2913"))
  
  act1.spdf = SpatialPointsDataFrame(act1.xy[, c('XCORD', 'YCORD')], 
                                     act1.xy, 
                                     proj4string=CRS("+init=epsg:2913"))
  
  act1.xy$TAZ <- over(act1.spdf, TAZPoly1994)[,"TAZ"]
  
  act1 <- act1 %>%
          left_join(act1.xy, by=c("SAMPN", "DAYNO", "PERNO", "ACTNO", "XCORD", "YCORD"))
  
  
  # Identify last location, activity and TAZ
  act1 <- act1%>% 
    arrange(SAMPN, DAYNO, PERNO, ACTNO) %>%
    group_by(SAMPN, DAYNO, PERNO) %>%
    mutate(LastHOME=lag(HOME),
           LastACT1.f=lag(as.character(ACT1.f)),
           LastOLOC=lag(OLOC),
           LastTAZ=lag(TAZ)
    ) %>%
    select(SAMPN, DAYNO, PERNO, ACTNO, MODE, HOME, LastHOME, ACT1, ACT1.f, LastACT1.f, OLOC, LastOLOC, TAZ, LastTAZ,
           TRIPHRS, TRIPMIN, XCORD, YCORD) 
  
  
  # Identify survey day and filter Saturday and Sunday 
  hh.dayno <- hh%>%
    dplyr::select(SAMPN, DAY1, DAY2)%>%
    gather(SURVEYDAY,DAYCODE, DAY1:DAY2)%>%
    mutate(DAYNO=ifelse(SURVEYDAY=="DAY1", 1, 2)) %>%
    filter(DAYCODE!=6 & DAYCODE!=7) %>%
    arrange(SAMPN, DAYNO) %>%
    group_by(SAMPN) %>%
    summarize(DAYNO=first(DAYNO))

  act1.test <- act1 %>%
    inner_join(hh.dayno,by=c("SAMPN", "DAYNO"))
  
  # identify trip purpose
  linkedTrip <- identifyTripPurpose(act1)
  
  # Calculate trip duration (minutes) and assume HHWGT is 1 for all household
  linkedTrip <- linkedTrip%>%
    mutate(TRPDUR=TRIPHRS *60+TRIPMIN,
           HHWGT=1)
  
  # Identify household TAZ
  hhtaz <- linkedTrip %>%
    filter(OLOC=="HOME"| OLOC=="RESIDENCE") %>%
    arrange(SAMPN, OLOC) %>%
    group_by(SAMPN) %>%  
    summarize(HTAZ=first(TAZ))
  
  # LastTAZ of each person's first trip is household TAZ
  linkedTrip <- linkedTrip%>%
    left_join(hhtaz) %>%
    mutate(LastTAZ=ifelse(is.na(LastTAZ)&ACTNO==1, HTAZ, LastTAZ)) %>%
    as.data.frame()
  
  
  # Identify trip route distance for linked trip
  # Source omx functions
  source("code/thirdparty/omx.r")
  
  # Load trip distance matrix
  load(file.path(INPUT_DIR, "emme1994MfNames.RData"))
  omx.file <- file.path(INPUT_DIR,"emme1994.omx")
  tdist <- readMatrixOMX(omx.file, "tdist")
  
  # Identify Route Distance
  linkedTrip$DistanceRoute <- with(linkedTrip, tdist[cbind(LastTAZ, TAZ)] * 5280) 
  linkedTrip <- as.data.frame(linkedTrip)
  
  
# Part3 calculate travel cost 
  
  # prepare data -> a data.frame for linked trips with columns
  # SAMPN, HHWGT,  HTAZ, inc.level, TripPurpose, MODE, tripdur.hours, tripdist.miles
  
  tcost.trip <- linkedTrip %>% 
    select(SAMPN, HHWGT, PERNO, TripPurpose, MODE, TRPDUR, DistanceRoute, HTAZ) %>%
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
  
  # Identify household district id, income level 
  hhdist <- linkedTrip %>%
    filter(OLOC=="HOME"| OLOC=="RESIDENCE") %>%
    arrange(SAMPN, OLOC) %>%
    filter(!is.na(XCORD)) %>%
    group_by(SAMPN) %>%  
    summarize(HXCORD=first(XCORD), HYCORD=first(YCORD)) %>%
    as.data.frame()
  
  hhdist.spdf = SpatialPointsDataFrame(hhdist[, c('HXCORD', 'HYCORD')], 
                                       hhdist, 
                                       proj4string=CRS("+init=epsg:2913"))
  
  
  districtsPoly <- readShapePoly(districtsPoly.shpfile, proj4string=CRS("+init=epsg:2913"))
  hhdist$district.id <- over(hhdist.spdf, districtsPoly)[,"DISTRICT"]

  hh.metro <- hh %>% 
    mutate(inc.level=cut(INCOME,
                         breaks=c(1, 6, 11, 13),
                         labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                         include.lowest=T, right=F)
    ) %>%
    left_join(hhdist, by="SAMPN") %>%
    dplyr::select(SAMPN, inc.level, HHSIZE, INCOME, district.id) %>%
    rename(HHSIZ=HHSIZE) %>%
    as.data.frame() 
  
  
  # Join trip cost and houseld income level, district 
  tcost.trip <- tcost.trip%>%
    left_join(hh.metro) 
  

if(SAVE.INTERMEDIARIES) {
    intm_file = file.path(INTERMEDIATE_DIR, "linkedTrip.RData")
    save(linkedTrip, tcost.trip, file=intm_file)
  }