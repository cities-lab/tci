# set the workplace
  getwd()

# load activity 1 data 
  act1<- read.table("data/portland_94/act1.txt", header=FALSE,  sep=",")
  str(act1)
  act1[1:10,]
  # change column names
  # colname is from http://www.surveyarchive.org/sda/Portland/doc/hcbk.htm
  
  act1.colnames <- c("PHASE", "STRATUM", "SAMPNO", "PERSNO", "DAYNO", "ACTNO", "ACT1", "OLOC", "THERE",
                         "ACTBEG", "AMPMS", "ACTEND", "AMPME", "TRPSTHEN", "MODE", "OMODE", "VEHAVAIL", 
                         "PAY4CAR", "HOWMUCH", "TIME1", "NOPARTY", "VEHPARTY", "PARTYPAY", "HOWMPP", 
                         "TIME2", "ROUTE1", "BOARDWHR", "ACCESSSTOP", "EGRESTOP", "PAYHOW", "SUBSIDWHO1", 
                         "TRANSFER", "TRANSWHE", "TRANSAG", "HOWMANY", "VEHWHICH", "DRIVER", "NUMINVEH",
                         "PRKWHERE", "PRKPAY", "PRKFARE", "TIME3", "SUBSIDWHO2", "USUBSCOST", "TIME4",
                         "TRIPBEG", "AMPMTB", "TRIPEND", "AMPMTE", "TRIPHRS", "TRIPMIN", "MODCHG", "CHGWHERE", 
                         "CHG2FRM", "HALFMILE")

  names(act1.colnames) <- as.character(c(1:55))

  colnames(act1) <- act1.colnames
  table(act1[,6])

# load activity 2 data
  act2 <- read.table("data/portland_94/act2.txt", header=FALSE,  sep=",")
  str(act2)
  act2[1:10,]

  # code book of act2 do not provide informatin for v6 (sixth column)
  act2.colnames <- c("PHASE", "SAMPNO", "PERSNO", "DAYNO", "ACTNO", "V6", "ADDTYP") 
  table(act2[, 5])  
  names(act2.colnames) <- as.character(c(1:7))
  
  colnames(act2) <- act2.colnames

# load household data
  hh <- read.table("data/portland_94/hh.txt", header=FALSE,  sep=",")
  str(hh)
  hh[1:10, 28]


  #  hh[9, 28] <- "0/Users/lmwang/Dropbox/OHAS/data/portland_94/geocode.raw" ??

  hh[9, 28]
  table(hh[,28])
  hh[9, 28] <- 0
 
  
  hh.colnames <- c("PHASE", "INTWNUMB", "SAMPNO", "CITY", "ZIP", "SAMPTYPE", "STRATUM", "HHSIZE",
                   "PHONES", "PARTYLIN", "CARPHONE", "VEHICLES", "OWNHOME", "YRSHOME", "HOMWTYPE",
                   "OLDAREA", "INCOME", "TRAVELD", "TRAVELD2", "DAY1", "DAY2", "DAY1ACT", "DAY2ACT", 
                   "TOTLACT", "DAY1TRIP", "DAY2TRIP", "TOTLTRIP", "HALFMILE")
  names(hh.colnames) <- as.character(c(1:28))

  colnames(hh) <- hh.colnames

# load person data
  per <- read.table("data/portland_94/per.txt", header=FALSE,  sep=",")
  str(per)

  per.colnames <- c("PHASE", "SAMPNO", "PERSNO", "RELATION", "GENDER", "AGE", "RACE", "HOMELANG",
                    "OTHLANG", "SPEAKENG", "LICENSED", "EMPLOYED", "WORKERS", "OCCUPAT", "INDUSTRY",
                    "WORKHOME", "HRSHOME", "SUBPARK", "SHIFTWRK", "PAY2PARK", "COST2PRK", "DRIVE", 
                    "CARPOOL", "TRANSIT", "OTHER", "NOWORK", "YRSWORK", "TWOJOBS", "LASTJOB", "STUDENT", 
                    "HHSTU", "STULEVEL", "SCHOOL", "SCHCITY", "SCHDRIVE", "SCHPOOL", "SCHBUS", "SCHOTHER", 
                    "NOSCHOOL", "HANDICAP", "HANDIOTH")
  length(per.colnames)

  names(per.colnames) <- as.character(c(1:41))
  
  colnames(per) <- per.colnames  

# load recrited household file 
  rhh <- read.table("data/portland_94/rhh.txt", header=FALSE,  sep=",")
  str(rhh)
  rhh[1:10,]
  # is second column "comp" ?? only two values, while there are 16 values in codebook for "COMP"
  table(rhh[ , 2])
  # rhh does not include STATE column??
  # in th codebook, STATE column is between forth column and fifth column
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
  table(rhh[,18])

# load recruited person file data
  rper <- read.table("data/portland_94/rper.txt", header=FALSE,  sep=",")
  str(rper)
  table(rper[,2])

  rper.colnames <- c("PHASE", "COMP", "SAMPNO", "PERSNO", "RELATION", "GENDER", "AGE", "RACE", "HOMELANG",
                     "OTHLANG", "SPEAKENG", "LICENSED", "EMPLOYED", "WORKHRS", "OCCUPAT", "INDUSTRY", 
                     "WORKHOME", "HRSHOME", "SUBPARK", "SHIFTWRK", "PAY2PARK", "COST2PPK", "DRIVE", "CARPOOL", 
                     "TRANSIT", "OTHER", "NOWORK", "YRSWORK", "TWOJOBS", "LASTJOB", "STUDENT", "HHSTU", 
                     "STULEVEL", "SCHOOL", "SCHCITY", "SCHDRIVE", "SCHPOOL", "SCHBUS", "SCHOTHER", "NOSCHOOL",
                     "HANDICAP", "HANDIOTH")
  length(rper.colnames)
  names(rper.colnames) <- as.character(c(1:42))
  colnames(rper) <- rper.colnames

# load vehicle file 
  veh <- read.table("data/portland_94/veh.txt", header=FALSE,  sep=",")
  str(veh)

  veh.colnames <- c("PHASE", "SAMPNO", "VEHNUMBER", "VEHOWNER", "YEAR", "ACQUIRED", "REPLACE", "MAKE", "MODEL", 
                    "CLASS", "TPYE", "FUEL", "BEGOD", "ENDOD", "MILES")
  length(veh.colnames)
  names(veh.colnames) <- as.character(c(1:15))

  colnames(veh) <- veh.colnames




# load data
  require(foreign)
  data<- read.dbf("data/portland_94/DATA94.DBF", as.is=FALSE)
  str(data)

# save all data 
  save(act1, act2, hh, per, rhh, rper, veh, file="data/portland_94.RData")
