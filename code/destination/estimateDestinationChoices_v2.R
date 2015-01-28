setwd("/Users/Potenza/Documents/02_Projects/TCI/workspace/") # on my macbook
setwd("/home/workspace/TCI/SPR357/DestinationChoice/") # on sapporo

library(RPostgreSQL)
library(mlogit)

### load data
conn <- dbConnect(PostgreSQL(), host="sapporo.usp.pdx.edu", user="smartdata", password="Smartaa00", dbname="portland")
linkedTrip <- dbReadTable(conn, c("ohas_v2", "trip")) # linked-trip file from the 2011 OTAS
person <- dbReadTable(conn, c("ohas_v2", "person")) # person file from the 2011 OTAS
household <- dbReadTable(conn, c("ohas_v2", "household")) # household file from the 2011 OTAS
zonalData <- dbReadTable(conn, c("lehd", "taz2161"))  # employment data at the zonal level
zonalVars <- c("newtaz","f_area","retemp","svcemp","finemp","totemp")
zonalData <- zonalData[, zonalVars] # 71 TAZs do not have infomation on employment, why?
tdist <- dbReadTable(conn, c("metroskims2010", "mf61")) # TAZ-to-TAZ shortes path trip distance from the 2010 Metro skims

### segment by trip purpose
# home-to-work trips
h2w <- subset(linkedTrip, (lastaggact=="Home"|lastaggact=="WorkAtHome")&tpurp==3)
h2w <- subset(h2w, !is.na(dtaz)) # need to figure out why there are 87 missing dtazs

### generate the TAZ choice set for each trip
#
numAlts <- 10
set.seed(1)
#
getAlts <- function(x, y) {
  # x: a destination TAZ for the trip
  # y: number of alternatives
  universalChoiceSet <- unique(linkedTrip$dtaz)
  allNonchosenAlts <- universalChoiceSet[universalChoiceSet!=x]
  selectNonchosenAlts <- sample(allNonchosenAlts, (y-1))
  choiceSet <- c(x, selectNonchosenAlts)
  choiceSet <- sample(choiceSet) # to randomly order alternatives
  return(choiceSet)
}
#
alts <- mapply(getAlts, x=h2w$dtaz, y=numAlts)
alts <- as.data.frame(t(alts))
colnames(alts) <- c(paste("TAZ", 1:numAlts, sep="."))

### add trip attributes
t1 <- cbind(h2w[, c("sampn","perno","plano","tpurp","mode","arr_hr","dep_hr","otaz","dtaz")], alts)
# check
nrow(t1[which(t1$dtaz!=t1$TAZ.1),])
# travel mode aggregation
t1$auto <- with(t1, ifelse(mode %in% c(3,4), 1, 0))
# time-of-day aggregation
t1$am3 <- with(t1, ifelse(arr_hr %in% c(6,7,8), 1, 0))

### add socioeconomic characteristics
t2 <- t1
t2 <- merge(t2, person, by=c("sampn","perno"))
t2 <- merge(t2, household, by="sampn")

### add zonal built environment data for each alternative of each trip
t3 <- t2
for (i in 1:numAlts) {
  t3$TAZ <- t3[, paste("TAZ", i, sep=".")]
  zonalData0 <- zonalData
  colnames(zonalData0)[2:ncol(zonalData0)] <- paste(colnames(zonalData0)[2:ncol(zonalData0)], i, sep=".")
  t3 <- merge(t3, zonalData0, by.x="TAZ", by.y="newtaz")
  t3$TAZ <- NULL
}
t3 <- with(t3, t3[order(sampn,perno,plano),])

### add TAZ-to-TAZ impedance data
t4 <- t3
syst1 <- Sys.time()
for (i in 1:numAlts) {
  t4[, paste("dist",i,sep=".")] <- tdist$dist[match(interaction(t4$otaz,t4[,paste("TAZ",i,sep=".")]),
                                                    interaction(tdist$otaz,tdist$dtaz))]
}
syst2 <- Sys.time()
syst2 - syst1 # Time difference of 2.04545 mins

### define the dependent variable
t5 <- t4
t5$chosen <- 0
for (i in 1:numAlts) {
  t5$TAZ <- t5[, paste("TAZ", i, sep=".")]
  t5$chosen <- with(t5, ifelse(dtaz==TAZ, i, chosen))
  t5$TAZ <- NULL
}
t5$chosen <- factor(t5$chosen, levels=c(1:numAlts))

### create as mlogit data
destination <- mlogit.data(t5, choice="chosen", shape="wide", varying=c(198:(ncol(t5)-1)), sep=".")
head(index(destination),11)

### estimate destination choice models
# null model
-length(unique(destination$chid))*log(length(unique(destination$alt)))
# add TAZ-to-TAZ impedance variables (alternative-specific)
f1 <- mFormula(chosen ~ dist | 0 | 0)
m1 <- mlogit(f1, data=destination)
summary(m1)
# add zonal built environment variables (alternative-specific)
f2 <- update(f1, ~ . + f_area | . | .)
m2 <- mlogit(f2, data=destination)
summary(m2)
# add socioeconomic characterisctis (individual-specific)
f3 <- update(f1, ~ . | . + gend | .)
m3 <- mlogit(f3, data=destination)
summary(m3)
# add trip attributes (individual-specific)
f4 <- update(f3, ~ . | . + auto | .)
m4 <- mlogit(f4, data=destination)
summary(m4)


