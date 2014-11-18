setwd("/Users/Potenza/Documents/02_Projects/TCI/Workspace/") # on my macbook
setwd("/home/workspace/TCI/SPR357/DestinationChoice/") # on sapporo

library(RPostgreSQL)
library(mlogit)

### load data
conn <- dbConnect(PostgreSQL(), host="sapporo.usp.pdx.edu", user="smartdata", password="Smartaa00", dbname="portland")
activity <- dbReadTable(conn, c("ohas_v2", "activity2")) # the activity file from the 2011 OTAS
#trip <- dbReadTable(conn, c("ohas_v2", "trip")) # the linked-trip file from the 2011 OTAS
person <- dbReadTable(conn, c("ohas_v2", "person")) # the person file from the 2011 OTAS
household <- dbReadTable(conn, c("ohas_v2", "household")) # the household file from the 2011 OTAS
zonalEmpData <- dbReadTable(conn, c("metro", "zonalEmpData"))  # zonal employment data at the TAZ level
tdist <- dbReadTable(conn, c("metro", "mf61")) # TAZ-to-TAZ shortes path trip distance from the 2010 Metro skims

### create linked trips
activity <- with(activity, activity[order(sampn,perno,plano),])
activity$actNo <- with(activity, ave(plano, sampn,perno, FUN=seq))
activity$maxAct <- with(activity, ave(actNo, sampn,perno, FUN=length))
nrow(activity[which(activity$tpurp==7&activity$actNo==1),])
nrow(activity[which(activity$tpurp==7&activity$actNo==activity$maxAct),])
linked <- subset(activity, !(tpurp==7 & actNo!=1 & actNo!=maxAct)) # remove tpurp=7 in the middle of the day
linked$actNo <- with(linked, ave(plano, sampn,perno, FUN=seq))
linked$maxAct <- with(linked, ave(actNo, sampn,perno, FUN=length))
linked$perid <- with(linked, sampn*10 + perno)
linked$tripid <- with(linked, perid*100 + actNo)

### OTAZ
linked$otaz <- c(NA, linked$dtaz[-length(linked$dtaz)])
linked$otaz[linked$actNo==1] <- NA

### indicators to aggregate trip purposes
# last tpurp
linked$last_tpurp <- c(NA, linked$tpurp[-length(linked$tpurp)])
linked$last_tpurp[linked$actNo==1] <- NA
# home-based work
attach(linked)
linked$hbw <- 0
linked$hbw[ ((last_tpurp==1|last_tpurp==2)&tpurp==3) | (last_tpurp==3&(tpurp==1|tpurp==2)) ] <- 1
# home-based shopping
linked$hbs <- 0
linked$hbs[ ((last_tpurp==1|last_tpurp==2)&tpurp%in%c(13,14)) | (last_tpurp%in%c(13,14)&(tpurp==1|tpurp==2)) ] <- 1
# home-based recreation
linked$hbr <- 0
linked$hbr[ ((last_tpurp==1|last_tpurp==2)&tpurp%in%c(20,21)) | (last_tpurp%in%c(20,21)&(tpurp==1|tpurp==2)) ] <- 1
# home-based other, excluding school and college
linked$hbo <- 0
linked$hbo[ ((last_tpurp==1|last_tpurp==2)&(tpurp%in%c(4,6,7,8,9,10,11,12,15,16,17,18,19,22,96,97))) |
              ((last_tpurp%in%c(4,6,7,8,9,10,11,12,15,16,17,18,19,22,96,97))&(tpurp==1|tpurp==2)) ] <- 1
detach(linked)
# note: remove trips with missing values for otaz and dtaz because many activities occurred outside the entire TAZ boundary
hbw <- subset(linked, hbw==1&!is.na(dtaz)&!is.na(otaz))
hbs <- subset(linked, hbs==1&!is.na(dtaz)&!is.na(otaz))
hbr <- subset(linked, hbr==1&!is.na(dtaz)&!is.na(otaz))
hbo <- subset(linked, hbo==1&!is.na(dtaz)&!is.na(otaz))

### generate the TAZ choice set for each trip
#
universalCS <- unique(linked$dtaz)
universalCS <- universalCS[!is.na(universalCS)] # remove NA
numAlts <- 10
#
getAlts <- function(dtaz) {
  allNonchosenAlts <- universalCS[universalCS!=dtaz]
  selectNonchosenAlts <- sample(allNonchosenAlts, (numAlts-1))
  randomCS <- c(dtaz, selectNonchosenAlts)
  randomCS <- sample(randomCS) # to randomly order alternatives
  return(randomCS)
}
#
getLong <- function(x) {
  set.seed(1)
  long <- expand.grid(alt=seq(1,numAlts,1), tripid=x$tripid)
  randomCS <- mapply(getAlts, dtaz=x$dtaz)
  randomCS <- as.vector(randomCS)
  long <- cbind(long,randomCS)
  long <- merge(long, x[c("tripid","dtaz")], by="tripid")
  long$chosen <- with(long, ifelse(randomCS==dtaz,TRUE,FALSE))
  print(sum(long$chosen))
  return(long)
}
#
hbw.long <- getLong(hbw)
hbs.long <- getLong(hbs)
hbr.long <- getLong(hbr)
hbo.long <- getLong(hbo)

### add independent variables
addIVs <- function(x,y) {
  # trip attributes
  x <- merge(x, y, by="tripid")
  x$drivealone <- with(x, ifelse(mode==3,1,0))
  # traveler characteristics
  x <- merge(x, person, by=c("sampn","perno"))
  x <- merge(x, household, by="sampn")
  x$female <- with(x, ifelse(gend==2,1,0))
  x$hsize1 <- with(x, ifelse(hhsiz==1,1,0))
  x$hsize2 <- with(x, ifelse(hhsiz==2,1,0))
  x$hsize3 <- with(x, ifelse(hhsiz==3,1,0))
  x$hsize4 <- with(x, ifelse(hhsiz>=4,1,0))
  x$hinc1 <- with(x, ifelse(income%in%c(1,2),1,0))
  x$hinc2 <- with(x, ifelse(income%in%c(3,4),1,0))
  x$hinc3 <- with(x, ifelse(income%in%c(5,6,7,8),1,0))
  # zonal attributes
  x <- merge(x, zonalEmpData, by.x="randomCS", by.y="TAZ", all.x=TRUE)
  # TAZ-to-TAZ impedance variables
  x$tdist <- tdist$dist[match(interaction(x$otaz,x$randomCS),interaction(tdist$otaz,tdist$dtaz))]
  #
  x <- with(x, x[order(tripid,alt),])
  # remove individual trips with a missing D-TAZ for zonal employment data
  del <- x[which(is.na(x$RetEmp)),"tripid"]
  x <- subset(x, !(x$tripid %in% del))
  return(x)
}
#
hbw.long.IVs <- addIVs(hbw.long, hbw)
hbs.long.IVs <- addIVs(hbs.long, hbs)
hbr.long.IVs <- addIVs(hbr.long, hbr)
hbo.long.IVs <- addIVs(hbo.long, hbo)

### create as mlogit data
hbw.md <- mlogit.data(hbw.long.IVs, shape="long", choice="chosen", id.var="tripid", alt.var="alt")
hbs.md <- mlogit.data(hbs.long.IVs, shape="long", choice="chosen", id.var="tripid", alt.var="alt")
hbr.md <- mlogit.data(hbr.long.IVs, shape="long", choice="chosen", id.var="tripid", alt.var="alt")
hbo.md <- mlogit.data(hbo.long.IVs, shape="long", choice="chosen", id.var="tripid", alt.var="alt")

### estimate destination choice models
#
formula1 <- mFormula(chosen ~ RetEmp + SvcEmp + FinEmp + GvtEmp + ParkAcres + tdist +
                       I(age*tdist) + I(female*tdist) + I(hinc1*tdist) +
                       I(drivealone*tdist) | 0 | 0)
# I(hinc2*tdist) + I(hinc3*tdist)
hbw.dcm <- mlogit(formula1, hbw.md)
hbs.dcm <- mlogit(formula1, hbs.md)
hbr.dcm <- mlogit(formula1, hbr.md)
hbo.dcm <- mlogit(formula1, hbo.md)
#
summary(hbw.dcm)
summary(hbs.dcm)
summary(hbr.dcm)
summary(hbo.dcm)


