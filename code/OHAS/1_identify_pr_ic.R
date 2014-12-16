# This scripts identify linked trips, not place-based trips, by income groups and trip purposes

### Load data from the PostgreSQL database
#* the household file of the 2011 OTAS

#* hba (home-based all); all home-based trips
#* hbw (home-based work); work, not including all other activities at work and work related activities
#* hbs (home-based shopping); both routine and special shopping
#* hbr (home-based recreation); both outdoor and indoor recreation
#* hbo (home-based other); other home-based trips

# Load required packages
library(RPostgreSQL)
library(pscl)
library(memisc)
library(dplyr)

## read activity table 
conn <- dbConnect(PostgreSQL(), host="sapporo.usp.pdx.edu", user="smartdata", password="Smartaa00", dbname="portland")
act <- dbReadTable(conn, c("ohas_v2", "activity")) # activity file from the 2011 OTAS
trip <- dbReadTable(conn, c("ohas_v2", "trip")) # trip file from the 2011 OTAS
hh <- dbReadTable(conn, c("ohas_v2", "household")) # household file from the 2011 OTAS

act <- arrange(act,sampn,perno,plano) # sort by sampn,perno, plano

##  get sequence of trips and get number of trips for each person 
act <- mutate(act,
              actNo = ave(plano, sampn,perno, FUN=seq),
              maxAct = ave(actNo, sampn,perno, FUN=length))

# create a new data set without the Change-of-Mode activities in the middle of the day to use only linked trips
linked <- filter(act, !(tpurp==7 & actNo!=1 & actNo!=maxAct))
linked <- arrange(linked, sampn,perno,plano)

save(linked, file="data\\OHASTTime\\RData\\linked.RData")
# update actNo and maxAct
linked <- mutate(linked, 
                 actNo = ave(plano, sampn,perno, FUN=seq),
                 maxAct = ave(actNo, sampn,perno, FUN=length))

# update lastaggact 
linked$lastaggact <- c(NA, linked$thisaggact[-length(linked$thisaggact)])
linked$lastaggact[linked$actNo==1] <- NA


# update last_tpurp  
linked$last_tpurp <- c(NA, linked$tpurp[-length(linked$tpurp)])
linked$last_tpurp[linked$actNo==1] <- NA


# identify home-based trips
linked <- mutate(linked, 
                 h2o = ifelse(actNo!=1&(lastaggact=='Home'|lastaggact=='WorkAtHome')&(thisaggact!='Home'&thisaggact!='WorkAtHome'), 1, 0),
                 o2h = ifelse(actNo!=1&(lastaggact!='Home'&lastaggact!='WorkAtHome')&(thisaggact=='Home'|thisaggact=='WorkAtHome'), 1, 0),
                 hba = ifelse(h2o==1|o2h==1, 1, 0))


# identify trip purposes 
linked <- mutate(linked, 
                 hbw = ifelse((h2o==1&tpurp==3)|(o2h==1&last_tpurp==3), 1, 0),
                 hbs = ifelse((h2o==1&(tpurp==13|tpurp==14))|(o2h==1&(last_tpurp==13|last_tpurp==14)), 1, 0),
                 hbr = ifelse((h2o==1&(tpurp==20|tpurp==21))|(o2h==1&(last_tpurp==20|last_tpurp==21)), 1, 0),
                 hbo = ifelse((h2o==1&(tpurp!=3&tpurp!=5&tpurp!=6&tpurp!=13&tpurp!=14&tpurp!=20&tpurp!=21))
                              |(o2h==1&(last_tpurp!=3&tpurp!=5&tpurp!=6&last_tpurp!=13&last_tpurp!=14&last_tpurp!=20&last_tpurp!=21)), 1, 0))

# subset linked data.frame to to calculate travel time and travel cost 
linkedsubset <- select(linked,sampn,perno,plano,trpdur,mode,actNo,maxAct,hba,hbw,hbs,hbr,hbo)


# add htaz to table 
hhsubset <- select(hh, sampn,income, htaz)
hhsubset <- arrange(hhsubset, sampn)
linkedsubset <- merge(linkedsubset, hhsubset, by="sampn", all.x=TRUE)

# reclassify income categories 
income <- c(1:8,99)
newincome <- c(1,1,2,2,3,3,3,3,NA)
incomenewcate  <- data.frame(income, newincome)
linkedsubset <- merge(linkedsubset, incomenewcate, by="income", all.x=TRUE)

save(linkedsubset, file="data/OHASTTime/RData/linkedsubset.RData")


