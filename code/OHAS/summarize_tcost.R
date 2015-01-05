# This scripts identify linked trips, not place-based trips, by income groups and trip purposes

# Load required packages
library(dplyr)
library(tidyr)

## read OHAS activity, household, trip table
load(file.path(INPUT_DIR, "OHAS_PDX.RData"))

activity <- activity %>%
  dplyr::select(sampn, perno, plano, thisaggact, tpurp, mode, trpdur) %>%
  arrange(sampn, perno, plano) %>%
  group_by(sampn, perno) %>%
  mutate(maxAct=n(), actNo=1:n())

linked <- activity %>%
  # remove the Change-Of-Mode trips (tpurp==7) in the middle of the day and
  # the first and last 'trip'
  filter(tpurp!=7 & actNo!=1 & actNo != maxAct) %>%
  arrange(sampn, perno, plano) %>%
  group_by(sampn, perno) %>%
  mutate(actNo=1:n(), maxAct=n(),
         lastaggact=lag(thisaggact),
         last_tpurp=lag(tpurp) # last tpurp
  )

# identify home-based trips
#* hba (home-based all); all home-based trips
#* hbw (home-based work); work, not including all other activities at work and work related activities
#* hbs (home-based shopping); both routine and special shopping
#* hbr (home-based recreation); both outdoor and indoor recreation
#* hbo (home-based other); other home-based trips
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

# use tidyr to create a character trip purpose column tpurp.ch
linked <- linked %>% gather(tpurp.ch, flag, hbw:hbo) %>% filter(flag==1) %>% dplyr::select(-flag)

# reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
household$inc.level = cut(household$income,
                          breaks=c(1, 3, 5, 9),
                          labels=c("lowInc", "midInc", "highInc"),
                          include.lowest=T, right=F)
#low <- (1,2); median <- (3,4); high <- 5:8
hh <- dplyr::select(household, sampn, inc.level, htaz)

linked <- left_join(linked, hh, by="sampn")

if(SAVE.INTERMEDIARIES) {
  intm_file = file.path(INTERMEDIATE_DIR, "linked_trips.RData")
  save(linked, file=intm_file)
}

linked <- left_join(linked, VOT.by.mode, by="mode")
linked <- mutate(linked, trip.tcost=VOT*trpdur/60)

# summarize trip-level travel time cost by taz, trip purpose, and income level
tcost.htaz.tpurp.inc <- linked %>%
  group_by(htaz, tpurp.ch, inc.level) %>%
  summarise(n = n(),
            tcost.min=min(trip.tcost, na.rm=T),
            tcost.avg=mean(trip.tcost, na.rm=T),
            tcost.max=max(trip.tcost, na.rm=T),
            tcost.sd=sd(trip.tcost, na.rm=T)
  )
print(tcost.htaz.tpurp.inc)

# calculate household-level travel time cost
tcost.hh <- linked %>%
  group_by(sampn) %>%
  summarise(tcost=sum(trip.tcost))

# summarize household-level travel time cost by taz and/or income level
tcost.hh <- left_join(tcost.hh, hh, by="sampn")
tcost.htaz.inc <- tcost.hh %>%
  group_by(htaz, inc.level) %>%
  summarise(n = n(),
            tcost.min=min(tcost, na.rm=T),
            tcost.avg=mean(tcost, na.rm=T),
            tcost.max=max(tcost, na.rm=T),
            tcost.sd=sd(tcost, na.rm=T)
  )
print(tcost.htaz.inc)

tcost.htaz <- tcost.hh %>%
  group_by(htaz) %>%
  summarise(n = n(),
            tcost.min=min(tcost, na.rm=T),
            tcost.avg=mean(tcost, na.rm=T),
            tcost.max=max(tcost, na.rm=T),
            tcost.sd=sd(tcost, na.rm=T)
  )
print(tcost.htaz)

# summarize household-level travel time cost by taz and/or income level
load(file.path(INPUT_DIR, "districts.RData"))
tcost.hh <- left_join(tcost.hh, districts, by=c("htaz"="zone"))
tcost.distr <- tcost.hh %>%
  dplyr::rename(district.id=ugb) %>%
  group_by(district.id) %>%
  summarise(n = n(),
            tcost.min=min(tcost, na.rm=T),
            tcost.avg=mean(tcost, na.rm=T),
            tcost.max=max(tcost, na.rm=T),
            tcost.sd=sd(tcost, na.rm=T)
  )
print(tcost.distr)

# summarize overall household-level travel time cost
tcost.all <- tcost.hh %>%
  mutate(all=1) %>%
  group_by(all) %>%
  summarise(n = n(),
            tcost.min=min(tcost, na.rm=T),
            tcost.avg=mean(tcost, na.rm=T),
            tcost.max=max(tcost, na.rm=T),
            tcost.sd=sd(tcost, na.rm=T)
  )
print(tcost.all)

if(SAVE.INTERMEDIARIES) {
  intm_file = file.path(INTERMEDIATE_DIR, "tcost.RData")
  save(tcost.htaz.tpurp.inc, tcost.hh, tcost.htaz.inc, tcost.htaz, tcost.distr, tcost.all, file=intm_file)
}