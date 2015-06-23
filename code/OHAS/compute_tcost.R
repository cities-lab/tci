# This scripts identify linked trips, not place-based trips, by income groups and trip purposes

# Load required packages
require(dplyr)
require(tidyr)

## read OHAS activity, household, trip table
load(file.path(INPUT_DIR, "OHAS_Final.Rdata"))
load(file.path(INPUT_DIR, "Zi.RData"))

# identify home-based trips in linkedTrip
linked <- linkedTrip %>% 
  select(SAMPN, PERNO, HHWGT, PERWGT, TripPurpose, MODE, TRPDUR, DistanceRoute) %>%
  mutate(TripPurpose = tolower(TripPurpose),
         TripPurpose=ifelse(TripPurpose=="hbshp", "hbs", TripPurpose),
         TripPurpose=ifelse(TripPurpose=="hbrec", "hbr", TripPurpose)
         #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
         #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
         ) %>%
  filter( TripPurpose %in% c("hbw", "hbs", "hbr", "hbo"))

# reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
hh$inc.level = cut(hh$INCOME,
                   breaks=c(1, 3, 5, 9),
                   labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                   include.lowest=T, right=F)
# low <- (1,2); median <- (3,4); high <- 5:8

hh.metro <- hh %>% filter(AREA==11) %>%
  #dplyr::select(SAMPN, inc.level, HTAZ)
  dplyr::select(SAMPN, inc.level, HXCORD, HYCORD) %>%
  as.data.frame()

require(maptools)
require(rgeos)
require(rgdal)
home.spdf = SpatialPointsDataFrame(hh.metro[, c('HXCORD', 'HYCORD')], 
                                   hh.metro, 
                                   proj4string=CRS("+init=epsg:4326"))

home.spdf2 <- spTransform(home.spdf, CRS("+init=epsg:2913"))

# spatial join with TAZ to get HTAZ (HOME TAZ)
TAZPoly <- readShapePoly(file.path(INPUT_DIR, "shp/TAZ.shp"),
                         proj4string=CRS("+init=epsg:2913"))
hh.metro$HTAZ <- over(home.spdf2, TAZPoly)$newtaz

linked <- left_join(linked, hh.metro, by="SAMPN")

if(SAVE.INTERMEDIARIES) {
  intm_file = file.path(INTERMEDIATE_DIR, "linked_trips.RData")
  save(linked, file=intm_file)
}

#append unit travel cost by mode (and potentially by income)
linked <- left_join(linked, unitcosts)
linked <- linked %>% 
  mutate(t.cost=VOT*TRPDUR/60,              #time costs
         m.cost=mcpm*DistanceRoute/5280,    #monetary costs
         tcost= t.cost + m.cost) %>%         #total costs
  na.omit()                                #exclude rows with unknown HTAZ, tpurp, or inc.level  
  
# summarize trip-level travel time cost by taz, trip purpose, and income level
tcost.HTAZ.tpurp.inc <- linked %>%
  group_by(HTAZ, TripPurpose, inc.level) %>%
  summarise(n = n(),
            tcost.min=min(tcost, na.rm=T),
            tcost.avg=mean(tcost, na.rm=T),
            tcost.max=max(tcost, na.rm=T),
            tcost.sd=sd(tcost, na.rm=T)
  ) 

print(tcost.HTAZ.tpurp.inc)

# calculate household-level travel time cost
tcost.hh <- linked %>%
  group_by(SAMPN) %>%
  summarise(tcost=sum(tcost),
            HTAZ=first(HTAZ),             #retain HTAZ, inc.level and HHWGT
            inc.level=first(inc.level),
            HHWGT=first(HHWGT)
            )

# summarize household-level travel time cost by taz and/or income level
tcost.HTAZ.inc <- tcost.hh %>%
  group_by(HTAZ, inc.level) %>%
  summarise(n = n(),
            tcost.min=min(tcost, na.rm=T),
            tcost.avg=mean(tcost, na.rm=T),
            tcost.max=max(tcost, na.rm=T),
            tcost.sd=sd(tcost, na.rm=T)
  )
print(tcost.HTAZ.inc)

tcost.HTAZ <- tcost.hh %>%
  group_by(HTAZ) %>%
  summarise(n = n(),
            tcost.min=min(tcost, na.rm=T),
            tcost.avg=mean(tcost, na.rm=T),
            tcost.max=max(tcost, na.rm=T),
            tcost.sd=sd(tcost, na.rm=T)
  )
print(tcost.HTAZ)

# summarize household-level travel time cost by district
load(file.path(INPUT_DIR, "districts.RData"))
tcost.hh <- left_join(tcost.hh, districts, by=c("HTAZ"="zone"))
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
  save(tcost.HTAZ.tpurp.inc, tcost.hh, tcost.HTAZ.inc, tcost.HTAZ, tcost.distr, tcost.all, file=intm_file)
}

#reshape data frame into arrays for plotting
require(reshape2)
#tcost by HTAZ, inc.level, and tpurp
mintcost.ZiIcPr <- acast(tcost.HTAZ.tpurp.inc, HTAZ~inc.level~TripPurpose, value.var="tcost.min")
avgtcost.ZiIcPr <- acast(tcost.HTAZ.tpurp.inc, HTAZ~inc.level~TripPurpose, value.var="tcost.avg")
maxtcost.ZiIcPr <- acast(tcost.HTAZ.tpurp.inc, HTAZ~inc.level~TripPurpose, value.var="tcost.max")

#tcost by HTAZ, inc.level
minhhtcost.ZiIc <- acast(tcost.HTAZ.inc, HTAZ~inc.level, value.var="tcost.min")
avghhtcost.ZiIc <- acast(tcost.HTAZ.inc, HTAZ~inc.level, value.var="tcost.avg")
maxhhtcost.ZiIc <- acast(tcost.HTAZ.inc, HTAZ~inc.level, value.var="tcost.max")

#tcost by HTAZ
flat.tcost.HTAZ <- dplyr::select(tcost.HTAZ, HTAZ, min=tcost.min, avg=tcost.avg, max=tcost.max) %>%
                   gather(func, value, -HTAZ)
hhCost.ZiCm <- acast(flat.tcost.HTAZ, HTAZ~func, value.var="value") #could use spread
