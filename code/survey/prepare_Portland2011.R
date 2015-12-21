require(dplyr)
require(tidyr)

# prepare data -> a data.frame for linked trips with columns
# SAMPN, HHWGT, HTAZ, inc.level, TripPurpose, MODE, tripdur.hours, tripdist.miles
tcost.trip <- linkedTrip %>% 
  select(SAMPN, PERNO, HHWGT, TripPurpose, MODE, TRPDUR, DistanceRoute) %>%
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

# reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
# low <- (1,2); median <- (3,4); high <- 5:8

# Adjust 1994 income to 2011
# lowInc: $0 ~ $34,999; midInc: $35,000 ~ $74,999; hignInc:75,000 ~ or more
# low <- (1,3); median <- (4,5); high <- 6:8

hh.metro <- hh %>% filter(AREA==11) %>%
  mutate(inc.level=cut(INCOME,
                       breaks=c(1, 4, 6, 9),
                       labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                       include.lowest=T, right=F
  )) %>%
  dplyr::select(SAMPN, INCOME, inc.level, HHSIZ, HXCORD, HYCORD) %>%
  rename(x=HXCORD, y=HYCORD) %>%
  as.data.frame() 

hh.metro$HTAZ <- get_xy_polyid(hh.metro, TAZ.shpfile, TAZ.id_name)
districts.shpfile <- file.path(INPUT_DIR, "shp/districts.shp")
districts.id_name <- "DISTRICT"
hh.metro$district.id <- get_xy_polyid(hh.metro, districts.shpfile, districts.id_name)

tcost.trip <- tcost.trip %>%
  left_join(hh.metro, by="SAMPN") #%>%
  #left_join(districts, by=c("HTAZ"="zone")) %>%
  #dplyr::rename(district.id=ugb)

if(SAVE.INTERMEDIARIES) {
  intm_file = file.path(INTERMEDIATE_DIR, "linkedTrip.RData")
  save(linkedTrip, tcost.trip, file=intm_file)
}
