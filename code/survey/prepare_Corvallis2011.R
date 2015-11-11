# Load required packages
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
hh.ready <- hh %>% 
  mutate(inc.level=cut(INCOME,
                       breaks=c(1, 3, 5, 9),
                       labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                       include.lowest=T, right=F
  )) %>%
  dplyr::select(SAMPN, INCOME, inc.level, HHSIZ, HXCORD, HYCORD) %>%
  rename(x=HXCORD, y=HYCORD) %>%
  as.data.frame() 


hh.ready$HTAZ <- get_xy_polyid(hh.ready, TAZ.shpfile, TAZ.id_name, xy.epsg='4326', shpfile.epsg="2992")
TAZPoly <- readShapePoly(TAZ.shpfile, proj4string=CRS("+init=epsg:2992"))

TAZ.DISTRICT <- TAZPoly@data %>%
                dplyr::select(TAZ, DISTRICT)%>%
                dplyr::rename(HTAZ=TAZ,
                              district.id=DISTRICT)
hh.ready <- hh.ready %>% 
            left_join(TAZ.DISTRICT)
tcost.trip <- tcost.trip %>%
  left_join(hh.ready, by="SAMPN") #%>%
#left_join(districts, by=c("HTAZ"="zone")) %>%
#dplyr::rename(district.id=ugb)


if(SAVE.INTERMEDIARIES) {
  intm_file = file.path(INTERMEDIATE_DIR, "linkedTrip_tcost.RData")
  save(linkedTrip, tcost.trip, file=intm_file)
}
