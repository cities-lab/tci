# Load required packages
require(dplyr)
require(tidyr)

# prepare data -> a data.frame for linked trips with columns
# SAMPN, HHWGT, HTAZ, inc.level, TripPurpose, MODE, tripdur.hours, tripdist.miles
  
  # reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
  # low <- (1,2); median <- (3,4); high <- 5:8
  
  # Adjust 1994 income to 2011
  # lowInc: $0 ~ $34,999; midInc: $35,000 ~ $74,999; hignInc:75,000 ~ or more
  # low <- (1,3); median <- (4,5); high <- 6:8
  
  hh.ready <- hh %>% 
    mutate(inc.level=cut(INCOME,
                         breaks=c(1, 4, 6, 9),
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
    left_join(TAZ.DISTRICT) %>%
    filter(!is.na(HTAZ))


# Differentiate TRANSIT by bus and rail 
# Use type of bus (TBUS) field to differentiate transit 
  place.ready <- place %>%
    filter(SAMPN %in% hh.ready$SAMPN) %>%
    select(SAMPN, PERNO, PLANO, TBUS) %>% 
    mutate(TBUS=factor(TBUS, levels=c(1:9,97), 
                       labels=c("LOCAL BUS", "LIGHT RAIL", "BUS RAPID TRANSIT", "OTHER BUS", "EXPRESS BUS", 
                                "CHERRIOTS", "PARK & RIDE SHUTTLE",  "CARTS", "SMART", "OTHER(SPECIFY)")))  
  
  linkedTrip.ready <- linkedTrip %>% 
    filter(SAMPN %in% hh.ready$SAMPN) %>%
    mutate(linkedTrip.id=SAMPN*1000 + PERNO*100 + PLANO) %>% 
    select(SAMPN, PERNO, PLANO,ThisMODE, linkedTrip.id) %>%       
    arrange(SAMPN, PERNO, PLANO)
  
  trip.ready <- trip %>% 
    filter(SAMPN %in% hh.ready$SAMPN) %>%
    select(SAMPN, PERNO, PLANO, MODENAME, TRPDUR, DistanceRoute) %>%
    left_join(place.ready) %>%
    left_join(linkedTrip.ready) %>%
    mutate(MODENAME=as.character(MODENAME),
           MODENAME=ifelse(TBUS %in% c("LOCAL BUS", "BUS RAPID TRANSIT", "OTHER BUS", "EXPRESS BUS")&!is.na(TBUS), "BUS", MODENAME),
           MODENAME=ifelse(TBUS=="LIGHT RAIL"&!is.na(TBUS), "RAIL", MODENAME),
           bus=ifelse(MODENAME=="BUS", 1, 0), 
           rail=ifelse(MODENAME=="RAIL", 1, 0)) 
  
  # Update linkedTrip.id field 
  for (i in nrow(trip.ready):1) {
    if (!is.na(trip.ready[i, "linkedTrip.id"])) {
      trip.ready[i, c("ThisMODE", "linkedTrip.id")] <- trip.ready[i, c("ThisMODE", "linkedTrip.id")]
    } else {
      trip.ready[i, c("ThisMODE", "linkedTrip.id")] <- trip.ready[i+1, c("ThisMODE", "linkedTrip.id")]
    }
  }
  
  linkedTrip.bus.rail <- trip.ready %>% 
    group_by(linkedTrip.id) %>% 
    summarise(bus.freq=sum(bus), 
              rail.freq=sum(rail)) %>%
    mutate(linkedTrip.bus=ifelse(bus.freq>0, "BUS", "NOBUS"), 
           linkedTrip.rail=ifelse(rail.freq>0, "RAIL", "NORAIL")) %>%
    filter(linkedTrip.bus=="BUS"|linkedTrip.rail=="RAIL") %>%
    select(linkedTrip.id, linkedTrip.bus, linkedTrip.rail)
  
# prepare data -> a data.frame for linked trips with columns
  # SAMPN, HHWGT, HTAZ, inc.level, TripPurpose, MODE, tripdur.hours, tripdist.miles
  tcost.trip <- linkedTrip %>% 
    filter(SAMPN %in% hh.ready$SAMPN) %>% 
    select(SAMPN, PERNO, PLANO, HHWGT, TripPurpose, ThisMODE) %>%
    mutate(TripPurpose = tolower(TripPurpose),
           TripPurpose=ifelse(TripPurpose=="hbshp", "hbs", TripPurpose),
           TripPurpose=ifelse(TripPurpose=="hbrec", "hbr", TripPurpose)
           #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
           #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
    ) %>%
    filter(TripPurpose %in% c("hbw", "hbs", "hbr", "hbo")) %>% 
    mutate(linkedTrip.id=SAMPN*1000 + PERNO*100 + PLANO) 
  
  
  # Differentiate transit types 
  tcost.trip <- tcost.trip %>%
    left_join(linkedTrip.bus.rail) %>%
    mutate(ThisMODE=ifelse(linkedTrip.bus=="BUS"&!is.na(linkedTrip.bus), "BUS", ThisMODE),
           ThisMODE=ifelse(linkedTrip.rail=="RAIL"&!is.na(linkedTrip.rail), "RAIL", ThisMODE),
           ThisMODE=ifelse(ThisMODE %in% c("PNR", "KNR", "TRANSIT"), "PARATRANSIT", ThisMODE)) %>%
    # filter(ThisMODE!="SCHOOLBUS") %>% 
    select(-linkedTrip.bus, -linkedTrip.rail) %>%
    as.data.frame()
  
  # Recalculate travel time and travel route distance for linkedTrips   
  linkedTrip.id.changemode <- trip.ready %>%
    group_by(linkedTrip.id) %>%
    summarise(freq=n()) %>%
    filter(freq>1)
  
  linkedTrip.ready.changeMode <- trip.ready %>%
    filter(linkedTrip.id %in% linkedTrip.id.changemode$linkedTrip.id) %>%
    rename(MODE=MODENAME) %>%
    left_join(unitcosts) %>%  
    mutate(t.cost=VOT*TRPDUR/60, 
           m.cost=mcpm*DistanceRoute/5280) %>%
    group_by(linkedTrip.id) %>%
    summarise(t.cost=sum(t.cost, na.rm=TRUE),
              m.cost=sum(m.cost, na.rm=TRUE)) %>%
    mutate(tcost= constant + t.cost + m.cost) %>%
    as.data.frame()
  
  linkedTrip.ready.nochangeMode <- trip.ready %>%
    filter(!(linkedTrip.id %in% linkedTrip.id.changemode$linkedTrip.id)) %>%
    rename(MODE=MODENAME) %>%
    left_join(unitcosts) %>%  
    mutate(t.cost=VOT*TRPDUR/60, 
           m.cost=mcpm*DistanceRoute/5280,
           tcost= constant + t.cost + m.cost) %>%
    select(linkedTrip.id, t.cost,  m.cost, tcost)
  
  linkedTrip.ready.tripdur.tripdist <- rbind(linkedTrip.ready.changeMode, linkedTrip.ready.nochangeMode)   
  
  tcost.trip <- tcost.trip %>%
    left_join(linkedTrip.ready.tripdur.tripdist) %>%
    rename(MODE=ThisMODE)

  per.child <- per %>%
    group_by(SAMPN) %>%
    summarize(has.child=ifelse(sum(AGE<=16) > 0, T, F))
  
  tcost.trip <- tcost.trip %>%
    left_join(hh.ready, by="SAMPN") %>%
    left_join(per.child, by="SAMPN")
  #left_join(districts, by=c("HTAZ"="zone")) %>%
  #dplyr::rename(district.id=ugb)
  
  
  if(SAVE.INTERMEDIARIES) {
    intm_file = file.path(INTERMEDIATE_DIR, "linkedTrip_tcost.RData")
    save(linkedTrip, tcost.trip, file=intm_file)
  }
