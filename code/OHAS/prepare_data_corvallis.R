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

# Define function for Corvallis 
# overlay x,y coordinate (longitude, lattitude) with a polygon shapefile (shpfile) to get polygon id (id_name)
get_xy_polyid <- function(xy.df, shpfile, id_name) {
  spdf = SpatialPointsDataFrame(xy.df[, c('x', 'y')], 
                                xy.df, 
                                proj4string=CRS("+init=epsg:4326"))
  spdf.proj <- spTransform(spdf, CRS("+init=epsg:2992"))
  
  # spatial join with the shp polygon to get polygon id (id_name)
  TAZPoly <- readShapePoly(shpfile,
                           proj4string=CRS("+init=epsg:2992"))
  id <- over(spdf.proj, TAZPoly)[, id_name]
  id
}

# reclassify income categories (low income: $0- $24,999; mid income: $25,000 - $49,999; high income: $50,000 or more; NA: refused)
# low <- (1,2); median <- (3,4); high <- 5:8
hh.metro <- hh %>% 
  mutate(inc.level=cut(INCOME,
                       breaks=c(1, 3, 5, 9),
                       labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
                       include.lowest=T, right=F
  )) %>%
  dplyr::select(SAMPN, INCOME, inc.level, HHSIZ, HXCORD, HYCORD) %>%
  rename(x=HXCORD, y=HYCORD) %>%
  as.data.frame() 


hh.metro$HTAZ <- get_xy_polyid(hh.metro, TAZ.shpfile, TAZ.id_name)
TAZPoly <- readShapePoly(TAZ.shpfile, proj4string=CRS("+init=epsg:2992"))

TAZ.DISTRICT <- TAZPoly@data %>%
                dplyr::select(TAZ, DISTRICT)%>%
                dplyr::rename(HTAZ=TAZ,
                              district.id=DISTRICT)
hh.metro <- hh.metro %>% 
            left_join(TAZ.DISTRICT)
tcost.trip <- tcost.trip %>%
  left_join(hh.metro, by="SAMPN") #%>%
#left_join(districts, by=c("HTAZ"="zone")) %>%
#dplyr::rename(district.id=ugb)


if(SAVE.INTERMEDIARIES) {
  intm_file = file.path(INTERMEDIATE_DIR, "linkedTrip.RData")
  save(linkedTrip, file=intm_file)
}
