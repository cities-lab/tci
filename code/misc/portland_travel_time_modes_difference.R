# This script analyze travel time and travel modes difference between 1994 and 2011 survey data
# Settings 
  # load required packages 
  require(ggplot2)
  
  # Define workplaces 
  INPUT_DIR <- "data/Portland/2011"
  OUTPUT_DIR <- "output/PortlandTrips_1994_VS_2011"
  TAZ.shpfile <- file.path(INPUT_DIR, "shp/TAZ.shp")
  TAZ.id_name <- "newtaz"
  
  # Source required scripts 
  source("code/functions.R")

# 2011 OHAS data  
# Load data 
  # Use ThisMODE instead of MODE 
  load("data/Portland/2011/OHAS_Final.Rdata") # linkedTrip only removes ChangeMode trip  
  
  MODE.df <- data.frame(MODE=c(1:10,97), 
                        MODE.name=c("walk", "bike", "auto driver", "auto passenger", "bus", "rail", 
                                    "dial-a-ride", "taxi", "school bus", "carpool/vanpool", "other(specify)"))
  linkedTrip <- linkedTrip%>% 
                left_join((MODE.df))
  
  hh.metro <- hh %>% 
              filter(AREA==11) %>% # Metro area 
              select(SAMPN, AREA)
  
  # Identify hbw trips 
  tcost.trip <- linkedTrip %>% 
                left_join(hh.metro) %>%
                filter(!is.na(AREA)) %>%
                filter(TripPurpose %in% c("HBW")) %>%
                mutate(peak=ifelse((LastDEP_HR>=6&LastDEP_HR<10)|(LastDEP_HR>=16&LastDEP_HR<20)|(ARR_HR>=6&ARR_HR<10)|(ARR_HR>=16&ARR_HR<20), 1, 0)) %>%
                select(SAMPN, PERNO, PLANO, AREA, ThisMODE, TRPDUR, CMMOTTRPDUR, DistanceRoute, peak, LastXCORD, LastYCORD, ThisXCORD, ThisYCORD) %>%
                rename(MODE=ThisMODE) %>%
                filter(MODE!="SCHOOLBUS") %>% # After checking trip by school bus, this maybe a mistake by metro, so remove trip by school bus
                arrange(SAMPN, PERNO) 
  
  # Identify TAZ and LastTAZ 
  tcost.trip.LastTAZ <- tcost.trip %>% 
    select(SAMPN, PERNO, PLANO, LastXCORD, LastYCORD) %>% 
    rename(x=LastXCORD, y=LastYCORD) %>%
    as.data.frame() 
  
  tcost.trip.LastTAZ$LastTAZ <- get_xy_polyid(tcost.trip.LastTAZ, TAZ.shpfile, TAZ.id_name)

  tcost.trip.ThisTAZ <- tcost.trip %>% 
    select(SAMPN, PERNO, PLANO, ThisXCORD, ThisYCORD) %>% 
    rename(x=ThisXCORD, y=ThisYCORD) %>%
    as.data.frame() 
  
  tcost.trip.ThisTAZ$ThisTAZ <- get_xy_polyid(tcost.trip.ThisTAZ, TAZ.shpfile, TAZ.id_name)

  # Identify trips 
  tcost.trip.hbw.2011 <- tcost.trip %>% 
    left_join(tcost.trip.LastTAZ) %>%
    select(-x, -y) %>%
    left_join(tcost.trip.ThisTAZ) %>%
    select(-x, -y) %>%
    select(SAMPN, PERNO, PLANO, MODE, TRPDUR, CMMOTTRPDUR, DistanceRoute, peak, LastTAZ, ThisTAZ) %>%
    mutate(distance.miles=DistanceRoute/5280)
  
  tcost.trip.hbw.2011 <- tcost.trip.hbw.2011 %>% 
    mutate(MODE=as.character(MODE)) %>%
    mutate(MODE=ifelse(MODE=="PNR", "TRANSIT", MODE)) %>%
    mutate(MODE=ifelse(MODE=="KNR", "TRANSIT", MODE)) %>%
    mutate(MODE=ifelse(MODE=="CAR/VANPOOL", "PASSENGER", MODE)) %>%
    filter(MODE!="TAXI") # There are 5 trips by taxi 
  
  tcost.trip.hbw.2011.transit <- tcost.trip.hbw.2011 %>% 
    filter(MODE %in% c("TRANSIT")) %>%
    mutate(tripdur.minutes = CMMOTTRPDUR)  %>% 
    select(SAMPN, PERNO, PLANO, MODE, tripdur.minutes, distance.miles, peak,LastTAZ, ThisTAZ)
  
  tcost.trip.hbw.2011.notransit <- tcost.trip.hbw.2011 %>% 
    filter(!(MODE %in% c("TRANSIT"))) %>%
    mutate(tripdur.minutes = TRPDUR) %>%
    select(SAMPN, PERNO, PLANO, MODE, tripdur.minutes, distance.miles, peak, LastTAZ, ThisTAZ)
  
  tcost.trip.hbw.2011 <- rbind(tcost.trip.hbw.2011.transit, tcost.trip.hbw.2011.notransit)
  
  # Open district shapefile, TAZ shapefile of 1994 and 2011 in QGIS
  # Identify 1:26 TAZs for 2011, 1:16 TAZs for 1994
  tcost.trip.hbw.CBD.both.2011 <- tcost.trip.hbw.2011 %>% 
    filter((LastTAZ %in% c(1:26)|ThisTAZ %in% c(1:26)))
  
 rm(list= ls()[!(ls() %in% c('tcost.trip.hbw.2011','tcost.trip.hbw.CBD.both.2011', 
                              'OUTPUT_DIR'))])
  
# 1994 OHAS data 
  # CBD includes TAZ 1:16, these TAZs are within district 1
 
  # Load linkedTrip data 
  load("data/Portland/1994/linkedTrip.RData")
  modes.df <- data.frame(MODE=c(1:8), MODE.name=c("other", "walk", "bicycle", "school bus", "public bus", "MAX", 
                                                  "personal vehicle", "non-personal vehicle"))
  
  tcost.trip.hbw.1994 <- linkedTrip %>% 
                          select(SAMPN, HHWGT, DAYNO, PERNO, ACTNO, TripPurpose, MODE, TRPDUR, DistanceRoute, HTAZ, LastTAZ, TAZ) %>%
                          mutate(TripPurpose = tolower(TripPurpose),
                                 TripPurpose=ifelse(TripPurpose=="hbshp", "hbs", TripPurpose),
                                 TripPurpose=ifelse(TripPurpose=="hbrec", "hbr", TripPurpose)
                                 #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
                                 #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
                          ) %>%
                          filter( TripPurpose %in% c("hbw")) %>%
                          mutate(tripdur.minutes=TRPDUR,
                                 tripdist.miles=DistanceRoute/5280
                          ) %>% 
                          filter(!is.na(MODE)) %>% 
                          left_join(modes.df)

  tcost.trip.hbw.1994 <- tcost.trip.hbw.1994 %>%
                          mutate(MODE.name=as.character(MODE.name)) %>%
                          mutate(MODE.name=ifelse(MODE.name=="MAX", "TRANSIT", MODE.name)) %>%
                          mutate(MODE.name=ifelse(MODE.name=="public bus", "TRANSIT", MODE.name)) %>%
                          mutate(MODE.name=ifelse(MODE.name=="other", "other(specify)", MODE.name)) %>%
                          mutate(MODE.name=ifelse(MODE.name=="non-personal vehicle", "PASSENGER", MODE.name)) %>%
                          mutate(MODE.name=ifelse(MODE.name=="bicycle", "BIKE", MODE.name)) %>%
                          mutate(MODE.name=ifelse(MODE.name=="personal vehicle", "DRIVER", MODE.name)) %>%
                          mutate(MODE.name=toupper(MODE.name))  %>%
                          filter(MODE.name!="OTHER(SPECIFY)") 
  
  
# get time priod for linkedTrips from act1 data for 1994 
  act1<- read.table(file.path("data/Portland/1994", "act1.txt"), header=FALSE,  sep=",", as.is=TRUE)
  
  # change column names
  # colname is from http://www.surveyarchive.org/sda/Portland/doc/hcbk.htm
  act1.colnames <- c("PHASE", "STRATUM", "SAMPN", "PERNO", "DAYNO", "ACTNO", "ACT1", "OLOC", "THERE",
                     "ACTBEG", "AMPMS", "ACTEND", "AMPME", "TRPSTHEN", "MODE", "OMODE", "VEHAVAIL", 
                     "PAY4CAR", "HOWMUCH", "TIME1", "NOPARTY", "VEHPARTY", "PARTYPAY", "HOWMPP", 
                     "TIME2", "ROUTE1", "BOARDWHR", "ACCESSSTOP", "EGRESTOP", "PAYHOW", "SUBSIDWHO1", 
                     "TRANSFER", "TRANSWHE", "TRANSAG", "HOWMANY", "VEHWHICH", "DRIVER", "NUMINVEH",
                     "PRKWHERE", "PRKPAY", "PRKFARE", "TIME3", "SUBSIDWHO2", "USUBSCOST", "TIME4",
                     "TRIPBEG", "AMPMTB", "TRIPEND", "AMPMTE", "TRIPHRS", "TRIPMIN", "MODCHG", "CHGWHERE", 
                     "CHG2FRM", "HALFMILE")
  
  colnames(act1) <- act1.colnames
  
  time.period <- act1 %>% 
      select(SAMPN, DAYNO, PERNO, ACTNO, ACTNO, TRIPBEG, AMPMTB, TRIPEND, AMPMTE) %>%
      mutate(peak=ifelse((TRIPBEG>=600&TRIPBEG<1000&AMPMTB==1)|(TRIPEND>600&TRIPEND<=1000&AMPMTE==1)|(TRIPBEG>=400&TRIPBEG<800&AMPMTB==2)|(TRIPEND>400&TRIPEND<=800&AMPMTE==2), 1, 0))
    
  tcost.trip.hbw.1994 <- tcost.trip.hbw.1994 %>% 
      left_join(time.period) %>%
      mutate(distance.miles=DistanceRoute/5280)  
    
  tcost.trip.hbw.CBD.both.1994 <- tcost.trip.hbw.1994 %>% 
      filter((TAZ %in% c(1:16)|LastTAZ %in% c(1:16)))   

# Compare trips of two years   
# For 2011, TRANSIT includes TRANSIT, KNR, PNR; PASSENGER includes PASSENGER and CAR/VANPOOL
  # All hbw trips 
  tcost.trip.hbw.2011 <- tcost.trip.hbw.2011 %>% 
    mutate(YEAR=2011) %>% 
    select(YEAR, SAMPN, PERNO, MODE, tripdur.minutes,peak, distance.miles) 
  
  tcost.trip.hbw.1994 <- tcost.trip.hbw.1994 %>% 
    select(SAMPN, PERNO, MODE.name, tripdur.minutes,peak, distance.miles) %>%
    rename(MODE=MODE.name) %>%
    mutate(YEAR=1994) %>% 
    select(YEAR, SAMPN, PERNO, MODE, tripdur.minutes, peak, distance.miles) 
  
  # CBD hbw trips 
  tcost.trip.hbw.CBD.both.2011 <- tcost.trip.hbw.CBD.both.2011 %>% 
    mutate(YEAR=2011) %>% 
    select(YEAR, SAMPN, PERNO, MODE, tripdur.minutes,peak, distance.miles) 
  
  tcost.trip.hbw.CBD.both.1994 <- tcost.trip.hbw.CBD.both.1994 %>% 
    select(SAMPN, PERNO, MODE.name, tripdur.minutes,peak, distance.miles) %>%
    rename(MODE=MODE.name) %>%
    mutate(YEAR=1994) %>% 
    select(YEAR, SAMPN, PERNO, MODE, tripdur.minutes, peak, distance.miles) 
  
# Summarise analysis   
  # All hbw trips 
  tcost.trip.hbw.1994.sum <- tcost.trip.hbw.1994 %>%
    group_by(MODE) %>%
    summarise(freq.1994=n(),
              ttime.min.1994=min(tripdur.minutes),
              ttime.avg.1994=mean(tripdur.minutes),
              ttime.median.1994=median(tripdur.minutes),
              ttime.max.1994=max(tripdur.minutes)) %>%
    mutate(probs.1994=freq.1994/sum(freq.1994)) 
  
  
  tcost.trip.hbw.2011.sum <- tcost.trip.hbw.2011 %>%
    group_by(MODE) %>%
    summarise(freq.2011=n(),
              ttime.min.2011=min(tripdur.minutes),
              ttime.avg.2011=mean(tripdur.minutes),
              ttime.median.2011=median(tripdur.minutes),
              ttime.max.2011=max(tripdur.minutes)) %>%
    mutate(probs.2011=freq.2011/sum(freq.2011))
  
  tcost.trip.hbw.sum <- tcost.trip.hbw.1994.sum %>%
    left_join(tcost.trip.hbw.2011.sum) %>% 
    select(MODE, freq.1994, freq.2011, ttime.min.1994, ttime.min.2011, ttime.avg.1994, ttime.avg.2011, 
           ttime.median.1994, ttime.median.2011,ttime.max.1994, ttime.max.2011, probs.1994, probs.2011) %>%
    arrange(freq.1994) %>%
    mutate(probs.1994=round(probs.1994*100, 2), 
           probs.2011=round(probs.2011*100, 2))%>%
    as.data.frame()
  
  tcost.trip.hbw <- rbind(tcost.trip.hbw.2011, tcost.trip.hbw.1994) %>%
    mutate(YEAR=as.factor(YEAR))
  
  tcost.trip.hbw.sum.plot.data <- rbind(tcost.trip.hbw.2011, tcost.trip.hbw.1994) %>% 
    group_by(YEAR, MODE) %>% 
    summarise(freq=n(), 
              ttime.min=min(tripdur.minutes),
              ttime.avg=mean(tripdur.minutes),
              ttime.median=median(tripdur.minutes),
              ttime.max=max(tripdur.minutes)) %>% 
    group_by(YEAR) %>% 
    mutate(probs=freq/sum(freq))
  
  # CBD hbw trips 
  tcost.trip.hbw.CBD.both.1994.sum <- tcost.trip.hbw.CBD.both.1994 %>%
    group_by(MODE) %>%
    summarise(freq.1994=n(),
              ttime.min.1994=min(tripdur.minutes),
              ttime.avg.1994=mean(tripdur.minutes),
              ttime.median.1994=median(tripdur.minutes),
              ttime.max.1994=max(tripdur.minutes)) %>%
    mutate(probs.1994=freq.1994/sum(freq.1994)) 
  
  tcost.trip.hbw.CBD.both.2011.sum <- tcost.trip.hbw.CBD.both.2011 %>%
    group_by(MODE) %>%
    summarise(freq.2011=n(),
              ttime.min.2011=min(tripdur.minutes),
              ttime.avg.2011=mean(tripdur.minutes),
              ttime.median.2011=median(tripdur.minutes),
              ttime.max.2011=max(tripdur.minutes)) %>%
    mutate(probs.2011=freq.2011/sum(freq.2011))
  
  tcost.trip.hbw.CBD.both.sum <- tcost.trip.hbw.CBD.both.1994.sum %>%
    left_join(tcost.trip.hbw.CBD.both.2011.sum) %>% 
    select(MODE, freq.1994, freq.2011, ttime.min.1994, ttime.min.2011, ttime.avg.1994, ttime.avg.2011, 
           ttime.median.1994, ttime.median.2011,ttime.max.1994, ttime.max.2011, probs.1994, probs.2011) %>%
    arrange(freq.1994) %>%
    mutate(probs.1994=round(probs.1994*100, 2), 
           probs.2011=round(probs.2011*100, 2))%>%
    as.data.frame()
  
  
  tcost.trip.hbw.CBD.both <- rbind(tcost.trip.hbw.CBD.both.2011, tcost.trip.hbw.CBD.both.1994) %>%
    mutate(YEAR=as.factor(YEAR))
  
  tcost.trip.hbw.CBD.both.sum.plot.data <- rbind(tcost.trip.hbw.CBD.both.2011, tcost.trip.hbw.CBD.both.1994) %>% 
    group_by(YEAR, MODE) %>% 
    summarise(freq=n(), 
              ttime.min=min(tripdur.minutes),
              ttime.avg=mean(tripdur.minutes),
              ttime.median=median(tripdur.minutes),
              ttime.max=max(tripdur.minutes)) %>% 
    group_by(YEAR) %>% 
    mutate(probs=freq/sum(freq))
  
# Plot
  # All CBD trips 
  pden.tcost.trip.hbw.2011 <- ggplot(tcost.trip.hbw.2011, aes(tripdur.minutes, colour=MODE)) + geom_density() + xlim(0,100) + ylim(0, 0.15)
  pden.tcost.trip.hbw.2011
  
  pden.tcost.trip.hbw.1994 <- ggplot(tcost.trip.hbw.1994, aes(tripdur.minutes, colour=MODE)) + geom_density() + xlim(0, 100) + ylim(0, 0.15)
  pden.tcost.trip.hbw.1994
  
  pbox.ttime.trip.hbw <- ggplot(tcost.trip.hbw, aes(x=MODE, y=tripdur.minutes, fill=YEAR)) + geom_boxplot() + ylim(0, 75)
  pbox.ttime.trip.hbw
  
  pbar.freq.trip.hbw <- ggplot(tcost.trip.hbw.sum.plot.data, aes(factor(MODE), freq, fill = factor(YEAR))) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1")
  pbar.freq.trip.hbw
  
  pbar.prob.trip.hbw <- ggplot(tcost.trip.hbw.sum.plot.data, aes(factor(MODE), probs, fill = factor(YEAR))) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1")
  pbar.prob.trip.hbw
  
  pbar.trip.hbw.ttime.avg <-  ggplot(tcost.trip.hbw.sum.plot.data, aes(factor(MODE), ttime.avg, fill = factor(YEAR))) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1")
  pbar.trip.hbw.ttime.avg 
  
  
  pbar.trip.hbw.ttime.median <-  ggplot(tcost.trip.hbw.sum.plot.data, aes(factor(MODE), ttime.median, fill = factor(YEAR))) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1")
  pbar.trip.hbw.ttime.median   
  
  # CBD hbw trips 
  pden.tcost.trip.hbw.CBD.both.2011 <- ggplot(tcost.trip.hbw.CBD.both.2011, aes(tripdur.minutes, colour=MODE)) + geom_density() + xlim(0,100) + ylim(0, 0.15)
  pden.tcost.trip.hbw.CBD.both.2011
  
  pden.tcost.trip.hbw.CBD.both.1994 <- ggplot(tcost.trip.hbw.CBD.both.1994, aes(tripdur.minutes, colour=MODE)) + geom_density() + xlim(0, 100) + ylim(0, 0.15)
  pden.tcost.trip.hbw.CBD.both.1994
  
  pbox.ttime.hbw.CBD.both <- ggplot(tcost.trip.hbw.CBD.both, aes(x=MODE, y=tripdur.minutes, fill=YEAR)) + geom_boxplot() + ylim(0, 75)
  pbox.ttime.hbw.CBD.both
  
  pbar.freq.hbw.CBD.both <- ggplot(tcost.trip.hbw.CBD.both.sum.plot.data, aes(factor(MODE), freq, fill = factor(YEAR))) + 
                                  geom_bar(stat="identity", position = "dodge") + 
                                  scale_fill_brewer(palette = "Set1")
  pbar.freq.hbw.CBD.both
  
  pbar.prob.hbw.CBD.both <- ggplot(tcost.trip.hbw.CBD.both.sum.plot.data, aes(factor(MODE), probs, fill = factor(YEAR))) + 
                                  geom_bar(stat="identity", position = "dodge") + 
                                  scale_fill_brewer(palette = "Set1")
  pbar.prob.hbw.CBD.both
  
  pbar.ttime.avg.hbw.CBD.both <-  ggplot(tcost.trip.hbw.CBD.both.sum.plot.data, aes(factor(MODE), ttime.avg, fill = factor(YEAR))) + 
                                        geom_bar(stat="identity", position = "dodge") + 
                                        scale_fill_brewer(palette = "Set1")
  pbar.ttime.avg.hbw.CBD.both 
  
  pbar.ttime.median.hbw.CBD.both <-  ggplot(tcost.trip.hbw.CBD.both.sum.plot.data, aes(factor(MODE), ttime.median, fill = factor(YEAR))) + 
                                            geom_bar(stat="identity", position = "dodge") + 
                                            scale_fill_brewer(palette = "Set1")
  pbar.ttime.median.hbw.CBD.both   
  
  
# Statistical analysis 
  # All hbw trips 
  head(tcost.trip.hbw.2011) 
  head(tcost.trip.hbw.1994)
  
  table(tcost.trip.hbw.2011$MODE)
  
  BIKE.hbw.2011.minutes <- tcost.trip.hbw.2011 %>%
    filter(MODE=="BIKE") %>%
    select(tripdur.minutes)
  
  DRIVER.hbw.2011.minutes <- tcost.trip.hbw.2011 %>%
    filter(MODE=="DRIVER") %>%
    select(tripdur.minutes)
  
  PASSENGER.hbw.2011.minutes <- tcost.trip.hbw.2011 %>%
    filter(MODE=="PASSENGER") %>%
    select(tripdur.minutes)
  
  TRANSIT.hbw.2011.minutes <- tcost.trip.hbw.2011 %>%
    filter(MODE=="TRANSIT") %>%
    select(tripdur.minutes)
  
  WALK.hbw.2011.minutes <- tcost.trip.hbw.2011 %>%
    filter(MODE=="WALK") %>%
    select(tripdur.minutes)
  
  
  BIKE.hbw.1994.minutes <- tcost.trip.hbw.1994 %>%
    filter(MODE=="BIKE") %>%
    select(tripdur.minutes)
  
  DRIVER.hbw.1994.minutes <- tcost.trip.hbw.1994 %>%
    filter(MODE=="DRIVER") %>%
    select(tripdur.minutes)
  
  PASSENGER.hbw.1994.minutes <- tcost.trip.hbw.1994 %>%
    filter(MODE=="PASSENGER") %>%
    select(tripdur.minutes)
  
  TRANSIT.hbw.1994.minutes <- tcost.trip.hbw.1994 %>%
    filter(MODE=="TRANSIT") %>%
    select(tripdur.minutes)
  
  WALK.hbw.1994.minutes <- tcost.trip.hbw.1994 %>%
    filter(MODE=="WALK") %>%
    select(tripdur.minutes)
  
  t.test(BIKE.hbw.2011.minutes, BIKE.hbw.1994.minutes, paired = FALSE, var.equal = FALSE)
  
  t.test(DRIVER.hbw.2011.minutes, DRIVER.hbw.1994.minutes, paired = FALSE, var.equal = FALSE)
  
  t.test(PASSENGER.hbw.2011.minutes, PASSENGER.hbw.1994.minutes, paired = FALSE, var.equal = FALSE)
  
  t.test(TRANSIT.hbw.2011.minutes, TRANSIT.hbw.1994.minutes, paired = FALSE, var.equal = FALSE)
  
  t.test(WALK.hbw.2011.minutes, WALK.hbw.1994.minutes, paired = FALSE, var.equal = FALSE)
  
  
  # CBD trips 
  head(tcost.trip.hbw.CBD.both.2011) 
  head(tcost.trip.hbw.CBD.both.1994)
  
  table(tcost.trip.hbw.CBD.both.2011$MODE)
  
  BIKE.hbw.CBD.both.2011.minutes <- tcost.trip.hbw.CBD.both.2011 %>%
    filter(MODE=="BIKE") %>%
    select(tripdur.minutes)
  
  DRIVER.hbw.CBD.both.2011.minutes <- tcost.trip.hbw.CBD.both.2011 %>%
    filter(MODE=="DRIVER") %>%
    select(tripdur.minutes)
  
  PASSENGER.hbw.CBD.both.2011.minutes <- tcost.trip.hbw.CBD.both.2011 %>%
    filter(MODE=="PASSENGER") %>%
    select(tripdur.minutes)
  
  TRANSIT.hbw.CBD.both.2011.minutes <- tcost.trip.hbw.CBD.both.2011 %>%
    filter(MODE=="TRANSIT") %>%
    select(tripdur.minutes)
  
  WALK.hbw.CBD.both.2011.minutes <- tcost.trip.hbw.CBD.both.2011 %>%
    filter(MODE=="WALK") %>%
    select(tripdur.minutes)
  
  
  BIKE.hbw.CBD.both.1994.minutes <- tcost.trip.hbw.CBD.both.1994 %>%
    filter(MODE=="BIKE") %>%
    select(tripdur.minutes)
  
  DRIVER.hbw.CBD.both.1994.minutes <- tcost.trip.hbw.CBD.both.1994 %>%
    filter(MODE=="DRIVER") %>%
    select(tripdur.minutes)
  
  PASSENGER.hbw.CBD.both.1994.minutes <- tcost.trip.hbw.CBD.both.1994 %>%
    filter(MODE=="PASSENGER") %>%
    select(tripdur.minutes)
  
  TRANSIT.hbw.CBD.both.1994.minutes <- tcost.trip.hbw.CBD.both.1994 %>%
    filter(MODE=="TRANSIT") %>%
    select(tripdur.minutes)
  
  WALK.hbw.CBD.both.1994.minutes <- tcost.trip.hbw.CBD.both.1994 %>%
    filter(MODE=="WALK") %>%
    select(tripdur.minutes)
  
  t.test(BIKE.hbw.CBD.both.2011.minutes, BIKE.hbw.CBD.both.1994.minutes, paired = FALSE, var.equal = FALSE)
  
  t.test(DRIVER.hbw.CBD.both.2011.minutes, DRIVER.hbw.CBD.both.1994.minutes, paired = FALSE, var.equal = FALSE)
  
  t.test(PASSENGER.hbw.CBD.both.2011.minutes, PASSENGER.hbw.CBD.both.1994.minutes, paired = FALSE, var.equal = FALSE)
  
  t.test(TRANSIT.hbw.CBD.both.2011.minutes, TRANSIT.hbw.CBD.both.1994.minutes, paired = FALSE, var.equal = FALSE)
  
  t.test(WALK.hbw.CBD.both.2011.minutes, WALK.hbw.CBD.both.1994.minutes, paired = FALSE, var.equal = FALSE)

# Regression analysis 
  
  # 2011 data 
  model.hbw.2011 <- lm(tripdur.minutes ~ distance.miles + as.factor(MODE) + peak, data=tcost.trip.hbw.2011)
  summary(model.hbw.2011)
  
  model.hbw.CBD.both.2011 <- lm(tripdur.minutes ~ distance.miles + as.factor(MODE) + peak, data=tcost.trip.hbw.CBD.both.2011)
  summary(model.hbw.CBD.both.2011)
  
  
  # 1994 data 
  model.hbw.1994 <- lm(tripdur.minutes ~ distance.miles + as.factor(MODE) + peak, data=tcost.trip.hbw.1994)
  summary(model.hbw.1994)
  
  model.hbw.CBD.both.1994 <- lm(tripdur.minutes ~ distance.miles + as.factor(MODE) + peak, data=tcost.trip.hbw.CBD.both.1994)
  summary(model.hbw.CBD.both.1994)
    
  require(stargazer)
  stargazer(model.hbw.1994, model.hbw.2011, model.hbw.CBD.both.1994, model.hbw.CBD.both.2011, type="text")  

  # Structure Break analysis 
    # All hbw trips 
    # Goldfeld-Quandt test
      rss.hbw.1994 <-  sum(model.hbw.1994$resid^2) 
      df.hbw.1994 <- model.hbw.1994$df.residual
      rss.hbw.2011 <-  sum(model.hbw.2011$resid^2) 
      df.hbw.2011 <- model.hbw.2011$df.residual
      GQstat<-(rss.hbw.1994/df.hbw.1994)/(rss.hbw.2011/df.hbw.2011)  # F-test
      GQstat
      1-pf(GQstat, df.hbw.1994, df.hbw.2011)  
    
    # CBD hbe trips   
      rss.hbw.CBD.both.1994 <-  sum(model.hbw.CBD.both.1994$resid^2) 
      df.hbw.CBD.both.1994 <- model.hbw.CBD.both.1994$df.residual
      rss.hbw.CBD.both.2011 <-  sum(model.hbw.CBD.both.2011$resid^2) 
      df.hbw.CBD.both.2011 <- model.hbw.CBD.both.2011$df.residual
      GQstat<-(rss.hbw.CBD.both.1994/df.hbw.CBD.both.1994)/(rss.hbw.CBD.both.2011/df.hbw.CBD.both.2011)  # F-test
      GQstat
      1-pf(GQstat, df.hbw.CBD.both.1994, df.hbw.CBD.both.2011)  
 

      
      
      # # differentiate transit by bus and rail 
      # linkedTrip.metro <- linkedTrip %>% 
      #   left_join(MODE.df) %>%
      #   left_join(hh.metro) %>%
      #   filter(!is.na(AREA)) %>%
      #   mutate(linkedTrip.id=SAMPN*1000 + PERNO*100 + PLANO) %>% 
      #   select(SAMPN, PERNO, PLANO, MODE, MODE.name, ThisMODE, CMMOTTRPDUR, linkedTrip.id) %>%       
      #   arrange(SAMPN, PERNO) 
      # 
      # trip.metro <- trip %>% 
      #   left_join(MODE.df) %>%
      #   left_join(hh.metro) %>%
      #   filter(!is.na(AREA)) %>%
      #   mutate(trip.id=SAMPN*1000 + PERNO*100 + PLANO) %>% # max(trip$PERNO)=8;  max(trip$PLANO)=40
      #   select(SAMPN, PERNO, PLANO, MODE, MODE.name, TRPDUR, trip.id) %>%
      #   left_join(linkedTrip.metro) %>%
      #   mutate(linkedTrip.id.o=linkedTrip.id,
      #          bus=ifelse(MODE.name=="bus", 1, 0), 
      #          rail=ifelse(MODE.name=="rail", 1, 0)) 
      # 
      # # Update linkedTrip.id field 
      # for (i in nrow(trip.metro):1) {
      #   if (!is.na(trip.metro[i, "linkedTrip.id"])) {
      #     trip.metro[i, c("ThisMODE", "linkedTrip.id")] <- trip.metro[i, c("ThisMODE", "linkedTrip.id")]
      #   } else {
      #     trip.metro[i, c("ThisMODE", "linkedTrip.id")] <- trip.metro[i+1, c("ThisMODE", "linkedTrip.id")]
      #   }
      # }
      # 
      # bus.rail <- trip.metro %>% 
      #   filter(ThisMODE %in% c("KNR", "PNR", "TRANSIT")) %>% 
      #   group_by(linkedTrip.id) %>% 
      #   summarise(bus.freq=sum(bus), 
      #             rail.freq=sum(rail)) %>%
      #   mutate(linked.bus=ifelse(bus.freq>0, "BUS", "NOBUS"), 
      #          linked.rail=ifelse(rail.freq>0, "RAIL", "NORAIL")) %>%
      #   select(linkedTrip.id, linked.bus, linked.rail)
      # 
      # trip.metro <- trip.metro %>%
      #   left_join(bus.rail)
      # 
      # linkedTrip.bus.rail <- trip.metro %>% 
      #   filter((ThisMODE %in% c("KNR", "PNR", "TRANSIT"))&!is.na(linkedTrip.id.o)) %>% 
      #   select(SAMPN, PERNO, PLANO, linked.bus, linked.rail)
      #      
       
      # # Differentiate transit by bus and rail 
      # tcost.trip.hbw.2011 <- tcost.trip.hbw.2011 %>%
      #   mutate(MODE.all.transit=MODE) %>%
      #   left_join(linkedTrip.bus.rail) %>%
      #   mutate(MODE=ifelse(linked.bus=="BUS"&!is.na(linked.bus), "BUS", MODE),
      #          MODE=ifelse(linked.rail=="RAIL"&!is.na(linked.bus), "RAIL", MODE)) %>%
      #   filter(MODE!="SCHOOLBUS") %>% # After checking trip by school bus, this maybe a mistake by metro, so remove trip by school bus 
      #   as.data.frame()
  
  
# # Plot     
# table(tcost.trip.CBD.both.1994$MODE.name)
# 
# pbox.tcost.trip.CBD.both.1994 <- ggplot(tcost.trip.CBD.both.1994, aes(MODE.name, tripdur.minutes)) + geom_boxplot() + ylim(0, 75)
# pbox.tcost.trip.CBD.both.1994
# output_file = file.path(OUTPUT_DIR, "boxplot_tcost.trip.CBD.both.1994.png")
# ggsave(pbox.tcost.trip.CBD.both.1994, file=output_file, type="cairo-png")
# 
# pden.tcost.trip.CBD.both.1994 <- ggplot(tcost.trip.CBD.both.1994, aes(tripdur.minutes, colour=MODE.name)) + geom_density() + xlim(0, 100) + ylim(0, 0.15)
# pden.tcost.trip.CBD.both.1994
# output_file = file.path(OUTPUT_DIR, "denplot_tcost.trip.CBD.both.1994.png")
# ggsave(pbox.tcost.trip.CBD.both.1994, file=output_file, type="cairo-png")
# 
# 
# tcost.trip.CBD.both.1994.sum <- tcost.trip.CBD.both.1994 %>% 
#   group_by(MODE.name) %>%
#   summarise(freq.1994=n(),
#             ttime.min.1994=min(tripdur.minutes), 
#             ttime.avg.1994=mean(tripdur.minutes),
#             ttime.median.1994=median(tripdur.minutes),
#             ttime.max.1994=max(tripdur.minutes)) %>%
#   mutate(probs.1994=freq.1994/sum(freq.1994)) %>%
#   rename(MODE=MODE.name)
# 
# head(tcost.trip.CBD.both.1994.sum)
# head(tcost.trip.CBD.both.2011.sum)
# 
# tcost.trip.CBD.both <- tcost.trip.CBD.both.2011.sum %>% 
#   left_join(tcost.trip.CBD.both.1994.sum) %>%
#   select(MODE, freq.1994, freq.2011, ttime.min.1994, ttime.min.2011, ttime.avg.1994, ttime.avg.2011, 
#          ttime.max.1994, ttime.max.2011, probs.1994, probs.2011) %>%
#   arrange(freq.1994) %>%
#   as.data.frame()
# 
# tcost.trip.CBD.both
# 
# output_file = file.path(OUTPUT_DIR, "tcost.trip.CBD.both.RData")
# save(tcost.trip.CBD.both, file=output_file)



# # Plot
# pbox.tcost.trip.CBD.both.2011 <- ggplot(tcost.trip.CBD.both.2011, aes(MODE.name, tripdur.minutes)) + geom_boxplot()
# output_file = file.path(OUTPUT_DIR, "boxplot_tcost.trip.CBD.both.2011.png")
# ggsave(pbox.tcost.trip.CBD.both.2011, file=output_file, type="cairo-png")
# 
# pbar.tcost.trip.CBD.both.2011 <- ggplot(tcost.trip.CBD.both.2011, aes(MODE.name)) + geom_bar()
# output_file = file.path(OUTPUT_DIR, "barplot_tcost.trip.CBD.both.2011.png")
# ggsave(pbar.tcost.trip.CBD.both.2011, file=output_file, type="cairo-png")
# 
# pbox.tcost.trip.CBD.to.2011 <- ggplot(tcost.trip.CBD.to.2011, aes(MODE.name, tripdur.minutes)) + geom_boxplot()
# output_file = file.path(OUTPUT_DIR, "boxplot_tcost.trip.CBD.to.2011.png")
# ggsave(pbox.tcost.trip.CBD.to.2011, file=output_file, type="cairo-png")
# 
# pbar.tcost.trip.CBD.to.2011 <- ggplot(tcost.trip.CBD.to.2011, aes(MODE.name)) + geom_bar()
# output_file = file.path(OUTPUT_DIR, "barplot_tcost.trip.CBD.to.2011.png")
# ggsave(pbar.tcost.trip.CBD.to.2011, file=output_file, type="cairo-png")
# 
# pbox.tcost.trip.CBD.from.2011 <- ggplot(tcost.trip.CBD.from.2011, aes(MODE.name, tripdur.minutes)) + geom_boxplot()
# output_file = file.path(OUTPUT_DIR, "boxplot_tcost.trip.CBD.from.2011.png")
# ggsave(pbox.tcost.trip.CBD.from.2011, file=output_file, type="cairo-png")
# 
# pbar.tcost.trip.CBD.from.2011 <- ggplot(tcost.trip.CBD.from.2011, aes(MODE.name)) + geom_bar()
# output_file = file.path(OUTPUT_DIR, "barplot_tcost.trip.CBD.from.2011.png")
# ggsave(pbar.tcost.trip.CBD.from.2011, file=output_file, type="cairo-png")



# # Plot data   
# pbox.tcost.trip.CBD.both.1994 <- ggplot(tcost.trip.CBD.both.1994, aes(MODE.name, tripdur.minutes)) + geom_boxplot()
# output_file = file.path(OUTPUT_DIR, "boxplot_tcost.trip.CBD.both.1994.png")
# ggsave(pbox.tcost.trip.CBD.both.1994, file=output_file, type="cairo-png")
# 
# pbar.tcost.trip.CBD.both.1994 <- ggplot(tcost.trip.CBD.both.1994, aes(MODE.name)) + geom_bar()
# output_file = file.path(OUTPUT_DIR, "barplot_tcost.trip.CBD.both.1994.png")
# ggsave(pbar.tcost.trip.CBD.both.1994, file=output_file, type="cairo-png")
# 
# pbox.tcost.trip.CBD.to.1994 <- ggplot(tcost.trip.CBD.to.1994, aes(MODE.name, tripdur.minutes)) + geom_boxplot()
# output_file = file.path(OUTPUT_DIR, "boxplot_tcost.trip.CBD.to.1994.png")
# ggsave(pbox.tcost.trip.CBD.to.1994, file=output_file, type="cairo-png")
# 
# pbar.tcost.trip.CBD.to.1994 <- ggplot(tcost.trip.CBD.to.1994, aes(MODE.name)) + geom_bar()
# output_file = file.path(OUTPUT_DIR, "barplot_tcost.trip.CBD.to.1994.png")
# ggsave(pbar.tcost.trip.CBD.to.1994, file=output_file, type="cairo-png")
# 
# pbox.tcost.trip.CBD.from.1994 <- ggplot(tcost.trip.CBD.from.1994, aes(MODE.name, tripdur.minutes)) + geom_boxplot()
# output_file = file.path(OUTPUT_DIR, "boxplot_tcost.trip.CBD.from.1994.png")
# ggsave(pbox.tcost.trip.CBD.from.1994, file=output_file, type="cairo-png")
# 
# pbar.tcost.trip.CBD.from.1994 <- ggplot(tcost.trip.CBD.from.1994, aes(MODE.name)) + geom_bar()
# output_file = file.path(OUTPUT_DIR, "barplot_tcost.trip.CBD.from.1994.png")
# ggsave(pbar.tcost.trip.CBD.from.1994, file=output_file, type="cairo-png")
# 



##################
# pie(tcost.trip.CBD.both.1994.sum$freq, tcost.trip.CBD.both.1994$MODE.name)
# tcost.trip.CBD.both.2011.sum
# 
# p1 <- ggplot(tcost.trip.CBD.both.1994, aes(x=MODE.name, y=tripdur.minutes, fill=MODE.name)) + ylim(0,125) + geom_boxplot()
# p2 <- ggplot(tcost.trip.CBD.both.2011, aes(x=MODE.name, y=tripdur.minutes, fill=MODE.name)) + ylim(0,125) + geom_boxplot()
# 
# 
# 
# 
# multiplot(p1,p2, cols=2)
# 
# detach("package:Rmisc", unload=TRUE)
# detach("package:plyr", unload=TRUE)
# 
# ggplot(test, aes(x=MODE.name, y=tripdur.minutes, fill=MODE.name)) + geom_boxplot() + facet_grid(. ~ Year)
# 
# ggplot(test, aes(x=MODE.name, fill=Year)) +
#   geom_histogram(binwidth=.5, position="dodge")
# 
# 
# ############################################################
# ############################################################
# 
# tcost.trip.CBD.both.1994 <- tcost.trip.CBD.both.1994 %>%
#   mutate(PLANO=ACTNO,
#          Year=1994,
#          Dir="both") %>%
#   select(SAMPN, PERNO, PLANO, TripPurpose, MODE, MODE.name, tripdur.minutes, Year, Dir)
# 
# tcost.trip.CBD.both.2011 <- tcost.trip.CBD.both.2011 %>%
#   mutate(Year=2011,
#          Dir="both") %>%
#   select(SAMPN, PERNO, PLANO, TripPurpose, MODE, MODE.name, tripdur.minutes, Year, Dir)
#   
#   
#   
#   
  
  
# ############################################################################################################
# ############################################################################################################
# ############################################################################################################
# ############################################################################################################
# ############################################################################################################
# ############################################################################################################
# ############################################################################################################
# ############################################################################################################
# # > head(tcost.trip.CBD.both.2011)
# # SAMPN PERNO PLANO MODE   MODE.name tripdur.minutes LastTAZ ThisTAZ
# # 1 8000590     3     2    3 auto driver              35     754       8
# # 2 8000631     1     2    3 auto driver              25     709       6
# # 3 8001160     1     4    1        walk               3       1       1
# # 4 8001160     1     6    1        walk               3       8       1
# # 5 8001955     1     2    3 auto driver              34     749       8
# # 6 8002557     1     2    3 auto driver              40     896       4
# # 
# 
# table(linkedTrip$MODE)
# table(trip$MODE)
# MODE.df
# head(tcost.trip.CBD.both.2011)
# trip %>% 
#   
#   
#   tcost.trip[which(tcost.trip$SAMPN==8001160), ]  
# 
# head(tcost.trip.CBD.both.2011)
# 
# test1 <- trip[which(trip$SAMPN==8001160), c("SAMPN", "PERNO", "PLANO", "MODE", "AGGACT", "TRPDUR")]  
# 
# test2 <-   linkedTrip[which(linkedTrip$SAMPN==8001160), c("SAMPN", "PERNO", "PLANO", "MODE", "AGGACT", "TRPDUR")]  
# 
# test1 <- trip[which(trip$SAMPN %in% c(8000590, 8000631, 8001160, 8001955)), c("SAMPN", "PERNO", "PLANO", "MODE", "AGGACT", "TRPDUR")] %>%   
#   mutate(trip.id = SAMPN*100 + PERNO*100 + PLANO)
# 
# 
# test2 <- linkedTrip[which(linkedTrip$SAMPN %in% c(8000590, 8000631, 8001160, 8001955)), c("SAMPN", "PERNO", "PLANO", "TripPurpose")] %>% 
#   mutate(linkedTrip.id = SAMPN*100 + PERNO*100 + PLANO)
# 
# test3 <- test1 %>% 
#   left_join(test2) %>% 
#   mutate(linkedTrip.id.o=linkedTrip.id)
# summary(trip[, c("PERNO", "PLANO")])
# summary(linkedTrip[, c("PERNO", "PLANO")])
# 
# # It works 
# for (i in nrow(test3):1) {
#   if (!is.na(test3[i, "linkedTrip.id"])) {
#     test3[i, "linkedTrip.id"] <- test3[i, "linkedTrip.id"]
#   } else {
#     
#     test3[i, "linkedTrip.id"] <- test3[i+1, "linkedTrip.id"]
#   }
# }
# 
# c(nrow(test3):1)
# 
# 
# test3.max.trpdur <- test3 %>%
#   group_by(linkedTrip.id) %>%
#   summarise(TRPDUR.MAX = max(TRPDUR), 
#             TRPDUR.SUM = sum(TRPDUR),
#             AGGACT.MAIN = last(AGGACT)) %>%
#   as.data.frame()
# 
# 
# test3 <- test3 %>% 
#   left_join(test3.max.trpdur) %>% 
#   mutate(main.mode=ifelse(TRPDUR==TRPDUR.MAX, MODE, NA)) %>%
#   group_by(linkedTrip.id) %>%
#   summarise(linkedTrip.mode = ) 
# 
# linkedTrip.main.mode <- test3 %>% 
#   filter(!is.na(main.mode)) %>% 
#   select(linkedTrip.id, main.mode)
# 
# linkedTrip.main.mode <- test3 %>% 
#   filter(!is.na(main.mode)) %>% 
#   select(linkedTrip.id, main.mode)
# 
# linkedTrip.hbw <- test3 %>% 
#   filter(TripPurpose=="HBW") %>%
#   select(linkedTrip.id, TRPDUR.SUM)
# 
# test3 %>% 
#   left_join(linkedTrip.main.mode)
# 
# 
# # > head(tcost.trip.CBD.both.2011)
# # SAMPN PERNO PLANO MODE   MODE.name tripdur.minutes LastTAZ ThisTAZ
# # 1 8000590     3     2    3 auto driver              35     754       8
# # 2 8000631     1     2    3 auto driver              25     709       6
# # 3 8001160     1     4    1        walk               3       1       1
# # 4 8001160     1     6    1        walk               3       8       1
# # 5 8001955     1     2    3 auto driver              34     749       8
# # 6 8002557     1     2    3 auto driver              40     896       4
# # 
# # 
# # table(linkedTrip$MODE)
# # table(trip$MODE)
# # MODE.df
# # head(tcost.trip.CBD.both.2011)
# # trip %>% 
# #   
# #   
# #   tcost.trip[which(tcost.trip$SAMPN==8001160), ]  
# # 
# # head(tcost.trip.CBD.both.2011)
# 
# # test1 <- trip[which(trip$SAMPN==8001160), c("SAMPN", "PERNO", "PLANO", "MODE", "AGGACT", "TRPDUR")]  
# #  
# #  test2 <-   linkedTrip[which(linkedTrip$SAMPN==8001160), c("SAMPN", "PERNO", "PLANO", "MODE", "AGGACT", "TRPDUR")]  
# #  
# #  test1 <- trip[which(trip$SAMPN %in% c(8000590, 8000631, 8001160, 8001955)), c("SAMPN", "PERNO", "PLANO", "MODE", "AGGACT", "TRPDUR")] %>%   
# #           mutate(trip.id = SAMPN*100 + PERNO*100 + PLANO)
# #    
# #  
# #   test2 <- linkedTrip[which(linkedTrip$SAMPN %in% c(8000590, 8000631, 8001160, 8001955)), c("SAMPN", "PERNO", "PLANO", "TripPurpose")] %>% 
# #            mutate(linkedTrip.id = SAMPN*100 + PERNO*100 + PLANO)
# # 
# #   test3 <- test1 %>% 
# #            left_join(test2) %>% 
# #            mutate(linkedTrip.id.o=linkedTrip.id)
# # summary(trip[, c("PERNO", "PLANO")])
# # summary(linkedTrip[, c("PERNO", "PLANO")])
# # 
# 
# 
# trip.sub <- trip %>% 
#   select(SAMPN, PERNO, PLANO, MODE, AGGACT, TRPDUR) %>%
#   mutate(trip.id = SAMPN*100 + PERNO*100 + PLANO)
# head(trip.sub)
# 
# linkedTrip.sub <- linkedTrip %>% 
#   select(SAMPN, PERNO, PLANO, TripPurpose, LastXCORD, LastYCORD, ThisXCORD, ThisYCORD) %>% 
#   mutate(linkedTrip.id = SAMPN*100 + PERNO*100 + PLANO)
# 
# trip.linkedTrip.sub <- trip.sub %>%
#   left_join(linkedTrip.sub) %>%
#   arrange(trip.id)
# head(trip.linkedTrip.sub)
# 
# 
# # # It works 
# #  for (i in nrow(test3):1) {
# #    if (!is.na(test3[i, "linkedTrip.id"])) {
# #      test3[i, "linkedTrip.id"] <- test3[i, "linkedTrip.id"]
# #    } else {
# #      
# #      test3[i, "linkedTrip.id"] <- test3[i+1, "linkedTrip.id"]
# #    }
# #  }
# #  
# #  c(nrow(test3):1)
# 
# 
# for (i in nrow(trip.linkedTrip.sub):1) {
#   if (!is.na(trip.linkedTrip.sub[i, "linkedTrip.id"])) {
#     trip.linkedTrip.sub[i, "linkedTrip.id"] <- trip.linkedTrip.sub[i, "linkedTrip.id"]
#   } else {
#     
#     trip.linkedTrip.sub[i, "linkedTrip.id"] <- trip.linkedTrip.sub[i+1, "linkedTrip.id"]
#   }
# }
# 
# trip.linkedTrip.sub.copy <- trip.linkedTrip.sub
# trip.linkedTrip.sub <- trip.linkedTrip.sub.copy 
# # test3.max.trpdur <- test3 %>%
# #                     group_by(linkedTrip.id) %>%
# #                     summarise(TRPDUR.MAX = max(TRPDUR), 
# #                               TRPDUR.SUM = sum(TRPDUR),
# #                               AGGACT.MAIN = last(AGGACT)) %>%
# #                     as.data.frame()
# 
# linkedTrip.max.trpdur <- trip.linkedTrip.sub %>% 
#   group_by(linkedTrip.id) %>%
#   summarise(TRPDUR.MAX = max(TRPDUR), 
#             TRPDUR.SUM = sum(TRPDUR))
# 
# # test3 <- test3 %>% 
# #           left_join(test3.max.trpdur) %>% 
# #           mutate(main.mode=ifelse(TRPDUR==TRPDUR.MAX, MODE, NA)) %>%
# #           group_by(linkedTrip.id) %>%
# #           summarise(linkedTrip.mode = ) 
# 
# trip.linkedTrip.sub <- trip.linkedTrip.sub %>% 
#   left_join(linkedTrip.max.trpdur) %>%
#   mutate(main.mode=ifelse(TRPDUR==TRPDUR.MAX, MODE, NA))
# 
# # linkedTrip.main.mode <- test3 %>% 
# #                         filter(!is.na(main.mode)) %>% 
# #                         select(linkedTrip.id, main.mode)
# 
# linkedTrip.main.mode <- trip.linkedTrip.sub %>% 
#   filter(!is.na(main.mode)) %>% 
#   select(linkedTrip.id, main.mode)
# 
# 
# 
# # linkedTrip.hbw <- test3 %>% 
# #                   filter(TripPurpose=="HBW") %>%
# #                   select(linkedTrip.id, TRPDUR.SUM)
# 
# 
# trip.linkedTrip.sub.hbw <-  trip.linkedTrip.sub %>% 
#   filter(TripPurpose=="HBW") %>%
#   select(SAMPN, PERNO, PLANO, linkedTrip.id, TRPDUR.SUM, LastXCORD, LastYCORD, ThisXCORD, ThisYCORD) %>%
#   left_join(linkedTrip.main.mode) 
# 
# 
# # trip.linkedTrip.sub.hbw %>% 
# #   left_join(linkedTrip.main.mode)

# linkedTrip.sub <- linkedTrip %>% 
#   select(SAMPN, PERNO, PLANO, TripPurpose, LastXCORD, LastYCORD, ThisXCORD, ThisYCORD) %>% 
#   mutate(linkedTrip.id = SAMPN*100 + PERNO*100 + PLANO)
# 
# trip.sub <- trip %>% 
#   select(SAMPN, PERNO, PLANO, MODE, AGGACT, TRPDUR) %>%
#   mutate(trip.id = SAMPN*100 + PERNO*100 + PLANO)
# head(trip.sub)
# 
# trip.linkedTrip.sub <- trip.sub %>%
#   left_join(linkedTrip.sub) %>%
#   arrange(trip.id)
# head(trip.linkedTrip.sub)
# 
# 
# #  for (i in nrow(test3):1) {
# #    if (!is.na(test3[i, "linkedTrip.id"])) {
# #      test3[i, "linkedTrip.id"] <- test3[i, "linkedTrip.id"]
# #    } else {
# #      
# #      test3[i, "linkedTrip.id"] <- test3[i+1, "linkedTrip.id"]
# #    }
# #  }
# #  
# #  c(nrow(test3):1)
# 
# 
# for (i in nrow(trip.linkedTrip.sub):1) {
#   if (!is.na(trip.linkedTrip.sub[i, "linkedTrip.id"])) {
#     trip.linkedTrip.sub[i, "linkedTrip.id"] <- trip.linkedTrip.sub[i, "linkedTrip.id"]
#   } else {
#     
#     trip.linkedTrip.sub[i, "linkedTrip.id"] <- trip.linkedTrip.sub[i+1, "linkedTrip.id"]
#   }
# }
# 
# trip.linkedTrip.sub.copy <- trip.linkedTrip.sub
# # trip.linkedTrip.sub <- trip.linkedTrip.sub.copy 
# # test3.max.trpdur <- test3 %>%
# #                     group_by(linkedTrip.id) %>%
# #                     summarise(TRPDUR.MAX = max(TRPDUR), 
# #                               TRPDUR.SUM = sum(TRPDUR),
# #                               AGGACT.MAIN = last(AGGACT)) %>%
# #                     as.data.frame()
# 
# linkedTrip.sum.trpdur <- trip.linkedTrip.sub %>% 
#   group_by(linkedTrip.id) %>%
#   summarise(# TRPDUR.MAX = max(TRPDUR), 
#             TRPDUR.SUM = sum(TRPDUR))
# 
# tcost.trip <- tcost.trip %>%
#               left_join(linkedTrip.sum.trpdur) %>%
#               mutate(tripdur.minutes=TRPDUR.SUM,
#                      tripdist.miles=DistanceRoute/5280
#               )
# 
# test3 <- test3 %>% 
#           left_join(test3.max.trpdur) %>% 
#           mutate(main.mode=ifelse(TRPDUR==TRPDUR.MAX, MODE, NA)) %>%
#           group_by(linkedTrip.id) %>%
#           summarise(linkedTrip.mode = ) 

# trip.linkedTrip.sub <- trip.linkedTrip.sub %>% 
#   left_join(linkedTrip.max.trpdur) %>%
#   mutate(main.mode=ifelse(TRPDUR==TRPDUR.MAX, MODE, NA))
# 
# # linkedTrip.main.mode <- test3 %>% 
# #                         filter(!is.na(main.mode)) %>% 
# #                         select(linkedTrip.id, main.mode)
# 
# linkedTrip.main.mode <- trip.linkedTrip.sub %>% 
#   filter(!is.na(main.mode)) %>% 
#   select(linkedTrip.id, main.mode)
# 
# 
# 
# linkedTrip.hbw <- test3 %>% 
#                   filter(TripPurpose=="HBW") %>%
#                   select(linkedTrip.id, TRPDUR.SUM)



# trip.linkedTrip.sub.hbw <-  trip.linkedTrip.sub %>% 
#   filter(TripPurpose=="HBW") %>%
#   select(SAMPN, PERNO, PLANO, linkedTrip.id, TRPDUR.SUM, LastXCORD, LastYCORD, ThisXCORD, ThisYCORD) %>%
#   left_join(linkedTrip.main.mode) 


# trip.linkedTrip.sub.hbw %>% 
#   left_join(linkedTrip.main.mode)


# tcost.trip <- linkedTrip %>% 
#   left_join(hh.metro) %>%
#   filter(!is.na(AREA)) %>%
#   select(SAMPN, PERNO, PLANO, AREA, HHWGT, TripPurpose, MODE, TRPDUR, DistanceRoute, LastXCORD, LastYCORD, ThisXCORD, ThisYCORD) %>%
#   mutate(TripPurpose = tolower(TripPurpose),
#          TripPurpose=ifelse(TripPurpose=="hbshp", "hbs", TripPurpose),
#          TripPurpose=ifelse(TripPurpose=="hbrec", "hbr", TripPurpose)
#          #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
#          #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
#   ) %>%
#   filter( TripPurpose %in% c("hbw", "hbs", "hbr", "hbo")) %>%
#   mutate(tripdur.minutes=TRPDUR,
#          tripdist.miles=DistanceRoute/5280
#   ) %>% 
#   arrange(SAMPN, PERNO)

# tcost.trip <- trip.linkedTrip.sub.hbw



