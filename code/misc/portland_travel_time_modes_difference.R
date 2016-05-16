# check travel time and travel modes difference between 1994 and 2011 
# Settings 
require(ggplot2)

# Open district shapefile, TAZ shapefile of 1994 and 2011 in QGIS
# Identify 1:26 TAZs for 2011, 1:16 TAZs for 1994

# 2011 OHAS data  
# Settings 
  INPUT_DIR <- "data/Portland/2011"
  OUTPUT_DIR <- "output/PortlandTrips_1994_VS_2011"
  TAZ.shpfile <- file.path(INPUT_DIR, "shp/TAZ.shp")
  TAZ.id_name <- "newtaz"

# Source required scripts 
  source("code/functions.R")

# Load data 
  # Use ThisMODE instead of MODE 
  load("data/Portland/2011/OHAS_Final.Rdata") # linkedTrip only removes ChangeMode trip  
  
  MODE.df <- data.frame(MODE=c(1:10,97), 
                        MODE.name=c("walk", "bike", "auto driver", "auto passenger", "bus", "rail", 
                                    "dial-a-ride", "taxi", "school bus", "carpool/vanpool", "other(specify)"))
  
  trip <- trip %>% 
          left_join(MODE.df)
  
  linkedTrip <- linkedTrip%>% 
                left_join((MODE.df))
  
  hh.metro <- hh %>% filter(AREA==11)
 
  tcost.trip <- linkedTrip %>% 
                left_join(hh.metro) %>%
                filter(!is.na(AREA)) %>%
                filter(AGGACT %in% c("Work", "WorkRelated")) %>%
                select(SAMPN, PERNO, PLANO, AREA, ThisMODE, TRPDUR, CMMOTTRPDUR, DistanceRoute, LastXCORD, LastYCORD, ThisXCORD, ThisYCORD) %>%
                rename(MODE=ThisMODE) %>%
                arrange(SAMPN, PERNO) %>% 
                mutate(linkedTrip.id = SAMPN*100 + PERNO*100 + PLANO)
  
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
  summary(tcost.trip.ThisTAZ)
  
  # "auto driver" is short of "auto/van/truck driver"
  # "auto passenger" is short of "auto/van/truck passenger"
  # "dial-a-ride" is short of "dial-a-ride/paratransit"
  
    
  # mutate(MODE=factor(MODE, levels=c(1:10,97), 
  #                    labels=c("walk", "bike", "auto driver", "auto passenger", "bus", "rail", 
  #                             "dial-a-ride", "taxi", "school bus", "carpool/vanpool", "other(specify)")))%>%

  tcost.trip <- tcost.trip %>% 
                left_join(tcost.trip.LastTAZ) %>%
                select(-x, -y) %>%
                left_join(tcost.trip.ThisTAZ) %>%
                select(-x, -y) %>%
                #left_join(MODE.df) %>%
                select(SAMPN, PERNO, PLANO, MODE, TRPDUR, CMMOTTRPDUR, LastTAZ, ThisTAZ)
  
  tcost.trip.CBD.both.2011 <- tcost.trip %>% 
                                filter((LastTAZ %in% c(1:26)|ThisTAZ %in% c(1:26)))
  
  tcost.trip.CBD.from.2011 <- tcost.trip %>% 
                                filter(LastTAZ %in% c(1:26))
  
  tcost.trip.CBD.to.2011 <- tcost.trip %>% 
                                filter(ThisTAZ %in% c(1:26))
  
# Ues CMMOTTRPDUR for transit (KNR, PNR, TRANSIT), TRPDUR for for non transit modes
  # TRPDUR, CMMOTTRPDUR,
  
  tcost.trip.CBD.both.2011.transit <- tcost.trip.CBD.both.2011 %>% 
    filter(MODE %in% c("KNR", "PNR", "TRANSIT")) %>%
    mutate(tripdur.minutes = CMMOTTRPDUR)  %>% 
    select(SAMPN, PERNO, PLANO, MODE, tripdur.minutes)
  
  
  tcost.trip.CBD.both.2011.notransit <- tcost.trip.CBD.both.2011 %>% 
    filter(!(MODE %in% c("KNR", "PNR", "TRANSIT"))) %>%
    mutate(tripdur.minutes = TRPDUR) %>%
    select(SAMPN, PERNO, PLANO, MODE, tripdur.minutes)
  
  tcost.trip.CBD.both.2011 <- rbind(tcost.trip.CBD.both.2011.transit, tcost.trip.CBD.both.2011.notransit)
  
  
  pbox.tcost.trip.CBD.both.2011 <- ggplot(tcost.trip.CBD.both.2011, aes(MODE, tripdur.minutes)) + geom_boxplot() + ylim(0, 75)
  pbox.tcost.trip.CBD.both.2011
  output_file = file.path(OUTPUT_DIR, "boxplot_tcost.trip.CBD.both.1994.png")
  ggsave(pbox.tcost.trip.CBD.both.2011, file=output_file, type="cairo-png")
  
  pden.tcost.trip.CBD.both.2011 <- ggplot(tcost.trip.CBD.both.2011, aes(tripdur.minutes, colour=MODE)) + geom_density() + xlim(0,100) + ylim(0, 0.15)
  pden.tcost.trip.CBD.both.2011
  output_file = file.path(OUTPUT_DIR, "denplot_tcost.trip.CBD.both.1994.png")
  ggsave(pbox.tcost.trip.CBD.both.2011, file=output_file, type="cairo-png")
  
  
  tcost.trip.CBD.both.2011.sum <- tcost.trip.CBD.both.2011 %>%
    group_by(MODE) %>%
    summarise(freq.2011=n(),
              ttime.min.2011=min(tripdur.minutes), 
              ttime.avg.2011=mean(tripdur.minutes),
              ttime.median.2011=median(tripdur.minutes),
              ttime.max.2011=max(tripdur.minutes)) %>%
    mutate(probs.2011=freq.2011/sum(freq.2011))
  

  #   tcost.trip.both <- tcost.trip %>% 
#                     filter(LastTAZ %in% c(1:26)|ThisTAZ %in% c(1:26)) %>%
#                     left_join(mode.df) %>%
#                     group_by(MODE.name) %>%
#                     summarise(tripdur.minutes.mean = mean(tripdur.hours)*60, 
#                               tripdist.miles.mean = mean(tripdist.miles, na.rm=TRUE), 
#                               trips.freq=n()) %>%
#                     mutate(type="both",
#                               trips.freq.share=trips.freq/sum(trips.freq)*100)
#   
#   tcost.trip.from <- tcost.trip %>% 
#                      filter(LastTAZ %in% c(1:26)) %>%
#                      left_join(mode.df) %>%
#                      group_by(MODE.name) %>%
#                      summarise(tripdur.minutes.mean = mean(tripdur.hours)*60, 
#                                tripdist.miles.mean = mean(tripdist.miles, na.rm=TRUE), 
#                                trips.freq=n()) %>%
#                      mutate(type="from",
#                                 trips.freq.share=trips.freq/sum(trips.freq)*100)
#   
#   tcost.trip.to <- tcost.trip %>% 
#                      filter(ThisTAZ %in% c(1:26)) %>%
#                      left_join(mode.df) %>%
#                      group_by(MODE.name) %>%
#                      summarise(tripdur.minutes.mean = mean(tripdur.hours)*60, 
#                                 tripdist.miles.mean = mean(tripdist.miles, na.rm=TRUE), 
#                                 trips.freq=n()) %>%
#                      mutate(type="to",
#                              trips.freq.share=trips.freq/sum(trips.freq)*100)
#                     
#   tcost.trip.CBD.2011 <- rbind(tcost.trip.both, tcost.trip.from, tcost.trip.to) %>%
#                                mutate(year=2011) %>% 
#                                as.data.frame()
#   
#   save(tcost.trip.CBD.2011, file="data/Portland/2011/tcost.trip.CBD.2011.RData")


  
  rm(list= ls()[!(ls() %in% c('tcost.trip.CBD.both.2011','tcost.trip.CBD.from.2011', 
                              'tcost.trip.CBD.to.2011', 'tcost.trip.CBD.both.2011.sum',
                              'OUTPUT_DIR', "pbox.tcost.trip.CBD.both.2011", "pden.tcost.trip.CBD.both.2011"))])
# 1994 OHAS 
  # CBD includes TAZ 1:16, these TAZs are within district 1
  # Settings 

  load("data/Portland/1994/linkedTrip.RData")
  modes.df <- data.frame(MODE=c(1:8), MODE.name=c("other", "walk", "bicycle", "school bus", "public bus", "MAX", 
                                                  "personal vehicle", "non-personal vehicle"))
  
  tcost.trip <- linkedTrip %>% 
    select(SAMPN, HHWGT, PERNO, ACTNO, TripPurpose, MODE, TRPDUR, DistanceRoute, HTAZ, LastTAZ, TAZ) %>%
    mutate(TripPurpose = tolower(TripPurpose),
           TripPurpose=ifelse(TripPurpose=="hbshp", "hbs", TripPurpose),
           TripPurpose=ifelse(TripPurpose=="hbrec", "hbr", TripPurpose)
           #TripPurpose=ifelse(TripPurpose=="hbsch", "hbo", TripPurpose),                #HB School trips ==> HBO trips
           #TripPurpose=ifelse(str_detect(TripPurpose, "^hb.*esc$"), "hbo", TripPurpose) #HB Escort trips ==> HBO trips
    ) %>%
    filter( TripPurpose %in% c("hbw", "hbs", "hbr", "hbo")) %>%
    mutate(tripdur.minutes=TRPDUR,
           tripdist.miles=DistanceRoute/5280
    ) %>% 
    filter(!is.na(MODE)) %>% 
    left_join(modes.df)
  
    
  # mutate(MODE = factor(MODE, levels=c(1:8), 
  #                     labels=c("Other", "Walk", "Bicycle", "School Bus", "Public Bus", "MAX", 
  #                             "Personal Vehicle", "Non-personal Vehicle")))
  
  tcost.trip.CBD.both.1994 <- tcost.trip %>% 
                              filter((TAZ %in% c(1:16)|LastTAZ %in% c(1:16))) 
  
  tcost.trip.CBD.to.1994 <- tcost.trip %>%
                             filter(TAZ %in% c(1:16)) 
  
  tcost.trip.CBD.from.1994 <- tcost.trip %>%
                               filter(LastTAZ %in% c(1:16))


# Comparison 
#   load("data/Portland/2011/tcost.trip.CBD.2011.RData")
#   load("data/Portland/1994/tcost.trip.CBD.1994.RData")
#   
#   tcost.trip.CBD <- rbind(tcost.trip.CBD.1994, tcost.trip.CBD.2011)
#   
#   tcost.trip.CBD
  
  
# ######################################
#   require(Rmisc)
#   mode.df <- data.frame(MODE=c(1:10,97), 
#                         MODE.name=c(  
#                           "dial-a-ride", "taxi", , "carpool/vanpool", ))
#   
#   "walk", "bike",;;;;;;"Walk", "Bicycle",
#   "auto driver", ;;;;;;  "Personal Vehicle"
#   "school bus";;;;"School Bus"
#   "bus", ;;;;"Public Bus"
#   "rail", , "MAX",
#   "other(specify" ;;; "Other"
#   "auto passenger", ;;;;;;;;;"Non-personal Vehicle")

tcost.trip.CBD.both.1994 <- tcost.trip.CBD.both.1994 %>%
  mutate(MODE.name=as.character(MODE.name)) %>%
  mutate(MODE.name=ifelse(MODE.name=="MAX", "TRANSIT", MODE.name)) %>%
  mutate(MODE.name=ifelse(MODE.name=="public bus", "TRANSIT", MODE.name)) %>%
  mutate(MODE.name=ifelse(MODE.name=="other", "other(specify)", MODE.name)) %>%
  mutate(MODE.name=ifelse(MODE.name=="non-personal vehicle", "PASSENGER", MODE.name)) %>%
  mutate(MODE.name=ifelse(MODE.name=="bicycle", "BIKE", MODE.name)) %>%
  mutate(MODE.name=ifelse(MODE.name=="personal vehicle", "DRIVER", MODE.name)) %>%
  mutate(MODE.name=toupper(MODE.name)) 
  
table(tcost.trip.CBD.both.1994$MODE.name)

pbox.tcost.trip.CBD.both.1994 <- ggplot(tcost.trip.CBD.both.1994, aes(MODE.name, tripdur.minutes)) + geom_boxplot() + ylim(0, 75)
pbox.tcost.trip.CBD.both.1994
output_file = file.path(OUTPUT_DIR, "boxplot_tcost.trip.CBD.both.1994.png")
ggsave(pbox.tcost.trip.CBD.both.1994, file=output_file, type="cairo-png")

pden.tcost.trip.CBD.both.1994 <- ggplot(tcost.trip.CBD.both.1994, aes(tripdur.minutes, colour=MODE.name)) + geom_density() + xlim(0, 100) + ylim(0, 0.15)
pden.tcost.trip.CBD.both.1994
output_file = file.path(OUTPUT_DIR, "denplot_tcost.trip.CBD.both.1994.png")
ggsave(pbox.tcost.trip.CBD.both.1994, file=output_file, type="cairo-png")


tcost.trip.CBD.both.1994.sum <- tcost.trip.CBD.both.1994 %>% 
  group_by(MODE.name) %>%
  summarise(freq.1994=n(),
            ttime.min.1994=min(tripdur.minutes), 
            ttime.avg.1994=mean(tripdur.minutes),
            ttime.median.1994=median(tripdur.minutes),
            ttime.max.1994=max(tripdur.minutes)) %>%
  mutate(probs.1994=freq.1994/sum(freq.1994)) %>%
  rename(MODE=MODE.name)

head(tcost.trip.CBD.both.1994.sum)
head(tcost.trip.CBD.both.2011.sum)

tcost.trip.CBD.both <- tcost.trip.CBD.both.2011.sum %>% 
  left_join(tcost.trip.CBD.both.1994.sum) %>%
  select(MODE, freq.1994, freq.2011, ttime.min.1994, ttime.min.2011, ttime.avg.1994, ttime.avg.2011, 
         ttime.max.1994, ttime.max.2011, probs.1994, probs.2011) %>%
  arrange(freq.1994) %>%
  as.data.frame()

tcost.trip.CBD.both

output_file = file.path(OUTPUT_DIR, "tcost.trip.CBD.both.RData")
save(tcost.trip.CBD.both, file=output_file)



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



