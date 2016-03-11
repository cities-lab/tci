# This script calculate Tracel Cost Index (TCI) with hybrid method 

# Load required packages
  require(ggmap)
  require(scales)
  require(kimisc)

# Settings
  var_list.0 <- ls()
  method.name <- "hybrid"
  project.name <- "Portland"
  year <- ""
  unit.name <- 'minutes'
  scenario.name <- "2010"

  # Define directory 
  INPUT_DIR <- file.path('data', project.name, scenario.name)  
  
  source("code/functions.R")
  source("code/thirdparty/omx.r")
  
  # Define income group abbreviation
  Ic <- c("lowInc", "midInc", "highInc")
  
  # Define trip purpose abbreviation
  # The purposes for this study (now) are limited to the home-based trips
  # They exclude nonhome-based trips, school trips and college trips
  Pr <- c("hbw", "hbs", "hbr", "hbo")
  
  # Define the travel modes
  Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")
  
  # Define time period 
  Tp <- c("peak", "offpeak")
  
# Calculation the compostion of trips by reip purposes 
  path.tdm <- file.path(INPUT_DIR, "TDM")
  file.trips <- file.path(path.tdm, "PrIcMdODTrips.omx")
  listOMX(file.trips)
  
# Calculate trip probability by trip purposes   
trips.sum.pr <- c(hbw=0, hbs=0, hbr=0, hbo=0)

for (pr in Pr){
  trips.pr <- NULL
  
  for (ic in Ic) {
    trips.pr.ic <- NULL
    
    for (md in Md) {
      trips.name <- paste0(pr, ic, md, "trips")
      trips.mtx <- readMatrixOMX(file.trips, trips.name)
      trips.pr.ic <- if (is.null(trips.pr.ic)) trips.mtx else trips.pr.ic + trips.mtx
      rm(trips.mtx)
      
    }
    trips.pr <- if (is.null(trips.pr)) trips.pr.ic else trips.pr + trips.pr.ic
    rm(trips.pr.ic)
    
  }
  trips.sum.pr[pr] <- sum(trips.pr)
  
}


trips.probs.pr <- trips.sum.pr/sum(trips.sum.pr)

# > trips.probs.pr
# pr     probs
# 1           HBW 0.2631526
# 2   HB Shopping 0.1833400
# 3 HB Recreation 0.2076655
# 4      HB Other 0.3458419
trips.probs.pr <- data.frame(pr = names(trips.probs.pr), probs = trips.probs.pr) %>%
                  mutate(pr = factor(pr, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other")))


# Load travel cost calculated by dummy basket 
load("output/Portland/2010/cluster/minutes/tcost.RData")
rm(tcost_trip.pr.ic, tcost_trip.pr, tcost_trip.ic, tcost_trip.all,
   tcost.htaz.ic, tcost.htaz.pr, tcost.pr.ic, tcost.pr, tcost.ic, tcost.all, tcost.htaz)

# trip level costs 

# tcost_trip.htaz.pr.ic <- tcost.trip %>%
#                           group_by(htaz, ic, pr) %>%
#                           summarize(tcost=sum(tcost.sum) / sum(trips))

tcost_trip.htaz.ic <-  tcost_trip.htaz.pr.ic %>% 
                        left_join(trips.probs.pr) %>% 
                        group_by(htaz,ic) %>% 
                        summarise(tcost.sum=weighted.mean(tcost, probs))

# tcost_trip.pr <- tcost.trip %>%
#                   group_by(pr) %>%
#                   summarize(tcost=sum(tcost.sum) / sum(trips))

# tcost_trip.pr.ic <- tcost.trip %>%
#   group_by(ic, pr) %>%
#   summarize(tcost=sum(tcost.sum) / sum(trips))
# 
# tcost_trip.pr <- tcost.trip %>%
#   group_by(pr) %>%
#   summarize(tcost=sum(tcost.sum) / sum(trips))
# 
# tcost_trip.ic <- tcost.trip %>%
#   group_by(ic) %>%
#   summarize(tcost=sum(tcost.sum) / sum(trips))
# 
# tcost_trip.all <- with(tcost.trip, sum(tcost.sum) / sum(trips))

# 
load(file.path(INPUT_DIR, "Zi.RData"))
load(file.path(INPUT_DIR, "hhs.ZiIc.RData"))


# household level trip costs
# hhs.ic <- hhs.ZiIc %>%
#   group_by(ic) %>%
#   summarize(hhs=sum(hhs))
# 
# hhs.htaz <- hhs.ZiIc %>%
#   group_by(htaz) %>%
#   summarize(hhs=sum(hhs))
# 
# tcost.htaz.pr.ic <- tcost.trip %>%
#   group_by(htaz, pr, ic) %>%
#   summarize(tcost.sum=sum(tcost.sum)) %>%
#   left_join(hhs.ZiIc) %>%
#   mutate(tcost=tcost.sum/hhs)

tcost.htaz.ic <-  tcost.htaz.pr.ic %>% 
                  left_join(trips.probs.pr) %>% 
                  group_by(htaz,ic) %>% 
                  summarise(tcost.sum=weighted.mean(tcost.sum, probs))  

# tcost.htaz.ic <- tcost.trip %>%
#                   group_by(htaz, ic) %>%
#                   summarize(tcost.sum=sum(tcost.sum)) %>%
#                   left_join(hhs.ZiIc) %>%
#                   mutate(tcost=tcost.sum/hhs)
# 
# tcost.htaz.pr <- tcost.trip %>%
#                   group_by(htaz, pr) %>%
#                   summarize(tcost.sum=sum(tcost.sum)) %>%
#                   left_join(hhs.htaz) %>%
#                   mutate(tcost=tcost.sum/hhs)
# 
# tcost.pr.ic <- tcost.trip %>%
#                 group_by(pr, ic) %>%
#                 summarize(tcost.sum=sum(tcost.sum)) %>%
#                 left_join(hhs.ic) %>%
#                 mutate(tcost=tcost.sum/hhs)

# plot charts
OUTPUT_DIR = file.path("output", project.subdir, method.name, unit.name)
OUTPUT_DIR <- "output/Portland/2010/hybrid/minutes"
pden.htaz.ic <- plot_density(plot.data=tcost.htaz.ic, x="tcost", xlab="Travel Cost", 
                             group="ic", legend.title="Income Level", 
                             unit.name=unit.name)
pden.htaz.ic
output_file = file.path(OUTPUT_DIR, "density_tcost_by_ic.png")
ggsave(pden.htaz.ic, file=output_file, type="cairo-png")
  










