# This script use trip distribution from emmebank 2011 three scenarios to apply cluster-based method in Portland 
# Set workspace
setwd("~/tci")
var_list.0 <- ls()

# Setting 
project.name <- 'Portland'
method.name <- 'cluster'
#year <- '2040N'

# year <- '2040S'
 year <- '2040T'

source("code/settings.R")  #source project level settings first
source("code/cluster/settings.R")
INPUT_DIR <- "data"

# load required packages
require(rhdf5)

# Load required functions
source("code/functions.R")
source("code/thirdparty/omx.r")
load("data/Zi.RData")

# compute_tcost.R 
# change load(file.path("output/intermediate/cluster", "centers.RData"))
# change TripsMd <- readMatrixOMX(file.path(INPUT_DIR, paste("emme2011_", year, ".omx", sep="")), TripsMdObjName)
# add scripts for calculating TotTrips.ZiMd

if (!exists(paste(Pr, "ci", sep="")))
  load(file.path("output/intermediate/cluster", "centers.RData"))

# loaded in 4_cal_md_trips_probs.R
# Calculate travel time by bike and walk from trip distance
#td_bw.df <- read.csv(file.path(INPUT_DIR, 'TDM/mf202.csv'), header=FALSE)
#td_bw.mx <- matrix(trip_distance.bw[,3], nrow=2162, byrow=TRUE)
#bikeTime <- td_bw.mx*60/bike.speed
#walkTime <- td_bw.mx*60/walk.speed

#Begin iteration by trip purpose
#-------------------------------  
for (pr in Pr) {
  CentersObjName <- paste(pr, "ci", sep="")
  Centers <- get(CentersObjName); rm(CentersObjName)
  
  # Begin iteration by income group
  for (ic in Ic) {
    # Begin iteration by time period
    for ( tp in Tp){
      TTimecost.ZiMdCm <- array(0, dim=(c(length(Zi), length(Md), length(Cm))), 
                                dimnames=list(Zi,Md,Cm))
      
      TOperationcost.ZiMdCm <- array(0, dim=(c(length(Zi), length(Md), length(Cm))), 
                                     dimnames=list(Zi,Md,Cm))
      
      TotTrips.ZiMd <- array(0, dim=c(length(Zi), length(Md)), 
                             dimnames=list(Zi, Md))
      
      # Begin iteration by mode 
      for (md in Md ) {
        # load trip matrices
        TripsMdObjName <- paste(pr, ic, md, "trips", sep="")
        if (! exists(TripsMdObjName)) {
          TripsMd <- readMatrixOMX(file.path(INPUT_DIR, paste("emme2011_", year, ".omx", sep="")), TripsMdObjName)
          assign(TripsMdObjName, TripsMd)
        }
        
        TotTrips.ZiMd[,md] <- rowSums(TripsMd[, Centers$TAZ])
        
        if ((md == "bike")|(md=="walk")) {
          # load travel time for bike and walk 
          ##TODO: how were these data processed? why bike & walk travel vary by purpose?
          TTimeObjName <- paste(md, "Time", sep="")
          TDistanceObjName <- paste(md, "Distance", sep="")
          
          # End calculate travel time for walk and bike 
        } else {          
          # load travel time for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
          TTimeObjName <- paste(md, tp, "Time", sep = "")
          TDistanceObjName <- paste(md, tp, "Distance", sep = "")
        }
        
        if (! exists(TTimeObjName)) {
          TTime.mx <- readMatrixOMX(file.path(INPUT_DIR, "TDM/Skims.omx"), TTimeObjName)
          assign(TTimeObjName, TTime.mx)
        }        
        
        if (! exists(TDistanceObjName)) {
          TDistance.mx <- readMatrixOMX(file.path(INPUT_DIR, "TDM/DistanceSkims.omx"), TDistanceObjName)
          assign(TDistanceObjName, TDistance.mx)
        } 
        
        for (cm in Cm) {
          func <- get(paste(cm, '_tt', sep=""))
          TTime <- func(Centers$TAZ, TTime.mx, TripsMd)
          TCost <- TTime * unitcosts[md, "VOT"] /60
          paste("min", pr, ic, md, "time", sep="")
          
          TDistance <- func(Centers$TAZ, TDistance.mx, TripsMd)
          TOperationcost <- TDistance*unitcosts[md, "mcpm"]
          # TOperationcost <- TDistance*unitcosts[md, "mcpm"]/(wage.Ic[ic])
          
          TTimecost.ZiMdCm[, md, cm] <- TCost
          TOperationcost.ZiMdCm[, md, cm] <- TOperationcost
        }
        
      } # End loop through mode
      
      Fullcost.ZiMdCm <- TTimecost.ZiMdCm + TOperationcost.ZiMdCm
      
      if (SAVE.INTERMEDIARIES) {
        intm.path <- file.path(INTERMEDIATE_DIR, 'costs')
        dir.create(intm.path, recursive = TRUE, showWarnings = FALSE)
        FullCostobj.name <- paste(pr, ic, tp, "FullCost.ZiMdCm", sep="")
        assign(FullCostobj.name, Fullcost.ZiMdCm)
        intm.file <- file.path(intm.path, paste(FullCostobj.name, ".RData", sep=""))
        save(list=c(FullCostobj.name), file=intm.file)
      }
      
      
      if (SAVE.INTERMEDIARIES) {
        TotTripsobj.name <- paste(pr, ic, "TotTrips.ZiMd", sep="")
        assign(TotTripsobj.name, TotTrips.ZiMd) 
        intm.path = file.path(INTERMEDIATE_DIR, 'trips')
        dir.create(intm.path, recursive = TRUE, showWarnings = FALSE)
        intm.file = file.path(intm.path, paste(TotTripsobj.name, ".RData", sep=""))
        save(list=TotTripsobj.name, file = intm.file)
      }
      
    } # End loop through time period
  } # End loop through income group 
} # End loop through purpose

# Source scripts to aggregate costs 
source("code/cluster/aggregate_tcost.R")
source("code/cluster/aggregate_by_geo.R")


# post_process.R
require(ggplot2)
require(ggmap)
require(reshape2)
require(magrittr)

# load saveGraph function
source("code/thirdparty/openGraphSaveGraph.R")
source("code/cluster/def_functions.R") # funcstions defined in code/functions.R do not work(tcost/tcost.wt)

#plot results from cluster-based approach
dtcost.htaz_tpurp_inc <- melt(weightedAggCost.ZiIcPr, 
                              varnames = c('htaz', 'inc.level', 'TripPurpose'),
                              value.name = "tcost")

dtrips.htaz_tpurp_inc <- melt(TripProd.ZiIcPr,
                              varnames = c('htaz', 'inc.level', 'TripPurpose'),
                              value.name = "trips")
load("data/Hh.ZiIc.RData")
dhhs.inc <- melt(Hh.ZiIc,
                 varnames = c('htaz', 'inc.level'),
                 value.name = "hhs")

dwtcost.htaz_tpurp_inc <- dtcost.htaz_tpurp_inc %>%
  left_join(dtrips.htaz_tpurp_inc) %>%
  left_join(dhhs.inc) %>%
  mutate(tcost.wt = tcost * trips / hhs,
         tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt)
  )


# dwtcost.htaz_tpurp_inc %<>% 
#   mutate(inc.level = factor(inc.level, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc")),
#          TripPurpose = factor(TripPurpose, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other")))

dwtcost.htaz_inc <- dwtcost.htaz_tpurp_inc %>%
  group_by(htaz, inc.level) %>%
  summarize(tcost.wt=sum(tcost * trips) / mean(hhs),
            hhs = first(hhs)) %>%
  as.data.frame() %>%
  mutate(tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt))

dwtcost.htaz_tpurp <- dwtcost.htaz_tpurp_inc %>%
  group_by(htaz, TripPurpose) %>%
  summarize(tcost.wt=mean(tcost.wt)) %>%
  as.data.frame() %>%
  mutate(tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt))

dwtcost.htaz <- dwtcost.htaz_inc %>%
  group_by(htaz) %>%
  summarize(tcost.wt=wt.mean(tcost.wt, hhs)) %>%
  as.data.frame() %>%
  mutate(tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt))

# prepare data for plotting
#     pden.inc <- ggplot(dwtcost.htaz_inc, aes(x = tcost.wt, colour=inc.level, group=inc.level)) +
#       geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 200) +
#       scale_colour_discrete(name = 'Income Level')+
#       ggtitle("Household-level travel cost by income groups") +
#       theme(plot.title = element_text(face="bold", size=12, vjust=1))

pden.inc.f <- function(plot.data=NULL, xlim.max=NULL) {
  
  default.xlim.max <- c(dollars=200, minutes=600)
  xlim.max <- ifelse(is.null(xlim.max), default.xlim.max[[unit.name]], xlim.max)
  
  xaxis.label <- paste("Travel Costs (", unit.name, ")", sep="")
  p <- ggplot(data=plot.data, aes(x = tcost.wt, colour=inc.level, group=inc.level)) 
  p + geom_density(fill=NA, size=1) + labs(x=xaxis.label) + xlim(0, xlim.max) +
    scale_colour_discrete(name = 'Income Level') +
    ggtitle("Household-level trip cost by income levels") +
    theme(plot.title = element_text(face="bold", size=12, vjust=1))
}

pden.inc <- pden.inc.f(plot.data=dwtcost.htaz_inc)
pden.inc
output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_inc.png")
ggsave(pden.inc, file=output_file, type="cairo-png")

#     pden.tpurp <- ggplot(dwtcost.htaz_tpurp, aes(x = tcost.wt, colour=TripPurpose, group=TripPurpose)) +
#       geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 100) +
#       scale_colour_discrete(name = 'Trip Purpose')+
#       ggtitle("Household-level travel cost by trip purposes") +
#       theme(plot.title = element_text(face="bold", size=12, vjust=1))


pden.tpurp.f <- function(plot.data=NULL, xlim.max=NULL) {
  default.xlim.max <- c(dollars=60, minutes=150)
  xlim.max <- ifelse(is.null(xlim.max), default.xlim.max[[unit.name]], xlim.max)
  xaxis.label <- paste("Travel Costs (", unit.name, ")", sep="")
  
  p <- ggplot(dwtcost.htaz_tpurp, aes(x = tcost.wt, colour=TripPurpose, group=TripPurpose))
  p + geom_density(fill=NA, size=1) + labs(x=xaxis.label) + xlim(0, xlim.max) +
      scale_colour_discrete(name = 'Trip Purpose')+
      ggtitle("Household-level travel cost by trip purposes") +
      theme(plot.title = element_text(face="bold", size=12, vjust=1))
}

pden.tpurp <- pden.tpurp.f(plot.data=dwtcost.htaz_tpurp)
pden.tpurp
output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_tpurp.png")
ggsave(pden.tpurp, file=output_file, type="cairo-png")

#     boxp.tpurp_inc <- ggplot(dwtcost.htaz_tpurp_inc, aes(x=TripPurpose, y=tcost.wt, fill=inc.level)) +
#       geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
#       scale_fill_discrete(name = 'Income Level') +
#       ggtitle("Household-level travel cost by trip purposes and income levels") +
#       theme(plot.title = element_text(face="bold", size=12, vjust=1))
# box plot for trip costs by income level
boxp.tpurp.inc.f <- function (plot.data=NULL, ylim.max=NULL) {
  
  default.ylim.max <- c(dollars=100, minutes=250)
  ylim.max <- ifelse(is.null(ylim.max), default.ylim.max[[unit.name]], ylim.max)
  
  yaxis.label=paste("Generalized Travel Costs (", unit.name, ")", sep="")
  
  #TODO: this should be done before plot.data is passed to the func
  plot.data <- plot.data %>% 
    mutate(inc.level = factor(inc.level, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc")),
           TripPurpose = factor(TripPurpose, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other"))
    )
  
  p <- ggplot(data=plot.data, aes(x=TripPurpose, y=tcost.wt, fill=inc.level)) 
  p + geom_boxplot()  + labs(y=yaxis.label) + xlab("Trip Purpose") + 
    ylim(0, ylim.max)  + scale_fill_discrete(name = 'Income Level') + 
    ggtitle("Household-level travel cost by trip purposes and income levels") +
    theme(plot.title = element_text(face="bold", size=12, vjust=1))
}


boxp.tpurp.inc <-boxp.tpurp.inc.f(plot.data=dwtcost.htaz_tpurp_inc) 
boxp.tpurp.inc
output_file = file.path(OUTPUT_DIR, "boxplot_tcost.hh_by_tpurp.inc.png")
ggsave(boxp.tpurp.inc, file=output_file, type="cairo-png")


dwtcost.htaz_inc %<>% select(-hhs)

# transform data for plotting maps
#     tcost.htaz_all <- dwtcost.htaz_tpurp_inc %>%
#       select(-c(trips, hhs, tcost)) %>%
#       mutate(inc.level=as.character(inc.level),
#              TripPurpose=as.character(TripPurpose)
#       )  %>%
#       union(mutate(ungroup(dwtcost.htaz_tpurp), inc.level="All Households", TripPurpose=as.character(TripPurpose))) %>%
#       union(mutate(ungroup(dwtcost.htaz_inc), inc.level=as.character(inc.level), TripPurpose="All Trips")) %>%
#       union(mutate(ungroup(dwtcost.htaz), inc.level="All Households", TripPurpose="All Trips"))  # %>%
#     #right_join(expand.grid(htaz=1:max.taz_id, TripPurpose=c('All', Pr), inc.level=c(Ic, 'All'), stringsAsFactors = F))
#     
#     tcost.htaz_all <- tcost.htaz_all%>% 
#       mutate(id=as.character(htaz), 
#              value = tcost.wt,
#              inc.level = factor(inc.level, levels=c(levels(inc.level), 'All '), labels=c(labels(), "All Households")),
#              TripPurpose = factor(TripPurpose, levels=c('All', Pr), labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))
#       )
#     

tcost.htaz_tpurp.inc <- expand.grid(htaz=as.numeric(Zi), TripPurpose=c(Pr), inc.level=c(Ic), stringsAsFactors = F) %>% 
  left_join(dwtcost.htaz_tpurp_inc %>% ungroup() %>% mutate(inc.level=as.character(inc.level), TripPurpose=as.character(TripPurpose))) %>%
  dplyr::select(htaz, TripPurpose, inc.level, tcost.wt)

tcost.htaz_tpurp.all <- expand.grid(htaz=as.numeric(Zi), TripPurpose=c(Pr), inc.level=c("All"), stringsAsFactors = F) %>%
  left_join(mutate(ungroup(dwtcost.htaz_tpurp), inc.level="All", TripPurpose=as.character(TripPurpose))) 

tcost.htaz_all.inc <- expand.grid(htaz=as.numeric(Zi), TripPurpose=c("All"), inc.level=c(Ic), stringsAsFactors = F) %>%
  left_join(mutate(ungroup(dwtcost.htaz_inc), inc.level=as.character(inc.level), TripPurpose="All")) 

tcost.htaz_all.all <- expand.grid(htaz=as.numeric(Zi), TripPurpose=c("All"), inc.level=c("All"), stringsAsFactors = F) %>%
  left_join(mutate(ungroup(dwtcost.htaz), inc.level="All", TripPurpose="All")) 

tcost.htaz_all <- rbind(tcost.htaz_tpurp.inc, tcost.htaz_tpurp.all, tcost.htaz_all.inc, tcost.htaz_all.all) %>%
  right_join(expand.grid(htaz=as.numeric(Zi), TripPurpose=c('All', Pr), inc.level=c(Ic, 'All'), stringsAsFactors = F))

#prepare data for plotting
tcost.htaz_all <-  tcost.htaz_all %>%
  mutate(id=as.character(htaz), 
         value = tcost.wt)

#                        ,
#              inc.level = factor(inc.level, levels=c("Low Inc", "Mid Inc", "High Inc", 'All Households')), #labels=c(labels(), "All Households")),
#              TripPurpose = factor(TripPurpose, levels=c('All Trips', "HBW", "HB Shopping", "HB Recreation", "HB Other") )#, labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))         
#       )

#     tcost.htaz_all <- tcost.htaz_all %>%
#       mutate(id=as.character(htaz), 
#              value = tcost.wt,
#              inc.level = factor(inc.level, levels=c(levels(inc.level), 'All '), labels=c(labels(), "All Households")),
#              TripPurpose = factor(TripPurpose, levels=c('All', Pr), labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))
#       )


require(ggmap)
require(scales)


taz <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "TAZ")
taz <- fortify(taz, region="newtaz")

#taz.data <- tcost.distr %>% mutate(id = as.character(district.id),
#                                         value = tcost.wtavg)
#taz <- left_join(taz, taz.data)

#     plot_map <- function(plot.Data) {
#       p <- ggplot() +
#         geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
#                      color = NA, size = 0.1) +
#         scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), limits = c(0, 200), 
#                              name = "Travel Costs\n(Minutes)", na.value = "grey80") +
#         guides(fill = guide_legend(reverse = TRUE)) +
#         theme_nothing(legend = TRUE)
#     }


plot_map <- function(plot.Data=NULL, limits.max=NULL) {
  default.limits.max <- c(dollars=100, minutes=200)
  limits.max <- ifelse(is.null(limits.max), default.limits.max[[unit.name]], limits.max)
  
  name.label <- paste("Travel Costs (", unit.name, ")", sep="")
  
  p <- ggplot() +
    geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
                 color = NA, size = 0.1) +
    scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), limits = c(0, limits.max), 
                         name = name.label, na.value = "grey80") +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_nothing(legend = TRUE)
}


plot.data <- full_join(taz, tcost.htaz_all)
maps <- plot_map(plot.Data=plot.data) + facet_grid(TripPurpose~inc.level)
#maps
output_file = file.path(OUTPUT_DIR, "map_taz_all.png")
ggsave(maps, file = output_file, width = 8.5, height = 11, type = "cairo-png")



var_list.1 <- ls()
rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
rm(var_list.1)



