require(maptools)
require(rgeos)
require(rgdal)
require(dplyr)
require(lazyeval)

# overlay x,y coordinate (longitude, lattitude) with a polygon shapefile (shpfile) to get polygon id (id_name)
get_xy_polyid <- function(xy.df, shpfile, id_name) {
  spdf = SpatialPointsDataFrame(xy.df[, c('x', 'y')], 
                                xy.df, 
                                proj4string=CRS("+init=epsg:4326"))
  spdf.proj <- spTransform(spdf, CRS("+init=epsg:2913"))
  
  # spatial join with the shp polygon to get polygon id (id_name)
  TAZPoly <- readShapePoly(shpfile,
                           proj4string=CRS("+init=epsg:2913"))
  id <- over(spdf.proj, TAZPoly)[, id_name]
  id
}

# summarise tcost with default summary quantities
summarize_tcost <- function(.data, w=NULL) {
  results <- summarize(.data, n = n(),
              tcost.min=min(tcost, na.rm=T),
              tcost.avg=mean(tcost, na.rm=T),
              tcost.max=max(tcost, na.rm=T),
              tcost.sd=sd(tcost, na.rm=T)
              )
  
  if (!is.null(w)) {
    require(SDMTools)
    tcost.wt <- summarize_(.data,
                #mutate_(tcost.avg=~weighted.mean(tcost, w, na.rm=T))
                  tcost.wtavg=interp(~wt.mean(tcost, w), 
                                   tcost=as.name("tcost"), w=as.name(w)),
                  tcost.wtsd=interp(~wt.sd(tcost, w), 
                                  tcost=as.name("tcost"), w=as.name(w))
                )
    results <- left_join(results, tcost.wt)
  }
  results
}

# compute tcosts by composing grouping and summarizing functions
compute_tcost <- function(df, by, func, w=NULL) {
  df %>%
    group_by_(.dots=by) %>%
    func(w=w)
}



# identify trip purposes 

identifyTripPurpose <- function (df) {
  
  workLabels = c("Work", "Work-related")
  othLabels  = c("Meals", "Personal services", "Medical care", "Professional services", "Household or personal business", 
                 "Household maintenance", "Household obligations", "Pick-Up/Drop-Off passengers", "Visiting", "Culture",
                 "Religion/Civil Services", "Civic", "Volunteer work",  "Hobbies", "Exercise/Athletics", 
                 "Rest and relaxation", "Spectator athletic events", "Incidental trip", "Tag along trip")
  shopLabels = c("Shopping (general)", "Shopping (major)")
  recLabels  = c("Casual entertaining", "Formal entertaining", "Amusements (at-home)", "Amusements (out-of-home)")
  schLabels  = c("School")
  
  df$TripPurpose=ifelse(df$ACT1.f %in% workLabels & df$LastHOME==1 & df$HOME!=1,"HBW","NA")
  df$TripPurpose=ifelse(df$ACT1.f %in% othLabels  & df$LastHOME==1 & df$HOME!=1,"HBO",df$TripPurpose)
  df$TripPurpose=ifelse(df$ACT1.f %in% shopLabels & df$LastHOME==1 & df$HOME!=1,"HBShp",df$TripPurpose)
  df$TripPurpose=ifelse(df$ACT1.f %in% recLabels  & df$LastHOME==1 & df$HOME!=1,"HBRec",df$TripPurpose)
  df$TripPurpose=ifelse(df$ACT1.f %in% schLabels  & df$LastHOME==1 & df$HOME!=1,"HBSch",df$TripPurpose)
  
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% workLabels & df$LastHOME!=1,"HBW",df$TripPurpose)
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% othLabels & df$LastHOME!=1,"HBO",df$TripPurpose)
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% shopLabels & df$LastHOME!=1,"HBShp",df$TripPurpose)
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% recLabels & df$LastHOME!=1,"HBRec",df$TripPurpose)
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% schLabels & df$LastHOME!=1,"HBSch",df$TripPurpose)
  
  return(df)
}



# Define plot functions 
pden.inc.f <- function(plot.data=NULL, unit.name=NULL) {
  
  xlim.max <- c(dollars=150, minutes=450)
  xaxis.label <- paste("Travel Costs (", unit.name, ")", sep="")
  
  p <- ggplot(data=plot.data, aes(x = tcost, colour=inc.level, group=inc.level)) 
  p + geom_density(fill=NA, size=1) + labs(x=xaxis.label) + xlim(0, xlim.max[unit.name]) +
    scale_colour_discrete(name = 'Income Level') +
    ggtitle("Household-level trip cost by income levels") +
    theme(plot.title = element_text(face="bold", size=12, vjust=1))
}


pden.hhsiz.f <- function(plot.data=NULL, unit.name=NULL) {
  
  xlim.max <- c(dollars=150, minutes=450)
  xaxis.label <- paste("Travel Costs (", unit.name, ")", sep="")
  
  plot.data <- plot.data %>% 
    mutate(hhsiz.cat=cut(HHSIZ,
                         breaks=c(1, 2, 3, 4, max(HHSIZ)),
                         labels=c("1", "2", "3", "4+"),   #allow alternative household grouping
                         include.lowest=T, right=F
    ))
  
  p <- ggplot(data=plot.data, aes(x = tcost, colour=hhsiz.cat, group=hhsiz.cat)) 
  p + geom_density(fill=NA, size=1) + labs(x=xaxis.label) + xlim(0, xlim.max[unit.name]) +
    scale_colour_discrete(name = 'Household Size')+ 
    ggtitle("Household-level travel cost by household size") +
    theme(plot.title = element_text(face="bold", size=12, vjust=1))
}

boxp.tpurp_inc.f <- function (plot.data=NULL, unit.name=NULL) {
  
  ylim.max <- c(dollars=100, minutes=250)
  yaxis.label=paste("Generalized Travel Costs (", unit.name, ")", sep="")
  
  plot.data <- plot.data %>% 
    mutate(inc.level = factor(inc.level, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc")),
           TripPurpose = factor(TripPurpose, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other"))
    )
  
  p <- ggplot(data=plot.data, aes(x=TripPurpose, y=tcost, fill=inc.level)) 
  p + geom_boxplot()  + labs(y=yaxis.label) + xlab("Trip Purpose") + 
    ylim(0, ylim.max[unit.name])  + scale_fill_discrete(name = 'Income Level') + 
    ggtitle("Household-level travel cost by trip purposes and income levels") +
    theme(plot.title = element_text(face="bold", size=12, vjust=1))
}

linep.tpurp.inc.f <- function (plot.data=NULL, unit.name=NULL) {
  ylim.max <- c(dollars=60, minutes=160)
  yaxis.label=paste("Travel Costs (", unit.name, ")", sep="")
  
  p <- ggplot(data=plot.data, aes(x = inc.level, y = tcost.wtavg, colour=TripPurpose, group=TripPurpose)) 
  p + geom_line(fill=NA, size=1) + labs(x="Income Level") + labs(y=yaxis.label) + ylim(0, ylim.max[unit.name]) +
    ggtitle("Trip-level travel cost by trip purpose and income levels") +
    theme(plot.title = element_text(face="bold", size=12, vjust=1))
}


