# plot maps and descriptives of travel time cost

# load saveGraph function
source("code/thirdparty/openGraphSaveGraph.R")

#load(file.path(OUTPUT_DIR, "tcost.RData"))

# plot travel costs distribution by income group
require(ggplot2)
require(ggmap)

pden.inc <- ggplot(tcost.hh, aes(x = tcost, colour=inc.level, group=inc.level)) +
  geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 360) +
  scale_colour_discrete(name = 'Income Level')
output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_inc.png")
ggsave(pden.inc, file=output_file, type="cairo-png")

tcost.hh <- tcost.hh %>% 
  mutate(hhsiz.cat=cut(HHSIZ,
                       breaks=c(1, 2, 3, 4, 9),
                       labels=c("1", "2", "3", "4+"),   #allow alternative household grouping
                       include.lowest=T, right=F
  ))
pden.hhsiz <- ggplot(tcost.hh, aes(x = tcost, colour=hhsiz.cat, group=hhsiz.cat)) +
  geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 360) +
  scale_colour_discrete(name = 'Household Size')
pden.hhsiz
output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_hhsiz.png")
ggsave(pden.hhsiz, file=output_file, type="cairo-png")

tcost.hh.tpurp <- tcost.hh.tpurp %>% 
  mutate(inc.level = factor(inc.level, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc")),
         TripPurpose = factor(TripPurpose, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other"))
  )

boxp.tpurp_inc <- ggplot(tcost.hh.tpurp, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
  geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 250)  +
  scale_fill_discrete(name = 'Income Level')
boxp.tpurp_inc
output_file = file.path(OUTPUT_DIR, "boxplot_tcost.hh_by_tpurp.inc.png")
ggsave(file=output_file, type="cairo-png")

m.sp <- ggplot(tcost.hh, aes(x = INCOME, y = tcost, colour=inc.level, group=inc.level))
m.sp + geom_point(fill=NA, size=2, position = "jitter") + labs(y="Travel Costs (minutes)") + ylim(0, 250)

m.bp <- ggplot(tcost.hh, aes(inc.level, tcost, fill=inc.level))
m.bp + geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + ylim(0, 250)

t.dp <- ggplot(tcost.tpurp.inc, aes(x = inc.level, y = tcost.wtavg, colour=TripPurpose, group=TripPurpose))
t.dp + geom_line(fill=NA, size=1) + labs(x="Income Level") + labs(y="Travel Costs (minutes)") + ylim(0, 120)
saveGraph(filename=output_file, type="pdf")

t.bp <- ggplot(tcost.hh, aes(factor(INCOME), tcost, fill=inc.level))
t.bp + geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + ylim(0, 250)

require(ggmap)
require(scales)

districts <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "districts")
districts <- fortify(districts, region="DISTRICT")

#districts.data <- tcost.distr %>% mutate(id = as.character(district.id),
#                                         value = tcost.wtavg)
#districts <- left_join(districts, districts.data)

plot_map <- function(plot.Data) {
  p <- ggplot() +
       geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
                    color = NA, size = 0.1) +
       scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), limits = c(0, 200), 
                            name = "Travel Costs\n(Minutes)", na.value = "grey80") +
       guides(fill = guide_legend(reverse = TRUE)) +
       theme_nothing(legend = TRUE)
}

#transform data for plotting
tcost.distr_all <- tcost.distr.tpurp.inc %>% 
  ungroup() %>%
  mutate(inc.level=as.character(inc.level),
         TripPurpose=as.character(TripPurpose)
  )  %>%
  union(mutate(ungroup(tcost.distr.tpurp), inc.level="All", TripPurpose=as.character(TripPurpose))) %>%
  union(mutate(ungroup(tcost.distr.inc), inc.level=as.character(inc.level), TripPurpose="All")) %>%
  union(mutate(ungroup(tcost.distr), inc.level="All", TripPurpose="All")) %>%
  right_join(expand.grid(district.id=1:20, TripPurpose=c('All', Pr), inc.level=c(Ic, 'All'), stringsAsFactors = F))

#prepare data for plotting
tcost.distr_all <- tcost.distr_all %>% 
  mutate(id=as.character(district.id), 
         value = tcost.wtavg,
         inc.level = factor(inc.level, levels=c(Ic, 'All'), labels=c("Low Inc", "Mid Inc", "High Inc", "All Households")),
         TripPurpose = factor(TripPurpose, levels=c('All', Pr), labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))
  )

plot.data <- full_join(districts, tcost.distr_all)
maps <- plot_map(plot.data) + facet_grid(TripPurpose~inc.level)

output_file = file.path(OUTPUT_DIR, "map_districts_all.png")
ggsave(maps, file = output_file, width = 8.5, height = 11, type = "cairo-png")

#p.all <- plot_map(districts)

#library(gridExtra)
#grid.arrange(p1, p2, ncol=2, main = "Main title")

# maps <- list()
# i <- 1 #keep track rows
# for (tpurp in c('All', Pr)) {
#   j <- 1 #keep track columns
#   for (inc in c(Ic, 'All')) {
#     attrs <- tcost.distr_all %>% 
#       filter_(interp(~TripPurpose==tpurp, tpurp=tpurp), 
#               interp(~inc.level==inc, inc=inc))
#     plot.Data <- left_join(districts, attrs)
#     map <- plot_map(plot.Data)
#     if (i==1) { #add header for the first row
#       map <- map + labs(title = inc, fill = "")
#     }
#     if (j==1) { #add header for the first col
#       map <- map +  annotate("text", x = min(districts$long) - 6000, y = median(districts$lat), label = tpurp, angle=90)
#     }
#     maps[[(i-1)*4+j]] <- map
#     j <- j + 1
#   }
#   i <- i + 1
# }
# grid.arrange(maps, ncol=j-1)

output_file = file.path(OUTPUT_DIR, "map_districts_all.png")
ggsave(p1, file = output_file, width = 7.5, height = 4.5, type = "cairo-png")



# load required libraries
library(maptools)
library(RColorBrewer)

# read in the taz data shapefile
TazFile <- file.path(INPUT_DIR, "shp/", "TAZ.shp") 
TazPoly <- readShapePoly(TazFile)

# extract the attribute data
TazData <- TazPoly@data

# make an index vector to the taz
TazIndex <- as.character(TazData$newtaz)

# get the location of the reference zone
TazCentroids.ZiXy <- coordinates(TazPoly)
rownames(TazCentroids.ZiXy) <- TazIndex


# write a function to plot taz values as a choropleth map
choropleth <- function(geo=TazPoly, data, DataIndex=TazIndex, palette="Blues", breaks,
                       LegendSize=1, PlotRef=TRUE, LegendOffset=c(1,1),
                       LegendTitle=NULL, RefColor="red", ...){
  DataCut <- cut(data[DataIndex], breaks, include.lowest=TRUE, labels=FALSE)
  ColorPalette <- brewer.pal(length(breaks)-1, palette)
  colors <- ColorPalette[DataCut]
  plot(geo, col=colors, xaxt="n", yaxt="n", border=NA, ...)
  if(PlotRef) points(RefZoneCent[1], RefZoneCent[2], pch=1, col=RefColor, cex=2, lwd=2)
  LegendText <- paste(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)], sep=" - ")
  if(LegendSize != 0){
    legend(7404637,611959, legend=LegendText,
           title=LegendTitle, cex=LegendSize, fill=ColorPalette)
  }
  if(PlotRef){
    points(7410262, 543378, pch=1, col=RefColor, cex=2, lwd=2)
    text(7415412, 543378, "Reference Zone", pos=4)
  }
}

for (cm in CmNames) {
  # Set up plot layout, map will go on top and histogram on bottom
  nf <- layout(matrix(1:12,nrow=4))
  Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
  breaks=c(0,0.001,1,2,3,6,40)
  # Iterate through all purposes and incomes and plot histograms
  
  obj.name <- paste(cm, ".ZiIcPr", sep="")
  tcost <- get(obj.name)
  
  # replace NA with 0
  tcost[is.na(tcost)] <- 0
  
  for(ic in Ic){
    for(pr in Pr){
      if((ic == "highInc") & (pr == "hbw")){
        choropleth(TazPoly, tcost[,ic,pr], TazIndex, "RdYlBu",
                   breaks=breaks, LegendSize=0.3, PlotRef=FALSE,
                   main="", LegendOffset=c(1.014, 1.012))
      } else {
        choropleth(TazPoly, tcost[,ic,pr], TazIndex, "RdYlBu",
                   breaks=breaks, LegendSize=0, PlotRef=FALSE,
                   main="")
      }
      if(ic == "lowInc") mtext(PrNames[pr], side=2, line=1)
      if(pr == "hbw") mtext(IcNames[ic], side=3, line=0.1)
    }
  }
  
  par(Opar)
  output_name = paste("map_", cm, "_by_income_purpose", sep="")
  output_file = file.path(OUTPUT_DIR, output_name)
  saveGraph(filename=output_file, type="pdf")    
}


###### Map minhhtcost.ZiIc,avghhtcost.ZiIc, mazhhtcost.ZiIc
#------------------------------------------------------
# Define a vector of data aggregation types
Ag <- c("minhhtcost.ZiIc", "avghhtcost.ZiIc", "maxhhtcost.ZiIc")
names(Ag) <- c("mintcost", "avgtcost", "maxtcost")

# Set up plot layout
nf <- layout(matrix(1:9,nrow=3))
Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms
for(ag in names(Ag)){
  for(ic in Ic){
    MapData <- get(Ag[ag])[,ic]
    MapData[is.na(MapData)] <- 0
    
    if((ic == "lowInc") & (ag == "maxtcost")){
      choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,5,9,14,85), LegendSize=0.3, PlotRef=FALSE,
                 main="", LegendOffset=c(1.011, 1.02))
    } else {
      choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,5,9,14,85), LegendSize=0, PlotRef=FALSE,
                 main="")
    }
    if(ag == "mintcost") mtext(IcNames[ic], side=2, line=0)
    if(ic == "lowInc") mtext(ag, side=3, line=0)
  }
}

output_file = file.path(OUTPUT_DIR, "map_hhtcost_by_income")
saveGraph(filename=output_file, type="pdf")  

###### Map hhcost.ZiCm

# Set up plot layout
nf <- layout(matrix(1:4,nrow=2))
Opar <- par(mar=c(1,1,1,1), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and plot map

for(cm in Cm){
  MapData <- hhCost.ZiCm[,cm]
  MapData[is.na(MapData)] <- 0
  if(cm == "avg") {
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.001,0.5,1,6,10,85), LegendSize=0.3, PlotRef=FALSE,
               main="", LegendOffset=c(1.011, 1.02))
  } else {
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.001,0.5,1,6,10,85), LegendSize=0, PlotRef=FALSE,
               main="")
  }
  mtext(CmNames[cm], side=2, line=0)
}
par(Opar)

output_file = file.path(OUTPUT_DIR, "map_hhtcost_by_taz")
saveGraph(filename=output_file, type="pdf")  


### plot tcost.distr
#Load district designations
DiNames <- as.character(c(1:20))

# Change data type 
tcost.distr <- as.data.frame(tcost.distr)

# Set up graphic parameters
Opar <- par(mfrow=c(2,2), mar=c(2,3,2,2), oma=c(1,1,2.25,1))

#Barplot of minimal travel time cost by distric
BarCenter <- barplot(tcost.distr[1:20,3], xlab="", ylab="Travel Time Cost", col=brewer.pal(8, "Pastel1"),
                     main=NULL, axisnames=FALSE)
mtext("mintcost", side=1, line=0.5, cex=0.75)
text(as.vector(BarCenter), 0.1, labels=DiNames, srt=90, pos=4, offset=0, cex=0.5)

#Barplot of minimal travel time cost by distric
BarCenter <- barplot(tcost.distr[1:20,4], xlab="", ylab="Travel Time Cost", col=brewer.pal(8, "Pastel1"),
                     main=NULL, axisnames=FALSE)
mtext("avgtcost", side=1, line=0.5, cex=0.75)
text(as.vector(BarCenter), 0.1, labels=DiNames, srt=90, pos=4, offset=0, cex=0.5)

#Barplot of minimal travel time cost by distric
BarCenter <- barplot(tcost.distr[1:20,5], xlab="", ylab="Travel Time Cost", col=brewer.pal(8, "Pastel1"),
                     main=NULL, axisnames=FALSE)
mtext("maxtcost", side=1, line=0.5, cex=0.75)
text(as.vector(BarCenter), 0.1, labels=DiNames, srt=90, pos=4, offset=0, cex=0.5)

mtext("Househodl Level Cost Values by Calculation Method", outer=TRUE, line=0, cex=1)

output_file = file.path(OUTPUT_DIR, "map_hhtcost_by_district")
saveGraph(filename=output_file, type="pdf")  


######## plot density line minttcost.ZiIcPr
#Plot density line of minimal travel time   costs for each calculate method and time period
#------------------------------------------------------------------------------------
for (cm in CmNames) {
  #::
  # Define a vector of data aggregation types
  # Set up plot layout, map will go on top and histogram on bottom
  nf <- layout(matrix(1:12,nrow=4))
  Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
  
  obj.name <- paste(cm, ".ZiIcPr", sep="")
  tcost <- get(obj.name)
  # replace NA with 0
  tcost[is.na(tcost)] <- 0
  
  # Iterate through all purposes and incomes and plot histograms
  for(ic in Ic){
    for(pr in Pr){
      DensityData <- tcost[,ic,pr]
      plot(density(DensityData), main="")
      if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
      if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
    }
  }
  
  par(Opar)
  output_name = paste("density_", cm, "_by_income_purpose", sep="")
  output_file = file.path(OUTPUT_DIR, output_name)
  saveGraph(filename=output_file, type="pdf")  
}


##### Plot density line of minhhtcost.ZiIc,avghhtcost.ZiIc, mazhhtcost.ZiIc
#------------------------------------------------------------------------------------

#::

# Define a vector of data aggregation types
Ag <- c("minhhtcost.ZiIc", "avghhtcost.ZiIc", "maxhhtcost.ZiIc")
names(Ag) <- c("mintcost", "avgtcost", "maxtcost")

# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:9,nrow=3))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ag in names(Ag)){
  for(ic in Ic){
    DensityData <- get(Ag[ag])[,ic]
    DensityData[is.na(DensityData)] <- 0
    plot(density(DensityData),main="")
    if(ag == "mintcost") mtext(IcNames[ic], side=2, line=3)
    if(ic == "lowInc") mtext(ag, side=3, line=1)
  }
}
par(Opar)

output_file = file.path(OUTPUT_DIR, "density_hhtcost_by_income")
saveGraph(filename=output_file, type="pdf")  
