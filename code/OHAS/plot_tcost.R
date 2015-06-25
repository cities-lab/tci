# plot maps and descriptives of travel time cost

# load saveGraph function
source("code/thirdparty/openGraphSaveGraph.R")

#load(file.path(OUTPUT_DIR, "tcost.RData"))

# plot travel costs distribution by income group
require(ggplot2)
output_file = file.path(OUTPUT_DIR, "density_hhtcost_by_inc")
saveGraph(filename=output_file, type="pdf")  

m <- ggplot(tcost.hh, aes(x = tcost, colour=inc.level, group=inc.level))
m + geom_density(fill=NA, size=2) + labs(x="travel costs")

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
