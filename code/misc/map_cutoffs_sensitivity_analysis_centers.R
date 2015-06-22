# This scripts automatically generate the results of sensitivity  analysis
# and travel cost with employment centres methods 

# Set workspace
setwd("~/tci")
var_list.0 <- ls()

source("code/cluster/settings.R")


# Combine all center index into arrray
Zi <- as.character(c(1:2162))
Dc <- c(.5, .6, .7, .8, .9,  .95)
Ci.ZiPrDc <- array(0, dim=c(length(Zi), length(Pr), length(Dc)), dimnames=list(Zi, Pr, Dc))


for (cutoff.p in c(.5, .6, .7, .8, .9,  .95) ){
  for (sum.cutoff.p in c(.25, .5, .75)) {
    
    OUTPUT_DIR <- paste("output/cluster/cutoffs/", "cutoffp", cutoff.p, "sum.cutoff.p", sum.cutoff.p, sep="")

    INTERMEDIATE_DIR <- paste("output/intermediate/cluster/cutoffs/",  cutoff.p, "sum.cutoff.p", sum.cutoff.p, sep="")

    load(file.path(INTERMEDIATE_DIR, "centers.RData"))
    
    dc.char <- as.character(cutoff.p)
    
    Ci.ZiPrDc[hbwci$TAZ, "hbw", dc.char] <- sum.cutoff.p
    Ci.ZiPrDc[hbsci$TAZ, "hbs", dc.char] <- sum.cutoff.p
    Ci.ZiPrDc[hbrci$TAZ, "hbr", dc.char] <- sum.cutoff.p
    Ci.ZiPrDc[hboci$TAZ, "hbo", dc.char] <- sum.cutoff.p
    
    remove(hbwci, hbsci, hbrci, hboci, dc.char)
  
  }
  
}

# Save results
INTERMEDIATE_DIR <- "output/intermediate/cluster"
OUTPUT_DIR <- 'output/cluster'
output.file <- file.path(OUTPUT_DIR, 'Ci.ZiPrDc.RData')
save(Ci.ZiPrDc, file=output.file)


# Map center index by trip purpose
# load required libraries
library(maptools)
library(RColorBrewer)

#Load mapping data and functions
#===============================

#::

# read in the taz data shapefile
TazFile <- file.path(INPUT_DIR, "shp/TAZ.shp")
TazPoly <- readShapePoly(TazFile)

# extract the attribute data
TazData <- TazPoly@data

# make an index vector to the taz
TazIndex <- as.character(TazData$newtaz)

# get the location of the reference zone
TazCentroids.ZiXy <- coordinates(TazPoly)
rownames(TazCentroids.ZiXy) <- TazIndex

## Joe's modify function to plot

# write a function to plot taz values as a choropleth map
choropleth <- function(geo=TazPoly, data, DataIndex=TazIndex, palette="Blues", breaks,
                       LegendSize=1, PlotRef=TRUE, LegendOffset=c(1,1),
                       LegendTitle=NULL, RefColor="red", LegendText=FALSE, ...){
  DataCut <- cut(data[DataIndex], breaks, include.lowest=TRUE, labels=FALSE)
  ColorPalette <- brewer.pal(length(breaks)-1, palette)
  colors <- ColorPalette[DataCut]
  plot(geo, col=colors, xaxt="n", yaxt="n", border=NA, ...)
  if(PlotRef) points(RefZoneCent[1], RefZoneCent[2], pch=1, col=RefColor, cex=2, lwd=2)
  if(LegendText[1] == FALSE) {LegendText <- paste(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)], sep=" - ")}
  print(LegendText)
  if(LegendSize != 0){
    legend(7404637,611959, legend=LegendText,
           title=LegendTitle, cex=LegendSize, fill=ColorPalette)
  }
  if(PlotRef){
    points(7410262, 543378, pch=1, col=RefColor, cex=2, lwd=2)
    text(7415412, 543378, "Reference Zone", pos=4)
  }
}


# Load saveGraph function 
source("code/thirdparty/openGraphSaveGraph.R")

graph.path <- file.path(OUTPUT_DIR, "graphics")


## Map hbwci

# Set up plot layout
nf <- layout(matrix(1:6,nrow=2))
Opar <- par(mar=c(0.5,0.5,1,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms
for (cutoff.p in c(.5, .6, .7, .8, .9,  .95) ){
  dc.char <- as.character(cutoff.p)
  MapData <- Ci.ZiPrDc[, "hbw", dc.char]
  maintitle <- paste("Density percentile", cutoff.p, sep=" ")
  if(cutoff.p==0.6){
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.1,0.3,0.6,0.8), LegendSize=0.5, PlotRef=FALSE,
               main=maintitle, LegendTitle="HBW Centers", LegendText=c("non-ci", "25% ci", "50% ci", "75% ci"))
  } else {
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.1,0.3,0.6,0.8), LegendSize=0, PlotRef=FALSE,
               main=maintitle)
  }
  
}
par(Opar)

saveGraph(filename=file.path(graph.path, "map_hbw_centers_by_den_tot_cutoffs"), type="pdf")

## Map hbsci

# Set up plot layout
nf <- layout(matrix(1:6,nrow=2))
Opar <- par(mar=c(0.5,0.5,1,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms
for (cutoff.p in c(.5, .6, .7, .8, .9,  .95) ){
  dc.char <- as.character(cutoff.p)
  MapData <- Ci.ZiPrDc[, "hbs", dc.char]
  maintitle <- paste("Density percentile", cutoff.p, sep=" ")
  if(cutoff.p==0.6){
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.1,0.3,0.6,0.8), LegendSize=0.5, PlotRef=FALSE,
               main=maintitle, LegendTitle="HBS Centers", LegendText=c("non-ci", "25% ci", "50% ci", "75% ci"))
  } else {
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.1,0.3,0.6,0.8), LegendSize=0, PlotRef=FALSE,
               main=maintitle)
  }
  
}
par(Opar)

saveGraph(filename=file.path(graph.path, "map_hbs_centers_by_den_tot_cutoffs"), type="pdf")

## Map hbrci

# Set up plot layout
nf <- layout(matrix(1:6,nrow=2))
Opar <- par(mar=c(0.5,0.5,1,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms
for (cutoff.p in c(.5, .6, .7, .8, .9,  .95) ){
  dc.char <- as.character(cutoff.p)
  MapData <- Ci.ZiPrDc[, "hbr", dc.char]
  maintitle <- paste("Density percentile", cutoff.p, sep=" ")
  if(cutoff.p==0.6){
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.1,0.3,0.6,0.8), LegendSize=0.5, PlotRef=FALSE,
               main=maintitle, LegendTitle="HBR Centers", LegendText=c("non-ci", "25% ci", "50% ci", "75% ci"))
  } else {
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.1,0.3,0.6,0.8), LegendSize=0, PlotRef=FALSE,
               main=maintitle)
  }
  
}
par(Opar)

saveGraph(filename=file.path(graph.path, "map_hbr_centers_by_den_tot_cutoffs"), type="pdf")

## Map hboci

# Set up plot layout
nf <- layout(matrix(1:6,nrow=2))
Opar <- par(mar=c(0.5,0.5,1,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms
for (cutoff.p in c(.5, .6, .7, .8, .9,  .95) ){
  dc.char <- as.character(cutoff.p)
  MapData <- Ci.ZiPrDc[, "hbo", dc.char]
  maintitle <- paste("Density percentile", cutoff.p, sep=" ")
  if(cutoff.p==0.6){
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.1,0.3,0.6,0.8), LegendSize=0.5, PlotRef=FALSE,
               main=maintitle, LegendTitle="HBO Centers", LegendText=c("non-ci", "25% ci", "50% ci", "75% ci"))
  } else {
    choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
               breaks=c(0,0.1,0.3,0.6,0.8), LegendSize=0, PlotRef=FALSE,
               main=maintitle)
  }
  
}
par(Opar)

saveGraph(filename=file.path(graph.path, "map_hbo_centers_by_den_tot_cutoffs"), type="pdf")

# Remove results
var_list.1 <- ls()
rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
rm(var_list.1)

  