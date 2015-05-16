# This scripts generate a variety of plots and maps of costs 

# load required libraries
library(maptools)
library(RColorBrewer)

# Load saveGraph function 
source("code/thirdparty/openGraphSaveGraph.R")

graph.path <- file.path(OUTPUT_DIR, "graphics")
dir.create(file.path(graph.path), recursive = TRUE, showWarnings = FALSE)

#::

#  min offpeak results
load(file.path(OUTPUT_DIR, "aggcostCmTp/minoffpeakAggCost.ZiIcPr.RData"))
load(file.path(OUTPUT_DIR, "aggcostCmTp/minoffpeakAggCost.Zi.RData"))
#  min peak results
load(file.path(OUTPUT_DIR, "aggcostCmTp/minpeakAggCost.ZiIcPr.RData"))
load(file.path(OUTPUT_DIR, "aggcostCmTp/minpeakAggCost.Zi.RData"))

#  weighted offpeak results
load(file.path(OUTPUT_DIR, "aggcostCmTp/weightedoffpeakAggCost.ZiIcPr.RData"))
load(file.path(OUTPUT_DIR, "aggcostCmTp/weightedoffpeakAggCost.Zi.RData"))
#  weighted peak results
load(file.path(OUTPUT_DIR, "aggcostCmTp/weightedpeakAggCost.ZiIcPr.RData"))
load(file.path(OUTPUT_DIR, "aggcostCmTp/weightedpeakAggCost.Zi.RData"))

load(file.path(OUTPUT_DIR, "AggCost.Di.RData"))

# load household number in each TAZ by income group
load(file.path(INPUT_DIR, 'Hh.ZiIc.RData'))

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



# make names for household incme groups and trip types for plotting 
     
     IcNames <- c("Low Income", "Mid Income", "High Income")
     Ic <- c("lowInc", "midInc", "highInc")
     names(IcNames) <- Ic

     PrNames <- c("Work", "Shopping", "Recreation", "Other")
     Pr <- c("hbw", "hbs", "hbr", "hbo")
     names(PrNames) <- Pr


## Map minimal travel time cost during offpeak time 
## -------------------------------------------

#::

  # round minimal travel time cost during offpeak
minoffpeakAggCost.Zi <- round(minoffpeakAggCost.Zi,2)
  
  # Set up plot layout
  par(mfrow=c(1,1))
## map 
choropleth(TazPoly, minoffpeakAggCost.Zi, TazIndex, "RdYlBu",
                  breaks=quantile(minoffpeakAggCost.Zi,na.rm=TRUE), LegendSize=0.6, PlotRef=FALSE,
                main="", LegendTitle="Travel Time Cost")

mtext("OffPeak Minimal Travel Time Cost", cex=1.2)
saveGraph(filename=file.path(graph.path, "minoffpeakAggCost_Zi"), type="pdf")

## Map minimal travel time cost during peak time
## -------------------------------------------

#::

# round minimal travel time cost during peak
minpeakAggCost.Zi <- round(minpeakAggCost.Zi,2)
  
  # Set up plot layout
  par(mfrow=c(1,1))

## map 
choropleth(TazPoly, minpeakAggCost.Zi, TazIndex, "RdYlBu",
          breaks=quantile(minpeakAggCost.Zi,na.rm=TRUE), LegendSize=0.6, PlotRef=FALSE,
          main="", LegendTitle="Travel Time Cost")

mtext("Peak Minimal Travel Time Cost", cex=1.2)
 saveGraph(filename=file.path(graph.path, "minpeakAggCost_Zi"), type="pdf")


## Map weighted travel time cost during offpeak time 
## -------------------------------------------

#::

  # round weighted travel time cost during offpeak time 
weightedoffpeakAggCost.Zi <- round(weightedoffpeakAggCost.Zi,2)
  
  # Set up plot layout
  par(mfrow=c(1,1))

## map 
choropleth(TazPoly, weightedoffpeakAggCost.Zi, TazIndex, "RdYlBu",
                  breaks=quantile(weightedoffpeakAggCost.Zi,na.rm=TRUE), LegendSize=0.6, PlotRef=FALSE,
                main="", LegendTitle="Travel Time Cost")

mtext("OffPeak Minimal Travel Time Cost", cex=1.2)
saveGraph(filename=file.path(graph.path, "weightedoffpeakAggCost_Zi"), type="pdf")



## Map weighted travel time cost during peak time
## -------------------------------------------

#::

# round weighted travel time cost during peak
weightedpeakAggCost.Zi <- round(weightedpeakAggCost.Zi,2)

  # Set up plot layout
  par(mfrow=c(1,1))

## map 
choropleth(TazPoly, weightedpeakAggCost.Zi, TazIndex, "RdYlBu",
          breaks=quantile(weightedpeakAggCost.Zi,na.rm=TRUE), LegendSize=0.6, PlotRef=FALSE,
          main="", LegendTitle="Travel Time Cost")

mtext("Peak Minimal Travel Time Cost", cex=1.2)

saveGraph(filename=file.path(graph.path, "/weightedpeakAggCost_Zi"), type="pdf")

##
#Plot maps of cost by incomes for each calculate method and time period
#-----------------------------------------------------------------------

#::
     
     
# Define a vector of data aggregation types
Ag <- c("minoffpeakAggCost.ZiIc", "minpeakAggCost.ZiIc", "weightedoffpeakAggCost.ZiIc","weightedpeakAggCost.ZiIc")
names(Ag) <- c("minoffpeak", "minpeak", "weightedoffpeak","weightedpeak")

# Set up plot layout
nf <- layout(matrix(1:12,nrow=3))
Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms
for(ag in names(Ag)){
  for(ic in Ic){
    MapData <- get(Ag[ag])[,ic]
    MapData[is.na(MapData)] <- 0
    
    if((ic == "lowInc") & (ag == "weightedpeak")){
      choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,4,6,14), LegendSize=0.3, PlotRef=FALSE,
                 main="", LegendOffset=c(1.011, 1.02))
    } else {
      choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,4,6,14), LegendSize=0, PlotRef=FALSE,
                 main="")
    }
    if(ag == "minoffpeak") mtext(IcNames[ic], side=2, line=0)
    if(ic == "lowInc") mtext(ag, side=3, line=0)
  }
}
par(Opar)

saveGraph(filename=file.path(graph.path, "/map_cost_by_income_tp_cm_income"), type="pdf")

#Plot maps of cost by purposes for each calculate method and time period

#------------------------------------------------------

#::


# Define a vector of data aggregation types
Ag <- c("minoffpeakAggCost.ZiPr", "minpeakAggCost.ZiPr", "weightedoffpeakAggCost.ZiPr","weightedpeakAggCost.ZiPr")
names(Ag) <- c("minoffpeak", "minpeak", "weightedoffpeak","weightedpeak")

# Set up plot layout
nf <- layout(matrix(1:16,nrow=4))
Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms
for(ag in names(Ag)){
  for(pr in Pr){
    MapData <- get(Ag[ag])[,pr]
    MapData[is.na(MapData)] <- 0
    
    if((pr == "hbw") & (ag == "weightedpeak")){
      choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,4,6,14), LegendSize=0.3, PlotRef=FALSE,
                 main="", LegendOffset=c(1.011, 1.02))
    } else {
      choropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,4,6,14), LegendSize=0, PlotRef=FALSE,
                 main="")
    }
    if(ag == "minoffpeak") mtext(PrNames[pr], side=2, line=1.75)
    if(pr == "hbw") mtext(ag, side=3, line=1.75)
  }
}
par(Opar)

saveGraph(filename=file.path(graph.path, "map_cost_by_income_tp_cm_purpose"), type="pdf")

#Plot household histograms of cost by purpose for each calculate method and time period
#------------------------------------------------------------------------------------

#::
# Define a vector of data aggregation types
Ag <- c("minoffpeakAggCost.ZiPr", "minpeakAggCost.ZiPr", "weightedoffpeakAggCost.ZiPr","weightedpeakAggCost.ZiPr")
names(Ag) <- c("minoffpeak", "minpeak", "weightedoffpeak","weightedpeak")

# Define breaks and limits for histograms
Breaks <- seq(0, 14, 1)
Xlim <- c(0,14)
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:16,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ag in names(Ag)){
  for(pr in Pr){
    HistData <- rep(get(Ag[ag])[,pr], rowSums(Hh.ZiIc))
    HistData <- HistData[(HistData > 0) & (HistData < 14)]
    hist(HistData, xlab="", breaks=Breaks, xlim=Xlim, ylim=c(0,1.75),
         ylab="", col="skyblue", main=NULL, freq=FALSE, axes=FALSE)
    axis(1, at=seq(0,14,1))
    if(ag == "minoffpeak") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(ag, side=3, line=1)
  }
}
par(Opar)
saveGraph(filename=file.path(graph.path, "hist_cost_by_cm_tp_purpose"), type="pdf")


#Plot household histograms of cost by income for each calculate method and time period
#------------------------------------------------------------------------------------

#::
# Define a vector of data aggregation types
Ag <- c("minoffpeakAggCost.ZiIc", "minpeakAggCost.ZiIc", "weightedoffpeakAggCost.ZiIc","weightedpeakAggCost.ZiIc")
names(Ag) <- c("minoffpeak", "minpeak", "weightedoffpeak","weightedpeak")


# Define breaks and limits for histograms
Breaks <- seq(0, 14, 1)
Xlim <- c(0,14)
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=3))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ag in names(Ag)){
  for(ic in Ic){
    HistData <- rep(get(Ag[ag])[,ic], rowSums(Hh.ZiIc))
    HistData <- HistData[(HistData > 0) & (HistData < 14)]
    hist(HistData, xlab="", breaks=Breaks, xlim=Xlim, ylim=c(0,1.75),
         ylab="", col="skyblue", main=NULL, freq=FALSE, axes=FALSE)
    axis(1, at=seq(0,14,1))
    if(ag == "minoffpeak") mtext(IcNames[ic], side=2, line=3)
    if(ic == "lowInc") mtext(ag, side=3, line=1)
  }
}
par(Opar)

saveGraph(filename=file.path(graph.path, "hist_cost_by_cm_tp_income"), type="pdf")

#Plot density line of cost by purpose for each calculate method and time period
#------------------------------------------------------------------------------------

#::
# Define a vector of data aggregation types
Ag <- c("minoffpeakAggCost.ZiPr", "minpeakAggCost.ZiPr", "weightedoffpeakAggCost.ZiPr","weightedpeakAggCost.ZiPr")
names(Ag) <- c("minoffpeak", "minpeak", "weightedoffpeak","weightedpeak")

# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:16,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ag in names(Ag)){
  for(pr in Pr){
    DensityData <- get(Ag[ag])[,pr]
    DensityData[is.na(DensityData)] <- 0
    plot(density(DensityData),main="")
    if(ag == "minoffpeak") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(ag, side=3, line=1)
  }
}
par(Opar)
saveGraph(filename=file.path(graph.path, "density_cost_by_cm_tp_purpose"), type="pdf")


#Plot density line of cost by income for each calculate method and time period
#------------------------------------------------------------------------------------

#::

# Define a vector of data aggregation types
Ag <- c("minoffpeakAggCost.ZiIc", "minpeakAggCost.ZiIc", "weightedoffpeakAggCost.ZiIc","weightedpeakAggCost.ZiIc")
names(Ag) <- c("minoffpeak", "minpeak", "weightedoffpeak","weightedpeak")

# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=3))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ag in names(Ag)){
  for(ic in Ic){
    DensityData <- get(Ag[ag])[,ic]
    DensityData[is.na(DensityData)] <- 0
    plot(density(DensityData),main="")
    if(ag == "minoffpeak") mtext(IcNames[ic], side=2, line=3)
    if(ic == "lowInc") mtext(ag, side=3, line=1)
  }
}
par(Opar)

saveGraph(filename=file.path(graph.path, "density_cost_by_cm_tp_income"), type="pdf")

#Plot comparisons of travel cost indices by UGB
#----------------------------------------------

#::

#Load district designations
DiNames <- as.character(c(1:20))

# add name index of districts 
index <- as.character(c(1:20))


# Set up graphic parameters
Opar <- par(mfrow=c(2,2), mar=c(2,3,2,2), oma=c(1,1,2.25,1))

#Barplot of offpeak minimal travel time cost 
BarCenter <- barplot(minoffpeakAggCost.Di[index], xlab="", ylab="Travel Time Cost", col=brewer.pal(8, "Pastel1"),
                     main=NULL, axisnames=FALSE)
mtext("Minimal OffPeak Travel Time Cost", side=1, line=0.5, cex=1)
mtext("Cost", side=2, line=2.5)
text(as.vector(BarCenter), 0.1, labels=DiNames, srt=90, pos=4, offset=0)

#Barplot of peak minimal travel time cost 
BarCenter <- barplot(minpeakAggCost.Di[index], xlab="", ylab="Travel Time Cost", col=brewer.pal(8, "Pastel1"),
                     main=NULL, axisnames=FALSE)
mtext("Cost", side=2, line=2.5)
mtext("Minimal Peak Travel Time Cost", side=1, line=0.5, cex=1)
text(as.vector(BarCenter), 0.2, labels=DiNames, srt=90, pos=4, offset=0)

#Barplot of non-auto market costs by district
BarCenter <- barplot(weightedoffpeakAggCost.Di[index], xlab="", ylab="Travel Time Cost", col=brewer.pal(8, "Pastel1"),
                     main=NULL, axisnames=FALSE)
mtext("Weighted OffPeak Travel Time Cost", side=1, line=0.5, cex=1)
mtext("Cost", side=2, line=2.5)
text(as.vector(BarCenter), 0.075, labels=DiNames, srt=90, pos=4, offset=0)

#Barplot of non-auto market costs by district
BarCenter <- barplot(weightedpeakAggCost.Di[index], xlab="", ylab="Travel Time Cost", col=brewer.pal(8, "Pastel1"),
                     main=NULL, axisnames=FALSE)
mtext("Weighted Peak Travel Time Cost", side=1, line=0.5, cex=1)
mtext("Cost", side=2, line=2.5)
text(as.vector(BarCenter), 0.075, labels=DiNames, srt=90, pos=4, offset=0)

mtext("Comparison of Cost Values by Calculation Method and Time Period", outer=TRUE, line=1, cex=1.15)

saveGraph(filename=file.path(graph.path, "district_cost"),type="pdf")

# Restore graphics parameters
par(Opar)


######## plot household histogram of *.ZiIcPr

#Plot household histograms of minimal travel time during offpeak costs for all purposes and incomes
#------------------------------------------------------------------------------

#::

# Define breaks and limits for histograms
Breaks <- c(seq(0,14,1))
Xlim <- c(0,14)
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ic in Ic){
  for(pr in Pr){
    HistData <- rep(minoffpeakAggCost.ZiIcPr[,ic,pr], Hh.ZiIc[,ic])
    HistData <- HistData[(HistData > 0) & (HistData < 14)]
    hist(HistData, xlab="", xlim=Xlim, breaks=Breaks, axes=FALSE,
         ylab="", col="skyblue", main=NULL, freq=FALSE, ylim=c(0,2.5))
    axis(1, at=seq(0, 14,1))
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
  }
}
par(Opar)
saveGraph(filename=file.path(graph.path, "hist_minoffpeak_cost_by_income_purpose"), type="pdf") 


#Plot household histograms of minimal travel time during peak costs for all purposes and incomes
#------------------------------------------------------------------------------

#::

# Define breaks and limits for histograms
Breaks <- c(seq(0,14,1))
Xlim <- c(0,14)
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ic in Ic){
  for(pr in Pr){
    HistData <- rep(minpeakAggCost.ZiIcPr[,ic,pr], Hh.ZiIc[,ic])
    HistData <- HistData[(HistData > 0) & (HistData < 14)]
    hist(HistData, xlab="", xlim=Xlim, breaks=Breaks, axes=FALSE,
         ylab="", col="skyblue", main=NULL, freq=FALSE, ylim=c(0,2.5))
    axis(1, at=seq(0,14,1))
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
  }
}
par(Opar)
saveGraph(filename=file.path(graph.path, "hist_minpeak_cost_by_income_purpose"), type="pdf") 

#Plot household histograms of weighted travel time during offpeak costs for all purposes and incomes
#------------------------------------------------------------------------------

#::

# Define breaks and limits for histograms
Breaks <- c(seq(0,14,1))
Xlim <- c(0,14)
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ic in Ic){
  for(pr in Pr){
    HistData <- rep(weightedoffpeakAggCost.ZiIcPr[,ic,pr], Hh.ZiIc[,ic])
    HistData <- HistData[(HistData > 0) & (HistData < 14)]
    hist(HistData, xlab="", xlim=Xlim, breaks=Breaks, axes=FALSE,
         ylab="", col="skyblue", main=NULL, freq=FALSE, ylim=c(0,2.5))
    axis(1, at=seq(0, 14,1))
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
  }
}
par(Opar)
saveGraph(filename=file.path(graph.path, "hist_weightedoffpeak_cost_by_income_purpose"), type="pdf") 


#Plot household histograms of weighted travel time during peak costs for all purposes and incomes
#------------------------------------------------------------------------------

#::

# Define breaks and limits for histograms
Breaks <- c(seq(0,14,1))
Xlim <- c(0,14)
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ic in Ic){
  for(pr in Pr){
    HistData <- rep(weightedpeakAggCost.ZiIcPr[,ic,pr], Hh.ZiIc[,ic])
    HistData <- HistData[(HistData > 0) & (HistData < 14)]
    hist(HistData, xlab="", xlim=Xlim, breaks=Breaks, axes=FALSE,
         ylab="", col="skyblue", main=NULL, freq=FALSE, ylim=c(0,2.5))
    axis(1, at=seq(0, 14,1))
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
  }
}
par(Opar)
saveGraph(filename=file.path(graph.path, "hist_weightedpeak_cost_by_income_purpose"), type="pdf") 

######## map *.ZiIcPr
#Plot maps of minimal travel time during offpeak costs for all purposes and incomes
#----------------------------------------------------------------------------------

#::

# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms

# replace NA with 0
minoffpeakAggCost.ZiIcPr[is.na(minoffpeakAggCost.ZiIcPr)] <- 0

for(ic in Ic){
  for(pr in Pr){
    if((ic == "highInc") & (pr == "hbw")){
      choropleth(TazPoly, minoffpeakAggCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,3,4,6,9), LegendSize=0.3, PlotRef=FALSE,
                 main="", LegendOffset=c(1.014, 1.012))
    } else {
      choropleth(TazPoly, minoffpeakAggCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,3,4,6,9), LegendSize=0, PlotRef=FALSE,
                 main="")
    }
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=0)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=0)
  }
}
par(Opar)

saveGraph(filename=file.path(graph.path, "map_minoffpeak_cost_by_income_purpose"), type="pdf")  


#Plot maps of minimal travel time during peak costs for all purposes and incomes
#----------------------------------------------------------------------------------

#::

# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms

# replace NA with 0
minpeakAggCost.ZiIcPr[is.na(minpeakAggCost.ZiIcPr)] <- 0

for(ic in Ic){
  for(pr in Pr){
    if((ic == "highInc") & (pr == "hbw")){
      choropleth(TazPoly, minpeakAggCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,3,4,6,11), LegendSize=0.3, PlotRef=FALSE,
                 main="", LegendOffset=c(1.014, 1.012))
    } else {
      choropleth(TazPoly, minpeakAggCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,3,4,6,11), LegendSize=0, PlotRef=FALSE,
                 main="")
    }
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=0)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=0)
  }
}
par(Opar)

saveGraph(filename=file.path(graph.path, "map_minpeak_cost_by_income_purpose"), type="pdf") 


#Plot maps of weighted travel time during offpeak costs for all purposes and incomes
#----------------------------------------------------------------------------------

#::

# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms

# replace NA with 0
weightedoffpeakAggCost.ZiIcPr[is.na(weightedoffpeakAggCost.ZiIcPr)] <- 0

for(ic in Ic){
  for(pr in Pr){
    if((ic == "highInc") & (pr == "hbw")){
      choropleth(TazPoly, weightedoffpeakAggCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,3,4,6,11), LegendSize=0.3, PlotRef=FALSE,
                 main="", LegendOffset=c(1.014, 1.012))
    } else {
      choropleth(TazPoly, weightedoffpeakAggCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,3,4,6,11), LegendSize=0, PlotRef=FALSE,
                 main="")
    }
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=0)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=0)
  }
}
par(Opar)

saveGraph(filename=file.path(graph.path, "map_weightedoffpeak_cost_by_income_purpose"), type="pdf")  


#Plot maps of weighted travel time during peak costs for all purposes and incomes
#----------------------------------------------------------------------------------

#::

# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
# Iterate through all purposes and incomes and plot histograms

# replace NA with 0
weightedpeakAggCost.ZiIcPr[is.na(weightedpeakAggCost.ZiIcPr)] <- 0

for(ic in Ic){
  for(pr in Pr){
    if((ic == "highInc") & (pr == "hbw")){
      choropleth(TazPoly, weightedpeakAggCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,3,4,6,13), LegendSize=0.3, PlotRef=FALSE,
                 main="", LegendOffset=c(1.014, 1.012))
    } else {
      choropleth(TazPoly, weightedpeakAggCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                 breaks=c(0,0.001,1,2,3,4,6,13), LegendSize=0, PlotRef=FALSE,
                 main="")
    }
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=0)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=0)
  }
}
par(Opar)

saveGraph(filename=file.path(graph.path, "map_weightedpeak_cost_by_income_purpose"), type="pdf")  

######## plot density line *ZiIcPr
#Plot density line of minimal travel time during offpeak costs for each calculate method and time period
#------------------------------------------------------------------------------------

#::
# Define a vector of data aggregation types
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ic in Ic){
  for(pr in Pr){
    DensityData <- minoffpeakAggCost.ZiIcPr[,ic,pr]
    DensityData[is.na(DensityData)] <- 0
    plot(density(DensityData),main="")
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
  }
}

par(Opar)
saveGraph(filename=file.path(graph.path, "density_minoffpeak_cost_by_income_purpose"), type="pdf")


#Plot density line of minimal travel time during peak costs for each calculate method and time period
#------------------------------------------------------------------------------------

#::
# Define a vector of data aggregation types
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ic in Ic){
  for(pr in Pr){
    DensityData <- minpeakAggCost.ZiIcPr[,ic,pr]
    DensityData[is.na(DensityData)] <- 0
    plot(density(DensityData),main="")
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
  }
}

par(Opar)

saveGraph(filename=file.path(graph.path, "density_minpeak_cost_by_income_purpose"), type="pdf")


#Plot density line of weighted average travel time costs during offpeak for each calculate method and time period
#------------------------------------------------------------------------------------

#::
# Define a vector of data aggregation types
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ic in Ic){
  for(pr in Pr){
    DensityData <- weightedoffpeakAggCost.ZiIcPr[,ic,pr]
    DensityData[is.na(DensityData)] <- 0
    plot(density(DensityData),main="")
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
  }
}

par(Opar)
saveGraph(filename=file.path(graph.path, "density_weightedoffpeak_cost_by_income_purpose"), type="pdf")


#Plot density line of weighted average travel time costs during peak for each calculate method and time period
#------------------------------------------------------------------------------------

#::
# Define a vector of data aggregation types
# Set up plot layout, map will go on top and histogram on bottom
nf <- layout(matrix(1:12,nrow=4))
Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
# Iterate through all purposes and incomes and plot histograms
for(ic in Ic){
  for(pr in Pr){
    DensityData <- weightedpeakAggCost.ZiIcPr[,ic,pr]
    DensityData[is.na(DensityData)] <- 0
    plot(density(DensityData),main="")
    if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
    if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
  }
}

par(Opar)
saveGraph(filename=file.path(graph.path, "density_weightedpeak_cost_by_income_purpose"), type="pdf")
