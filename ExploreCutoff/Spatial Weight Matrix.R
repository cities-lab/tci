# This script calculate spatila weight matrix and identify centers based on Gi 

# DriveAlone offpeak travel time for weight 
source("code/thirdparty/omx.r")
TTime <- readMatrixOMX(file.path(INPUT_DIR, "TDM/Skims.omx"), "driveAloneoffpeakTime")
TTime <- TTime[1:2147,1:2147]
str(TTime)
TTime[1:5,1:5]


# Calculate Gi with totemp, sizeterms(HBS,HBR,HBO))

  # AllData is calculate by scripts identify_centers.R
  # There are only 2147 TAZs in AllData
  str(AllData)
  TotEmp <- AllData$tot.emp
  STHBS <- AllData$st.hbs
  STHBR <- AllData$st.hbr
  STHBO <- AllData$st.hbo


  index <- c(1:2147)
  TTime[is.na(TTime)]
  TotEmp[is.na(TotEmp)] <- 0

# Calculate Gi.Zi.HBW for HBW

# 
Gi.Zi.HBW <- NULL
str(Gi)
for ( i in index) {
  sum.wij.xij <- sum((1/TTime[i,])*TotEmp) - (1/TTime[i,i])*TotEmp[i]
  sum.wij <- sum((1/TTime[i,-i]))
  mean.xij <- mean(TotEmp[-i])
  numerator <- sum.wij.xij - sum.wij*mean.xij
  
  s <- sqrt(sum(TotEmp[-i]^2)/2146 - mean.xij^2)
  S1i <- sum((1/TTime[i,-i])^2)
  denominator <- s*sqrt((2147*S1i-sum.wij^2)/2146)
  
  Gi.Zi.HBW[i] <- numerator/denominator
}

summary(Gi.Zi.HBW)

# Calculate Gi.Zi.HBS for HBS

Gi.Zi.HBS <- NULL
STHBS[is.na(STHBS)] <- 0

for ( i in index) {
  sum.wij.xij <- sum((1/TTime[i,])*STHBS) - (1/TTime[i,i])*STHBS[i]
  sum.wij <- sum((1/TTime[i,-i]))
  mean.xij <- mean(STHBS[-i])
  numerator <- sum.wij.xij - sum.wij*mean.xij
  
  s <- sqrt(sum(STHBS[-i]^2)/2146 - mean.xij^2)
  S1i <- sum((1/TTime[i,-i])^2)
  denominator <- s*sqrt((2147*S1i-sum.wij^2)/2146)
  
  Gi.Zi.HBS[i] <- numerator/denominator
}

summary(Gi.Zi.HBS)

# Calculate Gi.Zi.HBR for HBR

Gi.Zi.HBR <- NULL
STHBR[is.na(STHBR)] <- 0

for ( i in index) {
  sum.wij.xij <- sum((1/TTime[i,])*STHBR) - (1/TTime[i,i])*STHBR[i]
  sum.wij <- sum((1/TTime[i,-i]))
  mean.xij <- mean(STHBR[-i])
  numerator <- sum.wij.xij - sum.wij*mean.xij
  
  s <- sqrt(sum(STHBR[-i]^2)/2146 - mean.xij^2)
  S1i <- sum((1/TTime[i,-i])^2)
  denominator <- s*sqrt((2147*S1i-sum.wij^2)/2146)
  
  Gi.Zi.HBR[i] <- numerator/denominator
}

summary(Gi.Zi.HBR)
Gi.Zi.HBR[1:5]

# Calculate Gi.Zi.HBO for HBO

Gi.Zi.HBO <- NULL
STHBO[is.na(STHBO)] <- 0

for ( i in index) {
  sum.wij.xij <- sum((1/TTime[i,])*STHBO) - (1/TTime[i,i])*STHBO[i]
  sum.wij <- sum((1/TTime[i,-i]))
  mean.xij <- mean(STHBO[-i])
  numerator <- sum.wij.xij - sum.wij*mean.xij
  
  s <- sqrt(sum(STHBO[-i]^2)/2146 - mean.xij^2)
  S1i <- sum((1/TTime[i,-i])^2)
  denominator <- s*sqrt((2147*S1i-sum.wij^2)/2146)
  
  Gi.Zi.HBO[i] <- numerator/denominator
}

summary(Gi.Zi.HBO)

# Identify tazs with Gi > 1.96

taz <- c(1:2147)
Gi.ZiPr <- data.frame(taz, Gi.Zi.HBW,Gi.Zi.HBS,Gi.Zi.HBR,Gi.Zi.HBO)
Gi.ZiPr[1:5,]

Gi.ZiPr$IfGiHBW <- 0
Gi.ZiPr$IfGiHBW[which(Gi.ZiPr$Gi.Zi.HBW > 1.96)] <- 1 

Gi.ZiPr$IfGiHBS <- 0
Gi.ZiPr$IfGiHBS[which(Gi.ZiPr$Gi.Zi.HBS > 1.96)] <- 1 

Gi.ZiPr$IfGiHBR <- 0
Gi.ZiPr$IfGiHBR[which(Gi.ZiPr$Gi.Zi.HBR > 1.96)] <- 1 

Gi.ZiPr$IfGiHBO <- 0
Gi.ZiPr$IfGiHBO[which(Gi.ZiPr$Gi.Zi.HBO > 1.96)] <- 1 


# Identify centers based on Gi and Sizeterms LQ
#-------------
load(file.path(INTERMEDIATE_DIR, "lq.centers.RData"))
# 
taz <- c(1:2147)
taz <- data.frame(taz)

colnames(lq.hbwci) <- c("taz","lq.hbwci")
lq.ci <- merge(taz, lq.hbwci, by="taz", all.x=TRUE)
lq.ci$lq.hbwci[!is.na(lq.ci$lq.hbwci)] <- 1
lq.ci$lq.hbwci[is.na(lq.ci$lq.hbwci)] <- 0

lq.ci[1:5,]


colnames(lq.hbsci) <- c("taz","lq.hbsci")
lq.ci <- merge(lq.ci, lq.hbsci, by="taz", all.x=TRUE)
lq.ci$lq.hbsci[!is.na(lq.ci$lq.hbsci)] <- 1
lq.ci$lq.hbsci[is.na(lq.ci$lq.hbsci)] <- 0

colnames(lq.hbrci) <- c("taz","lq.hbrci")
lq.ci <- merge(lq.ci, lq.hbrci, by="taz", all.x=TRUE)
lq.ci$lq.hbrci[!is.na(lq.ci$lq.hbrci)] <- 1
lq.ci$lq.hbrci[is.na(lq.ci$lq.hbrci)] <- 0


colnames(lq.hboci) <- c("taz","lq.hboci")
lq.ci <- merge(lq.ci, lq.hboci, by="taz", all.x=TRUE)
lq.ci$lq.hboci[!is.na(lq.ci$lq.hboci)] <- 1
lq.ci$lq.hboci[is.na(lq.ci$lq.hboci)] <- 0
nrow(lq.ci)
nrow(Gi.ZiPr)

STLQCi.Gi <- merge(Gi.ZiPr, lq.ci, by="taz", all=TRUE)
STLQCi.Gi[1:5,]

STLQCi.Gi$IfHBW <- STLQCi.Gi$IfGiHBW*10 + STLQCi.Gi$lq.hbwci
STLQCi.Gi$IfHBS <- STLQCi.Gi$IfGiHBS*10 + STLQCi.Gi$lq.hbsci
STLQCi.Gi$IfHBR <- STLQCi.Gi$IfGiHBR*10 + STLQCi.Gi$lq.hbrci
STLQCi.Gi$IfHBO <- STLQCi.Gi$IfGiHBO*10 + STLQCi.Gi$lq.hboci

require(dplyr)

# Calculate frequency of classificaiotn of TAZs

HBWClassification <-  STLQCi.Gi %>%
  group_by(IfHBW) %>%
  summarise(freq=n())

HBSClassification <-  STLQCi.Gi %>%
  group_by(IfHBS) %>%
  summarise(freq=n())

HBRClassification <-  STLQCi.Gi %>%
  group_by(IfHBR) %>%
  summarise(freq=n())

HBOClassification <-  STLQCi.Gi %>%
  group_by(IfHBO) %>%
  summarise(freq=n())



# Map the Centers based on Gi and Sizeterms LQ by Purppose
# Load saveGraph function 
source("code/thirdparty/openGraphSaveGraph.R")

graph.path <- file.path(OUTPUT_DIR, "graphics")

# Names 
load(file.path(INPUT_DIR, "Zi.RData"))

# Define map function
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



## Map IfHBW based on Gi and Sizeterm LQ
## -------------------------------------------

IfHBW <- STLQCi.Gi$IfHBW
names(IfHBW ) <- Zi[1:2147]

# Set up plot layout
par(mfrow=c(1,1))
## map 
choropleth(TazPoly, IfHBW, TazIndex, "RdYlBu",
           breaks=c(-0.1,0.9,1.1,10.1,11.1), LegendSize=0.2, PlotRef=FALSE,
           main="", LegendTitle="Centers", LegendText=c("HBW Free", "HBW Specialized", "HBW Periphery","HBW PCR"))

mtext("HBW Centers based on Gi and sizeterm LQ", cex=1)

saveGraph(filename=file.path(graph.path, "Centers_Gi_sizeterm_lq_hbwci"), type="pdf")


## Map IfHBS based on Gi and Sizeterm LQ
## -------------------------------------------

IfHBS <- STLQCi.Gi$IfHBS
names(IfHBS ) <- Zi[1:2147]

# Set up plot layout
par(mfrow=c(1,1))
## map 
choropleth(TazPoly, IfHBS, TazIndex, "RdYlBu",
           breaks=c(-0.1,0.9,1.1,10.1,11.1), LegendSize=0.2, PlotRef=FALSE,
           main="", LegendTitle="Centers", LegendText=c("HBS Free", "HBS Specialized", "HBS Periphery","HBS PCR"))

mtext("HBS Centers based on Gi and sizeterm LQ", cex=1)

saveGraph(filename=file.path(graph.path, "Centers_Gi_sizeterm_lq_HBSci"), type="pdf")

## Map IfHBR based on Gi and Sizeterm LQ
## -------------------------------------------

IfHBR <- STLQCi.Gi$IfHBR
names(IfHBR ) <- Zi[1:2147]

# Set up plot layout
par(mfrow=c(1,1))
## map 
choropleth(TazPoly, IfHBR, TazIndex, "RdYlBu",
           breaks=c(-0.1,0.9,1.1,10.1,11.1), LegendSize=0.2, PlotRef=FALSE,
           main="", LegendTitle="Centers", LegendText=c("HBR Free", "HBR Specialized", "HBR Periphery","HBR PCR"))

mtext("HBR Centers based on Gi and sizeterm LQ", cex=1)

saveGraph(filename=file.path(graph.path, "Centers_Gi_sizeterm_lq_HBRci"), type="pdf")

## Map IfHBO based on Gi and Sizeterm LQ
## -------------------------------------------

IfHBO <- STLQCi.Gi$IfHBO
names(IfHBO ) <- Zi[1:2147]

# Set up plot layout
par(mfrow=c(1,1))
## map 
choropleth(TazPoly, IfHBO, TazIndex, "RdYlBu",
           breaks=c(-0.1,0.9,1.1,10.1,11.1), LegendSize=0.2, PlotRef=FALSE,
           main="", LegendTitle="Centers", LegendText=c("HBO Free", "HBO Specialized", "HBO Periphery","HBO PCR"))

mtext("HBO Centers based on Gi and sizeterm LQ", cex=1)

saveGraph(filename=file.path(graph.path, "Centers_Gi_sizeterm_lq_HBOci"), type="pdf")


## TDM trips LQ and Gi
# Calculate Gi with totemp, sizeterms(HBS,HBR,HBO))

# DriveAlone offpeak travel time for weight 
source("code/thirdparty/omx.r")
TTime <- readMatrixOMX(file.path(INPUT_DIR, "TDM/Skims.omx"), "driveAloneoffpeakTime")
str(TTime)
TTime[1:5,1:5]

# trips is calculate by scripts ExploreLQ.R
# There are only 2162 TAZs in trips
str(trips)
trips[1:5,]

HBWtrips <- trips$hbwtrips
HBStrips <- trips$hbstrips
HBRtrips <- trips$hbrtrips
HBOtrips <- trips$hbotrips



index <- c(1:2162)
TTime[is.na(TTime)]
HBWtrips[is.na(HBWtrips)]


# Calculate TripsGi.Zi.HBW for HBW

# 
TripsGi.Zi.HBW <- NULL
str(Gi)
for ( i in index) {
  sum.wij.xij <- sum((1/TTime[i,])*HBWtrips) - (1/TTime[i,i])*HBWtrips[i]
  sum.wij <- sum((1/TTime[i,-i]))
  mean.xij <- mean(HBWtrips[-i])
  numerator <- sum.wij.xij - sum.wij*mean.xij
  
  s <- sqrt(sum(HBWtrips[-i]^2)/2162 - mean.xij^2)
  S1i <- sum((1/TTime[i,-i])^2)
  denominator <- s*sqrt((2162*S1i-sum.wij^2)/2161)
  
  TripsGi.Zi.HBW[i] <- numerator/denominator
}

summary(TripsGi.Zi.HBW)

# Calculate TripsGi.Zi.HBS for HBS

# 
TripsGi.Zi.HBS <- NULL
str(Gi)
for ( i in index) {
  sum.wij.xij <- sum((1/TTime[i,])*HBStrips) - (1/TTime[i,i])*HBStrips[i]
  sum.wij <- sum((1/TTime[i,-i]))
  mean.xij <- mean(HBStrips[-i])
  numerator <- sum.wij.xij - sum.wij*mean.xij
  
  s <- sqrt(sum(HBStrips[-i]^2)/2162 - mean.xij^2)
  S1i <- sum((1/TTime[i,-i])^2)
  denominator <- s*sqrt((2162*S1i-sum.wij^2)/2161)
  
  TripsGi.Zi.HBS[i] <- numerator/denominator
}

summary(TripsGi.Zi.HBS)


# Calculate TripsGi.Zi.HBR for HBR

# 
TripsGi.Zi.HBR <- NULL
str(Gi)
for ( i in index) {
  sum.wij.xij <- sum((1/TTime[i,])*HBRtrips) - (1/TTime[i,i])*HBRtrips[i]
  sum.wij <- sum((1/TTime[i,-i]))
  mean.xij <- mean(HBRtrips[-i])
  numerator <- sum.wij.xij - sum.wij*mean.xij
  
  s <- sqrt(sum(HBRtrips[-i]^2)/2162 - mean.xij^2)
  S1i <- sum((1/TTime[i,-i])^2)
  denominator <- s*sqrt((2162*S1i-sum.wij^2)/2161)
  
  TripsGi.Zi.HBR[i] <- numerator/denominator
}

summary(TripsGi.Zi.HBR)

# Calculate TripsGi.Zi.HBO for HBO

# 
TripsGi.Zi.HBO <- NULL
str(Gi)
for ( i in index) {
  sum.wij.xij <- sum((1/TTime[i,])*HBOtrips) - (1/TTime[i,i])*HBOtrips[i]
  sum.wij <- sum((1/TTime[i,-i]))
  mean.xij <- mean(HBOtrips[-i])
  numerator <- sum.wij.xij - sum.wij*mean.xij
  
  s <- sqrt(sum(HBOtrips[-i]^2)/2162 - mean.xij^2)
  S1i <- sum((1/TTime[i,-i])^2)
  denominator <- s*sqrt((2162*S1i-sum.wij^2)/2161)
  
  TripsGi.Zi.HBO[i] <- numerator/denominator
}

summary(TripsGi.Zi.HBO)

# Identify tazs with Gi > 1.96

taz <- c(1:2162)
TripsGi.ZiPr <- data.frame(taz, TripsGi.Zi.HBW,TripsGi.Zi.HBS,TripsGi.Zi.HBR,TripsGi.Zi.HBO)
TripsGi.ZiPr[1:5,]

TripsGi.ZiPr$IfGiHBW <- 0
TripsGi.ZiPr$IfGiHBW[which(TripsGi.ZiPr$TripsGi.Zi.HBW > 1.96)] <- 1 

TripsGi.ZiPr$IfGiHBS <- 0
TripsGi.ZiPr$IfGiHBS[which(TripsGi.ZiPr$TripsGi.Zi.HBS > 1.96)] <- 1 

TripsGi.ZiPr$IfGiHBR <- 0
TripsGi.ZiPr$IfGiHBR[which(TripsGi.ZiPr$TripsGi.Zi.HBR > 1.96)] <- 1 

TripsGi.ZiPr$IfGiHBO <- 0
TripsGi.ZiPr$IfGiHBO[which(TripsGi.ZiPr$TripsGi.Zi.HBO > 1.96)] <- 1 

## TDM trips LQ and Gi
# Identify centers based on Gi and TDM trips LQ 
load(file.path(INTERMEDIATE_DIR, "trips.lq.centers.RData"))
ci.zipr[1:5,]

taz <- c(1:2162)
TripsLQCi.ZiPr <- data.frame(taz, ci.zipr)

TripsLQCi.Gi <- merge(TripsGi.ZiPr, TripsLQCi.ZiPr, by="taz", all.x=TRUE)
TripsLQCi.Gi[1:5,]


TripsLQCi.Gi$IfHBW <- TripsLQCi.Gi$IfGiHBW*10 + TripsLQCi.Gi$lq.hbwci
TripsLQCi.Gi$IfHBS <- TripsLQCi.Gi$IfGiHBS*10 + TripsLQCi.Gi$lq.hbsci
TripsLQCi.Gi$IfHBR <- TripsLQCi.Gi$IfGiHBR*10 + TripsLQCi.Gi$lq.hbrci
TripsLQCi.Gi$IfHBO <- TripsLQCi.Gi$IfGiHBO*10 + TripsLQCi.Gi$lq.hboci
TripsLQCi.Gi[1:5,]


# Calculate frequency of classificaiotn of TAZs

HBWClassification <-  TripsLQCi.Gi %>%
  group_by(IfHBW) %>%
  summarise(freq=n())

HBSClassification <-  TripsLQCi.Gi %>%
  group_by(IfHBS) %>%
  summarise(freq=n())

HBRClassification <-  TripsLQCi.Gi %>%
  group_by(IfHBR) %>%
  summarise(freq=n())

HBOClassification <-  TripsLQCi.Gi %>%
  group_by(IfHBO) %>%
  summarise(freq=n())


# Map the Centers based on Gi and TDM trips LQ by Purppose
# Load saveGraph function 
source("code/thirdparty/openGraphSaveGraph.R")

graph.path <- file.path(OUTPUT_DIR, "graphics")

# Names 
load(file.path(INPUT_DIR, "Zi.RData"))

# Define map function
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



## Map IfHBW based on Gi and TDM trips LQ
## -------------------------------------------

IfHBW <- TripsLQCi.Gi$IfHBW
names(IfHBW ) <- Zi[1:2147]

# Set up plot layout
par(mfrow=c(1,1))
## map 
choropleth(TazPoly, IfHBW, TazIndex, "RdYlBu",
           breaks=c(-0.1,0.9,1.1,10.1,11.1), LegendSize=0.2, PlotRef=FALSE,
           main="", LegendTitle="Centers", LegendText=c("HBW Free", "HBW Specialized", "HBW Periphery","HBW PCR"))

mtext("HBW Centers based on Gi and trips LQ", cex=1)

saveGraph(filename=file.path(graph.path, "Centers_Gi_trips_lq_hbwci"), type="pdf")

## Map IfHBS based on Gi and TDM trips LQ
## -------------------------------------------

IfHBS <- TripsLQCi.Gi$IfHBS
names(IfHBS ) <- Zi[1:2147]

# Set up plot layout
par(mfrow=c(1,1))
## map 
choropleth(TazPoly, IfHBS, TazIndex, "RdYlBu",
           breaks=c(-0.1,0.9,1.1,10.1,11.1), LegendSize=0.2, PlotRef=FALSE,
           main="", LegendTitle="Centers", LegendText=c("HBS Free", "HBS Specialized", "HBS Periphery","HBS PCR"))

mtext("HBS Centers based on Gi and trips LQ", cex=1)

saveGraph(filename=file.path(graph.path, "Centers_Gi_trips_lq_HBSci"), type="pdf")

## Map IfHBR based on Gi and TDM trips LQ
## -------------------------------------------

IfHBR <- TripsLQCi.Gi$IfHBR
names(IfHBR ) <- Zi[1:2147]

# Set up plot layout
par(mfrow=c(1,1))
## map 
choropleth(TazPoly, IfHBR, TazIndex, "RdYlBu",
           breaks=c(-0.1,0.9,1.1,10.1,11.1), LegendSize=0.2, PlotRef=FALSE,
           main="", LegendTitle="Centers", LegendText=c("HBR Free", "HBR Specialized", "HBR Periphery","HBR PCR"))

mtext("HBR Centers based on Gi and trips LQ", cex=1)

saveGraph(filename=file.path(graph.path, "Centers_Gi_trips_lq_HBRci"), type="pdf")


## Map IfHBO based on Gi and TDM trips LQ
## -------------------------------------------

IfHBO <- TripsLQCi.Gi$IfHBO
names(IfHBO ) <- Zi[1:2147]

# Set up plot layout
par(mfrow=c(1,1))
## map 
choropleth(TazPoly, IfHBO, TazIndex, "RdYlBu",
           breaks=c(-0.1,0.9,1.1,10.1,11.1), LegendSize=0.2, PlotRef=FALSE,
           main="", LegendTitle="Centers", LegendText=c("HBO Free", "HBO Specialized", "HBO Periphery","HBO PCR"))

mtext("HBO Centers based on Gi and trips LQ", cex=1)

saveGraph(filename=file.path(graph.path, "Centers_Gi_trips_lq_HBOci"), type="pdf")






