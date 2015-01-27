# plot maps and descriptives of travel time cost
# TODO

# load saveGraph function
 source("code/openGraphSaveGraph.R")

# Load data 
 
 # load(file.path(INTERMEDIATE_DIR,"newtcost.RData"))

#Load mapping data and functions   
  #===============================
  
  #::
  
  # load required libraries
  library(maptools)
  library(RColorBrewer)
  
  # read in the taz data shapefile
  TazFile <- paste("data/CommonData/gis/", "TAZ.shp", sep="/") 
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
  
  
# make names for household incme groups, trip types and calculation method for plotting 
  
  IcNames <- c("Low Income", "Mid Income", "High Income")
  Ic <- c("lowInc", "midInc", "highInc")
  names(IcNames) <- Ic
  
  PrNames <- c("Work", "Shopping", "Recreation", "Other")
  Pr <- c("hbw", "hbs", "hbr", "hbo")
  names(PrNames) <- Pr
  
  CmNames <- c("mintcost", "avgtcost", "maxtcost")
  Cm <- c("min", "avg", "max")
  names(CmNames) <- Cm  

 

    
######## map minttcost.ZiIcPr
  #Plot maps of minimal travel time   costs for all purposes and incomes
  #----------------------------------------------------------------------------------
  
  #::
  
  # Set up plot layout, map will go on top and histogram on bottom
  nf <- layout(matrix(1:12,nrow=4))
  Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
  # Iterate through all purposes and incomes and plot histograms
  
  # replace NA with 0
  minttcost.ZiIcPr[is.na(minttcost.ZiIcPr)] <- 0
  
  for(ic in Ic){
    for(pr in Pr){
      if((ic == "highInc") & (pr == "hbw")){
        choropleth(TazPoly, minttcost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                  breaks=c(0,0.001,1,2,3,6,40), LegendSize=0.3, PlotRef=FALSE,
                  main="", LegendOffset=c(1.014, 1.012))
      } else {
        choropleth(TazPoly, minttcost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                  breaks=c(0,0.001,1,2,3,6,40), LegendSize=0, PlotRef=FALSE,
                  main="")
      }
      if(ic == "lowInc") mtext(PrNames[pr], side=2, line=1)
      if(pr == "hbw") mtext(IcNames[ic], side=3, line=0.1)
    }
  }
  par(Opar)

  saveGraph(filename="data/OHASTTime/graphics/map_min_tcost_by_income_purpose", type="pdf")  
  
  
######## map avgttcost.ZiIcPr
  #Plot maps of minimal travel time   costs for all purposes and incomes
  #----------------------------------------------------------------------------------
  
  #::
  
  # Set up plot layout, map will go on top and histogram on bottom
  nf <- layout(matrix(1:12,nrow=4))
  Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
  # Iterate through all purposes and incomes and plot histograms
  
  # replace NA with 0
  avgttcost.ZiIcPr[is.na(avgttcost.ZiIcPr)] <- 0
  
  for(ic in Ic){
    for(pr in Pr){
      if((ic == "highInc") & (pr == "hbw")){
        choropleth(TazPoly, avgttcost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                  breaks=c(0,0.001,1,2,3,6,40), LegendSize=0.3, PlotRef=FALSE,
                  main="", LegendOffset=c(1.014, 1.012))
      } else {
        choropleth(TazPoly, avgttcost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                  breaks=c(0,0.001,1,2,3,6,40), LegendSize=0, PlotRef=FALSE,
                  main="")
      }
      if(ic == "lowInc") mtext(PrNames[pr], side=2, line=1)
      if(pr == "hbw") mtext(IcNames[ic], side=3, line=0.1)
    }
  }
  par(Opar)
  
  saveGraph(filename="data/OHASTTime/graphics/map_avg_tcost_by_income_purpose", type="pdf")  
  

  
  ######## map maxttcost.ZiIcPr
  #Plot maps of minimal travel time   costs for all purposes and incomes
  #----------------------------------------------------------------------------------
  
  #::
  
  # Set up plot layout, map will go on top and histogram on bottom
  nf <- layout(matrix(1:12,nrow=4))
  Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
  # Iterate through all purposes and incomes and plot histograms
  
  # replace NA with 0
  maxttcost.ZiIcPr[is.na(maxttcost.ZiIcPr)] <- 0
  
  for(ic in Ic){
    for(pr in Pr){
      if((ic == "highInc") & (pr == "hbw")){
        choropleth(TazPoly, maxttcost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                  breaks=c(0,0.001,1,2,3,6,40), LegendSize=0.3, PlotRef=FALSE,
                  main="", LegendOffset=c(1.014, 1.012))
      } else {
        choropleth(TazPoly, maxttcost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                  breaks=c(0,0.001,1,2,3,6,40), LegendSize=0, PlotRef=FALSE,
                  main="")
      }
      if(ic == "lowInc") mtext(PrNames[pr], side=2, line=1)
      if(pr == "hbw") mtext(IcNames[ic], side=3, line=0.1)
    }
  }
  par(Opar)
  
  saveGraph(filename="data/OHASTTime/graphics/map_max_tcost_by_income_purpose", type="pdf")  
  

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
 
  
  saveGraph(filename="data/OHASTTime/graphics/map_hhtcost_by_income_method", type="pdf")
  
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
  
  
  saveGraph(filename="data/OHASTTime/graphics/map_hhtcost_by_method", type="pdf")

  
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
  
  saveGraph(filename="data/OHASTTime/graphics/hhtcost_district",type="pdf")


  
######## plot density line minttcost.ZiIcPr
  #Plot density line of minimal travel time   costs for each calculate method and time period
  #------------------------------------------------------------------------------------
  
  #::
  # Define a vector of data aggregation types
  # Set up plot layout, map will go on top and histogram on bottom
  nf <- layout(matrix(1:12,nrow=4))
  Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
  # Iterate through all purposes and incomes and plot histograms
  for(ic in Ic){
    for(pr in Pr){
      DensityData <- minttcost.ZiIcPr[,ic,pr]
      DensityData[is.na(DensityData)] <- 0
      plot(density(DensityData),main="")
      if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
      if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
    }
  }
  
  par(Opar)
  saveGraph(filename="data/OHASTTime/graphics/density_mintcost_by_income_purpose", type="pdf")
  
######## plot density line avgttcost.ZiIcPr
  #Plot density line of minimal travel time   costs for each calculate method and time period
  #------------------------------------------------------------------------------------
  
  #::
  # Define a vector of data aggregation types
  # Set up plot layout, map will go on top and histogram on bottom
  nf <- layout(matrix(1:12,nrow=4))
  Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
  # Iterate through all purposes and incomes and plot histograms
  for(ic in Ic){
    for(pr in Pr){
      DensityData <- avgttcost.ZiIcPr[,ic,pr]
      DensityData[is.na(DensityData)] <- 0
      plot(density(DensityData),main="")
      if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
      if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
    }
  }
  
  
  saveGraph(filename="data/OHASTTime/graphics/density_avgtcost_by_income_purpose", type="pdf")
  
  
######## plot density line maxttcost.ZiIcPr
  #Plot density line of minimal travel time   costs for each calculate method and time period
  #------------------------------------------------------------------------------------
  
  #::
  # Define a vector of data aggregation types
  # Set up plot layout, map will go on top and histogram on bottom
  nf <- layout(matrix(1:12,nrow=4))
  Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
  # Iterate through all purposes and incomes and plot histograms
  for(ic in Ic){
    for(pr in Pr){
      DensityData <- maxttcost.ZiIcPr[,ic,pr]
      DensityData[is.na(DensityData)] <- 0
      plot(density(DensityData),main="")
      if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
      if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
    }
  }
  
  
  saveGraph(filename="data/OHASTTime/graphics/density_maxtcost_by_income_purpose", type="pdf")
  
  
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
  
  saveGraph(filename="data/OHASTTime/graphics/density_hhtcost_by_income_method", type="pdf")
  
  
# set workplace
  setwd(WD)
