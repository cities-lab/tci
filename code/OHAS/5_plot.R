# This script generates a variety of plots and maps of cost

# load saveGraph function
source("code/openGraphSaveGraph.R")

WD <- getwd()
setwd("data/OHASTTime/")

  
# load results 
#===============

#::

 #  load cost
  load("results/Cost.ZiPr.RData")
  load("results/Cost.ZiIc.RData")
  load("results/Cost.Zi.RData")
  load("results/Cost.Di.Rdata")

# load household number in each TAZ by income group
	load("Rdata/Hh.ZiIc.RData")

#Load mapping data and functions   
#===============================

#::

	# load required libraries
	library(maptools)
	library(RColorBrewer)

	# read in the taz data shapefile
	TazFile <- paste("gis/", "TAZ.shp", sep="/") 
	TazPoly <- readShapePoly(TazFile)

	# extract the attribute data
	TazData <- TazPoly@data

	# make an index vector to the taz
	TazIndex <- as.character(TazData$newtaz)

	# get the location of the reference zone
	TazCentroids.ZiXy <- coordinates(TazPoly)
	rownames(TazCentroids.ZiXy) <- TazIndex


	# write a function to plot taz values as a coropleth map
	coropleth <- function(geo=TazPoly, data, DataIndex=TazIndex, palette="Blues", breaks,
                      LegendSize=1, PlotRef=TRUE, LegendOffset=c(1,1),
                      LegendTitle=NULL, RefColor="red", ...){
 	 	DataCut <- cut(data[DataIndex], breaks, include.lowest=TRUE, labels=FALSE)
  		ColorPalette <- brewer.pal(length(breaks)-1, palette)
  		colors <- ColorPalette[DataCut]
  		plot(geo, col=colors, xaxt="n", yaxt="n", border=NA, ...)
  		if(PlotRef) points(RefZoneCent[1], RefZoneCent[2], pch=1, col=RefColor, cex=2, lwd=2)
  		LegendText <- paste(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)], sep=" - ")
  		if(LegendSize != 0){
    			legend("bottomleft", legend=LegendText,
           		title=LegendTitle, cex=LegendSize, fill=ColorPalette)
  		   }
  		if(PlotRef){
   		  points(7410262, 543378, pch=1, col=RefColor, cex=2, lwd=2)
    		  text(7415412, 543378, "Reference Zone", pos=4)
 	 }
	}

# load vector of zone names 
     load("Rdata/Zi.RData")

# make names for household incme groups and trip types for plotting 
     
     IcNames <- c("Low Income", "Mid Income", "High Income")
     Ic <- c("lowInc", "midInc", "highInc")
     names(IcNames) <- Ic

     PrNames <- c("Work", "Shopping", "Recreation", "Other")
     Pr <- c("hbw", "hbs", "hbr", "hbo")
     names(PrNames) <- Pr


# Map cost  
# -------------------------------------------

#::

  # load minimal travel time cost during offpeak
	  Cost.Zi <- round(Cost.Zi,2)
    Cost.Zi[is.na(Cost.Zi)] <- 0
	## map 
	coropleth(TazPoly, Cost.Zi, TazIndex, "RdYlBu",
                  breaks=c(seq(0, 4, 0.5),6,10,35), LegendSize=0.6, PlotRef=FALSE,
          	      main="", LegendTitle="Travel Time Cost")

	mtext("OHAS Travel Time Cost", cex=1.8)
	saveGraph(filename="graphics/Cost_Zi", type="pdf")


# Plot maps of cost by purposes 
#------------------------------------------------------

#::
     
     
     # Set up plot layout
     nf <- layout(matrix(1:4,nrow=2))
     Opar <- par(mar=c(1,1,1,1), oma=c(1,2.5,2.5,1))
     # Iterate through all purposes and plot map
    
          for(pr in Pr){
               MapData <- Cost.ZiPr[,pr]
               MapData[is.na(MapData)] <- 0
               if(pr == "hbs") {
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0, 5, 1),7,10,20,60), LegendSize=0.6, PlotRef=FALSE,
                    main="", LegendOffset=c(1.011, 1.02))
               } else {
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0, 5, 1),7,10,20,60), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               mtext(PrNames[pr], side=2, line=0.5)
                              }
            par(Opar)
    
      
     saveGraph(filename="graphics/map_cost_by_purpose", type="pdf")



# Plot maps of cost by income groups 
#------------------------------------------------------

#::
     
     # Set up plot layout
     nf <- layout(matrix(1:4,nrow=2))
     Opar <- par(mar=c(1,1,1,1), oma=c(1,2.5,2.5,1))
     # Iterate through all purposes and plot map
    
          for(ic in Ic){
               MapData <- Cost.ZiIc[,ic]
               MapData[is.na(MapData)] <- 0
               if(ic == "midInc") {
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0, 5, 1),7,10,20,70), LegendSize=0.6, PlotRef=FALSE,
                    main="", LegendOffset=c(1.011, 1.02))
               } else {
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0, 5, 1),7,10,20,70), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               mtext(IcNames[ic], side=2, line=0.5)
                              }
            par(Opar)
    
      
     saveGraph(filename="graphics/map_cost_by_income", type="pdf")


# Plot household histograms of cost by purpose for each calculate method and time period
#------------------------------------------------------------------------------------

#::

     # Define breaks and limits for histograms
     Breaks <- seq(0, 14, 1)
     Xlim <- c(0,14)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:4,nrow=2))
     # Iterate through all purposes and plot histograms
  
          for(pr in Pr){
               HistData <- rep(Cost.ZiPr[,pr], rowSums(Hh.ZiIc))
               HistData <- HistData[(HistData > 0) & (HistData < 14)]
               hist(HistData, xlab="", breaks=Breaks, xlim=Xlim, ylim=c(0,1.75),
               ylab="", col="skyblue", main=NULL, freq=FALSE, axes=FALSE)
               axis(1, at=seq(0,14,1))
              
		   mtext(PrNames[pr], side=2, line=0.5)

               }
         
     par(Opar)
     saveGraph(filename="graphics/hist_cost_purpose", type="pdf")

# Plot household histograms of cost by income for each calculate method and time period
#------------------------------------------------------------------------------------

#::

     # Define breaks and limits for histograms
     Breaks <- seq(0, 14, 1)
     Xlim <- c(0,14)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:4,nrow=2))
     # Iterate through all purposes and plot histograms
  
          for(ic in Ic){
               HistData <- rep(Cost.ZiIc[,ic], rowSums(Hh.ZiIc))
               HistData <- HistData[(HistData > 0) & (HistData < 14)]
               hist(HistData, xlab="", breaks=Breaks, xlim=Xlim, ylim=c(0,1.75),
               ylab="", col="skyblue", main=NULL, freq=FALSE, axes=FALSE)
               axis(1, at=seq(0,14,1))
              
		   mtext(IcNames[ic], side=2, line=0.5)

               }
         
     par(Opar)
     saveGraph(filename="graphics/hist_cost_income", type="pdf")


# Plot comparisons of travel time costs by UGB
#----------------------------------------------

#::

     #Load district designations
     DiNames <- as.character(c(1:20))
     
     # add name index of districts 
     index <- as.character(c(1:20))



     #Barplot of offpeak minimal travel time cost 
     BarCenter <- barplot(Cost.Di[index], xlab="", ylab="Travel Time Cost", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Districts", side=1, line=0.5, cex=1)
     text(as.vector(BarCenter), 0.1, labels=DiNames, srt=90, pos=4, offset=0)

          
     saveGraph(filename="graphics/district_cost",type="pdf")

# set workplace
setwd(WD)
