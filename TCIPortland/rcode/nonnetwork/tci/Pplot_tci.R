#This script prepares the workspace and file directories for calculating the Travel Cost Index and related measures. The script needs to be executed at the top level of a JEMnR model structure.
 setwd("C:/Users/huajie/Desktop/TCIPortland50")
 codeLoc<-getwd()
 setwd(paste(codeLoc,"/performancemeasures/",sep=""))
 codeLoc<-getwd()

#::

     # Reference data
     load("final_output/tci/AveMarketCost.ZiIcPr.RData")
     load("intm_output/reference/AttractionScore.RData")
     load("intm_output/reference/ReferenceZone.RData")
     load("intm_output/reference/ReferenceAttractions.RData")

     # Number of household of income groups in each TAZ
     load("inputs/Rdata/Hh.ZiIc.RData")       
     
     # Zi 
     load("inputs/Rdata/Zi.RData")
     # Best market cost
     load("final_output/tci/BestMarketCost.ZiIc.RData")
     load("final_output/tci/BestMarketCost.ZiPr.RData")
     load("final_output/tci/BestMarketCost.Zi.RData")
     load("final_output/tci/BestMarketCost.DiIc.RData")
     load("final_output/tci/BestMarketCost.DiPr.RData")
     load("final_output/tci/BestMarketCost.Di.RData")

     # Composite market cost
     load("final_output/tci/CompMarketCost.ZiIc.RData")
     load("final_output/tci/CompMarketCost.ZiPr.RData")
     load("final_output/tci/CompMarketCost.Zi.RData")
     load("final_output/tci/CompMarketCost.DiIc.RData")
     load("final_output/tci/CompMarketCost.DiPr.RData")
     load("final_output/tci/CompMarketCost.Di.RData")


     # Average market cost
     load("final_output/tci/AveMarketCost.ZiIc.RData")
     load("final_output/tci/AveMarketCost.ZiPr.RData")
     load("final_output/tci/AveMarketCost.Zi.RData")
     load("final_output/tci/AveMarketCost.DiIc.RData")
     load("final_output/tci/AveMarketCost.DiPr.RData")
     load("final_output/tci/AveMarketCost.Di.RData")


     # Travel Cost Index
     load("final_output/tci/Tci.ZiIc.RData")
     load("final_output/tci/Tci.ZiPr.RData")
     load("final_output/tci/Tci.Zi.RData")
     load("final_output/tci/Tci.DiIc.RData")
     load("final_output/tci/Tci.DiPr.RData")
     load("final_output/tci/Tci.Di.RData")


     # Travel Cost Index 2
     load("final_output/tci/Tci2.ZiIc.RData")
     load("final_output/tci/Tci2.ZiPr.RData")
     load("final_output/tci/Tci2.Zi.RData")
     load("final_output/tci/Tci2.DiIc.RData")
     load("final_output/tci/Tci2.DiPr.RData")
     load("final_output/tci/Tci2.Di.RData")


     # Travel Cost Index 3
     load("final_output/tci/Tci3.ZiIc.RData")
     load("final_output/tci/Tci3.ZiPr.RData")
     load("final_output/tci/Tci3.Zi.RData")
     load("final_output/tci/Tci3.DiIc.RData")
     load("final_output/tci/Tci3.DiPr.RData")
     load("final_output/tci/Tci3.Di.RData")


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
     RefZoneCent <- TazCentroids.ZiXy[ReferenceZone,]

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
               legend("topright", legend=LegendText,
                    title=LegendTitle, cex=LegendSize, fill=ColorPalette)
               }
          if(PlotRef){
            points(7410262, 543378, pch=1, col=RefColor, cex=2, lwd=2)
            text(7415412, 543378, "Reference Zone", pos=4)
            }
          }

     # Make names for household income groups and trip types for plotting
     
     # Define income group abbreviation
     Ic <- c("lowInc", "midInc", "highInc")     

     # Define trip purpose abbreviation
     # The purposes for this study (now) are limited to the home-based trips
     # They exclude nonhome-based trips, school trips and college trips

     Pr <- c("hbw", "hbs", "hbr", "hbo")
     IcNames <- c("Low Income", "Mid Income", "High Income")
     names(IcNames) <- Ic
     PrNames <- c("Work", "Shopping", "Recreation", "Other")
     names(PrNames) <- Pr

#Plot histograms and maps of various measures
#============================================

#Plot histograms and map of attraction scores
#--------------------------------------------

#::

     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(c(1,2),nrow=2), widths=4, heights=c(3,1))
     # Calculate log of scores and set -Inf to -14
     AttractionScore2 <- log(AttractionScore.Zi)
     AttractionScore2[is.infinite(AttractionScore2)] <- -14
     # Set margins for 1st plot
     Opar <- par(mar=c(1,1,2.25,1))
     # Plot map of scores
     coropleth(TazPoly, AttractionScore2, TazIndex, "RdYlBu",
          breaks=c(-14, seq(-5, 0, 1),1, 2, 3),
          main="", LegendOffset=c(1.05,1), LegendTitle="log(score)",
          RefColor="red")
     # Add plot title
     # mtext("Geographic and Frequency Distributions of Attraction Scores", line=1, cex=1.15)
     # Set margins for 2nd plot
     par(mar=c(4,5,1,1))
     # Plot histogram of scores, using the same breaks and colors as the map
     HistData <- rep(AttractionScore2, rowSums(Hh.ZiIc))
     hist(HistData, xlab="", ylab="",
          breaks=c(-14, seq(-10, 0, 2), 0.5, 1, 2, 3, 4),
          col=brewer.pal(11, "RdYlBu"), main="", freq=FALSE)
     mtext("Household\nFrequency", side=2, line=2.25)
     mtext("log(score)", side=1, line=2)
     # Save the plot and restore the graphics parameters
     savePlot(filename="final_output/tci/graphics/attraction_scores", type="pdf") 
     par(Opar)

     

#Plot household histograms of average market costs for all purposes and incomes
#------------------------------------------------------------------------------

#::

     # Define breaks and limits for histograms
     Breaks <- seq(0, 4, 0.5)
     Xlim <- c(0,5)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ic in Ic){
          for(pr in Pr){
               HistData <- rep(AveMarketCost.ZiIcPr[,ic,pr], Hh.ZiIc[,ic])
               hist(HistData, xlab="", xlim=Xlim, breaks=Breaks, axes=FALSE,
               ylab="", col="skyblue", main=NULL, freq=FALSE, ylim=c(0,2.5))
               axis(1, at=seq(0,4,0.5))
               if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
               if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
               }
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/hist_ave_cost_by_income_purpose", type="pdf")     

#Plot maps of average market costs for all purposes and incomes
#--------------------------------------------------------------

#::

     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ic in Ic){
          for(pr in Pr){
               if((ic == "highInc") & (pr == "hbw")){
                    coropleth(TazPoly, AveMarketCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                    breaks=c(seq(0.5,5.5, 0.5)), LegendSize=0.6, PlotRef=FALSE,
                    main="", LegendOffset=c(1.014, 1.012))
               } else {
                    coropleth(TazPoly, AveMarketCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                    breaks=c(seq(0.5, 5.5, 0.5)), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               if(ic == "lowInc") mtext(PrNames[pr], side=2, line=1.75)
               if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
               }
          }
     par(Opar)
     
     savePlot(filename="final_output/tci/graphics/map_ave_cost_by_income_purpose", type="pdf")

#Plot household histograms of TCI values by purpose for each mode aggregregation type
#------------------------------------------------------------------------------------

#::
# Define a vector of data aggregation types
     Ag <- c("Tci.ZiPr", "Tci2.ZiPr", "Tci3.ZiPr")
     names(Ag) <- c("Average", "Minimum", "Composite")
     # Define breaks and limits for histograms
     Breaks <- seq(0, 6, 0.5)
     Xlim <- c(0,6)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ag in names(Ag)){
          for(pr in Pr){
               HistData <- rep(get(Ag[ag])[,pr], rowSums(Hh.ZiIc))
               HistData <- HistData[(HistData > 0) & (HistData < 6)]
               hist(HistData, xlab="", breaks=Breaks, xlim=Xlim, ylim=c(0,1.75),
               ylab="", col="skyblue", main=NULL, freq=FALSE, axes=FALSE)
               axis(1, at=seq(0,6,1))
               if(ag == "Average") mtext(PrNames[pr], side=2, line=3)
               if(pr == "hbw") mtext(ag, side=3, line=1)
               }
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/hist_tci_by_modeagg_purpose", type="pdf")
     

#Plot household histograms of TCI values by income for each mode aggregregation type
#-----------------------------------------------------------------------------------

#::
     windows(6, 4.5)
     # Define a vector of data aggregation types
     Ag <- c("Tci.ZiIc", "Tci2.ZiIc", "Tci3.ZiIc")
     names(Ag) <- c("Average", "Minimum", "Composite")
     # Define breaks and limits for histograms
     Breaks <- seq(0, 6, 0.5)
     Xlim <- c(0,6)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:9,nrow=3))
     Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ag in names(Ag)){
          for(ic in Ic){
               HistData <- rep(get(Ag[ag])[,ic], Hh.ZiIc[,ic])
               HistData <- HistData[(HistData > 0) & (HistData < 6)]
               hist(HistData, xlab="", breaks=Breaks, xlim=Xlim, ylim=c(0,1.75),
               ylab="", col="skyblue", main=NULL, freq=FALSE, axes=FALSE)
               axis(1, at=seq(0,6,1))
               if(ag == "Average") mtext(IcNames[ic], side=2, line=3)
               if(ic == "lowInc") mtext(ag, side=3, line=1)
               }
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/hist_tci_by_modeagg_income", type="pdf")
     

#Plot maps of TCI values by aggregation type by incomes
#------------------------------------------------------

#::

     windows(6, 4.5)
     # Define a vector of data aggregation types
     Ag <- c("Tci.ZiIc", "Tci2.ZiIc", "Tci3.ZiIc")
     names(Ag) <- c("Average", "Minimum", "Composite")
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:9,nrow=3))
     Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ag in names(Ag)){
          for(ic in Ic){
               MapData <- get(Ag[ag])[,ic]
                 MapData[MapData < 0] <- 35
                 MapData[MapData > 40] <- 35 #### replace larger than 40 with 35 
               if((ic == "lowInc") & (ag == "Composite")){
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0, 3, 0.5),6,15,35), LegendSize=0.6, PlotRef=FALSE,
                    main="", LegendOffset=c(1.011, 1.02))
               } else {
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0, 3, 0.5),6,15,35), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               if(ag == "Average") mtext(IcNames[ic], side=2, line=1.75)
               if(ic == "lowInc") mtext(ag, side=3, line=1.75)
               }
          }
     par(Opar)
    
     savePlot(filename="final_output/tci/graphics/map_tci_by_modeagg_income",type="pdf")
     dev.off()

#Plot maps of TCI values by aggregation type by purpose
#------------------------------------------------------

#::

     # Define a vector of data aggregation types
     Ag <- c("Tci.ZiPr", "Tci2.ZiPr", "Tci3.ZiPr")
     names(Ag) <- c("Average", "Minimum", "Composite")
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ag in names(Ag)){
          for(pr in Pr){
               MapData <- get(Ag[ag])[,pr]
               MapData[MapData < 0] <- 35
               MapData[MapData > 40] <- 35       #### replace larger than 40 with 35  
               if((pr == "hbw") & (ag == "Composite")){
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0, 3, 0.5),6,15,35), LegendSize=0.6, PlotRef=FALSE,
                    main="", LegendOffset=c(1.02, 1.02))
               } else {
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0, 3, 0.5),6,15,35), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               if(ag == "Average") mtext(PrNames[pr], side=2, line=1.75)
               if(pr == "hbw") mtext(ag, side=3, line=1.75)
               }
          }
     par(Opar)

     savePlot(filename="final_output/tci/graphics/map_tci_by_modeagg_purpose", type="pdf")



#Set up districts reference vectors
#----------------------------------

#::
     load("inputs/RData/districts.RData")
     Districts.Zo <- districts$ugb
     names(Districts.Zo) <- districts$zone
     DiNames <- as.character(c(1:20))
     names(DiNames) <- unique(Districts.Zo)
     Districts.Zi <- Districts.Zo[Zi] ; rm(Districts.Zo)

# add name index of districts 
index <- as.character(c(1:20))

#Plot comparisons of travel cost indices by UGB
#----------------------------------------------

#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,3,2,2), oma=c(1,1,2.25,1))
     #Barplot of average market costs by district
     BarCenter <- barplot(Tci.Di[index], xlab="", ylab="Travel Cost Index", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Average Market Cost", side=1, line=0.5, cex=1)
     mtext("TCI", side=2, line=2.5)
     text(as.vector(BarCenter), 0.1, labels=DiNames, srt=90, pos=4, offset=0)
     #Barplot of auto market costs by district
     BarCenter <- barplot(Tci2.Di[index], xlab="", ylab="Travel Cost Index", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("TCI", side=2, line=2.5)
     mtext("Minimum Market Cost", side=1, line=0.5, cex=1)
     text(as.vector(BarCenter), 0.2, labels=DiNames, srt=90, pos=4, offset=0)
     #Barplot of non-auto market costs by district
     BarCenter <- barplot(Tci3.Di[index], xlab="", ylab="Travel Cost Index", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Composite Market Cost", side=1, line=0.5, cex=1)
     mtext("TCI", side=2, line=2.5)
     text(as.vector(BarCenter), 0.075, labels=DiNames, srt=90, pos=4, offset=0)
     # mtext("Comparison of TCI Values by Calculation Method and UGB", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/district_tci",type="pdf")
     # Restore graphics parameters
     par(Opar)





