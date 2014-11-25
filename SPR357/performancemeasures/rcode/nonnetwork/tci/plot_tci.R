#===================
#plot_tci_measures.R
#===================

#:author: Brian Gregor
#:date: 9/26/05
#:contact: brian.j.gregor@odot.state.or.us
#:copyright: Oregon Department of Transportation
#:license: GPL2


#Description
#===========

#This script generates a variety of plots and maps of the tci related measures.

#Load the results
#================

#::

     # Reference data
     #load("final_output/tci/MarketAttractions.ZiIcPr.RData")
     #load("final_output/tci/MarketLogSums.ZiIcPr.RData")
     load("intm_output/reference/AttractionScore.RData")
     #load("final_output/tci/AttractionScoreTransit.RData")
     #load("final_output/tci/ReferenceZone.RData")
     #load("final_output/tci/ReferenceAttractions.RData")

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

     # Average non-auto market cost
     load("final_output/tci/NonAutoMarketCost.ZiIc.RData")
     load("final_output/tci/NonAutoMarketCost.ZiPr.RData")
     load("final_output/tci/NonAutoMarketCost.Zi.RData")
     load("final_output/tci/NonAutoMarketCost.DiIc.RData")
     load("final_output/tci/NonAutoMarketCost.DiPr.RData")
     load("final_output/tci/NonAutoMarketCost.Di.RData")

     # Average auto market cost
     load("final_output/tci/AutoMarketCost.ZiIc.RData")
     load("final_output/tci/AutoMarketCost.ZiPr.RData")
     load("final_output/tci/AutoMarketCost.Zi.RData")
     load("final_output/tci/AutoMarketCost.DiIc.RData")
     load("final_output/tci/AutoMarketCost.DiPr.RData")
     load("final_output/tci/AutoMarketCost.Di.RData")

     # Alternative mode market coverage
     load("final_output/tci/AltMarketCoverage.ZiIcPr.RData")
     load("final_output/tci/AltMarketCoverage.ZiIc.RData")
     load("final_output/tci/AltMarketCoverage.ZiPr.RData")
     load("final_output/tci/AltMarketCoverage.Zi.RData")
     load("final_output/tci/AltMarketCoverage.DiIc.RData")
     load("final_output/tci/AltMarketCoverage.DiPr.RData")
     load("final_output/tci/AltMarketCoverage.Di.RData")

     # NonAutoCostRatio
     load("final_output/tci/NonAutoCostRatio.ZiIcPr.RData")
     load("final_output/tci/NonAutoCostRatio.ZiIc.RData")
     load("final_output/tci/NonAutoCostRatio.ZiPr.RData")
     load("final_output/tci/ANonAutoCostRatio.Zi.RData")
     load("final_output/tci/NonAutoCostRatio.DiIc.RData")
     load("final_output/tci/NonAutoCostRatio.DiPr.RData")
     load("final_output/tci/ANonAutoCostRatio.Di.RData")

#Load mapping data and functions
#===============================

#::

     # load required libraries
     library("maptools")
     library("RColorBrewer")

     # read in the taz data shapefile
     TazFile <- paste("gis/", "TAZ.shp", sep="/")
     TazShape <- read.shape(TazFile)

     # extract the attribute data
     TazData <- TazShape$att.data

     # convert to polygon file for mapping
     TazPoly <- Map2poly(TazShape, TazShape$att.data$TAZ)

     # make an index vector to the taz
     TazIndex <- as.character(TazData$TAZ)

     # get the location of the reference zone
     TazCentroids.ZiXy <- get.Pcent(TazShape)
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
               legend(720000*LegendOffset[1], 277100*LegendOffset[2], legend=LegendText,
                    title=LegendTitle, cex=LegendSize, fill=ColorPalette)
               }
          if(PlotRef){
            points(619220, 150140, pch=1, col=RefColor, cex=2, lwd=2)
            text(623250, 150140, "Reference Zone", pos=4)
            }
          }

     # Make names for household income groups and trip types for plotting
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
          breaks=c(-14, seq(-10, 0, 2), 0.5, 1, 2, 3, 4),
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
     savePlot(filename="final_output/tci/graphics/attraction_scores")
     par(Opar)

#Plot household histograms of average market costs for all purposes and incomes
#------------------------------------------------------------------------------

#::

     # Define breaks and limits for histograms
     Breaks <- seq(0, 3, 0.25)
     Xlim <- c(0,3)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ic in Ic){
          for(pr in Pr){
               HistData <- rep(AveMarketCost.ZiIcPr[,ic,pr], Hh.ZiIc[,ic])
               hist(HistData, xlab="", xlim=Xlim, breaks=Breaks, axes=FALSE,
               ylab="", col="skyblue", main=NULL, freq=FALSE, ylim=c(0,2.5))
               axis(1, at=seq(0,2.5,0.5))
               if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
               if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
               }
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/hist_ave_cost_by_income_purpose")

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
                    breaks=c(seq(0, 2, 0.25), 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
                    main="", LegendOffset=c(1.014, 1.012))
               } else {
                    coropleth(TazPoly, AveMarketCost.ZiIcPr[,ic,pr], TazIndex, "RdYlBu",
                    breaks=c(seq(0, 2, 0.25), 3, 4.5), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               if(ic == "lowInc") mtext(PrNames[pr], side=2, line=1.75)
               if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
               }
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/map_ave_cost_by_income_purpose")
     
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
     savePlot(filename="final_output/tci/graphics/hist_tci_by_modeagg_purpose")

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
     savePlot(filename="final_output/tci/graphics/hist_tci_by_modeagg_income")
     dev.off()

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
               if((ic == "lowInc") & (ag == "Composite")){
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0.25, 2, 0.25), 3, 6, 12, 36), LegendSize=0.75, PlotRef=FALSE,
                    main="", LegendOffset=c(1.011, 1.02))
               } else {
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0.25, 2, 0.25), 3, 6, 12, 36), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               if(ag == "Average") mtext(IcNames[ic], side=2, line=1.75)
               if(ic == "lowInc") mtext(ag, side=3, line=1.75)
               }
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/map_tci_by_modeagg_income")
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
               if((pr == "hbw") & (ag == "Composite")){
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0.25, 2, 0.25), 3, 6, 12, 36), LegendSize=0.75, PlotRef=FALSE,
                    main="", LegendOffset=c(1.02, 1.02))
               } else {
                    coropleth(TazPoly, MapData, TazIndex, "RdYlBu",
                    breaks=c(seq(0.25, 2, 0.25), 3, 6, 12, 36), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               if(ag == "Average") mtext(PrNames[pr], side=2, line=1.75)
               if(pr == "hbw") mtext(ag, side=3, line=1.75)
               }
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/map_tci_by_modeagg_purpose")


#Plot histograms of alternative mode market coverage by purpose and income
#-------------------------------------------------------------------------

#::

     # Set up breaks and limits for plotting
     Breaks <- c(0,10,20,30,40,50,60,70,80,90,100)
     Xlim <- c(0,100)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ic in Ic){
          for(pr in Pr){
               HistData <- rep(AltMarketCoverage.ZiIcPr[,ic,pr], Hh.ZiIc[,ic])
               hist(HistData, xlab="", xlim=Xlim, breaks=Breaks, axes=FALSE,
               ylab="", col="skyblue", ylim=c(0,0.07), main=NULL, freq=FALSE)
               axis(1, at=seq(0, 100, 20))
               if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
               if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
               }
          }
     savePlot(filename="final_output/tci/graphics/hist_nonauto_percent_by_income_purpose")
     # Restore graphics parameters
     par(Opar)

#Map alternative mode market coverage by purpose and income
#----------------------------------------------------------

#::

     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ic in Ic){
          for(pr in Pr){
               if((ic == "highInc") & (pr == "hbw")){
                    coropleth(TazPoly, round(AltMarketCoverage.ZiIcPr[,ic,pr],0), TazIndex, "RdYlBu",
                    breaks=seq(0,100,10), LegendSize=0.75, PlotRef=FALSE,
                    main="", LegendOffset=c(1.02, 1.012))
               } else {
                    coropleth(TazPoly, round(AltMarketCoverage.ZiIcPr[,ic,pr],0), TazIndex, "RdYlBu",
                    breaks=seq(0,100,10), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               if(ic == "lowInc") mtext(PrNames[pr], side=2, line=1.75)
               if(pr == "hbw") mtext(IcNames[ic], side=3, line=1.75)
               }
          }
     par(Opar)          
     savePlot(filename="final_output/tci/graphics/map_nonauto_percent_by_income_purpose")

#Plot histograms of non-auto cost ratio by purpose and income
#------------------------------------------------------------

#::

    # Set up breaks and limits for plotting
     Breaks <- seq(0, 275, 25)
     Xlim <- c(0, 275)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(2,2,1,1), oma=c(2,3,2,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ic in Ic){
          for(pr in Pr){
               HistData <- rep(NonAutoCostRatio.ZiIcPr[,ic,pr], Hh.ZiIc[,ic])
               hist(HistData, xlab="", xlim=Xlim, breaks=Breaks, axes=FALSE,
               ylab="", col="skyblue", ylim=c(0,0.025), main=NULL, freq=FALSE)
               axis(1, at=seq(0, 250, 50))
               if(ic == "lowInc") mtext(PrNames[pr], side=2, line=3)
               if(pr == "hbw") mtext(IcNames[ic], side=3, line=1)
               }
          }
     savePlot(filename="final_output/tci/graphics/hist_nonauto_auto_cost_ratio_by_income_purpose")
     # Restore graphics parameters
     par(Opar)

#Map non-auto cost ratio by purpose and income
#---------------------------------------------

#::

     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(1,2.5,2.5,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ic in Ic){
          for(pr in Pr){
               if((ic == "highInc") & (pr == "hbw")){
                    coropleth(TazPoly, round(NonAutoCostRatio.ZiIcPr[,ic,pr],0), TazIndex, "RdYlBu",
                    breaks=seq(0, 275,25), LegendSize=0.75, PlotRef=FALSE,
                    main="", LegendOffset=c(1.02, 1.012))
               } else {
                    coropleth(TazPoly, round(NonAutoCostRatio.ZiIcPr[,ic,pr],0), TazIndex, "RdYlBu",
                    breaks=seq(0, 275,25), LegendSize=0, PlotRef=FALSE,
                    main="")
               }
               if(ic == "lowInc") mtext(PrNames[pr], side=2, line=1.75)
               if(pr == "hbw") mtext(IcNames[ic], side=3, line=1.75)
               }
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/map_nonauto_auto_cost_ratio_by_income_purpose")

#Set up districts reference vectors
#----------------------------------

#::

     Districts.Zo <- districts$ugb
     names(Districts.Zo) <- districts$zone
     DiNames <- c("Outside UGB", "Eagle Point", "Central Point", "Medford", "Jacksonville",
               "Phoenix", "Talent", "Ashland")
     names(DiNames) <- unique(Districts.Zo)
     Districts.Zi <- Districts.Zo[Zi] ; rm(Districts.Zo)


#Plot comparisons of travel cost indices by UGB
#----------------------------------------------

#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,3,2,2), oma=c(1,1,2.25,1))
     #Barplot of average market costs by district
     BarCenter <- barplot(Tci.Di, xlab="", ylab="Travel Cost Index", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Average Market Cost", side=1, line=0.5, cex=1)
     mtext("TCI", side=2, line=2.5)
     text(as.vector(BarCenter), 0.1, labels=DiNames, srt=90, pos=4, offset=0)
     #Barplot of auto market costs by district
     BarCenter <- barplot(Tci2.Di, xlab="", ylab="Travel Cost Index", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("TCI", side=2, line=2.5)
     mtext("Minimum Market Cost", side=1, line=0.5, cex=1)
     text(as.vector(BarCenter), 0.2, labels=DiNames, srt=90, pos=4, offset=0)
     #Barplot of non-auto market costs by district
     BarCenter <- barplot(Tci3.Di, xlab="", ylab="Travel Cost Index", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Composite Market Cost", side=1, line=0.5, cex=1)
     mtext("TCI", side=2, line=2.5)
     text(as.vector(BarCenter), 0.075, labels=DiNames, srt=90, pos=4, offset=0)
     # mtext("Comparison of TCI Values by Calculation Method and UGB", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/district_tci")
     # Restore graphics parameters
     par(Opar)

#Plot alternative mode coverage
#------------------------------

#::

     windows(6.5, 4)
     # Set up graphic parameters
     Opar <- par(mar=c(2,3,2,2), oma=c(1,1,1,1))
      # Barplot of alternative mode market coverage
     BarCenter <- barplot(AltMarketCoverage.Di, xlab="", ylab="", col=brewer.pal(8, "Pastel1"),
           main=NULL, axisnames=FALSE, ylim=c(0,100))
     mtext("Percent", side=2, line=2.5)
     text(as.vector(BarCenter), 5, labels=DiNames, srt=90, pos=4, offset=0)
     savePlot(filename="final_output/tci/graphics/district_alt_mode_coverage")
     # Restore graphics parameters
     par(Opar)
     dev.off()
     

#Plot non-auto to auto cost ratio
#--------------------------------

#::

     windows(6.5, 4)
     # Set up graphic parameters
     Opar <- par(mar=c(2,3,2,2), oma=c(1,1,1,1))
     # Barplot of ratio of non-auto market cost to auto market cost
     BarCenter <- barplot(NonAutoCostRatio.Di, xlab="", ylab="", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Ratio", side=2, line=2.5)
     text(as.vector(BarCenter), 5, labels=DiNames, srt=90, pos=4, offset=0)
     # mtext("Alternative Mode Coverage and Cost Ratio", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/district_nonauto_auto_cost")
     # Restore graphics parameters
     par(Opar)
     dev.off()
