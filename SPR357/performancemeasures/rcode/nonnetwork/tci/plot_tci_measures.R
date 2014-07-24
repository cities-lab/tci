#===================
#plot_tci_measures.R
#===================

#:author: Brian Gregor
#:date: 6/06/05
#:contact: brian.j.gregor@odot.state.or.us
#:copyright: Oregon Department of Transportation
#:license: GPL2


#Description
#===========

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


#Plot histograms and maps of various measures
#============================================
#::

#Plot histograms and map of attraction scores
#--------------------------------------------
#::

     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(c(1,2),nrow=2), widths=4, heights=c(3,1))
     # Calculate log of scores and set -Inf to -14
     AttractionScore2 <- log(AttractionScore)
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
     par(mar=c(4,4,1,1))
     # Plot histogram of scores, using the same breaks and colors as the map
     hist(AttractionScore2, xlab="", ylab="",
          breaks=c(-14, seq(-10, 0, 2), 0.5, 1, 2, 3, 4),
          col=brewer.pal(11, "RdYlBu"), main="")
     mtext("TAZ Frequency", side=2, line=2.25)
     mtext("log(score)", side=1, line=2)
     # Save the plot and restore the graphics parameters
     savePlot(filename="final_output/tci/graphics/attraction_scores")
     par(Opar)

#Plot histograms of average market costs
#---------------------------------------
#::

     # Define breaks and limits for histograms
     Breaks <- seq(0, 5, 0.25)
     Xlim <- c(0,4)
     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     hist(AveMarketCost.Zi, xlab="Dollars", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
          main=NULL)
     title(main="All Households", line=-1)
     # Averages by income group
     for(ic in Ic){
          hist(AveMarketCost.ZiIc[,ic], xlab="Dollars", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
               main=NULL)
          title(main=paste(toupper(ic), "Households"), line=-1)
          }
     # mtext("Average Market Costs by Income", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_average_market_cost_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          hist(AveMarketCost.ZiPr[,pr], xlab="Dollars", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
          main=NULL)
          title(main=paste(toupper(pr), "Trips"), line=-1)
          }
     # mtext("Average Market Costs by Purpose", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_average_market_cost_by_purpose")
     # Restore graphics parameters
     par(Opar)

#Plot histograms of average market costs for all purposes and incomes
#--------------------------------------------------------------------

     # Define breaks and limits for histograms
     Breaks <- seq(0, 5, 0.25)
     Xlim <- c(0,2.5)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(2,2,1,1), oma=c(3,3,1,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ic in Ic){
          for(pr in Pr){
               hist(AveMarketCost.ZiIcPr[,ic,pr], xlab="", xlim=Xlim, breaks=Breaks,
               ylab="", col="skyblue", ylim=c(0,400), main=NULL)
               if(ic == "lowInc") mtext(paste(toupper(pr), "trips"), side=2, line=3)
               }
          if(pr == "hbo") mtext(paste(toupper(ic), "households"), side=1, line=3)
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/hist_ave_cost_by_income_purpose")

#Plot maps of average market costs
#---------------------------------
#::    c(seq(0.5, 2, 0.25), 2.5, 3, 4.5)

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     coropleth(TazPoly, AveMarketCost.Zi, TazIndex, "RdYlBu",
               breaks=c(seq(0, 2, 0.25), 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext("All Households", side=1, line=0.5, cex=1)
     # Averages by income group
     for(ic in Ic){
          coropleth(TazPoly, AveMarketCost.ZiIc[,ic], TazIndex, "RdYlBu",
               breaks=c(seq(0, 2, 0.25), 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(ic), "Households"), side=1, line=0.5, cex=1)
          }
     # mtext("Average Market Costs by Income (dollars)", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_ave_market_cost_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          coropleth(TazPoly, AveMarketCost.ZiPr[,pr], TazIndex, "RdYlBu",
               breaks=c(seq(0, 2, 0.25), 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(pr), "Trips"), side=1, line=0.5, cex=1)
          }
     # mtext("Average Market Costs by Purpose (dollars)", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_ave_market_cost_by_purpose")
     # Restore graphics parameters
     par(Opar)

#Plot maps of average market costs for all purposes and incomes
#--------------------------------------------------------------------

     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(0.5,0.5,0.5,0.5), oma=c(2.5,2.5,1,1))
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
               if(ic == "lowInc") mtext(paste(toupper(pr), "trips"), side=2, line=1.75)
               }
          if(pr == "hbo") mtext(paste(toupper(ic), "households"), side=1, line=1.75)
          }
     par(Opar)

     savePlot(filename="final_output/tci/graphics/map_ave_cost_by_income_purpose")

#Plot histograms of average market costs by purpose for each mode aggregregation type
#------------------------------------------------------------------------------------

     # Define a vector of data aggregation types
     Ag <- c("Tci.ZiPr", "Tci2.ZiPr", "Tci3.ZiPr")
     names(Ag) <- c("Average", "Minimum", "Composite")
     # Define breaks and limits for histograms
     Breaks <- seq(0, 6, 0.5)
     Xlim <- c(0,6)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:12,nrow=4))
     Opar <- par(mar=c(2,2,1,1), oma=c(3,3,1,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ag in names(Ag)){
          for(pr in Pr){
               HistData <- get(Ag[ag])[,pr]
               HistData <- HistData[(HistData > 0) & (HistData < 6)]
               hist(HistData, xlab="", breaks=Breaks, xlim=Xlim,
               ylab="", col="skyblue", ylim=c(0,400), main=NULL)
               if(ag == "Average") mtext(paste(toupper(pr), "Trips"), side=2, line=3)
               }
          if(pr == "hbo") mtext(ag, side=1, line=3)
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/hist_ave_cost_by_modeagg_purpose")

#Plot histograms of average market costs by income for each mode aggregregation type
#------------------------------------------------------------------------------------

     # Define a vector of data aggregation types
     Ag <- c("Tci.ZiIc", "Tci2.ZiIc", "Tci3.ZiIc")
     names(Ag) <- c("Average", "Minimum", "Composite")
     # Define breaks and limits for histograms
     Breaks <- seq(0, 6, 0.5)
     Xlim <- c(0,6)
     # Set up plot layout, map will go on top and histogram on bottom
     nf <- layout(matrix(1:9,nrow=3))
     Opar <- par(mar=c(2,2,1,1), oma=c(3,3,1,1))
     # Iterate through all purposes and incomes and plot histograms
     for(ag in names(Ag)){
          for(ic in Ic){
               HistData <- get(Ag[ag])[,ic]
               HistData <- HistData[(HistData > 0) & (HistData < 6)]
               hist(HistData, xlab="", breaks=Breaks, xlim=Xlim,
               ylab="", col="skyblue", ylim=c(0,400), main=NULL)
               if(ag == "Average") mtext(paste(toupper(ic), "Households"), side=2, line=3)
               }
          if(ic == "highInc") mtext(ag, side=1, line=3)
          }
     par(Opar)
     savePlot(filename="final_output/tci/graphics/hist_ave_cost_by_modeagg_income")

#Plot histograms of auto market costs
#---------------------------------------
#::

     # Define breaks and limits for histograms
     Breaks <- seq(0, 5, 0.25)
     Xlim <- c(0,4)
     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     hist(AutoMarketCost.Zi, xlab="Dollars", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
          main=NULL)
     title(main="All Households", line=-1)
     # Averages by income group
     for(ic in Ic){
          hist(AutoMarketCost.ZiIc[,ic], xlab="Dollars", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
               main=NULL)
          title(main=paste(toupper(ic), "Households"), line=-1)
          }
     # mtext("Auto Market Costs by Income", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_auto_market_cost_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          hist(AutoMarketCost.ZiPr[,pr], xlab="Dollars", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
          main=NULL)
          title(main=paste(toupper(pr), "Trips"), line=-1)
          }
     # mtext("Auto Market Costs by Purpose", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_auto_market_cost_by_purpose")
     # Restore graphics parameters
     par(Opar)

#Plot maps of auto market costs
#---------------------------------
#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     coropleth(TazPoly, AutoMarketCost.Zi, TazIndex, "RdYlBu",
               breaks=c(seq(0.5, 2, 0.25), 2.5, 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext("All Households", side=1, line=0.5, cex=1)
     # Averages by income group
     for(ic in Ic){
          coropleth(TazPoly, AutoMarketCost.ZiIc[,ic], TazIndex, "RdYlBu",
               breaks=c(seq(0.5, 2, 0.25), 2.5, 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(ic), "Households"), side=1, line=0.5, cex=1)
          }
     # mtext("Auto Market Costs by Income (dollars)", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_auto_market_cost_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          coropleth(TazPoly, AutoMarketCost.ZiPr[,pr], TazIndex, "RdYlBu",
              breaks=c(seq(0.5, 2, 0.25), 2.5, 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(pr), "Trips"), side=1, line=0.5, cex=1)
          }
     # mtext("Auto Market Costs by Purpose (dollars)",  outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_auto_market_cost_by_purpose")
     # Restore graphics parameters
     par(Opar)

#Plot histograms of nonauto market costs
#---------------------------------------
#::

     # Define breaks and limits for histograms
     Breaks <- seq(0, 700, 50)
     Xlim <- c(0,700)
     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     hist(NonAutoMarketCost.Zi, xlab="Dollars", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
          main=NULL)
     title(main="All Households", line=-1)
     # Averages by income group
     for(ic in Ic){
          hist(NonAutoMarketCost.ZiIc[,ic], xlab="Dollars", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
               main=NULL)
          title(main=paste(toupper(ic), "Households"), line=-1)
          }
     # mtext("Non-Auto Market Costs by Income", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_nonauto_market_cost_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          hist(NonAutoMarketCost.ZiPr[,pr], xlab="Dollars", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
          main=NULL)
          title(main=paste(toupper(pr), "Trips"), line=-1)
          }
     # mtext("Non-Auto Market Costs by Purpose", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_nonauto_market_cost_by_purpose")
     # Restore graphics parameters
     par(Opar)

#Plot maps of non-auto market costs
#---------------------------------
#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     coropleth(TazPoly, NonAutoMarketCost.Zi, TazIndex, "RdYlBu",
               breaks=c(seq(0, 400, 50),700), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext("All Incomes", side=1, line=0.5, cex=1)
     # Averages by income group
     for(ic in Ic){
          coropleth(TazPoly, NonAutoMarketCost.ZiIc[,ic], TazIndex, "RdYlBu",
               breaks=c(seq(0, 400, 50),700), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(ic), "Households"), side=1, line=0.5, cex=1)
          }
     # mtext("Non-Auto Market Costs by Income (dollars)", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_nonauto_market_cost_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          coropleth(TazPoly, NonAutoMarketCost.ZiPr[,pr], TazIndex, "RdYlBu",
               breaks=c(seq(0, 400, 50),700), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(pr), "Trips"), side=1, line=0.5, cex=1)
          }
     # mtext("Non-Auto Market Costs by Purpose (dollars)", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_nonauto_market_cost_by_purpose")
     # Restore graphics parameters
     par(Opar)

#Plot maps of best market costs
#---------------------------------
#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     coropleth(TazPoly, BestMarketCost.Zi, TazIndex, "RdYlBu",
               breaks=c(seq(0, 2, 0.25), 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext("All Households", side=1, line=0.5, cex=1)
     # Averages by income group
     for(ic in Ic){
          coropleth(TazPoly, BestMarketCost.ZiIc[,ic], TazIndex, "RdYlBu",
               breaks=c(seq(0, 2, 0.25), 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(ic), "Households"), side=1, line=0.5, cex=1)
          }
     # mtext("Minimum Market Costs by Income (dollars)", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_min_market_cost_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          coropleth(TazPoly, BestMarketCost.ZiPr[,pr], TazIndex, "RdYlBu",
               breaks=c(seq(0, 2, 0.25), 3, 4.5), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(pr), "Trips"), side=1, line=0.5, cex=1)
          }
     # mtext("Minimum Market Costs by Purpose (dollars)", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_min_market_cost_by_purpose")
     # Restore graphic parameters
     par(Opar)
     
#Plot histograms of travel cost index (based on average costs)
#-------------------------------------------------------------
#::

     # Define breaks and limits for histograms
     Breaks <- seq(0, 3, 0.25)
     Xlim <- c(0, 3)
     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     par(mfrow=c(2,2))
     # Overall average
     hist(Tci.Zi, xlab="TCI", xlim=Xlim, breaks=Breaks,
     ylab="Number of Zones", col="skyblue", ylim=c(0,400),
     main=NULL)
     title(main="All Households", line=-1)
     # Averages by income group
     for(ic in Ic){
          hist(Tci.ZiIc[,ic], xlab="TCI", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
          main=NULL)
          title(main=paste(toupper(ic), "Households"), line=-1)
          }
     # mtext("Travel Cost Index by Income", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_tci_ave_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          hist(Tci.ZiPr[,pr], xlab="TCI", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,400),
          main=NULL)
          title(main=paste(toupper(pr), "Trips"), line=-1)
          }
     # mtext("Travel Cost Index by Purpose", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_tci_ave_by_purpose")
     # Restore graphics parameters
     par(mfrow=c(1,1))


#Map travel cost index based on average costs
#--------------------------------------------
#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     coropleth(TazPoly, Tci.Zi, TazIndex, "RdYlBu", 
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext("All Households", side=1, line=0.5, cex=1)
     # Averages by income group
     for(ic in Ic){
          coropleth(TazPoly, Tci.ZiIc[,ic], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(ic), "Households"), side=1, line=0.5, cex=1)
          }
     # mtext("Travel Cost Index by Income Based on Average Costs", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_tci_ave_by_income")
     # Averages by trip purpose
     for(pr in Pr){
          coropleth(TazPoly, Tci.ZiPr[,pr], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(pr), "Trips"), side=1, line=0.5, cex=1)
          }
     # mtext("Travel Cost Index by Purpose Based on Average Costs", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_tci_ave_by_purpose")
     # Restore graphics parameters
     par(Opar)
     
#Map travel cost index based on minimum costs
#--------------------------------------------
#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     coropleth(TazPoly, Tci2.Zi, TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext("All Households", side=1, line=0.5, cex=1)
     # Averages by income group
     for(ic in Ic){
          coropleth(TazPoly, Tci2.ZiIc[,ic], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(ic), "Households"), side=1, line=0.5, cex=1)
          }
     # mtext("Travel Cost Index by Income Based on Minimum Costs", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_tci_min_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          coropleth(TazPoly, Tci2.ZiPr[,pr], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(pr), "Trips"), side=1, line=0.5, cex=1)
          }
     # mtext("Travel Cost Index by Purpose Based on Minimum Costs", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_tci_min_by_purpose")
     # Restore graphic parameters
     par(Opar)
     
#Map travel cost index based on composite costs
#----------------------------------------------
#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     coropleth(TazPoly, Tci3.Zi, TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext("All Households", side=1, line=0.5, cex=1)
     # Averages by income group
     for(ic in Ic){
          coropleth(TazPoly, Tci3.ZiIc[,ic], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(ic), "Households"), side=1, line=0.5, cex=1)
          }
     # mtext("Travel Cost Index by Income Based on Composite Costs", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_tci_comp_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          coropleth(TazPoly, Tci3.ZiPr[,pr], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(pr), "Trips"), side=1, line=0.5, cex=1)
          }
     # mtext("Travel Cost Index by Purpose Based on Composite Costs", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_tci_comp_by_purpose")
     # Restore graphics parameters
     par(Opar)

#Plot income comparisons of travel cost index using different cost bases
#-----------------------------------------------------------------------
#::

     windows(width=7, height=9)
     # Set up graphic parameters
     Opar <- par(mfcol=c(3,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # Plot overall average
     # Plot Tci based on average prices
     coropleth(TazPoly, Tci.Zi, TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Average Costs", side=1, line=0.5, cex=0.85)
     mtext("All Households", side=3, line=2, cex=1.1)
     # Plot Tci based on minimum prices
     coropleth(TazPoly, Tci2.Zi, TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Minimum Costs", side=1, line=0.5, cex=0.85)
     # Plot Tci based on composite prices
     coropleth(TazPoly, Tci3.Zi, TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Composite Costs", side=1, line=0.5, cex=0.85)

     # Plot low income
     # Plot Tci based on average prices
     coropleth(TazPoly, Tci.ZiIc[,1], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Average Costs", side=1, line=0.5, cex=0.85)
     mtext("Low Income Households", side=3, line=2, cex=1.1)
     # Plot Tci based on minimum prices
     coropleth(TazPoly, Tci2.ZiIc[,1], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Minimum Costs", side=1, line=0.5, cex=0.85)
     # Plot Tci based on composite prices
     coropleth(TazPoly, Tci3.ZiIc[,1], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Composite Costs", side=1, line=0.5, cex=0.85)
     savePlot(filename="final_output/tci/graphics/map_tci_comparisons_income1")
     dev.off()
     
     # Plot mid income
     windows(width=7, height=9)
     # Set up graphic parameters
     Opar <- par(mfcol=c(3,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # Plot Tci based on average prices
     coropleth(TazPoly, Tci.ZiIc[,2], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Average Costs", side=1, line=0.5, cex=0.85)
     mtext("Mid Income Households", side=3, line=2, cex=1.1)
     # Plot Tci based on minimum prices
     coropleth(TazPoly, Tci2.ZiIc[,2], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Minimum Costs", side=1, line=0.5, cex=0.85)
     # Plot Tci based on composite prices
     coropleth(TazPoly, Tci3.ZiIc[,2], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Composite Costs", side=1, line=0.5, cex=0.85)

     # Plot high income
     # Plot Tci based on average prices
     coropleth(TazPoly, Tci.ZiIc[,3], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Average Costs", side=1, line=0.5, cex=0.85)
     mtext("High Income Households", side=3, line=2, cex=1.1)
     # Plot Tci based on minimum prices
     coropleth(TazPoly, Tci2.ZiIc[,3], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Minimum Costs", side=1, line=0.5, cex=0.85)
     # Plot Tci based on composite prices
     coropleth(TazPoly, Tci3.ZiIc[,3], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Composite Costs", side=1, line=0.5, cex=0.85)

     savePlot(filename="final_output/tci/graphics/map_tci_comparisons_income2")
     dev.off()

#Plot purpose comparisons of travel cost index using different cost bases
#------------------------------------------------------------------------
#::

     windows(width=7, height=9)
     # Set up graphic parameters
     Opar <- par(mfcol=c(3,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # Plot hbw trips
     # Plot Tci based on average prices
     coropleth(TazPoly, Tci.ZiPr[,"hbw"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Average Costs", side=1, line=0.5, cex=0.85)
     mtext("HBW Trips", side=3, line=2, cex=1.1)
     # Plot Tci based on minimum prices
     coropleth(TazPoly, Tci2.ZiPr[,"hbw"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Minimum Costs", side=1, line=0.5, cex=0.85)
     # Plot Tci based on composite prices
     coropleth(TazPoly, Tci3.ZiPr[,"hbw"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Composite Costs", side=1, line=0.5, cex=0.85)

     # Plot hbs trips
     # Plot Tci based on average prices
     coropleth(TazPoly, Tci.ZiPr[,"hbs"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Average Costs", side=1, line=0.5, cex=0.85)
     mtext("HBS Trips", side=3, line=2, cex=1.1)
     # Plot Tci based on minimum prices
     coropleth(TazPoly, Tci2.ZiPr[,"hbs"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Minimum Costs", side=1, line=0.5, cex=0.85)
     # Plot Tci based on composite prices
     coropleth(TazPoly, Tci3.ZiPr[,"hbs"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Composite Costs", side=1, line=0.5, cex=0.85)
     savePlot(filename="final_output/tci/graphics/map_tci_comparisons_purpose1")
     dev.off()

     # Plot hbr trips
     windows(width=7, height=9)
     # Set up graphic parameters
     Opar <- par(mfcol=c(3,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # Plot Tci based on average prices
     coropleth(TazPoly, Tci.ZiPr[,"hbr"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Average Costs", side=1, line=0.5, cex=0.85)
     mtext("HBR Trips", side=3, line=2, cex=1.1)
     # Plot Tci based on minimum prices
     coropleth(TazPoly, Tci2.ZiPr[,"hbr"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Minimum Costs", side=1, line=0.5, cex=0.85)
     # Plot Tci based on composite prices
     coropleth(TazPoly, Tci3.ZiPr[,"hbr"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Composite Costs", side=1, line=0.5, cex=0.85)

     # Plot hbo trips
     # Plot Tci based on average prices
     coropleth(TazPoly, Tci.ZiPr[,"hbo"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Average Costs", side=1, line=0.5, cex=0.85)
     mtext("HBO Trips", side=3, line=2, cex=1.1)
     # Plot Tci based on minimum prices
     coropleth(TazPoly, Tci2.ZiPr[,"hbo"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Minimum Costs", side=1, line=0.5, cex=0.85)
     # Plot Tci based on composite prices
     coropleth(TazPoly, Tci3.ZiPr[,"hbo"], TazIndex, "RdYlBu",
               breaks=round(exp(seq(log(0.5), log(10), 0.25)),1), LegendSize=0.75, PlotRef=FALSE,
               main="", LegendOffset=c(1.015,1.01))
     mtext("Composite Costs", side=1, line=0.5, cex=0.85)

     savePlot(filename="final_output/tci/graphics/map_tci_comparisons_purpose2")
     dev.off()



#Plot histograms of alternative mode market coverage
#---------------------------------------------------
#::

     # Set up breaks and limits for plotting
     Breaks <- c(0,10,20,30,40,50,60,70,80,90,100)
     Xlim <- c(0,100)
     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2),oma=c(1,1,2.25,1))
     # Overall average
     hist(AltMarketCoverage.Zi, xlab="Percent", xlim=Xlim, breaks=Breaks,
     ylab="Number of Zones", col="skyblue", ylim=c(0,425),
          main=NULL)
     title(main="All Households", line=-1)
     # Averages by income group
     for(ic in Ic){
          hist(AltMarketCoverage.ZiIc[,ic], xlab="Percent", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,425),
          main=NULL)
          title(main=paste(toupper(ic), "Households"), line=-1)
          }
     # mtext("Non-Auto Market Percentage by Income", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_nonauto_percent_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          hist(AltMarketCoverage.ZiPr[,pr], xlab="Percent", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,425),
          main=NULL)
          title(main=paste(toupper(pr), "Trips"), line=-1)
          }
     # mtext("Non-Auto Market Percentage by Purpose", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_nonauto_percent_by_purpose")
     # Restore graphics parameters
     par(mfrow=c(1,1))

#Map alternative mode market coverage
#------------------------------------
#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     coropleth(TazPoly, round(AltMarketCoverage.Zi,0), TazIndex, "RdYlBu",
               breaks=seq(0,100,10), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext("All Households", side=1, line=0.5, cex=1)
     # Averages by income group
     for(ic in Ic){
          coropleth(TazPoly, round(AltMarketCoverage.ZiIc[,ic],0), TazIndex, "RdYlBu",
               breaks=seq(0,100,10), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(ic), "Households"), side=1, line=0.5, cex=1)
          }
     # mtext("Non-Auto Market Percentage by Income", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_nonauto_percent_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
     coropleth(TazPoly, round(AltMarketCoverage.ZiPr[,pr],0), TazIndex, "RdYlBu",
               breaks=seq(0,100,10), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext(paste(toupper(pr), "Trips"), side=1, line=0.5, cex=1)
          }
     # mtext("Non-Auto Market Percentage by Purpose", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_nonauto_percent_by_purpose")
     # Restore graphics parameters
     par(Opar)

#Plot histograms of non-auto cost ratio
#--------------------------------------
#::

     # Define breaks and limits for histograms
     Breaks <- seq(0, 225,25)
     Xlim <- c(0, 225)
     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     hist(NonAutoCostRatio.Zi, xlab="Non-Auto Cost / Auto Cost", xlim=Xlim, breaks=Breaks,
          ylab="Number of Zones", col="skyblue", ylim=c(0,300),
           main=NULL, sub="All Incomes")
     title(main="All Households", line=-1, cex=1)
     # Averages by income group
     for(ic in Ic){
          hist(NonAutoCostRatio.ZiIc[,ic], xlab="Non-Auto Cost / Auto Cost",
           xlim=Xlim, breaks=Breaks,
           ylab="Number of Zones", col="skyblue", ylim=c(0,300),
               main=NULL)
           title(main=paste(toupper(ic), "Households"), line=-1, cex=1)
          }
     # mtext("Ratio of Non-Auto to Auto Market Costs by Income", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_nonauto_auto_cost_ratio_by_income")
     # Second set of plots
     # Averages by trip purpose
     for(pr in Pr){
          hist(NonAutoCostRatio.ZiPr[,pr], xlab="Non-Auto Cost / Auto Cost",
           xlim=Xlim, breaks=Breaks,
           ylab="Number of Zones", col="skyblue", ylim=c(0,300),
               main=NULL)
          title(main=paste(toupper(pr), "Trips"), line=-1, cex=1)
          }
     # mtext("Ratio of Non-Auto to Auto Market Costs by Purpose", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/hist_nonauto_auto_cost_ratio_by_purpose")
     # Restore graphic parameters
     par(Opar)

#Map non-auto time ratio
#------------------------
#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,2,2,2), oma=c(1,1,2.25,1))
     # First set of plots
     # Overall average
     coropleth(TazPoly, NonAutoCostRatio.Zi, TazIndex, "RdYlBu",
               breaks=seq(0, 225,25), LegendSize=0.75, PlotRef=FALSE,
               main="")
     mtext("All Households", side=1, line=0.5, cex=1)
     # Averages by income group
     for(ic in Ic){
          coropleth(TazPoly, NonAutoCostRatio.ZiIc[,ic], TazIndex, "RdYlBu",
               breaks=seq(0, 225,25), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(ic), "Households"), side=1, line=0.5, cex=1)
          }
     # mtext("Ratio of Non-Auto to Auto Market Costs by Income", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/map_nonauto_auto_cost_ratio_by_income")
     # Second set of plots
     # Averages by purpose
     for(pr in Pr){
          coropleth(TazPoly, NonAutoCostRatio.ZiPr[,pr], TazIndex, "RdYlBu",
               breaks=seq(0, 225,25), LegendSize=0.75, PlotRef=FALSE,
               main="")
          mtext(paste(toupper(pr), "Trips"), side=1, line=0.5, cex=1)
          }
      # mtext("Ratio of Non-Auto to Auto Market Costs by Purpose", outer=TRUE, line=1, cex=1.15)
      savePlot(filename="final_output/tci/graphics/map_nonauto_auto_cost_ratio_by_purpose")
      # Restore graphic parameters
      par(Opar)

#Set up districts reference vectors
#----------------------------------
#::

Districts.Zo <- districts$ugb
names(Districts.Zo) <- districts$zone
DiNames <- c("Outside UGB", "Eagle Point", "Central Point", "Medford", "Jacksonville",
               "Phoenix", "Talent", "Ashland")
names(DiNames) <- unique(Districts.Zo)
Districts.Zi <- Districts.Zo[Zi] ; rm(Districts.Zo)

#Plot comparisons of market costs by UGB
#---------------------------------------
#::

     # Set up graphic parameters
     Opar <- par(mfrow=c(2,2), mar=c(2,3,2,2), oma=c(1,1,2.25,1))
     #Barplot of average market costs by district
     BarCenter <- barplot(AveMarketCost.Di, xlab="", ylab="Dollars", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Average Market Cost", side=1, line=0.5, cex=1)
     mtext("Dollars", side=2, line=2.5)
     text(as.vector(BarCenter), 0.5, labels=DiNames, srt=90)
     #Barplot of auto market costs by district
     BarCenter <- barplot(AutoMarketCost.Di, xlab="", ylab="Dollars", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Auto Market Cost", side=1, line=0.5, cex=1)
     mtext("Dollars", side=2, line=2.5)
     #Barplot of non-auto market costs by district
     BarCenter <- barplot(NonAutoMarketCost.Di, xlab="", ylab="Dollars", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Non-Auto Market Cost", side=1, line=0.5, cex=1)
     mtext("Dollars", side=2, line=2.5)
     #Barplot of minimum market costs by district
     BarCenter <- barplot(BestMarketCost.Di, xlab="", ylab="Dollars", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Minimum Market Cost", side=1, line=0.5, cex=1)
     mtext("Dollars", side=2, line=2.5)
     # mtext("Comparison of Market Costs by Calculation Method and UGB", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/district_market_cost")
     # Restore graphics parameters
     par(Opar)


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
     text(as.vector(BarCenter), 0.5, labels=DiNames, srt=90)
     #Barplot of auto market costs by district
     BarCenter <- barplot(Tci2.Di, xlab="", ylab="Travel Cost Index", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("TCI", side=2, line=2.5)
     mtext("Minimum Market Cost", side=1, line=0.5, cex=1)
     #Barplot of non-auto market costs by district
     BarCenter <- barplot(Tci3.Di, xlab="", ylab="Travel Cost Index", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Composite Market Cost", side=1, line=0.5, cex=1)
     mtext("TCI", side=2, line=2.5)
     # mtext("Comparison of TCI Values by Calculation Method and UGB", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/district_tci")
     # Restore graphics parameters
     par(Opar)

#Plot alternative mode coverage and non-auto cost ratio
#------------------------------------------------------

     windows(6.5, 4)
     # Set up graphic parameters
     Opar <- par(mfcol=c(1,2), mar=c(2,3,2,2), oma=c(1,1,2.25,1))
      # Barplot of alternative mode market coverage
     BarCenter <- barplot(AltMarketCoverage.Di, xlab="", ylab="", col=brewer.pal(8, "Pastel1"),
           main=NULL, axisnames=FALSE)
     mtext("Alternative Mode Coverage", side=1, line=0.5, cex=1)
     mtext("Percent", side=2, line=2.5)
     text(as.vector(BarCenter), 20, labels=DiNames, srt=90)
     # Barplot of ratio of non-auto market cost to auto market cost
     BarCenter <- barplot(NonAutoCostRatio.Di, xlab="", ylab="", col=brewer.pal(8, "Pastel1"),
          main=NULL, axisnames=FALSE)
     mtext("Non-Auto Cost / Auto Cost", side=1, line=0.5, cex=1)
     mtext("Ratio", side=2, line=2.5)
     # mtext("Alternative Mode Coverage and Cost Ratio", outer=TRUE, line=1, cex=1.15)
     savePlot(filename="final_output/tci/graphics/district_alt_mode_coverage_cost")
     # Restore graphics parameters
     par(Opar)
     
