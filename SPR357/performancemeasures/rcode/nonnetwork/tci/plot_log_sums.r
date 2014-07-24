#===============
#plot_log_sums.R
#===============

#:author: Brian Gregor
#:date: 8/25/05
#:contact: brian.j.gregor@odot.state.or.us
#:copyright: Oregon Department of Transportation
#:license: GPL2


#Description
#===========

#This script plots the logsum values in decreasing order for each trip purpose examined for the TCI calculations.
#The pupose of this is to identify a threshold logsum value to use for comparing the number of attractions
# in a market area for each trip purpose. The steps are as follows:
#* The list of zone to zone log sums for each income group is loaded for each trip purpose
#- The logsum values are averaged across incomes
#- Each row in the resulting matrix is placed in decreasing order
#- The average of each column of the matrix is calculated
#* A graph of the results are plotted for each trip purpose
#- The matrix is plotted
#- The average is plotted


#Iterate through trip purposes and create data to plot
#=====================================================

     for(pr in Pr){
          InObjName <- paste("logSum", pr, sep="")
          FileName <- paste("intm_output/access/", InObjName, ".RData", sep="")
          load(FileName)
          LogSum.Ic_ZoZo <- get(InObjName); rm(list=InObjName)
          LogSum.ZoZoIc <- array(unlist(LogSum.Ic_ZoZo), dim=c(length(Zo), 
                                   length(Zo), length(Ic))); rm(LogSum.Ic_ZoZo)
          LogSum.ZoZo <- apply(LogSum.ZoZoIc, c(1,2), mean); rm(LogSum.ZoZoIc)
          LogSumSort <- t(apply(LogSum.ZoZo, 1, function(x) rev(sort(x)))); rm(LogSum.ZoZo)
          LogSumSortMean <- apply(LogSumSort, 2, mean)
          LogSumSortName <- paste(InObjName, "Sort", sep="")
          assign(LogSumSortName, LogSumSort); rm(LogSumSort)
          LogSumSortMeanName <- paste(LogSumSortName, "Mean", sep="")
          assign(LogSumSortMeanName, LogSumSortMean); rm(LogSumSortMean) 
          rm(InObjName, FileName, LogSumSortName, LogSumSortMeanName)
          }

#Iterate through trip purposes and create plots
#==============================================

     jpeg(filename="final_output/tci/graphics/sorted_logsums.jpeg", width=600, height=600)
     Opar <- par(mfrow=c(2,2))
     for(pr in Pr){
          MatplotObjName <- paste("logSum", pr, "Sort", sep="")
          MeanplotObjName <- paste(MatplotObjName, "Mean", sep="")
          matplot(t(get(MatplotObjName)), type="l", col="grey",
                    main=paste("Sorted", toupper(pr), "Logsum Values"),
                    ylab="Logsum")
          lines(get(MeanplotObjName))
          }
     par(Opar)
     dev.off()

#Clean up the workspace
#======================

     for(pr in Pr){
          MatplotObjName <- paste("logSum", pr, "Sort", sep="")
          MeanplotObjName <- paste(MatplotObjName, "Mean", sep="")
          rm(list=MatplotObjName); rm(list=MeanplotObjName)
          }
     rm(MatplotObjName, MeanplotObjName)
                              
