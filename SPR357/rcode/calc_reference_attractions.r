#============================
#calc_reference_attractions.R
#============================

#:author: Brian Gregor
#:date: 9/26/05
#:contact: brian.j.gregor@odot.state.or.us
#:copyright: Oregon Department of Transportation
#:license: GPL2


#Description
#===========

#This script identifies a reference zone for the travel cost index and related measures and computes the reference market place attractions.

#The reference zone is identified as the TAZ that has the most attractions within it's market area. The log sums of the JEMnR access utilities are used to identify market areas. These measure the composite cost of travel between zones and are calculated in JEMnR for each trip purpose and income group. The reference zone is identified as the zone that has greatest market attractions, subject to limits of transit availability and presence of households. Attraction scores are calculated by dividing the market attractions for each zone by the maximum market attractions for all zones. This is done for each trip purpose and income group. The results are summed for each zone to get a total score. Since there are three income groups and four trip purposes, the maximum score is 12. Reference market baskets are calculated for each combination of income and trip purpose. Market baskets are calculated for each TAZ by calculating the percentage of trip attractions to each TAZ and placing this vector of values in order of descending log sums. Then a cumulative sum of the percentages is calculated and the zones whoses values add to 50% are identified. This is the market area for the TAZ. The market basket for the TAZ is calculated by summing the size terms from the destination choice model for TAZs in the market area. The reference market basket is calculated as the mean of the market baskets for all zones.

     
#Define a function that calculates zonal market access scores and market baskets
#===============================================================================

#This function is applied to each TAZ by trip purpose and income group the steps are:
#*   Calculate the percent of trips going to each zone
#*   Order the percent of trips by the order of the log sums
#*   Calculate the cumulative sums of the percents
#*   Identify the zones that sum to 50%
#*   Sum the size terms for those zones to get values for calculating the market basket
#*   Identify zones that have a log sum of one or greater
#*   Sum the size terms in those zones to get values for calculation reference zone scores
#:Parameter: Trips.Zi - A vector of trips from a production zone to attraction zones
#:Parameter: SizeVar.Zi - A vector of size variables
#:Parameter: LogSum.Zi - A vector of logsums from a production zone to attraction zones
#:Parameter: PctBreak - A percentage of trips to be the criteria for determining a market area
#:Return: TripAttractions - The total of size variables to be used for calculating market basket
#:Return: ScoreAttractions - The total of size variables to be used for calculating scores

#::

     calcMarketAccess <- function(Trips.Zi, SizeVar.Zi, LogSum.Zi, PctBreak=50){
          TripPct.Zi <- 100 * Trips.Zi / sum(Trips.Zi)
          LogsumOrder <- rev(order(LogSum.Zi))
          TripPctCumSum <- cumsum(TripPct.Zi[LogsumOrder])
          MarketZones <- names(TripPctCumSum)[TripPctCumSum <= PctBreak]
          TripAttractions <- sum(SizeVar.Zi[MarketZones])
          ScoreAttractions <- sum(SizeVar.Zi[LogSum.Zi > 1])
          c(TripAttractions, ScoreAttractions)
          }

#Calculate market attractions and logsums for each trip purpose, income group and zone
#=====================================================================================

#Iterate through each trip purpose and income group and call calcMarketAccess for each TAZ. Create arrays of values by zone, income group and trip purpose. TripAttractions.ZiIcPr is an array containing the values of total size terms that is used to calculate reference market baskets. ScoreAttractions.ZiIcPr is an array containing the values of total size terms used to identify the reference zone. The abbreviations after the period in the names of these arrays indicates their dimensionality. Zi is a vector of the names of TAZs that are internal to the model. Ic is a vector of the names of income groups. Pr is a vector of the names of trip purposes.

#::

     # Create arrays to store trip attractions and score attractions
     TripAttractions.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)),
                            dimnames=list(Zi,Ic,Pr))
     ScoreAttractions.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)),
                            dimnames=list(Zi,Ic,Pr))


     # Begin iteration by trip purpose
     for(pr in Pr){
     
          # Load trip distribution matrices for each income group
          for(ic in Ic){
               DistFileName <- paste("tripdist/", pr, ic, "Dist.Rdata", sep="")
               load(DistFileName)
               rm(DistFileName)
               }

          # Load size variable matrices for each income group
          for(ic in Ic){
               SizeVarFileName <- paste("sizevars/SizeVar", pr, ic, ".RData", sep="")
               load(SizeVarFileName)
               rm(SizeVarFileName)
               }

          # Load log sums
          # Unlike trip distribution data, the logsums for all income groups
          # are contained in one list.
          LogsumFileName <- paste("access/", "logSum", pr, ".Rdata", sep="")
          load(LogsumFileName)
          rm(LogsumFileName)

          # Calculate Accessibility of Market Areas by Income Group and Zone

          # Create a matrices to store the market area logsums, times and costs for a purpose
          TripAttractions.ZiIc <- matrix(0, length(Zi), length(Ic), dimnames=list(Zi, Ic))
          ScoreAttractions.ZiIc <- matrix(0, length(Zi), length(Ic), dimnames=list(Zi, Ic))

          for(ic in Ic){

               # Get trip matrix trim off external TAZs
               TripsObjName <- paste(pr, ic, "Dist", sep="")
               Trips.ZoZo <- get(TripsObjName) ; rm(TripsObjName)
               dimnames(Trips.ZoZo) <- list(Zo, Zo) # Zo is vector of names of all zones
               Trips.ZiZi <- Trips.ZoZo[Zi,Zi] ; rm(Trips.ZoZo)

               # Get size data matrix and trim off external TAZs
               SizeVarObjName <- paste("sizeVar", pr, ic, sep="")
               SizeVar.ZoZo <- get(SizeVarObjName); rm(SizeVarObjName)
               dimnames(SizeVar.ZoZo) <- list(Zo, Zo)
               SizeVar.ZiZi <- SizeVar.ZoZo[Zi,Zi] ; rm(SizeVar.ZoZo)

               # Get log sum data matrix and trim off external TAZs
               LogSumObjName <- paste("logSum", pr, sep="")
               LogSum.ZoZo <- get(LogSumObjName)[[ic]] ; rm(LogSumObjName)
               dimnames(LogSum.ZoZo) <- list(Zo, Zo)
               LogSum.ZiZi <- LogSum.ZoZo[Zi,Zi] ; rm(LogSum.ZoZo)

               # For each TAZ, calculate trip attractions and score attractions
               MarketValues <- matrix(0, length(Zi), 2, dimnames=list(Zi, c("Attractions", "Logsum")))
               for(zi in Zi){
                    MarketValues[zi,] <- calcMarketAccess(Trips.ZiZi[zi,], SizeVar.ZiZi[zi,],
                                   LogSum.ZiZi[zi,])
                    }
                    
               # Assign values to TripAttractions.ZiIc and ScoreAttractions.ZiIc
               TripAttractions.ZiIc[,ic] <- MarketValues[,1]
               ScoreAttractions.ZiIc[,ic] <- MarketValues[,2]
               rm(MarketValues)
               }

          # Assign values for a purpose to TripAttractions.ZiIcPr and ScoreAttractions.ZiIcPr
          TripAttractions.ZiIcPr[,,pr] <- TripAttractions.ZiIc ; rm(TripAttractions.ZiIc)
          ScoreAttractions.ZiIcPr[,,pr] <- ScoreAttractions.ZiIc ; rm(ScoreAttractions.ZiIc)

          # End iteration by trip purpose
          }


#Find the Reference Zone
#=======================

#The reference zone is the zone which has transit available, has at least 10 households low, middle and high income households, and has the greatest number of attractions in the market area.

#::

     # Calculate a total score for each TAZ
     MaxAttractions <- apply(ScoreAttractions.ZiIcPr, c(2,3), function(x) max(x))
     NormScoreAttractions.ZiIcPr <- sweep(ScoreAttractions.ZiIcPr, c(2,3),
            MaxAttractions, "/")
     AttractionScore.Zi <- apply(NormScoreAttractions.ZiIcPr, 1, sum)

     # Select zones that have at least 10 households and has transit service
     # Load household data
     load("pregen/whiazAry.RData")
     Hh.ZoIc <- t(apply(whiazAry, c(3,5), sum))
     rownames(Hh.ZoIc) <- Zo
     Hh.ZiIc <- Hh.ZoIc[Zi,]
     rm(whiazAry, Hh.ZoIc)
     Hh.ZiIc <- t(apply(Hh.ZiIc, 1, function(x) c(x[1] + x[2], x[3], x[4])))
     colnames(Hh.ZiIc) <- Ic
     HasEnoughHh <- apply(Hh.ZiIc >= 10, 1, all)
     # Identify available internal zones where transit is available
     tAvail <-  read.csv("inputs/tAvail.csv")
     tAvail <- tAvail[order(tAvail$taz),]
     tAvail <- as.logical(tAvail$tAvail)
     TransitAvailable <- tAvail
     names(TransitAvailable) <- Zo
     TransitAvailable <- TransitAvailable[Zi]
     # Select the zones that have enough households and transit is available
     AttractionScoreQualified.Zi <- AttractionScore.Zi[HasEnoughHh & TransitAvailable]

     # Identify the zone with the maximum score from the qualified zones
     ReferenceZone <- names(AttractionScoreQualified.Zi)[
               which(AttractionScoreQualified.Zi == max(AttractionScoreQualified.Zi))]

#Find the Reference market baskets
#=================================

#The reference market baskets are the mean of the market baskets calculated for each zone.

#::

     ReferenceAttractions <- apply(TripAttractions.ZiIcPr, c(2,3), mean)

#Save the results
#================

#::

     save(TripAttractions.ZiIcPr, file="tci/TripAttractions.ZiIcPr.RData")
     save(AttractionScore, file="tci/AttractionScore.RData")
     save(AttractionScoreQualified.Zi, file="tci/AttractionScoreQualified.Zi.RData")
     save(ReferenceZone, file="tci/ReferenceZone.RData")
     save(ReferenceAttractions, file="tci/ReferenceAttractions.RData")
     

