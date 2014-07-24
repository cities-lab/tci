#==========
#calc_tci.R
#==========

#:author: Brian Gregor
#:date: 9/26/05
#:contact: brian.j.gregor@odot.state.or.us
#:copyright: Oregon Department of Transportation
#:license: GPL2


#Description
#===========

#This script calculates the travel cost index (TCI), percent of market accessible by non-auto travel modes, and auto dependence index.


#Load the reference data
#=======================
     # Load the reference zone
     load(paste("intm_output/reference/", "ReferenceZone.RData", sep=""))
     # Load the reference attractions
     load(paste("intm_output/reference/", "ReferenceAttractions.RData", sep=""))

     
#Define functions used in the script
#=================================== 

#Define a function to calculate cost to travel to market place
#-------------------------------------------------------------

#This function is applied to each TAZ by trip purpose and income group the steps are:
#*   Put zones in order of ascending cost. This is used to define the market place for the zone. The costs used for this purpose may be different than the costs used to calculate average market access costs. For example, the average cost for all modes might be used to define the market place, but the market access cost might be calculated for a particular mode.
#*   Calculate the cumulative sum of size variables for the zones in the ascending cost order.
#*   Identify the market place as the set of zones that have size variables equal to the reference market basket.
#*   Calculate a weighted average of travel costs.
#:Parameter: SizeVar.Zi - A vector of size variables
#:Parameter: AveCost.Zi - A vector of costs to use for placing zones in order of cost
#:Parameter: Cost.Zi - The vector of costs to use for calculating the average market cost
#:Parameter: RefAttr - The reference market basket for the trip purpose and income group
#:Return: AveMarketCost - The average cost to access the market basket

#::

     calcAveMarketCost <- function(SizeVar.Zi, AveCost.Zi, Cost.Zi, RefAttr){
          CostOrder <- order(AveCost.Zi)
          AttrCumSum <- cumsum(SizeVar.Zi[CostOrder])
          MarketZones <- names(AttrCumSum)[AttrCumSum <= RefAttr]
          AveMarketCost <- sum(Cost.Zi[MarketZones] * SizeVar.Zi[MarketZones]) /
                         sum(SizeVar.Zi[MarketZones])
          AveMarketCost
          }

#Define a function to calculate the average cost for non-auto modes to travel to market place
#--------------------------------------------------------------------------------------------

#This function is applied to each TAZ by trip purpose and income group the steps are:
#*   Put zones in order of ascending cost.
#*   Calculate the cumulative sum of size variables for the zones in the ascending cost order.
#*   Identify the market place as the set of zones that have size variables equal to the reference market basket.
#*   Identify the minimum non-auto cost to each zone.
#*   Calculate the non-auto market access cost as a weighted average of the minimum non-auto costs.
#:Parameter: SizeVar.Zi - A vector of size variables
#:Parameter: AveCost.Zi - A vector of costs to use for placing zones in order of cost
#:Parameter: ModeCost.ZiMd - The matrix of costs by zone and mode to use for calculating the average non-auto market access cost
#:Parameter: RefAttr - The reference market basket for the trip purpose and income group
#:Return: NonAutoMarketCost - The average non-auto cost to access the market basket

#::

     calcNonAutoMarketCost <- function(SizeVar.Zi, AveCost.Zi, ModeCost.ZiMd, RefAttr){
          CostOrder <- order(AveCost.Zi)
          AttrCumSum <- cumsum(SizeVar.Zi[CostOrder])
          MarketZones <- names(AttrCumSum)[AttrCumSum <= RefAttr]
          NonAutoMarketCosts.ZiMd <- ModeCost.ZiMd[MarketZones, 
                                        c("busWalk", "parkAndRideBus", "bike", "walk")]
          NonAutoMarketCosts.ZiMd[is.infinite(NonAutoMarketCosts.ZiMd)] <- NA
          MinNonAutoCosts.Zi <- apply(NonAutoMarketCosts.ZiMd, 1, function(x)
                                   min(x, na.rm=TRUE))
          NonAutoMarketCost <- sum(MinNonAutoCosts.Zi * SizeVar.Zi[MarketZones]) /
                               sum(SizeVar.Zi[MarketZones])
          NonAutoMarketCost
          }

#Define a function to calculate the average cost for auto modes to travel to market place
#----------------------------------------------------------------------------------------

#This function is applied to each TAZ by trip purpose and income group the steps are:
#*   Put zones in order of ascending cost.
#*   Calculate the cumulative sum of size variables for the zones in the ascending cost order.
#*   Identify the market place as the set of zones that have size variables equal to the reference market basket.
#*   Identify the average auto cost to each zone.
#*   Calculate the auto market access cost as a weighted average of the average auto costs.
#:Parameter: SizeVar.Zi - A vector of size variables
#:Parameter: AveCost.Zi - A vector of costs to use for placing zones in order of cost
#:Parameter: ModeCost.ZiMd - The matrix of costs by zone and mode to use for calculating the average auto market access cost
#:Parameter: RefAttr - The reference market basket for the trip purpose and income group
#:Return: AveMarketCost - The average auto cost to access the market basket

#::

     calcAutoMarketCost <- function(SizeVar.Zi, AveCost.Zi, ModeCost.ZiMd, RefAttr){
          CostOrder <- order(AveCost.Zi)
          AttrCumSum <- cumsum(SizeVar.Zi[CostOrder])
          MarketZones <- names(AttrCumSum)[AttrCumSum <= RefAttr]
          AutoMarketCosts.ZiMd <- ModeCost.ZiMd[MarketZones,
                                        c("driveAlone", "drivePass", "pass")]
          MeanAutoCosts.Zi <- apply(AutoMarketCosts.ZiMd, 1, function(x) mean(x, na.rm=TRUE))
          AutoMarketCost <- sum(MeanAutoCosts.Zi * SizeVar.Zi[MarketZones]) /
                               sum(SizeVar.Zi[MarketZones])
          AutoMarketCost
          }

#Define a function to calculate percent of market place accessible by non-auto modes
#-----------------------------------------------------------------------------------

#This function is applied to each TAZ by trip purpose and income group the steps are:
#*   Put zones in order of ascending cost.
#*   Calculate the cumulative sum of size variables for the zones in the ascending cost order.
#*   Identify the market place as the set of zones that have size variables equal to the reference market basket.
#*   Identify the zones in the market place that are accessible by non-auto modes
#*   Sum the size variables in the zones that are accessible by non-auto modes
#*   Divide by the sum of size variables for all zones in the market place
#:Parameter: SizeVar.Zi - A vector of size variables
#:Parameter: AveCost.Zi - A vector of costs to use for placing zones in order of cost
#:Parameter: AltPractical.ZiMd - A matrix identifying the zones that are accessible by each mode
#:Parameter: RefAttr - The reference market basket for the trip purpose and income group
#:Return: PctAltCoverage - The percentage of the market basket that is accessible by alternate modes

#::

     calcModeMarketPct <- function(SizeVar.Zi, AveCost.Zi, AltPractical.ZiMd, RefAttr){
          CostOrder <- order(AveCost.Zi)
          AttrCumSum <- cumsum(SizeVar.Zi[CostOrder])
          MarketZones <- names(AttrCumSum)[AttrCumSum <= RefAttr]
          AltMarketSet.ZiMd <- AltPractical.ZiMd[MarketZones,]
          AltMarket.Md <- apply(AltMarketSet.ZiMd, 2, function(x) sum(SizeVar.Zi[MarketZones][x]))
          NonAutoMarketSet.Zi <- apply(AltMarketSet.ZiMd, 1, function(x) any(x))
          NonAutoMarket <- sum(SizeVar.Zi[MarketZones][NonAutoMarketSet.Zi])
          AltMarket.Md <- c(AltMarket.Md, allNonAuto=NonAutoMarket)
          PctAltCoverage <- 100 * AltMarket.Md / sum(SizeVar.Zi[MarketZones])
          PctAltCoverage
          }


#Calculate the measures for each zone, mode, income and purpose
#==============================================================

#Make an array which identifies zones impractical to reach by non-auto modes of travel
#-------------------------------------------------------------------------------------

#This is used in the subsequent calculations that loop through purpose and income but only has to 
#be done once.

#::

     AltImpractical.ZiZiMd <- array(FALSE, dim=c(length(Zi), length(Zi), 4),
                              dimnames=list(Zi, Zi, Md[4:7]))
     # Load trip distance and time data
     load(paste(codeLoc,"/inputs/Rdata/tripDist.Rdata",sep=""))
     dimnames(tripDist) <- list(Zo,Zo)
     tripDist.ZiZi <- tripDist[Zi,Zi]
     BikeTime.ZiZi <- 60 * tripDist.ZiZi / 10
     WalkTime.ZiZi <- 60 * tripDist.ZiZi / 3
     load(paste(codeLoc,"/inputs/Rdata/ivTimepeakbusWalk.RData",sep=""))
     dimnames(ivTimepeakbusWalk) <- list(Zo,Zo)
     BusWalkTime.ZiZi <- ivTimepeakbusWalk[Zi,Zi]; rm(ivTimepeakbusWalk)
     load(paste(codeLoc,"/inputs/Rdata/ivTimepeakparkAndRideBus.RData",sep=""))
     dimnames(ivTimepeakparkAndRideBus) <- list(Zo,Zo)
     BusPnRTime.ZiZi <- ivTimepeakparkAndRideBus[Zi,Zi]; rm(ivTimepeakparkAndRideBus)
     load(paste(codeLoc,"/inputs/Rdata/ivTimepeakdriveAlone.RData",sep=""))
     dimnames(ivTimepeakdriveAlone) <- list(Zo,Zo)
     AutoTime.ZiZi <- ivTimepeakdriveAlone[Zi,Zi]; rm(ivTimepeakdriveAlone)
     # Impractical trips are those that take longer than 30 minutes and are
     # 30 minutes longer than corresponding auto trips
     AltImpractical.ZiZiMd[,,"bike"] <- (BikeTime.ZiZi > 30) |
                                        (BikeTime.ZiZi > 30 + AutoTime.ZiZi)
     AltImpractical.ZiZiMd[,,"walk"] <- (WalkTime.ZiZi > 30) |
                                        (WalkTime.ZiZi > 30 + AutoTime.ZiZi)
     AltImpractical.ZiZiMd[,,"busWalk"] <- (BusWalkTime.ZiZi > 30) |
                                        (BusWalkTime.ZiZi > 30 + AutoTime.ZiZi)
     AltImpractical.ZiZiMd[,,"parkAndRideBus"] <- (BusPnRTime.ZiZi > 30) |
                                        (BusPnRTime.ZiZi > 30 + AutoTime.ZiZi)
     # Convert NA values (no bus service) to TRUE
     AltImpractical.ZiZiMd[is.na(AltImpractical.ZiZiMd)] <- TRUE
     # Convert into array of zones that are practical to reach by alt modes
     AltPractical.ZiZiMd <- !AltImpractical.ZiZiMd ; rm(AltImpractical.ZiZiMd)

#Begin iteration by trip purpose
#-------------------------------

#::
     for(pr in Pr){

          # Define Arrays to Store Results
          AveMarketCost.ZiIc <- array(0, dim=c(length(Zi), length(Ic)),
                              dimnames=list(Zi, Ic))
          BestMarketCost.ZiIc <- array(0, dim=c(length(Zi), length(Ic)),
                              dimnames=list(Zi, Ic))
          CompMarketCost.ZiIc <- array(0, dim=c(length(Zi), length(Ic)),
                              dimnames=list(Zi, Ic))
          AveMarketCost.ZiMdIc <- array(0, dim=c(length(Zi), length(Md), length(Ic)),
                              dimnames=list(Zi, Md, Ic))
          NonAutoMarketCost.ZiIc <- array(0, dim=c(length(Zi), length(Ic)),
                              dimnames=list(Zi, Ic))
          AutoMarketCost.ZiIc <- array(0, dim=c(length(Zi), length(Ic)),
                              dimnames=list(Zi, Ic))
          AltMarketCoverage.ZiMdIc <- array(0, dim=c(length(Zi), 5, length(Ic)),
                              dimnames=list(Zi, c(Md[4:7], "allNonAuto"), Ic))

     # Begin iteration by income group
          for(ic in Ic){
          
               # Load the size variable data and assign to SizeVar.ZiZi
               SizeVarFileName <- paste("intm_output/sizevars/SizeVar", pr, ic, ".RData", sep="")
               load(SizeVarFileName) ; rm(SizeVarFileName)
               SizeVarObjName <- paste("sizeVar", pr, ic, sep="")
               SizeVar.ZoZo <- get(SizeVarObjName)
               rm(list=ls()[ls()==SizeVarObjName]) ; rm(SizeVarObjName)
               dimnames(SizeVar.ZoZo) <- list(Zo, Zo)
               SizeVar.ZiZi <- SizeVar.ZoZo[Zi,Zi] ; rm(SizeVar.ZoZo)

               # Identify the reference market attractions
               RefAttr <- ReferenceAttractions[ic,pr]

               # Initialize an array to hold all the mode utility data           
               ModesExpUtils.ZiZiMd <- array(0, dim=c(length(Zi), length(Zi), length(Md)), 
                                   dimnames=list(Zi, Zi, Md))

               # Populate the array with the mode utility data
               for(md in Md){

                    # Load the array of zone to zone utilities and assign to Util.ZiZi
                    if(md == "bike"){
                         ExpUtil.ZiZi <- exp(BikeAccessCoeff.Pr[pr] * 60 * tripDist.ZiZi / 10)
                         }
                    if(md == "walk"){
                         ExpUtil.ZiZi <- exp(WalkAccessCoeff.Pr[pr] * 60 * tripDist.ZiZi / 3)
                         }
                    if((md != "bike") & (md != "walk")) {
                         ModeUtilFileName <- paste("intm_output/access/util", md, ic, pr, ".Rdata", sep="")
                         load(ModeUtilFileName) ; rm(ModeUtilFileName)
                         ModeUtilObjName <- paste("util", md, ic, pr, sep="")
                         ExpUtil.ZoZo <- get(ModeUtilObjName)
                         rm(list=ls()[ls()==ModeUtilObjName]) ; rm(ModeUtilObjName)
                         dimnames(ExpUtil.ZoZo) <- list(Zo, Zo)
                         ExpUtil.ZiZi <- ExpUtil.ZoZo[Zi,Zi] ; rm(ExpUtil.ZoZo)
                         }
                         
                    # Add the mode matrix to the array
                    ModesExpUtils.ZiZiMd[,,md] <- ExpUtil.ZiZi ; rm(ExpUtil.ZiZi)
                    }

               # Correct problem with utility for short walk and bike trips
               # Setting everything greater than 1 to 1 makes a minimum
               # trip time of approximately X minutes
               ModesExpUtils.ZiZiMd[ModesExpUtils.ZiZiMd > 0.99] <- 0.99
               
               # Calculate the dollar cost corresponding to the utilities
               ModesCosts.ZiZiMd <- log(ModesExpUtils.ZiZiMd) / OpCostCoeff.PrIc[pr,ic]

               # Calculate the mode probabilities for all modes
               ModeProbs.ZiZiMd <- sweep(ModesExpUtils.ZiZiMd, c(1,2),
                                        apply(ModesExpUtils.ZiZiMd, c(1,2), sum), "/")

               # Calculate the best cost among the modes
               BestCosts.ZiZi <- apply(ModesCosts.ZiZiMd, c(1,2), function(x) min(x, na.rm=TRUE))
               
               # Calculate the composite cost of all modes
               CompCosts.ZiZi <- log(apply(ModesExpUtils.ZiZiMd, c(1,2), sum)) / OpCostCoeff.PrIc[pr,ic]
               
               # Calculate the average cost across all modes
               AveCosts.ZiZi <- apply(ModesCosts.ZiZiMd * ModeProbs.ZiZiMd, c(1,2),
                                   function(x) sum(x, na.rm=TRUE))

               # Clean up memory
               rm(ModeProbs.ZiZiMd); gc()
               
               # Calculate the mode probabilities for just the non-auto modes
               AltModeProbs.ZiZiMd <- sweep(ModesExpUtils.ZiZiMd[,,4:7], c(1,2),
                                        apply(ModesExpUtils.ZiZiMd[,,4:7], c(1,2), sum), "/")
               
               # Calculate the average cost across non-auto modes
               AveAltCosts.ZiZi <- apply(ModesCosts.ZiZiMd[,,4:7] * AltModeProbs.ZiZiMd, c(1,2),
                                   function(x) sum(x, na.rm=TRUE))

               # Clean up memory
               rm(AltModeProbs.ZiZiMd); gc()
               
               # Calculate the mode probabilities for just the auto modes
               AutoModeProbs.ZiZiMd <- sweep(ModesExpUtils.ZiZiMd[,,1:3], c(1,2),
                                        apply(ModesExpUtils.ZiZiMd[,,1:3], c(1,2), sum), "/")
               
               # Calculate the average cost across auto modes
               AveAutoCosts.ZiZi <- apply(ModesCosts.ZiZiMd[,,1:3] * AutoModeProbs.ZiZiMd, c(1,2),
                                   function(x) sum(x, na.rm=TRUE))
                                   
               # Clean up memory
               rm(AutoModeProbs.ZiZiMd); gc()

               # Calculate best cost to access market place
               BestMarketCost.Zi <- numeric(length(Zi))
               names(BestMarketCost.Zi) <- Zi
               for(zi in Zi){
                    BestMarketCost.Zi[zi] <- calcAveMarketCost(SizeVar.ZiZi[zi,],
                        BestCosts.ZiZi[zi,], BestCosts.ZiZi[zi,], RefAttr)
                    }
                    
               BestMarketCost.ZiIc[,ic] <- BestMarketCost.Zi; rm(BestMarketCost.Zi)
               
               # Calculate composite cost to access market place
               CompMarketCost.Zi <- numeric(length(Zi))
               names(CompMarketCost.Zi) <- Zi
               for(zi in Zi){
                    CompMarketCost.Zi[zi] <- calcAveMarketCost(SizeVar.ZiZi[zi,],
                        CompCosts.ZiZi[zi,], CompCosts.ZiZi[zi,], RefAttr)
                  }

               CompMarketCost.ZiIc[,ic] <- CompMarketCost.Zi; rm(CompMarketCost.Zi)
               
               # Calculate average cost to access market place
               AveMarketCost.Zi <- numeric(length(Zi))
               names(AveMarketCost.Zi) <- Zi
               for(zi in Zi){
                    AveMarketCost.Zi[zi] <- calcAveMarketCost(SizeVar.ZiZi[zi,],
                              AveCosts.ZiZi[zi,], AveCosts.ZiZi[zi,], RefAttr)
                    }

               AveMarketCost.ZiIc[,ic] <- AveMarketCost.Zi; rm(AveMarketCost.Zi)

               # Calculate average cost to access market place by mode
               AveMarketCost.ZiMd <- matrix(0, length(Zi), length(Md), dimnames=list(Zi,Md))
               for(zi in Zi){
                    for(md in Md){
                         AveMarketCost.ZiMd[zi,md] <- calcAveMarketCost(SizeVar.ZiZi[zi,],
                              AveCosts.ZiZi[zi,], ModesCosts.ZiZiMd[zi,,md], RefAttr)
                         }
                    }

               AveMarketCost.ZiMdIc[,,ic] <- AveMarketCost.ZiMd; rm(AveMarketCost.ZiMd)

               # Calculate non-auto market costs
               NonAutoMarketCost.Zi <- numeric(length(Zi))
               names(NonAutoMarketCost.Zi) <- Zi
               ModesCostsInfToNa.ZiZiMd <- ModesCosts.ZiZiMd
               ModesCostsInfToNa.ZiZiMd[is.infinite(ModesCostsInfToNa.ZiZiMd)] <- NA
               for(zi in Zi){
                    NonAutoMarketCost.Zi[zi] <- calcNonAutoMarketCost(SizeVar.ZiZi[zi,],
                         AveCosts.ZiZi[zi,], ModesCostsInfToNa.ZiZiMd[zi,,], RefAttr)
                    }
               rm(ModesCostsInfToNa.ZiZiMd)

               NonAutoMarketCost.ZiIc[,ic] <- NonAutoMarketCost.Zi; rm(NonAutoMarketCost.Zi)

               # Calculate auto market times
               AutoMarketCost.Zi <- numeric(length(Zi))
               names(AutoMarketCost.Zi) <- Zi
               for(zi in Zi){
                    AutoMarketCost.Zi[zi] <- calcAutoMarketCost(SizeVar.ZiZi[zi,],
                         AveCosts.ZiZi[zi,], ModesCosts.ZiZiMd[zi,,], RefAttr)
                    }

               AutoMarketCost.ZiIc[,ic] <- AutoMarketCost.Zi; rm(AutoMarketCost.Zi)


               # Calculate market proportions accessible by non-auto modes
               AltMarketCoverage.ZiMd <- matrix(0, length(Zi), 5,
                    dimnames=list(Zi,c(Md[4:7], "allNonAuto")))
               for(zi in Zi){
                    AltMarketCoverage.ZiMd[zi,] <- calcModeMarketPct(SizeVar.ZiZi[zi,],
                         AveAltCosts.ZiZi[zi,], AltPractical.ZiZiMd[zi,,], RefAttr)
                         }

               AltMarketCoverage.ZiMdIc[,,ic] <- AltMarketCoverage.ZiMd; rm(AltMarketCoverage.ZiMd)

               # Clean up memory
               rm(ModesExpUtils.ZiZiMd, ModesCosts.ZiZiMd)
          
          # Save the results for each income group
          save(BestMarketCost.ZiIc,
               file=paste("final_output/tci/", pr, "BestMarketCost.ZiIc.RData", sep=""))
          save(CompMarketCost.ZiIc,
               file=paste("final_output/tci/", pr, "CompMarketCost.ZiIc.RData", sep=""))
          save(AveMarketCost.ZiIc,
               file=paste("final_output/tci/", pr, "AveMarketCost.ZiIc.RData", sep=""))
          save(AveMarketCost.ZiMdIc,
               file=paste("final_output/tci/", pr, "AveMarketCost.ZiMdIc.RData", sep=""))
          save(NonAutoMarketCost.ZiIc,
               file=paste("final_output/tci/", pr, "NonAutoMarketCost.ZiIc.RData", sep=""))
          save(AutoMarketCost.ZiIc,
               file=paste("final_output/tci/", pr, "AutoMarketCost.ZiIc.RData", sep=""))
          save(AltMarketCoverage.ZiMdIc,
               file=paste("final_output/tci/", pr, "AltMarketCoverage.ZiMdIc.RData", sep=""))

          # End loop through income groups
          }

     # Clean up memory
     rm(BestMarketCost.ZiIc, CompMarketCost.ZiIc, AveMarketCost.ZiIc,
          AveMarketCost.ZiMdIc, NonAutoMarketCost.ZiIc, AutoMarketCost.ZiIc,
          AltMarketCoverage.ZiMdIc)
     gc()

     # End loop through purposes
     }


#Combine the results into arrays by purpose
#==========================================

#::

      BestMarketCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)),
                                          dimnames=list(Zi,Ic,Pr))
      for(pr in Pr){
            FileName <- paste("final_output/tci/", pr, "BestMarketCost.ZiIc.RData", sep="")
            load(FileName)
            BestMarketCost.ZiIcPr[,,pr] <- BestMarketCost.ZiIc
            rm(BestMarketCost.ZiIc)
            save(BestMarketCost.ZiIcPr, file="final_output/tci/BestMarketCost.ZiIcPr.RData")
            }
            
      CompMarketCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)),
                                          dimnames=list(Zi,Ic,Pr))
      for(pr in Pr){
            FileName <- paste("final_output/tci/", pr, "CompMarketCost.ZiIc.RData", sep="")
            load(FileName)
            CompMarketCost.ZiIcPr[,,pr] <- CompMarketCost.ZiIc
            rm(CompMarketCost.ZiIc)
            save(CompMarketCost.ZiIcPr, file="final_output/tci/CompMarketCost.ZiIcPr.RData")
            }

      AveMarketCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)),
                                          dimnames=list(Zi,Ic,Pr))
      for(pr in Pr){
            FileName <- paste("final_output/tci/", pr, "AveMarketCost.ZiIc.RData", sep="")
            load(FileName)
            AveMarketCost.ZiIcPr[,,pr] <- AveMarketCost.ZiIc
            rm(AveMarketCost.ZiIc)
            save(AveMarketCost.ZiIcPr, file="final_output/tci/AveMarketCost.ZiIcPr.RData")
            }

      AveMarketCost.ZiMdIcPr <- array(0, dim=c(length(Zi), length(Md), length(Ic), length(Pr)),
                                          dimnames=list(Zi,Md,Ic,Pr))
      for(pr in Pr){
            FileName <- paste("final_output/tci/", pr, "AveMarketCost.ZiMdIc.RData", sep="")
            load(FileName)
            AveMarketCost.ZiMdIcPr[,,,pr] <- AveMarketCost.ZiMdIc
            rm(AveMarketCost.ZiMdIc)
            save(AveMarketCost.ZiMdIcPr, file="final_output/tci/AveMarketCost.ZiMdIcPr.RData")
            }

      NonAutoMarketCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)),
                                          dimnames=list(Zi,Ic,Pr))
      for(pr in Pr){
            FileName <- paste("final_output/tci/", pr, "NonAutoMarketCost.ZiIc.RData", sep="")
            load(FileName)
            NonAutoMarketCost.ZiIcPr[,,pr] <- NonAutoMarketCost.ZiIc
            rm(NonAutoMarketCost.ZiIc)
            save(NonAutoMarketCost.ZiIcPr, file="final_output/tci/NonAutoMarketCost.ZiIcPr.RData")
            }

      AutoMarketCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)),
                                          dimnames=list(Zi,Ic,Pr))
      for(pr in Pr){
            FileName <- paste("final_output/tci/", pr, "AutoMarketCost.ZiIc.RData", sep="")
            load(FileName)
            AutoMarketCost.ZiIcPr[,,pr] <- AutoMarketCost.ZiIc
            rm(AutoMarketCost.ZiIc)
            save(AutoMarketCost.ZiIcPr, file="final_output/tci/AutoMarketCost.ZiIcPr.RData")
            }

      AltMarketCoverage.ZiMdIcPr <- array(0, dim=c(length(Zi), 5, length(Ic), length(Pr)),
                                          dimnames=list(Zi,c(Md[4:7], "allNonAuto"),Ic,Pr))
      for(pr in Pr){
            FileName <- paste("final_output/tci/", pr, "AltMarketCoverage.ZiMdIc.RData", sep="")
            load(FileName)
            AltMarketCoverage.ZiMdIcPr[,,,pr] <- AltMarketCoverage.ZiMdIc
            rm(AltMarketCoverage.ZiMdIc)
            save(AltMarketCoverage.ZiMdIcPr, file="final_output/tci/AltMarketCoverage.ZiMdIcPr.RData")
            }


#Summarize the aternative mode market coverage
#---------------------------------------------

#::

     #Extract the average value that was computed
     AltMarketCoverage.ZiIcPr <- AltMarketCoverage.ZiMdIcPr[,5,,]
     

#Calculate averages by income group and purpose
#==============================================

#The number of trips produced by income group and purpose will be used to aggregate tci measures

#Load trip data by purpose and aggregate to zone and income group level
#----------------------------------------------------------------------

#::

     # Load trip production data
     load(paste(codeLoc,"/tripgen/hbwTripProdAry.RData",sep=""))
     load(paste(codeLoc,"/tripgen/hbsTripProdAry.RData",sep=""))
     load(paste(codeLoc,"/tripgen/hbrTripProdAry.RData",sep=""))
     load(paste(codeLoc,"/tripgen/hboTripProdAry.RData",sep=""))


     # Aggregate trips to income group level     
     hbwTripProd.ZiIc <- t(apply(apply(hbwTripProdAry, c(5,3), sum), 1, 
          function(x) c(x[1] + x[2], x[3], x[4])))[Zo %in% Zi,]
     dimnames(hbwTripProd.ZiIc) <- list(Zi, Ic)
     rm(hbwTripProdAry) 

     # Aggregate hbs trips
     hbsTripProd.ZiIc <- t(apply(apply(hbsTripProdAry, c(5,3), sum), 1, 
          function(x) c(x[1] + x[2], x[3], x[4])))[Zo %in% Zi,]
     dimnames(hbsTripProd.ZiIc) <- list(Zi, Ic)
     rm(hbsTripProdAry) 

     # Aggregate hbr trips
     hbrTripProd.ZiIc <- t(apply(apply(hbrTripProdAry, c(5,3), sum), 1, 
          function(x) c(x[1] + x[2], x[3], x[4])))[Zo %in% Zi,]
     dimnames(hbrTripProd.ZiIc) <- list(Zi, Ic) 
     rm(hbrTripProdAry)
     
     # Aggregate hbo trips
     hboTripProd.ZiIc <- t(apply(apply(hboTripProdAry, c(5,3), sum), 1, 
          function(x) c(x[1] + x[2], x[3], x[4])))[Zo %in% Zi,]
     dimnames(hboTripProd.ZiIc) <- list(Zi, Ic) 
     rm(hboTripProdAry)

     # Put all the trips into an array
     TripProd.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), 
               dimnames=list(Zi,Ic,Pr))     
     TripProd.ZiIcPr[,,"hbw"] <- hbwTripProd.ZiIc
     TripProd.ZiIcPr[,,"hbs"] <- hbsTripProd.ZiIc
     TripProd.ZiIcPr[,,"hbr"] <- hbrTripProd.ZiIc
     TripProd.ZiIcPr[,,"hbo"] <- hboTripProd.ZiIc
                    
#Calculate proportions by income and purpose
#-------------------------------------------

#::

     TripProd.ZiPr <- apply(TripProd.ZiIcPr, c(1,3), sum)
     TripProd.ZiIc <- apply(TripProd.ZiIcPr, c(1,2), sum)
     TripProd.Zi <- apply(TripProd.ZiIcPr, 1, sum)
     TripProdIncProp.ZiIcPr <- sweep(TripProd.ZiIcPr, c(1,3), TripProd.ZiPr, "/")
     TripProdPurProp.ZiIcPr <- sweep(TripProd.ZiIcPr, c(1,2), TripProd.ZiIc, "/")
     TripProdIncPurProp.ZiIcPr <- sweep(TripProd.ZiIcPr, 1, TripProd.Zi, "/")
     
#Calculate best market cost by purpose and income
#---------------------------------------------------

#::

     BestMarketCost.ZiIc <- apply(BestMarketCost.ZiIcPr * TripProdPurProp.ZiIcPr,
                                   c(1,2), sum)
     BestMarketCost.ZiPr <- apply(BestMarketCost.ZiIcPr * TripProdIncProp.ZiIcPr,
                                   c(1,3), sum)
     BestMarketCost.Zi <- apply(BestMarketCost.ZiIcPr * TripProdIncPurProp.ZiIcPr,
                                   1, sum)
                                   
#Calculate composite market cost by purpose and income
#-----------------------------------------------------

#::

     CompMarketCost.ZiIc <- apply(CompMarketCost.ZiIcPr * TripProdPurProp.ZiIcPr,
                                   c(1,2), sum)
     CompMarketCost.ZiPr <- apply(CompMarketCost.ZiIcPr * TripProdIncProp.ZiIcPr,
                                   c(1,3), sum)
     CompMarketCost.Zi <- apply(CompMarketCost.ZiIcPr * TripProdIncPurProp.ZiIcPr,
                                   1, sum)

#Calculate average market cost by purpose and income
#---------------------------------------------------

#::

     AveMarketCost.ZiIc <- apply(AveMarketCost.ZiIcPr * TripProdPurProp.ZiIcPr,
                                   c(1,2), sum)
     AveMarketCost.ZiPr <- apply(AveMarketCost.ZiIcPr * TripProdIncProp.ZiIcPr,
                                   c(1,3), sum)
     AveMarketCost.Zi <- apply(AveMarketCost.ZiIcPr * TripProdIncPurProp.ZiIcPr,
                                   1, sum)

#Calculate transportation cost index (tci) from averages
#-------------------------------------------------------

#The TCI is calculated from the average market cost

#::

     Tci.ZiIc <- sweep(AveMarketCost.ZiIc, 2, AveMarketCost.ZiIc[ReferenceZone,], "/")
     Tci.ZiPr <- sweep(AveMarketCost.ZiPr, 2, AveMarketCost.ZiPr[ReferenceZone,], "/")
     Tci.Zi <- AveMarketCost.Zi / AveMarketCost.Zi[ReferenceZone]
     
#Calculate transportation cost index (tci) from best costs
#---------------------------------------------------------

#The TCI2 calculates the TCI using the minimum market access cost

#::

     Tci2.ZiIc <- sweep(BestMarketCost.ZiIc, 2, BestMarketCost.ZiIc[ReferenceZone,], "/")
     Tci2.ZiPr <- sweep(BestMarketCost.ZiPr, 2, BestMarketCost.ZiPr[ReferenceZone,], "/")
     Tci2.Zi <- BestMarketCost.Zi / BestMarketCost.Zi[ReferenceZone]

#Calculate transportation cost index (tci) from composite costs
#--------------------------------------------------------------

#The TCI3 calculates the TCI using the composite market access cost

#::

     Tci3.ZiIc <- 1 / sweep(CompMarketCost.ZiIc, 2, CompMarketCost.ZiIc[ReferenceZone,], "/")
     Tci3.ZiPr <- 1 / sweep(CompMarketCost.ZiPr, 2, CompMarketCost.ZiPr[ReferenceZone,], "/")
     Tci3.Zi <- 1 / (CompMarketCost.Zi / CompMarketCost.Zi[ReferenceZone])

#Calculate average auto market cost by purpose and income
#------------------------------------------------------------

#::

     AutoMarketCost.ZiIc <- apply(AutoMarketCost.ZiIcPr * TripProdPurProp.ZiIcPr,
                                   c(1,2), sum)
     AutoMarketCost.ZiPr <- apply(AutoMarketCost.ZiIcPr * TripProdIncProp.ZiIcPr,
                                   c(1,3), sum)
     AutoMarketCost.Zi <- apply(AutoMarketCost.ZiIcPr * TripProdIncPurProp.ZiIcPr,
                                   1, sum)

#Calculate average non-auto market cost by purpose and income
#------------------------------------------------------------

#::

     NonAutoMarketCost.ZiIc <- apply(NonAutoMarketCost.ZiIcPr * TripProdPurProp.ZiIcPr,
                                   c(1,2), sum)
     NonAutoMarketCost.ZiPr <- apply(NonAutoMarketCost.ZiIcPr * TripProdIncProp.ZiIcPr,
                                   c(1,3), sum)
     NonAutoMarketCost.Zi <- apply(NonAutoMarketCost.ZiIcPr * TripProdIncPurProp.ZiIcPr,
                                   1, sum)

#Calculate average alternative mode market coverage
#--------------------------------------------------

#::

     AltMarketCoverage.ZiIc <- apply(AltMarketCoverage.ZiIcPr * TripProdPurProp.ZiIcPr,
                                   c(1,2), sum)
     AltMarketCoverage.ZiPr <- apply(AltMarketCoverage.ZiIcPr * TripProdIncProp.ZiIcPr,
                                   c(1,3), sum)
     AltMarketCoverage.Zi <- apply(AltMarketCoverage.ZiIcPr * TripProdIncPurProp.ZiIcPr,
                                   1, sum)

#Calculate ratio of non-auto time to auto time
#------------------------------------------------

#::

     NonAutoCostRatio.ZiIcPr <- NonAutoMarketCost.ZiIcPr / AutoMarketCost.ZiIcPr
     NonAutoCostRatio.ZiIc <- NonAutoMarketCost.ZiIc / AutoMarketCost.ZiIc
     NonAutoCostRatio.ZiPr <- NonAutoMarketCost.ZiPr / AutoMarketCost.ZiPr
     NonAutoCostRatio.Zi <- NonAutoMarketCost.Zi / AutoMarketCost.Zi


#Calculate regional averages
#===========================

#Load district designations
#--------------------------

#::

     load(paste(codeLoc,"/inputs/Rdata/districts.RData",sep=""))
     District.Zo <- districts$ugb
     names(District.Zo) <- districts$zone
     District.Zi <- District.Zo[Zi] ; rm(District.Zo)
     District.Zi <- as.character(District.Zi)
     Di <- unique(District.Zi)

#Calculate intra-district proportions
#------------------------------------

#::

     TripProd.Di <- tapply(TripProd.Zi, District.Zi, sum)
     TripProdDi.Zi <-  TripProd.Di[match(District.Zi, names(TripProd.Di))]
     TripProdDiProp.Zi <- TripProd.Zi / TripProdDi.Zi
     
     TripProd.DiIc <- apply(TripProd.ZiIc, 2, function(x) tapply(x, District.Zi, sum))
     TripProdDi.ZiIc <- apply(TripProd.DiIc, 2, function(x) x[match(District.Zi, names(x))])
     TripProdDiProp.ZiIc <- TripProd.ZiIc / TripProdDi.ZiIc

     TripProd.DiPr <- apply(TripProd.ZiPr, 2, function(x) tapply(x, District.Zi, sum))
     TripProdDi.ZiPr <- apply(TripProd.DiPr, 2, function(x) x[match(District.Zi, names(x))])
     TripProdDiProp.ZiPr <- TripProd.ZiPr / TripProdDi.ZiPr

     
#Calculate best market time by district
#--------------------------------------

#::

     BestMarketCost.Di <- tapply(TripProdDiProp.Zi * BestMarketCost.Zi, District.Zi, sum)
     BestMarketCost.DiIc <- apply(TripProdDiProp.ZiIc * BestMarketCost.ZiIc, 2, function(x)
                                   tapply(x, District.Zi, sum))
     BestMarketCost.DiPr <- apply(TripProdDiProp.ZiPr * BestMarketCost.ZiPr, 2, function(x)
                                   tapply(x, District.Zi, sum))

#Calculate composite market time by district
#-------------------------------------------

#::

     CompMarketCost.Di <- tapply(TripProdDiProp.Zi * CompMarketCost.Zi, District.Zi, sum)
     CompMarketCost.DiIc <- apply(TripProdDiProp.ZiIc * CompMarketCost.ZiIc, 2, function(x)
                                   tapply(x, District.Zi, sum))
     CompMarketCost.DiPr <- apply(TripProdDiProp.ZiPr * CompMarketCost.ZiPr, 2, function(x)
                                   tapply(x, District.Zi, sum))

#Calculate average market time by district
#-----------------------------------------

#::

     AveMarketCost.Di <- tapply(TripProdDiProp.Zi * AveMarketCost.Zi, District.Zi, sum)
     AveMarketCost.DiIc <- apply(TripProdDiProp.ZiIc * AveMarketCost.ZiIc, 2, function(x)
                                   tapply(x, District.Zi, sum))
     AveMarketCost.DiPr <- apply(TripProdDiProp.ZiPr * AveMarketCost.ZiPr, 2, function(x)
                                   tapply(x, District.Zi, sum))

#Calculate transportation cost index (tci) by district
#-----------------------------------------------------

#The TCI is calculated from the average market cost

#::

     Tci.DiIc <- sweep(AveMarketCost.DiIc, 2, AveMarketCost.ZiIc[ReferenceZone,], "/")
     Tci.DiPr <- sweep(AveMarketCost.DiPr, 2, AveMarketCost.ZiPr[ReferenceZone,], "/")
     Tci.Di <- AveMarketCost.Di / AveMarketCost.Zi[ReferenceZone]

#Calculate tci2 by district
#--------------------------
#The TCI2 calculates the TCI using the minimum market access cost

#::

     Tci2.DiIc <- sweep(BestMarketCost.DiIc, 2, BestMarketCost.ZiIc[ReferenceZone,], "/")
     Tci2.DiPr <- sweep(BestMarketCost.DiPr, 2, BestMarketCost.ZiPr[ReferenceZone,], "/")
     Tci2.Di <- BestMarketCost.Di / BestMarketCost.Zi[ReferenceZone]

#Calculate tci3 by district
#--------------------------
#The TCI3 calculates the TCI using the composite market access cost

#::

     Tci3.DiIc <- 1 / sweep(CompMarketCost.DiIc, 2, CompMarketCost.ZiIc[ReferenceZone,], "/")
     Tci3.DiPr <- 1 / sweep(CompMarketCost.DiPr, 2, CompMarketCost.ZiPr[ReferenceZone,], "/")
     Tci3.Di <- 1 / (CompMarketCost.Di / CompMarketCost.Zi[ReferenceZone])

#Calculate average auto market cost by district
#--------------------------------------------------

#::

     AutoMarketCost.Di <- tapply(TripProdDiProp.Zi * AutoMarketCost.Zi, District.Zi, sum)
     AutoMarketCost.DiIc <- apply(TripProdDiProp.ZiIc * AutoMarketCost.ZiIc, 2, function(x)
                                   tapply(x, District.Zi, sum))
     AutoMarketCost.DiPr <- apply(TripProdDiProp.ZiPr * AutoMarketCost.ZiPr, 2, function(x)
                                   tapply(x, District.Zi, sum))

#Calculate average non-auto market cost by district
#--------------------------------------------------

#::

     NonAutoMarketCost.Di <- tapply(TripProdDiProp.Zi * NonAutoMarketCost.Zi, District.Zi, sum)
     NonAutoMarketCost.DiIc <- apply(TripProdDiProp.ZiIc * NonAutoMarketCost.ZiIc, 2, function(x)
                                   tapply(x, District.Zi, sum))
     NonAutoMarketCost.DiPr <- apply(TripProdDiProp.ZiPr * NonAutoMarketCost.ZiPr, 2, function(x)
                                   tapply(x, District.Zi, sum))


#Calculate average alternative mode market coverage by district
#--------------------------------------------------------------

#::

     AltMarketCoverage.Di <- tapply(TripProdDiProp.Zi * AltMarketCoverage.Zi, District.Zi, sum)
     AltMarketCoverage.DiIc <- apply(TripProdDiProp.ZiIc * AltMarketCoverage.ZiIc, 2, function(x)
                                   tapply(x, District.Zi, sum))
     AltMarketCoverage.DiPr <- apply(TripProdDiProp.ZiPr * AltMarketCoverage.ZiPr, 2, function(x)
                                   tapply(x, District.Zi, sum))

#Calculate ratio of non-auto time to average time by district
#------------------------------------------------------------

#::

     NonAutoCostRatio.DiIc <- NonAutoMarketCost.DiIc / AutoMarketCost.DiIc
     NonAutoCostRatio.DiPr <- NonAutoMarketCost.DiPr / AutoMarketCost.DiPr
     NonAutoCostRatio.Di <- NonAutoMarketCost.Di / AutoMarketCost.Di


#Save the results
#================

#::

     # Best market cost
     save(BestMarketCost.ZiIc, file="final_output/tci/BestMarketCost.ZiIc.RData")
     save(BestMarketCost.ZiPr, file="final_output/tci/BestMarketCost.ZiPr.RData")
     save(BestMarketCost.Zi, file="final_output/tci/BestMarketCost.Zi.RData")
     save(BestMarketCost.DiIc, file="final_output/tci/BestMarketCost.DiIc.RData")
     save(BestMarketCost.DiPr, file="final_output/tci/BestMarketCost.DiPr.RData")
     save(BestMarketCost.Di, file="final_output/tci/BestMarketCost.Di.RData")

     # Composite market cost
     save(CompMarketCost.ZiIc, file="final_output/tci/CompMarketCost.ZiIc.RData")
     save(CompMarketCost.ZiPr, file="final_output/tci/CompMarketCost.ZiPr.RData")
     save(CompMarketCost.Zi, file="final_output/tci/CompMarketCost.Zi.RData")
     save(CompMarketCost.DiIc, file="final_output/tci/CompMarketCost.DiIc.RData")
     save(CompMarketCost.DiPr, file="final_output/tci/CompMarketCost.DiPr.RData")
     save(CompMarketCost.Di, file="final_output/tci/CompMarketCost.Di.RData")

     # Average market cost
     save(AveMarketCost.ZiIc, file="final_output/tci/AveMarketCost.ZiIc.RData")
     save(AveMarketCost.ZiPr, file="final_output/tci/AveMarketCost.ZiPr.RData")
     save(AveMarketCost.Zi, file="final_output/tci/AveMarketCost.Zi.RData")
     save(AveMarketCost.DiIc, file="final_output/tci/AveMarketCost.DiIc.RData")
     save(AveMarketCost.DiPr, file="final_output/tci/AveMarketCost.DiPr.RData")
     save(AveMarketCost.Di, file="final_output/tci/AveMarketCost.Di.RData")

     # Travel Cost Index
     save(Tci.ZiIc, file="final_output/tci/Tci.ZiIc.RData")
     save(Tci.ZiPr, file="final_output/tci/Tci.ZiPr.RData")
     save(Tci.Zi, file="final_output/tci/Tci.Zi.RData")
     save(Tci.DiIc, file="final_output/tci/Tci.DiIc.RData")
     save(Tci.DiPr, file="final_output/tci/Tci.DiPr.RData")
     save(Tci.Di, file="final_output/tci/Tci.Di.RData")

     # Travel Cost Index 2
     save(Tci2.ZiIc, file="final_output/tci/Tci2.ZiIc.RData")
     save(Tci2.ZiPr, file="final_output/tci/Tci2.ZiPr.RData")
     save(Tci2.Zi, file="final_output/tci/Tci2.Zi.RData")
     save(Tci2.DiIc, file="final_output/tci/Tci2.DiIc.RData")
     save(Tci2.DiPr, file="final_output/tci/Tci2.DiPr.RData")
     save(Tci2.Di, file="final_output/tci/Tci2.Di.RData")

     # Travel Cost Index 3
     save(Tci3.ZiIc, file="final_output/tci/Tci3.ZiIc.RData")
     save(Tci3.ZiPr, file="final_output/tci/Tci3.ZiPr.RData")
     save(Tci3.Zi, file="final_output/tci/Tci3.Zi.RData")
     save(Tci3.DiIc, file="final_output/tci/Tci3.DiIc.RData")
     save(Tci3.DiPr, file="final_output/tci/Tci3.DiPr.RData")
     save(Tci3.Di, file="final_output/tci/Tci3.Di.RData")

     # Average non-auto market cost
     save(NonAutoMarketCost.ZiIc, file="final_output/tci/NonAutoMarketCost.ZiIc.RData")
     save(NonAutoMarketCost.ZiPr, file="final_output/tci/NonAutoMarketCost.ZiPr.RData")
     save(NonAutoMarketCost.Zi, file="final_output/tci/NonAutoMarketCost.Zi.RData")
     save(NonAutoMarketCost.DiIc, file="final_output/tci/NonAutoMarketCost.DiIc.RData")
     save(NonAutoMarketCost.DiPr, file="final_output/tci/NonAutoMarketCost.DiPr.RData")
     save(NonAutoMarketCost.Di, file="final_output/tci/NonAutoMarketCost.Di.RData")

     # Average auto market cost
     save(AutoMarketCost.ZiIc, file="final_output/tci/AutoMarketCost.ZiIc.RData")
     save(AutoMarketCost.ZiPr, file="final_output/tci/AutoMarketCost.ZiPr.RData")
     save(AutoMarketCost.Zi, file="final_output/tci/AutoMarketCost.Zi.RData")
     save(AutoMarketCost.DiIc, file="final_output/tci/AutoMarketCost.DiIc.RData")
     save(AutoMarketCost.DiPr, file="final_output/tci/AutoMarketCost.DiPr.RData")
     save(AutoMarketCost.Di, file="final_output/tci/AutoMarketCost.Di.RData")

     # NonAutoCostRatio
     save(NonAutoCostRatio.ZiIcPr, file="final_output/tci/NonAutoCostRatio.ZiIcPr.RData")
     save(NonAutoCostRatio.ZiIc, file="final_output/tci/NonAutoCostRatio.ZiIc.RData")
     save(NonAutoCostRatio.ZiPr, file="final_output/tci/NonAutoCostRatio.ZiPr.RData")
     save(NonAutoCostRatio.Zi, file="final_output/tci/ANonAutoCostRatio.Zi.RData")
     save(NonAutoCostRatio.DiIc, file="final_output/tci/NonAutoCostRatio.DiIc.RData")
     save(NonAutoCostRatio.DiPr, file="final_output/tci/NonAutoCostRatio.DiPr.RData")
     save(NonAutoCostRatio.Di, file="final_output/tci/ANonAutoCostRatio.Di.RData")

     # Alternative mode market coverage
     save(AltMarketCoverage.ZiIcPr, file="final_output/tci/AltMarketCoverage.ZiIcPr.RData")
     save(AltMarketCoverage.ZiIc, file="final_output/tci/AltMarketCoverage.ZiIc.RData")
     save(AltMarketCoverage.ZiPr, file="final_output/tci/AltMarketCoverage.ZiPr.RData")
     save(AltMarketCoverage.Zi, file="final_output/tci/AltMarketCoverage.Zi.RData")
     save(AltMarketCoverage.DiIc, file="final_output/tci/AltMarketCoverage.DiIc.RData")
     save(AltMarketCoverage.DiPr, file="final_output/tci/AltMarketCoverage.DiPr.RData")
     save(AltMarketCoverage.Di, file="final_output/tci/AltMarketCoverage.Di.RData")

#write out Zonal Summary CSV
     TCI_Zone<-data.frame(t(rbind(round(BestMarketCost.Zi,2),round(CompMarketCost.Zi,2),round(AveMarketCost.Zi,2),
      round(AutoMarketCost.Zi,0),round(NonAutoMarketCost.Zi,0),round(NonAutoCostRatio.Zi,2),
      round(AltMarketCoverage.Zi,1),round(Tci.Zi,2),round(Tci2.Zi,2),
      round(Tci3.Zi,2))))
      colnames(TCI_Zone)<-c("BestMarketCost","CompMarketCost","AveMarketCost","AutoMarketCost",
      "NonAutoMarketCost","NonAutoCostRatio","AltMarketCoverage","Tci_Best","Tci2_Composite","Tci2_Average")
      
      write.table(TCI_Zone,paste(getwd(),"/final_output/Scenario",Scen,"_",ScenYear,"TCIPerformanceMeasures.csv",sep="")
      ,sep=",",row.names=TRUE,col.names=TRUE)

#write out District Summary CSV
     TCI_Dist<-data.frame(t(rbind(round(BestMarketCost.Di,2),round(CompMarketCost.Di,2),round(AveMarketCost.Di,2),
      round(AutoMarketCost.Di,0),round(NonAutoMarketCost.Di,0),round(NonAutoCostRatio.Di,2),
      round(AltMarketCoverage.Di,1),round(Tci.Di,2),round(Tci2.Di,2),
      round(Tci3.Di,2))))
      colnames(TCI_Dist)<-c("BestMarketCost","CompMarketCost","AveMarketCost","AutoMarketCost",
      "NonAutoMarketCost","NonAutoCostRatio","AltMarketCoverage","Tci_Best","Tci2_Composite","Tci2_Average")
      
      write.table(TCI_Dist,paste(getwd(),"/final_output/Scenario",Scen,"_",ScenYear,"TCIPerformanceMeasures.csv",sep="")
      ,sep=",",row.names=TRUE,col.names=TRUE)