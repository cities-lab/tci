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
          AveMarketCost <- sum(Cost.Zi[MarketZones] * SizeVar.Zi[MarketZones],na.rm=TRUE) /
                         sum(SizeVar.Zi[MarketZones],na.rm=TRUE)
          AveMarketCost
          }

# Load trip distance 
     load(paste(codeLoc,"/inputs/Rdata/tripDist.Rdata",sep=""))
     dimnames(tripDist) <- list(Zo,Zo)
     tripDist.ZiZi <- tripDist[Zi,Zi]

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
         

          # End loop through income groups
          }

     # Clean up memory
     rm(BestMarketCost.ZiIc, CompMarketCost.ZiIc, AveMarketCost.ZiIc,
          AveMarketCost.ZiMdIc)
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

     
     

#Calculate averages by income group and purpose
#==============================================

#The number of trips produced by income group and purpose will be used to aggregate tci measures

#Load trip data by purpose and aggregate to zone and income group level
#----------------------------------------------------------------------

#::

     # Load trip production data
     load(paste(codeLoc,"/tripgen/hbwTripProd.ZiIc.RData",sep=""))
     load(paste(codeLoc,"/tripgen/hbsTripProd.ZiIc.RData",sep=""))
     load(paste(codeLoc,"/tripgen/hbrTripProd.ZiIc.RData",sep=""))
     load(paste(codeLoc,"/tripgen/hboTripProd.ZiIc.RData",sep=""))


     # Aggregate trips to income group level     
         dimnames(hbwTripProd.ZiIc) <- list(Zi, Ic)
    

     # Aggregate hbs trips
    
     dimnames(hbsTripProd.ZiIc) <- list(Zi, Ic)
     

     # Aggregate hbr trips
     
     dimnames(hbrTripProd.ZiIc) <- list(Zi, Ic) 
    
     
     # Aggregate hbo trips

     dimnames(hboTripProd.ZiIc) <- list(Zi, Ic) 
 

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

#### replace NAs with zero (because the NAs are generated by dividing by zero
     TripProdIncProp.ZiIcPr[is.na(TripProdIncProp.ZiIcPr)] <- 0
     TripProdPurProp.ZiIcPr[is.na(TripProdPurProp.ZiIcPr)] <- 0
     TripProdIncPurProp.ZiIcPr[is.na(TripProdIncPurProp.ZiIcPr)] <- 0



     
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

#### replace -Inf with zero (because the -Inf are generated by dividing by zero
     
     Tci3.ZiIc[is.infinite(Tci3.ZiIc)] <- 0
     Tci3.ZiPr[is.infinite(Tci3.ZiPr)] <- 0
     Tci3.Zi[is.infinite(Tci3.Zi)] <- 0


#Calculate regional averages
#===========================

#Load district designations
#--------------------------

#::

     load("inputs/RData/districts.RData")
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

     BestMarketCost.Di <- tapply(TripProdDiProp.Zi * BestMarketCost.Zi, District.Zi, function(x) sum(x, na.rm=TRUE))
     BestMarketCost.DiIc <- apply(TripProdDiProp.ZiIc * BestMarketCost.ZiIc, 2, function(x)
                                   tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
     BestMarketCost.DiPr <- apply(TripProdDiProp.ZiPr * BestMarketCost.ZiPr, 2, function(x)
                                   tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
#Calculate composite market time by district
#-------------------------------------------

#::

     CompMarketCost.Di <- tapply(TripProdDiProp.Zi * CompMarketCost.Zi, District.Zi, function(x) sum(x, na.rm=TRUE))

     CompMarketCost.DiIc <- apply(TripProdDiProp.ZiIc * CompMarketCost.ZiIc, 2, function(x)
                                   tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))

     CompMarketCost.DiPr <- apply(TripProdDiProp.ZiPr * CompMarketCost.ZiPr, 2, function(x)
                                   tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))

#Calculate average market time by district
#-----------------------------------------

#::

     AveMarketCost.Di <- tapply(TripProdDiProp.Zi * AveMarketCost.Zi, District.Zi, function(x) sum(x, na.rm=TRUE))

     AveMarketCost.DiIc <- apply(TripProdDiProp.ZiIc * AveMarketCost.ZiIc, 2, function(x)
                                   tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))

     AveMarketCost.DiPr <- apply(TripProdDiProp.ZiPr * AveMarketCost.ZiPr, 2, function(x)
                                   tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))


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

    

    

