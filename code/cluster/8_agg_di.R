#Calculate regional averages cost
#================================

#Load district designations
#--------------------------

#:: 

     load("data/CenTTime/CenRData/districts.RData")
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

     
#Calculate offpeak mininal travel time cost by district
#--------------------------------------

#::

minoffpeakAggCost.Di <- tapply(TripProdDiProp.Zi * minoffpeakAggCost.Zi, District.Zi, function(x) sum(x, na.rm=TRUE))
save(minoffpeakAggCost.Di,file="data/CenTTime/results/aggcostcmtp/minoffpeakAggCost.Di.Rdata")

minoffpeakAggCost.DiIc <- apply(TripProdDiProp.ZiIc * minoffpeakAggCost.ZiIC, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(minoffpeakAggCost.DiIc,file="data/CenTTime/results/aggcostcmtp/minoffpeakAggCost.DiIc.Rdata")

minoffpeakAggCost.DiPr <- apply(TripProdDiProp.ZiPr * minoffpeakAggCost.ZiPr, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(minoffpeakAggCost.DiPr,file="data/CenTTime/results/aggcostcmtp/minoffpeakAggCost.DiPr.Rdata")


#Calculate offpeak weighted travel time cost by district
#--------------------------------------

#::

weightedoffpeakAggCost.Di <- tapply(TripProdDiProp.Zi * weightedoffpeakAggCost.Zi, District.Zi, function(x) sum(x, na.rm=TRUE))
save(weightedoffpeakAggCost.Di,file="data/CenTTime/results/aggcostcmtp/weightedoffpeakAggCost.Di.Rdata")

weightedoffpeakAggCost.DiIc <- apply(TripProdDiProp.ZiIc * weightedoffpeakAggCost.ZiIC, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(weightedoffpeakAggCost.DiIc,file="data/CenTTime/results/aggcostcmtp/weightedoffpeakAggCost.DiIc.Rdata")

weightedoffpeakAggCost.DiPr <- apply(TripProdDiProp.ZiPr * weightedoffpeakAggCost.ZiPr, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(weightedoffpeakAggCost.DiPr,file="data/CenTTime/results/aggcostcmtp/weightedoffpeakAggCost.DiPr.Rdata")

#Calculate peak mininal travel time cost by district
#--------------------------------------

#::

minpeakAggCost.Di <- tapply(TripProdDiProp.Zi * minpeakAggCost.Zi, District.Zi, function(x) sum(x, na.rm=TRUE))
save(minpeakAggCost.Di,file="data/CenTTime/results/aggcostcmtp/minpeakAggCost.Di.Rdata")

minpeakAggCost.DiIc <- apply(TripProdDiProp.ZiIc * minpeakAggCost.ZiIC, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(minpeakAggCost.DiIc,file="data/CenTTime/results/aggcostcmtp/minpeakAggCost.DiIc.Rdata")

minpeakAggCost.DiPr <- apply(TripProdDiProp.ZiPr * minpeakAggCost.ZiPr, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(minpeakAggCost.DiPr,file="data/CenTTime/results/aggcostcmtp/minpeakAggCost.DiPr.Rdata")


#Calculate peak weighted travel time cost by district
#--------------------------------------

#::

weightedpeakAggCost.Di <- tapply(TripProdDiProp.Zi * weightedpeakAggCost.Zi, District.Zi, function(x) sum(x, na.rm=TRUE))
save(weightedpeakAggCost.Di,file="data/CenTTime/results/aggcostcmtp/weightedpeakAggCost.Di.Rdata")

weightedpeakAggCost.DiIc <- apply(TripProdDiProp.ZiIc * weightedpeakAggCost.ZiIC, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(weightedpeakAggCost.DiIc,file="data/CenTTime/results/aggcostcmtp/weightedpeakAggCost.DiIc.Rdata")

weightedpeakAggCost.DiPr <- apply(TripProdDiProp.ZiPr * weightedpeakAggCost.ZiPr, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(weightedpeakAggCost.DiPr,file="data/CenTTime/results/aggcostcmtp/weightedpeakAggCost.DiPr.Rdata")

