# This script aggregates cost by districts 
  
# Combine the number of trips produced by income group and purpose into array 

# aggregate Trips 

TripProd.ZiIc <-  apply(Trips.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))
TripProd.ZiPr <-  apply(Trips.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))
TripProd.Zi <-  apply(Trips.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))

#Load district designations
#--------------------------

#:: 

load("data/OHASTTime/Rdata/districts.RData")
District.Zo <- districts$ugb
names(District.Zo) <- districts$zone
District.Zi <- District.Zo[Zi] ; rm(District.Zo)
District.Zi <- as.character(District.Zi)
Di <- unique(District.Zi)


# Calculate intra-district proportions
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

Cost.Di <- tapply(TripProdDiProp.Zi * Cost.Zi, District.Zi, function(x) sum(x, na.rm=TRUE))
save(Cost.Di,file="data/OHASTTime/results/Cost.Di.Rdata")

Cost.DiIc <- apply(TripProdDiProp.ZiIc * Cost.ZiIc, 2, function(x) 
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(Cost.DiIc,file="data/OHASTTime/results/Cost.DiIc.Rdata")

Cost.DiPr <- apply(TripProdDiProp.ZiPr * Cost.ZiPr, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(Cost.DiPr,file="data/OHASTTime/results/Cost.DiPr.Rdata")

