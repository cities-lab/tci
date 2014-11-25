## This scripts aggregate cost by districts 

## Combine the number of trips produced by income group and purpose into array 

TripProd.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))

load("Rdata/tripgen/hbwTripProd.ZiIc.RData")
load("Rdata/tripgen/hbsTripProd.ZiIc.RData")
load("Rdata/tripgen/hbrTripProd.ZiIc.RData")
load("Rdata/tripgen/hboTripProd.ZiIc.RData")

TripProd.ZiIcPr[,,"hbw"] <- hbwTripProd.ZiIc
TripProd.ZiIcPr[,,"hbs"] <- hbsTripProd.ZiIc
TripProd.ZiIcPr[,,"hbr"] <- hbrTripProd.ZiIc
TripProd.ZiIcPr[,,"hbo"] <- hboTripProd.ZiIc

TripProd.ZiIc <-  apply(TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))
TripProd.ZiPr <-  apply(TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))
TripProd.Zi <-  apply(TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))

#Load district designations
#--------------------------

#:: 

load("RData/districts.RData")
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

Cost.Di <- tapply(TripProdDiProp.Zi * Cost.Zi, District.Zi, function(x) sum(x, na.rm=TRUE))
save(Cost.Di,file="results/Cost.Di.Rdata")

Cost.DiIc <- apply(TripProdDiProp.ZiIc * Cost.ZiIc, 2, function(x) 
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(Cost.DiIc,file="results/Cost.DiIc.Rdata")

Cost.DiPr <- apply(TripProdDiProp.ZiPr * Cost.ZiPr, 2, function(x)
  tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
save(Cost.DiPr,file="results/Cost.DiPr.Rdata")

