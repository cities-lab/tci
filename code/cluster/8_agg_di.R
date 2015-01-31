#Aggregate costs by geography
#================================

#Load district designations
#--------------------------
#districts data frame map TAZ (zone) to district id (ugb???)
load(file.path(INPUT_DIR, "districts.RData"))
row.names(districts) <- as.character(districts$zone)
District.Zi <- districts[Zi, 'ugb']
Di <- unique(District.Zi)

#Calculate intra-district proportions
#------------------------------------
TripProd.Di <- tapply(TripProd.Zi, District.Zi, sum)
TripProdDi.Zi <-  TripProd.Di[match(District.Zi, names(TripProd.Di))]
TripProdDiProp.Zi <- TripProd.Zi / TripProdDi.Zi
    
TripProd.DiIc <- apply(TripProd.ZiIc, 2, function(x) tapply(x, District.Zi, sum))
TripProdDi.ZiIc <- apply(TripProd.DiIc, 2, function(x) x[match(District.Zi, names(x))])
TripProdDiProp.ZiIc <- TripProd.ZiIc / TripProdDi.ZiIc

TripProd.DiPr <- apply(TripProd.ZiPr, 2, function(x) tapply(x, District.Zi, sum))
TripProdDi.ZiPr <- apply(TripProd.DiPr, 2, function(x) x[match(District.Zi, names(x))])
TripProdDiProp.ZiPr <- TripProd.ZiPr / TripProdDi.ZiPr

Tp = c('peak', 'offpeak')
Cm = c('min', 'weighted', 'max')

vars.ls <- ls()

for (cm in Cm) {
  for (tp in Tp) {
    zicost.name <- paste(cm, tp, 'AggCost.Zi', sep="")
    zicost <- get(zicost.name)
    dscost <- tapply(TripProdDiProp.Zi * zicost, District.Zi, function(x) sum(x, na.rm=TRUE))
    dicost.name <- paste(cm, tp, 'AggCost.Di', sep="")
    assign(dicost.name, dscost)
    
    #by district & Income
    zicost.name <- paste(cm, tp, 'AggCost.ZiIc', sep="")
    zicost <- get(zicost.obj.name)
    dscost <- apply(TripProdDiProp.ZiIc * zicost, District.Zi, function(x)
                    tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
    dicost.name <- paste(cm, tp, 'AggCost.DiIc', sep="")
    assign(dicost.name, dscost)

    #by district & trip purpose
    zicost.name <- paste(cm, tp, 'AggCost.ZiPr', sep="")
    zicost <- get(zicost.name)
    dscost <- apply(TripProdDiProp.ZiPr * zicost, District.Zi, function(x)
                    tapply(x, District.Zi, function(x) sum(x, na.rm=TRUE)))
    dicost.name <- paste(cm, tp, 'AggCost.DiPr', sep="")
    assign(dicost.name, dscost)
  }
}

rm(c('zicost.name', 'dicost.name'))

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, 'AggCost.Di.RData')
  save(list=setdiff(vars.ls, ls()), file=intm.file)
}