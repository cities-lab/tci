##refactor this script to a function call
require(dplyr)
aggregate_by_geo <- function(costs.df, weights.df, mapping.df) {
  if (is.null(weights.df)) {
    weights.df <- costs.df 
    weights.df$weights <- 1.0
    weights.df$costs <- NULL
  }
  
  stopifnot(all(weights.df$weights>=0))
  merged.df <- left_join(costs.df, mapping.df) #join by common column(s)
  merged.df <- left_join(merged.df, weights.df)
  geocosts.df <- merged.df %>%
    na.omit() %>%
    group_by(geo.id) %>%
    summarise(costs=weighted.mean(costs, weights))
  geocosts.df
}

aggregated.weighted.mean <- function(values, weights, group.id) {
  df <- data.frame(values=values, weights=weights, group.id=group.id)
  results.df <- df %>%
    na.omit() %>%
    group_by(group.id) %>%
    summarise(results=weighted.mean(values, weights))
  results.df$results
}

## begin test
costs.df <- data.frame(id=1:9, costs=c(rnorm(8),NA))
mapping.df <- data.frame(id    =c(3, 1, 2, 4, 5, 6, 7, 8),
                         geo.id=c(2, 1, 1, 2, 3, 4, 4, 4))
weights.df <- data.frame(id=8:1,
                         weights=runif(8) * 8)

merged.df <- merge(costs.df, mapping.df)
merged.df <- merge(merged.df, weights.df)
costs <- as.vector(by(merged.df, 
                      merged.df$geo.id, 
                      function(x) weighted.mean(x$costs, x$weights)
                      )
                   )
results.exp <- data.frame(geo.id=1:4, costs=costs)

results.ret <- aggregate_by_geo(costs.df, weights.df, mapping.df)
results.ret <- as.data.frame(results.ret)
stopifnot(all.equal(results.ret, results.exp))
## end test


## 
load(file.path(INPUT_DIR, "districts.RData"))
row.names(districts) <- as.character(districts$zone)
District.Zi <- districts[Zi, 'ugb']
Di <- unique(District.Zi)

load(file.path(INPUT_DIR, "districts.RData"))
row.names(districts) <- as.character(districts$zone)
District.Zi <- districts[Zi, 'ugb']

#for (cm in Cm) {
#  for (tp in Tp) {
#    zicost.name <- paste(cm, tp, 'AggCost.Zi', sep="")
#    zicost <- get(zicost.name)
#    dicost <- aggregated.weighted.mean(zicost, TripProd.Zi, District.Zi) #how are the results ordered?
#  }
#}

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