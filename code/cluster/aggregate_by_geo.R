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
  #costs <- with(merged.df, aggregated.weighted.mean(costs, weights, geo.id))
  #costs
  
  geocosts.df <- merged.df %>%
    na.omit() %>%
    group_by(geo.id) %>%
    summarise(costs=weighted.mean(costs, weights))
  geocosts.df
}

aggregated.weighted.mean <- function(values, weights, group.id) {
  df <- data.frame(values=values, weights=weights, id=group.id)
  results.df <- df %>%
    na.omit() %>%
    group_by(id) %>%
    summarise(results=weighted.mean(values, weights))
  results.arr <- array(results.df$results, dimnames=list(results.df$id))
  results.arr
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
stopifnot(all(results.ret == results.exp))
#results.ret == results.exp

## end test

#Aggregate costs by geography
#================================

#Load district designations
#--------------------------
#districts data frame map TAZ (zone) to district id (ugb???)
load(file.path(INPUT_DIR, "districts.RData"))
row.names(districts) <- as.character(districts$zone)
District.Zi <- districts[Zi, 'ugb']
Di <- unique(District.Zi)
Di <- Di[!is.na(Di)]
Di <- Di[order(Di)]

vars.ls <- ls()

for (cm in Cm) {
  for (tp in Tp) {
    zicost.name <- paste(cm, tp, 'AggCost.Zi', sep="")
    zicost <- get(zicost.name)
    AggCost.Di <- aggregated.weighted.mean(zicost, TripProd.Zi, District.Zi)
    dicost.name <- paste(cm, tp, 'AggCost.Di', sep="")
    assign(dicost.name, AggCost.Di)
    
    #by district & Income
    ZiIccost.name <- paste(cm, tp, 'AggCost.ZiIc', sep="")
    ZiIccost <- get(ZiIccost.name)
    AggCost.DiIc <- array(0, dim=c(length(Di), length(Ic)), dimnames=c(list(Di), list(Ic)))
    for (ic in Ic) {
      AggCost.DiIc[, ic] <- aggregated.weighted.mean(ZiIccost[, ic], TripProd.ZiIc[, ic], District.Zi)
    }
    dicost.name <- paste(cm, tp, 'AggCost.DiIc', sep="")
    assign(dicost.name, AggCost.DiIc)

    #by district & trip purpose
    ZiPrcost.name <- paste(cm, tp, 'AggCost.ZiPr', sep="")
    ZiPrcost <- get(ZiPrcost.name)
    AggCost.DiPr <- array(0, dim=c(length(Di), length(Pr)), dimnames=c(list(Di), list(Pr)))
    for (pr in Pr) {
      AggCost.DiPr[, pr] <- aggregated.weighted.mean(ZiPrcost[, pr], TripProd.ZiPr[, pr], District.Zi)
    }
    dicost.name <- paste(cm, tp, 'AggCost.DiPr', sep="")
    assign(dicost.name, AggCost.DiPr)
  }
}

output.file <- file.path(OUTPUT_DIR, 'AggCost.Di.RData')
save(list=setdiff(vars.ls, c(ls(), c('zicost.name', 'dicost.name'))), file=output.file)
