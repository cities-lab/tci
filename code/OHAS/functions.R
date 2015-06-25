require(maptools)
require(rgeos)
require(rgdal)
require(dplyr)
require(lazyeval)

# overlay X,Y coordinate with a TAZ shapefile (shpfile) to get TAZ id (id_name)
get_htaz <- function(xy.df, shpfile, id_name) {
  spdf = SpatialPointsDataFrame(xy.df[, c('x', 'y')], 
                                xy.df, 
                                proj4string=CRS("+init=epsg:4326"))
  spdf.proj <- spTransform(spdf, CRS("+init=epsg:2913"))
  
  # spatial join with TAZ to get HTAZ (HOME TAZ)
  TAZPoly <- readShapePoly(shpfile,
                           proj4string=CRS("+init=epsg:2913"))
  id <- over(spdf.proj, TAZPoly)[, id_name]
  id
}

# summarise tcost with default summary quantities
summarize_tcost <- function(.data, w=NULL) {
  results <- summarize(.data, n = n(),
              tcost.min=min(tcost, na.rm=T),
              tcost.avg=mean(tcost, na.rm=T),
              tcost.max=max(tcost, na.rm=T),
              tcost.sd=sd(tcost, na.rm=T)
              )
  
  if (!is.null(w)) {
    tcost.wavg <- summarize_(.data,
                #mutate_(tcost.avg=~weighted.mean(tcost, w, na.rm=T))
                  tcost.wavg=interp(~weighted.mean(tcost, w, na.rm=TRUE), 
                                   tcost=as.name("tcost"), w=as.name(w)))
    results <- left_join(results, tcost.wavg)
  }
  results
}

# compute tcosts by composing grouping and summarizing functions
compute_tcost <- function(df, by, func, w=NULL) {
  df %>%
    group_by_(.dots=by) %>%
    func(w=w)
}