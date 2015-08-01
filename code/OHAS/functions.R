require(maptools)
require(rgeos)
require(rgdal)
require(dplyr)
require(lazyeval)

# overlay x,y coordinate (longitude, lattitude) with a polygon shapefile (shpfile) to get polygon id (id_name)
get_xy_polyid <- function(xy.df, shpfile, id_name) {
  spdf = SpatialPointsDataFrame(xy.df[, c('x', 'y')], 
                                xy.df, 
                                proj4string=CRS("+init=epsg:4326"))
  spdf.proj <- spTransform(spdf, CRS("+init=epsg:2913"))
  
  # spatial join with the shp polygon to get polygon id (id_name)
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
    require(SDMTools)
    tcost.wt <- summarize_(.data,
                #mutate_(tcost.avg=~weighted.mean(tcost, w, na.rm=T))
                  tcost.wtavg=interp(~wt.mean(tcost, w), 
                                   tcost=as.name("tcost"), w=as.name(w)),
                  tcost.wtsd=interp(~wt.sd(tcost, w), 
                                  tcost=as.name("tcost"), w=as.name(w))
                )
    results <- left_join(results, tcost.wt)
  }
  results
}

# compute tcosts by composing grouping and summarizing functions
compute_tcost <- function(df, by, func, w=NULL) {
  df %>%
    group_by_(.dots=by) %>%
    func(w=w)
}