## This script identifies TAZs index of clusters with at least two tazs, each with employment 
## density above D, that together with at least E total employment or sizeterms 

# load required libraries
require(maptools)
require(rgeos)
require(rgdal)
require(dplyr)
require(lazyeval)

# define a functon to identify clusters 

identify_clusters <- function(map.df, filter=NULL, dist=NULL) {
  #map.df has to be a SpatialPolygonsDataFrame 
  #e.g. a shp file loaded with maptools::readShapePoly
  data.df <- map.df@data
  n <- nrow(data.df)
  if (is.null(filter))  filter <- 1:n
  data.df$cluster.id <- 1:n
  data.df$cluster.id[!filter] <- 0
  
  for (i in which(filter)) {
    for (j in which(filter)) {
        if (!is.null(dist))
            is.adjacent <- gWithinDistance(map.df[i,], map.df[j,], dist)
        else
            is.adjacent <- gTouches(map.df[i,], map.df[j,])
        if (is.adjacent){
          #browser()
          new.cluster.id <- min(data.df[i,'cluster.id'], data.df[j,'cluster.id'])
          idx <- with(data.df, cluster.id %in% c(cluster.id[i], cluster.id[j]))
          data.df$cluster.id[idx] <- new.cluster.id
        }  
    }
  }
  map.df@data <- data.df
  map.df
}

#example code
#map.df <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
#                       proj4string=CRS("+proj=longlat +datum=NAD27"))
#
#filter <- map.df@data$AREA<.140
#map.df2 <- identify_clusters(map.df, filter=filter)
#unlink("./test_map.*")
#writeOGR(map.df2[filter, ],"./","test_map",driver="ESRI Shapefile")


identify_centers <- function(map.df, colname, cutoff, dist=NULL, sum.col=NULL, sum.cutoff=0){
    data.df <- map.df@data
    filter <- data.df[,colname] >= cutoff
    clusters.shp <- identify_clusters(map.df, filter=filter, dist=dist)
    clusters.df <- clusters.shp@data
    summary(clusters.df$tot.emp)
    if (!is.null(sum.col)) {
      valid.clusters <- clusters.df %>%
        group_by(cluster.id) %>%
        summarise_(cluster.sum = paste("sum(", sum.col, ")", sep=""), cluster.count="n()") %>%
        filter(cluster.id != 0 & cluster.count > 1 & cluster.sum >= sum.cutoff) %>%
        dplyr::select(cluster.id)
    }
    cluster.df <- inner_join(clusters.df, valid.clusters)
    cluster.df
}



