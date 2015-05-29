# This script defines functions used in calculating travel time costs with employment centers approaches 

## Define a function that identifies TAZs index of clusters with at least two tazs, each with employment 
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


identify_centers <- function(map.df, 
                             colname, 
                             cutoff.val=0,
                             cutoff.percentile=0,
                             dist=NULL, 
                             sum.col=NULL, 
                             sum.cutoff.val=0,
                             sum.cutoff.percentile=0,
                             cutoffs=NULL,
                             ){
   data.df <- map.df@data
   if (!is.null(cutoffs)) {
     if ("cutoff.val" %in% names(cutoffs)) cutoff.val <- cutoffs[["cutoff.val"]]
     if ("cutoff.percentile" %in% names(cutoffs)) cutoff.percentile <- cutoffs[["cutoff.percentile"]]
     if ("sum.cutoff.val" %in% names(cutoffs)) sum.cutoff.val <- cutoffs[["sum.cutoff.val"]]
     if ("sum.cutoff.percentile" %in% names(cutoffs)) sum.cutoff.percentile <- cutoffs[["sum.cutoff.percentile"]]
   }

   cutoff.val2 <- quantile(data.df[,colname], cutoff.percentile)
   cutoff <- max(cutoff.val, cutoff.val2)} 
   
   filter <- data.df[,colname] >= cutoff
   clusters.shp <- identify_clusters(map.df, filter=filter, dist=dist)
   clusters.df <- clusters.shp@data
   summary(clusters.df$tot.emp)
   if (!is.null(sum.col)) {
     sum.cutoff.val2 <- quantile(clusters.df[, sum.col], sum.cutoff.percentile)
     sum.cutoff <- max(sum.cutoff.val, sum.cutoff.val2)
     
     valid.clusters <- clusters.df %>%
       group_by(cluster.id) %>%
       summarise_(cluster.sum = paste("sum(", sum.col, ")", sep=""), cluster.count="n()") %>%
       filter(cluster.id != 0 & cluster.count > 1 & cluster.sum >= sum.cutoff) %>%
       dplyr::select(cluster.id)
   }
   cluster.df <- inner_join(clusters.df, valid.clusters)
   cluster.df
 }

  
# Define a function that calculates minimal travel time
min_tt <- function(centers, tt, trips) {
  # trips is unused in min_tt function
    min_tt <- NULL
    #loop because we need to handle NAs, otherwise tt[, centers] would work
    for (zi in 1:dim(tt)[1]) {
       if (all(is.na(tt[zi, centers]))) {
           print(paste("TAZ", zi, "has no access to the centers", sep=" ")) #there are zones without certain access to centers, eg. busWalk
       }
    
      min_tt[zi] <- min(tt[zi, centers], na.rm=TRUE)
      min_tt[zi] <- ifelse(is.infinite(min_tt[zi]), NA, min_tt[zi])
    }
  min_tt
}


# Define a function that calculates weighted average travel time 
weighted_tt <- function(centers, tt, trips) {
    weighted_avg_tt <- NULL
    for (zi in 1:dim(tt)[1]) {
      trip.sum <- sum(trips[zi, centers], na.rm=TRUE)
      weighted_avg_tt[zi] <- sum(tt[zi, centers]*trips[zi, centers], na.rm=TRUE)/trip.sum
    }
    weighted_avg_tt
}

sum.na.rm <- function(x) sum(x, na.rm=T)

nd.weighted.mean <- function(X, W, dims) {
  apply(X*W, dims, sum.na.rm) / apply(W, dims, sum.na.rm)
}

in.memory <- function(obj.names)
  all(obj.names %in% ls())
