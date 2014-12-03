## This script identifies TAZs index of clusters with at least two tazs, each with employment density above 
##  D, that together with at least E total employment or sizeterms 

# load required libraries
require(maptools)
require(rgeos)
require(rgdal)


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
    valid.clusters <- filter(summarise(group_by(clusters.df, cluster.id),
                                       cluster.sum = sum(clusters.df[, sum.col]), cluster.count=n()),
                             cluster.id != 0 & cluster.count > 1 & cluster.sum >= sum.cutoff)
    }
    cluster.df <- filter(clusters.df, cluster.id %in% valid.clusters$cluster.id)
    cluster.df
}

##setwd("/workspace/TCI/data")
## set workplace and file directories 
#setwd("C:\\Users\\huajie\\Desktop\\CenTTime")
#
##TAZ shp file with employment and sizeterms densities
#map.df <- readShapePoly("gisserver/tazden/tazden.shp", 
#                        proj4string=CRS("+proj=lcc +lat_1=46 +lat_2=44.33333333333334 +lat_0=43.66666666666666 +lon_0=-120.5 +x_0=2500000.0001424 +y_0=0 +ellps=GRS80 +to_meter=0.3048 +no_defs"))
#data.df <- map.df@data
#
### identify hbw clusters 
#  data.df$TotEmpDen[is.na(data.df$TotEmpDen)] <- 0
#  hbwfilter <- data.df$TotEmpDen >= 2500
#  map.df@data <- data.df
#  #use gWithinDistance instead of gTouches to work around precision issues in geom data
#  hbwcluster <- identify_clusters(map.df, filter=hbwfilter, dist=1.0)
#  
#  # save shape file 
#  unlink("gisserver/centers/hbwclusters.*")
#  writeOGR(hbwcluster[filter, ],"C:/Users/huajie/Desktop/CenTTime/gisserver/centers","hbwclusters",driver="ESRI Shapefile")
#  save(hbwcluster, file="CenRdata/hbwcluster.RData")
#
#  # select TAZs index of clusters with at least two tazs, each with employment density above 
#  # 2500, that together with at least 1000 total employment
#  tazclus <- subset(hbwcluster@data, select=c(newtaz,TotEmp, cluster.id))
#
#  # calculate clusters including how many TAZs 
#  clusnumb <- as.data.frame(table(tazclus[,"cluster.id"]),stringsAsFactors=FALSE)
#  colnames(clusnumb) <- c("cluster.id", "numb")
#  tazclusnumb <- merge(tazclus,clusnumb, by="cluster.id",  all.x=TRUE )
#
#  # aggregate employment by cluster.id 
#  aggtotemp <- aggregate(tazclusnumb$TotEmp, by=list(tazclusnumb$cluster.id), FUN=sum, na.rm=TRUE)
#  colnames(aggtotemp) <- c("cluster.id", "aggtotemp")
#  tazclusnumbagg <- merge(tazclusnumb, aggtotemp, by ="cluster.id", all.x=TRUE)
#
#  # select TAZs index 
#  hbwci <- subset(tazclusnumbagg, tazclusnumbagg$cluster.id!=0&tazclusnumbagg$numb!=1&tazclusnumbagg$aggtotemp >= 1000, select=c(newtaz))
#  hbwci <- hbwci[order(hbwci$newtaz),] # sort TAZs index ascending 
#  save(hbwci, file="CenRdata/hbwci.RData")
#
#
### identify hbs clusters 
#  data.df$STDenHBS[is.na(data.df$STDenHBS)] <- 0
#  hbsfilter <- data.df$STDenHBS >= 102
#  map.df@data <- data.df
#  #use gWithinDistance instead of gTouches to work around precision issues in geom data
#  hbscluster <- identify_clusters(map.df, filter=hbsfilter, dist=1.0)
#
#  # save shape file 
#  unlink("gisserver/centers/hbsclusters.*")
#  writeOGR(hbscluster[filter, ],"C:/Users/huajie/Desktop/CenTTime/gisserver/centers","hbsclusters",driver="ESRI Shapefile")
#  save(hbscluster, file="CenRdata/hbscluster.RData")
#
#  # select TAZs index of clusters with at least two tazs, each with employment density above 
#  # 2500, that together with at least 1000 total employment
#  tazclus <- subset(hbscluster@data, select=c(newtaz,STHBS, cluster.id))
#
#  # calculate clusters including how many TAZs 
#  clusnumb <- as.data.frame(table(tazclus[,"cluster.id"]),stringsAsFactors=FALSE)
#  colnames(clusnumb) <- c("cluster.id", "numb")
#  tazclusnumb <- merge(tazclus,clusnumb, by="cluster.id",  all.x=TRUE )
#
#  # aggregate employment by cluster.id 
#  aggtotsizeterms <- aggregate(tazclusnumb$STHBS, by=list(tazclusnumb$cluster.id), FUN=sum, na.rm=TRUE)
#  colnames(aggtotsizeterms) <- c("cluster.id", "aggsizeterms")
#  tazclusnumbagg <- merge(tazclusnumb, aggtotsizeterms, by ="cluster.id", all.x=TRUE)
#
#
#  # select TAZs index 
#  hbsci <- subset(tazclusnumbagg, tazclusnumbagg$cluster.id!=0&tazclusnumbagg$numb!=1&tazclusnumbagg$aggsizeterms >= 1000, select=c(newtaz))
#  hbsci <- hbsci[order(hbsci$newtaz),] # sort TAZs index ascending 
#  save(hbsci, file="CenRdata/hbsci.RData")
#
#
### identify hbr clusters 
#  data.df$STDenHBR[is.na(data.df$STDenHBR)] <- 0
#  hbrfilter <- data.df$STDenHBR >= 1763
#  map.df@data <- data.df
#  #use gWithinDistance instead of gTouches to work around precision issues in geom data
#  hbrcluster <- identify_clusters(map.df, filter=hbrfilter, dist=1.0)
#
#  # save shape file 
#  unlink("gisserver/centers/hbrclusters.*")
#  writeOGR(hbrcluster[filter, ],"C:/Users/huajie/Desktop/CenTTime/gisserver/centers","hbrclusters",driver="ESRI Shapefile")
#  save(hbrcluster, file="CenRdata/hbrcluster.RData")
#
#  # select TAZs index of clusters with at least two tazs, each with employment density above 
#  # 2500, that together with at least 1000 total employment
#  tazclus <- subset(hbrcluster@data, select=c(newtaz,STHBR, cluster.id))
#
#  # calculate clusters including how many TAZs 
#  clusnumb <- as.data.frame(table(tazclus[,"cluster.id"]),stringsAsFactors=FALSE)
#  colnames(clusnumb) <- c("cluster.id", "numb")
#  tazclusnumb <- merge(tazclus,clusnumb, by="cluster.id",  all.x=TRUE )
#
#  # aggregate employment by cluster.id 
#  aggtotsizeterms <- aggregate(tazclusnumb$STHBR, by=list(tazclusnumb$cluster.id), FUN=sum, na.rm=TRUE)
#  colnames(aggtotsizeterms) <- c("cluster.id", "aggsizeterms")
#  tazclusnumbagg <- merge(tazclusnumb, aggtotsizeterms, by ="cluster.id", all.x=TRUE)
#
#
#  # select TAZs index 
#  hbrci <- subset(tazclusnumbagg, tazclusnumbagg$cluster.id!=0&tazclusnumbagg$numb!=1&tazclusnumbagg$aggsizeterms >= 1000, select=c(newtaz))
#  hbrci <- hbrci[order(hbrci$newtaz),] # sort TAZs index ascending 
#  save(hbrci, file="CenRdata/hbrci.RData")
#
#
### identify hbo clusters 
#  data.df$STDenHBO[is.na(data.df$STDenHBO)] <- 0
#  hbofilter <- data.df$STDenHBO >= 495
#  map.df@data <- data.df
#  #use gWithinDistance instead of gTouches to work around precision issues in geom data
#  hbocluster <- identify_clusters(map.df, filter=hbofilter, dist=1.0)
#
#  # save shape file 
#  unlink("gisserver/centers/hboclusters.*")
#  writeOGR(hbocluster[filter, ],"C:/Users/huajie/Desktop/CenTTime/gisserver/centers","hboclusters",driver="ESRI Shapefile")
#  save(hbocluster, file="CenRdata/hbocluster.RData")
#
#  # select TAZs index of clusters with at least two tazs, each with employment density above 
#  # 2500, that together with at least 1000 total employment
#  tazclus <- subset(hbocluster@data, select=c(newtaz,STHBO, cluster.id))
#
#  # calculate clusters including how many TAZs 
#  clusnumb <- as.data.frame(table(tazclus[,"cluster.id"]),stringsAsFactors=FALSE)
#  colnames(clusnumb) <- c("cluster.id", "numb")
#  tazclusnumb <- merge(tazclus,clusnumb, by="cluster.id",  all.x=TRUE )
#
#  # aggregate employment by cluster.id 
#  aggtotsizeterms <- aggregate(tazclusnumb$STHBO, by=list(tazclusnumb$cluster.id), FUN=sum, na.rm=TRUE)
#  colnames(aggtotsizeterms) <- c("cluster.id", "aggsizeterms")
#  tazclusnumbagg <- merge(tazclusnumb, aggtotsizeterms, by ="cluster.id", all.x=TRUE)
#
#
#  # select TAZs index 
#  hboci <- subset(tazclusnumbagg, tazclusnumbagg$cluster.id!=0&tazclusnumbagg$numb!=1&tazclusnumbagg$aggsizeterms >= 1000, select=c(newtaz))
#  hboci <- hboci[order(hboci$newtaz),] # sort TAZs index ascending 
#  save(hboci, file="CenRdata/hboci.RData")




