require(maptools)
require(rgeos)
require(rgdal)
require(dplyr)
require(lazyeval)
require(ggmap)

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
                             cutoffs=NULL
){
  data.df <- map.df@data
  if (!is.null(cutoffs)) {
    if ("cutoff.val" %in% names(cutoffs)) cutoff.val <- cutoffs[["cutoff.val"]]
    if ("cutoff.percentile" %in% names(cutoffs)) cutoff.percentile <- cutoffs[["cutoff.percentile"]]
    if ("sum.cutoff.val" %in% names(cutoffs)) sum.cutoff.val <- cutoffs[["sum.cutoff.val"]]
    if ("sum.cutoff.percentile" %in% names(cutoffs)) sum.cutoff.percentile <- cutoffs[["sum.cutoff.percentile"]]
  }
  
  cutoff.val2 <- quantile(data.df[,colname], cutoff.percentile)
  cutoff <- max(cutoff.val, cutoff.val2)
  print(cutoff)
  
  filter <- data.df[,colname] >= cutoff
  clusters.shp <- identify_clusters(map.df, filter=filter, dist=dist)
  clusters.df <- clusters.shp@data
  summary(clusters.df$tot.emp)
  if (!is.null(sum.col)) {
    
    clusters.df.sum <- clusters.df %>% 
      group_by(cluster.id) %>%
      summarise_(cluster.sum = paste("sum(", sum.col, ")", sep=""), cluster.count="n()") %>%
      filter(cluster.id!=0 & cluster.count > 1)  %>%
      arrange(cluster.sum,cluster.count)
    
    sum.cutoff.val2 <- quantile(clusters.df.sum$cluster.sum, sum.cutoff.percentile)
    sum.cutoff <- max(sum.cutoff.val, sum.cutoff.val2)
    print(sum.cutoff)
    
    valid.clusters <- clusters.df.sum %>%
      filter(cluster.sum >= sum.cutoff) %>%
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

in.memory <- function(obj.names) {
  print("use exists() instead.")
  all(obj.names %in% ls())
}

# overlay x,y coordinate (longitude, lattitude) with a polygon shapefile (shpfile) to get polygon id (id_name)
get_xy_polyid <- function(xy.df, shpfile, id_name, xy.epsg='4326', shpfile.epsg='2913') {
  spdf = SpatialPointsDataFrame(xy.df[, c('x', 'y')], 
                                xy.df, 
                                proj4string=CRS(paste0("+init=epsg:", xy.epsg)))
  shpfile.CRS <- CRS(paste0("+init=epsg:", shpfile.epsg))
  spdf.proj <- spTransform(spdf, shpfile.CRS)
  
  # spatial join with the shp polygon to get polygon id (id_name)
  TAZPoly <- readShapePoly(shpfile,
                           proj4string=shpfile.CRS)
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

# identify trip purposes for OHAS dataset

identifyTripPurpose <- function (df) {
  
  workLabels = c("Work", "Work-related")
  othLabels  = c("Meals", "Personal services", "Medical care", "Professional services", "Household or personal business", 
                 "Household maintenance", "Household obligations", "Pick-Up/Drop-Off passengers", "Visiting", "Culture",
                 "Religion/Civil Services", "Civic", "Volunteer work",  "Hobbies", "Exercise/Athletics", 
                 "Rest and relaxation", "Spectator athletic events", "Incidental trip", "Tag along trip")
  shopLabels = c("Shopping (general)", "Shopping (major)")
  recLabels  = c("Casual entertaining", "Formal entertaining", "Amusements (at-home)", "Amusements (out-of-home)")
  schLabels  = c("School")
  
  df$TripPurpose=ifelse(df$ACT1.f %in% workLabels & df$LastHOME==1 & df$HOME!=1,"HBW","NA")
  df$TripPurpose=ifelse(df$ACT1.f %in% othLabels  & df$LastHOME==1 & df$HOME!=1,"HBO",df$TripPurpose)
  df$TripPurpose=ifelse(df$ACT1.f %in% shopLabels & df$LastHOME==1 & df$HOME!=1,"HBShp",df$TripPurpose)
  df$TripPurpose=ifelse(df$ACT1.f %in% recLabels  & df$LastHOME==1 & df$HOME!=1,"HBRec",df$TripPurpose)
  df$TripPurpose=ifelse(df$ACT1.f %in% schLabels  & df$LastHOME==1 & df$HOME!=1,"HBSch",df$TripPurpose)
  
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% workLabels & df$LastHOME!=1,"HBW",df$TripPurpose)
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% othLabels & df$LastHOME!=1,"HBO",df$TripPurpose)
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% shopLabels & df$LastHOME!=1,"HBShp",df$TripPurpose)
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% recLabels & df$LastHOME!=1,"HBRec",df$TripPurpose)
  df$TripPurpose=ifelse(df$HOME==1 & df$LastACT1.f %in% schLabels & df$LastHOME!=1,"HBSch",df$TripPurpose)
  
  return(df)
}

# Density plots
plot_density <- function(plot.data=NULL, 
                         x=NULL, xlab="", xlim.max=NULL, 
                         group=NULL, legend.title="",
                         unit.name="dollars", 
                         title="",
                         bw=F) {
  
  default.xlim.max <- c(dollars=150, minutes=450)
  xlim.max <- ifelse(is.null(xlim.max), default.xlim.max[[unit.name]], xlim.max)
  
  xaxis.label <- paste(xlab, " (", unit.name, ")", sep="")
  p <- ggplot(data=plot.data, aes_string(x = x, colour=group, group=group)) +
    geom_density(fill=NA, size=1) + labs(x=xaxis.label) + xlim(0, xlim.max) +
    scale_colour_grey(name = legend.title) +
    ggtitle(title) +
    theme(plot.title = element_text(face="bold", size=12, vjust=1)) 
  if (bw) p <- p + theme_bw()
  p
}

plot_density.linetype <- function(plot.data=NULL, 
                         x=NULL, xlab="", xlim.max=NULL, 
                         group=NULL, legend.title="",
                         unit.name="dollars", 
                         title="",
                         bw=F) {
  
  default.xlim.max <- c(dollars=150, minutes=450)
  xlim.max <- ifelse(is.null(xlim.max), default.xlim.max[[unit.name]], xlim.max)
  
  xaxis.label <- paste(xlab, " (", unit.name, ")", sep="")
  
  p <- ggplot(data=plot.data, aes_string(x = x, group=group, linetype=group)) +
    geom_density(size=1, color='grey20') + labs(x=xaxis.label) + xlim(0, xlim.max) +
    scale_linetype(name=legend.title #, values=c("Low Inc"="solid", "Mid Inc"="dashed", "High Inc"="dotted")
                   ) +
    ggtitle(title) +
    theme(plot.title = element_text(face="bold", size=12, vjust=1)) 
  if (bw) p <- p + theme_bw()
  p
}

# Box plots
plot_boxplot <- function(plot.data=NULL, 
                         x=NULL, xlab="",
                         y=NULL, ylab="", ylim.max=NULL,
                         fill=NULL, legend.title="",
                         unit.name="dollars", 
                         title="",
                         bw=F) {
  
  default.ylim.max <- c(dollars=100, minutes=250)
  ylim.max <- ifelse(is.null(ylim.max), default.ylim.max[[unit.name]], ylim.max)
  
  yaxis.label=paste(ylab, " (", unit.name, ")", sep="")
  
  p <- ggplot(data=plot.data, aes_string(x=x, y=y, fill=fill)) + 
    geom_boxplot()  + labs(y=yaxis.label) + xlab(xlab) + 
    ylim(0, ylim.max)  + scale_fill_grey(name = legend.title) + 
    ggtitle(title) +
    theme(plot.title = element_text(face="bold", size=12, vjust=1)) 
  if (bw) p <- p + theme_bw()
  p
}


# Line plots
plot_line <- function (plot.data=NULL, 
                       x=NULL, xlab="",
                       y=NULL, ylab="", ylim.max=NULL,
                       group=NULL,
                       unit.name="dollars", 
                       title="",
                       bw=F) {
  
  default.ylim.max <- c(dollars=60, minutes=160)
  ylim.max <- ifelse(is.null(ylim.max), default.ylim.max[[unit.name]], ylim.max)
  
  yaxis.label=paste(ylab, " (", unit.name, ")", sep="")
  
  p <- ggplot(data=plot.data, aes_string(x = x, y = y, colour=group, group=group)) +
    geom_line(size=1) + labs(x=xlab) + labs(y=yaxis.label) + ylim(0, ylim.max) +
    ggtitle(title) +
    theme(plot.title = element_text(face="bold", size=12, vjust=1)) + scale_color_grey() 
  if (bw) p <- p + theme_bw()
  p
}

# ggplot() +
#   geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
#                color = NA, size = 0.1) +
#   scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), limits = c(0, 600), 
#                        name = "Travel Costs\n(Minutes)", na.value = "grey80") +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   theme_nothing(legend = TRUE)


# Maps
plot_map <- function(plot.data=NULL,
                     name="",
                     group=NULL,
                     fill=NULL,
                     unit.name="dollars", 
                     limits.max=NULL,
                     filter.col=NULL,
                     filter.min=3
                     ) {
  default.limits.max <- c(dollars=100, minutes=410)
  limits.max <- ifelse(is.null(limits.max), default.limits.max[[unit.name]], limits.max)
  
  name.label <- paste(name, " (", unit.name, ")", sep="")
  
  # truncate the plot.data to be within limits.max
  plot.data_x <- plot.data
  if (any(is.na(plot.data_x[, fill]))){
    plot.data_x[is.na(plot.data_x[, fill]), fill] <- -9999
  }
  plot.data_x[plot.data_x[, fill] > limits.max, fill] <- limits.max
  plot.data_x[plot.data_x[, fill] == -9999, fill] <- NA
  
  if (!is.null(filter.col)) {
    plot.data_x[plot.data_x[, filter.col] < filter.min, 'value'] <- NA
  }
  
  p <- ggplot() +
    geom_polygon(data = plot.data_x, aes_string(x = "long", y = "lat", group = group, fill = fill), 
                 color = "grey50", size = 0.1) +
    scale_fill_gradient(low="grey80", high="grey10",
                        breaks = pretty_breaks(n = 7),
                        name = name.label, na.value = "white") +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_nothing(legend = TRUE)
  p
}

clean.up <- function(preserve.list=NULL) {
  var.list <- ls()
  rm.list <- ifelse(is.null(preserve.list), var.list, var.list[!(var.list %in% preserve.list)])
  rm(list=rm.list)
  rm(var.list, rm.list)
}
