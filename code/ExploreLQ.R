# Load the rhdf5 library
  library(rhdf5)

# load trips omx file
  
source("code/thirdparty/omx.r")
in.file_tripdistribution <- file.path(INPUT_DIR, 'TDM/TripDistribution.omx')
listOMX(in.file_tripdistribution)
  

  # calculate hbw trips 
  hbwhighIncDist <- readMatrixOMX(in.file_tripdistribution, "hbwmidIncDist")
  hbwhighIncDist_rowsum <- rowSums(hbwhighIncDist)

  hbwmidIncDist <- readMatrixOMX(in.file_tripdistribution, "hbwmidIncDist")
  hbwmidIncDist_rowsum <- rowSums(hbwmidIncDist)

  hbwlowIncDist <- readMatrixOMX(in.file_tripdistribution, "hbwlowIncDist")
  hbwlowIncDist_rowsum <- rowSums(hbwlowIncDist)
  
  hbwtrips <- hbwhighIncDist_rowsum + hbwmidIncDist_rowsum + hbwlowIncDist_rowsum
  
  
  # calculate hbs trips 
  hbshighIncDist <- readMatrixOMX(in.file_tripdistribution, "hbsmidIncDist")
  hbshighIncDist_rowsum <- rowSums(hbshighIncDist)
  
  hbsmidIncDist <- readMatrixOMX(in.file_tripdistribution, "hbsmidIncDist")
  hbsmidIncDist_rowsum <- rowSums(hbsmidIncDist)
  
  hbslowIncDist <- readMatrixOMX(in.file_tripdistribution, "hbslowIncDist")
  hbslowIncDist_rowsum <- rowSums(hbslowIncDist)
  
  hbstrips <- hbshighIncDist_rowsum + hbsmidIncDist_rowsum + hbslowIncDist_rowsum
  
  # calculate hbr trips 
  hbrhighIncDist <- readMatrixOMX(in.file_tripdistribution, "hbrmidIncDist")
  hbrhighIncDist_rowsum <- rowSums(hbrhighIncDist)
  
  hbrmidIncDist <- readMatrixOMX(in.file_tripdistribution, "hbrmidIncDist")
  hbrmidIncDist_rowsum <- rowSums(hbrmidIncDist)
  
  hbrlowIncDist <- readMatrixOMX(in.file_tripdistribution, "hbrlowIncDist")
  hbrlowIncDist_rowsum <- rowSums(hbrlowIncDist)
  
  hbrtrips <- hbrhighIncDist_rowsum + hbrmidIncDist_rowsum + hbrlowIncDist_rowsum
  
  # calculate hbo trips 
  hbohighIncDist <- readMatrixOMX(in.file_tripdistribution, "hbomidIncDist")
  hbohighIncDist_rowsum <- rowSums(hbohighIncDist)
  
  hbomidIncDist <- readMatrixOMX(in.file_tripdistribution, "hbomidIncDist")
  hbomidIncDist_rowsum <- rowSums(hbomidIncDist)
  
  hbolowIncDist <- readMatrixOMX(in.file_tripdistribution, "hbolowIncDist")
  hbolowIncDist_rowsum <- rowSums(hbolowIncDist)
  
  hbotrips <- hbohighIncDist_rowsum + hbomidIncDist_rowsum + hbolowIncDist_rowsum
  
  # combine trips 
  TAZ <- c(1:2162)
  trips <- data.frame(TAZ, hbwtrips, hbstrips, hbrtrips,hbotrips) 
  trips[1:10,]
  summary(trips)
  
  require(dplyr)
  trips <-   trips %>% 
      mutate ( tot.trips = hbwtrips + hbstrips + hbrtrips + hbotrips, 
               lq.hbw = (hbwtrips/tot.trips) / (sum(hbwtrips)/sum(tot.trips)),
               lq.hbs = (hbstrips/tot.trips) / (sum(hbstrips)/sum(tot.trips)),
               lq.hbr = (hbrtrips/tot.trips) / (sum(hbrtrips)/sum(tot.trips)),
               lq.hbo = (hbotrips/tot.trips) / (sum(hbotrips)/sum(tot.trips)))
                      
  325/1434
  sum(trips$hbwtrips)/sum(trips$hbwtrips+trips$hbstrips+trips$hbrtrips+trips$hbotrips)
  (325/1434) /(sum(trips$hbwtrips)/sum(trips$hbwtrips+trips$hbstrips+trips$hbrtrips+trips$hbotrips))
  
  483.77049/1434.15153/((sum(trips$hbotrips)/sum(trips$hbwtrips+trips$hbstrips+trips$hbrtrips+trips$hbotrips)))
  
  # density plot 
  plot(density(trips$lq.hbw,na.rm=TRUE), main="HBW Trips Location Quotient")
  
  plot(density(trips$lq.hbs,na.rm=TRUE), main="HBS Trips Location Quotient")
  
  plot(density(trips$lq.hbr,na.rm=TRUE), main="HBR Trips Location Quotient")
  
  plot(density(trips$lq.hbo,na.rm=TRUE), main="HBO Trips Location Quotient")
  
  
  # summary location quotient values by purpose
  summary(trips$lq.hbw)
  
  summary(trips$lq.hbs)
  
  summary(trips$lq.hbr)
  
  summary(trips$lq.hbo)
  
# Identify centers based on trips  
  
  # load required package
  require(maptools)
  require(rgeos)
  require(rgdal)
  
  TAZPoly <- readShapePoly(file.path(INPUT_DIR, "shp/TAZ.shp"),
                           proj4string=CRS("+init=epsg:2913"))
  
  TAZData <- TAZPoly@data
  Area <- TAZData %>% 
    dplyr::select(TAZ=newtaz,Area=f_area) %>%
    mutate(seq=1:n())
  
  # Merge Area and EmpHHoldParkAcre
  AllData <- left_join(Area, trips,  by="TAZ")
  
  # re-order by the original row order (seq), as shp file depends on the order
  AllData <- AllData %>% 
    arrange(seq) 
    
  
  TAZPoly@data <- AllData
  
  # eliminate TAZ with null value
  TAZPloyNoNA <- TAZPoly[!is.na(TAZPoly@data$lq.hbw),]
  summary(TAZPloyNoNA)
  summary(AllData)
  
  # Explore LQ for use in determining center identification threshold 
  # Load required functions
  source("code/cluster/def_functions.R")
  
  # identify hbw tazs of centers based on LQ
  lq.hbwci <- identify_centers(TAZPloyNoNA, "lq.hbw", 1, dist=1.0, sum.col="hbwtrips", sum.cutoff=0.1)
  lq.hbwci <- lq.hbwci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
  
  # identify hbs tazs of centers based on LQ
  lq.hbsci <- identify_centers(TAZPloyNoNA, "lq.hbs", 1, dist=1.0, sum.col="hbstrips", sum.cutoff=0.1)
  lq.hbsci <- lq.hbsci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
  
  # identify hbr tazs of centers based on LQ
  lq.hbrci <- identify_centers(TAZPloyNoNA, "lq.hbr", 1, dist=1.0, sum.col="hbrtrips", sum.cutoff=0.1)
  lq.hbrci <- lq.hbrci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
  
  # identify hbo tazs of centers based on LQ
  lq.hboci <- identify_centers(TAZPloyNoNA, "lq.hbo", 1, dist=1.0, sum.col="hbotrips", sum.cutoff=0.1)
  lq.hboci <- lq.hboci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
  
  if (SAVE.INTERMEDIARIES) {
    intm.file <- file.path(INTERMEDIATE_DIR, "trips.lq.centers.RData")
    save(lq.hbwci, lq.hbsci, lq.hbrci, lq.hboci, file=intm.file)
  }
  
  
  # 
  nrow(lq.hbwci)
  nrow(lq.hbsci)
  nrow(lq.hbrci)
  nrow(lq.hboci)
  
# Map centers 
#-------------
  
  taz <- c(1:2162)
  taz <- data.frame(taz)
  
  colnames(lq.hbwci) <- c("taz","lq.hbwci")
  lq.ci <- merge(taz, lq.hbwci, by="taz", all.x=TRUE)
  lq.ci$lq.hbwci[!is.na(lq.ci$lq.hbwci)] <- 1
  lq.ci$lq.hbwci[is.na(lq.ci$lq.hbwci)] <- 0
  
  
  colnames(lq.hbsci) <- c("taz","lq.hbsci")
  lq.ci <- merge(lq.ci, lq.hbsci, by="taz", all.x=TRUE)
  lq.ci$lq.hbsci[!is.na(lq.ci$lq.hbsci)] <- 1
  lq.ci$lq.hbsci[is.na(lq.ci$lq.hbsci)] <- 0
  
  colnames(lq.hbrci) <- c("taz","lq.hbrci")
  lq.ci <- merge(lq.ci, lq.hbrci, by="taz", all.x=TRUE)
  lq.ci$lq.hbrci[!is.na(lq.ci$lq.hbrci)] <- 1
  lq.ci$lq.hbrci[is.na(lq.ci$lq.hbrci)] <- 0
  
  
  colnames(lq.hboci) <- c("taz","lq.hboci")
  lq.ci <- merge(lq.ci, lq.hboci, by="taz", all.x=TRUE)
  lq.ci$lq.hboci[!is.na(lq.ci$lq.hboci)] <- 1
  lq.ci$lq.hboci[is.na(lq.ci$lq.hboci)] <- 0
  
  # 
  load("data/CommonData/Zi.RData")
  LQPrCi <- c("lq.hbwci", "lq.hbsci", "lq.hbrci", "lq.hboci") 
  
  ci.zipr <- array(0, dim=c(length(Zi),length(LQPrCi)), dimnames = list(Zi,LQPrCi))
  
  ci.zipr[1:10,]
  
  ci.zipr[, "lq.hbwci"] <- lq.ci[,"lq.hbwci"]
  ci.zipr[, "lq.hbsci"] <- lq.ci[,"lq.hbsci"]
  ci.zipr[, "lq.hbrci"] <- lq.ci[,"lq.hbrci"]
  ci.zipr[, "lq.hboci"] <- lq.ci[,"lq.hboci"]
  
  
  # load required libraries
  library(maptools)
  library(RColorBrewer)
  
  #Load mapping data and functions
  #===============================
  
  #::
    
  # read in the taz data shapefile
  TazFile <- file.path(INPUT_DIR, "shp/TAZ.shp")
  TazPoly <- readShapePoly(TazFile)
  
  # extract the attribute data
  TazData <- TazPoly@data
  
  # make an index vector to the taz
  TazIndex <- as.character(TazData$newtaz)
  
  # get the location of the reference zone
  TazCentroids.ZiXy <- coordinates(TazPoly)
  rownames(TazCentroids.ZiXy) <- TazIndex
  
  ## Joe's modify function to plot
  
  # write a function to plot taz values as a choropleth map
  choropleth <- function(geo=TazPoly, data, DataIndex=TazIndex, palette="Blues", breaks,
                         LegendSize=1, PlotRef=TRUE, LegendOffset=c(1,1),
                         LegendTitle=NULL, RefColor="red", LegendText=FALSE, ...){
    DataCut <- cut(data[DataIndex], breaks, include.lowest=TRUE, labels=FALSE)
    ColorPalette <- brewer.pal(length(breaks)-1, palette)
    colors <- ColorPalette[DataCut]
    plot(geo, col=colors, xaxt="n", yaxt="n", border=NA, ...)
    if(PlotRef) points(RefZoneCent[1], RefZoneCent[2], pch=1, col=RefColor, cex=2, lwd=2)
    if(LegendText[1] == FALSE) {LegendText <- paste(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)], sep=" - ")}
    print(LegendText)
    if(LegendSize != 0){
      legend(7404637,611959, legend=LegendText,
             title=LegendTitle, cex=LegendSize, fill=ColorPalette)
    }
    if(PlotRef){
      points(7410262, 543378, pch=1, col=RefColor, cex=2, lwd=2)
      text(7415412, 543378, "Reference Zone", pos=4)
    }
  }
  
  
  # Load saveGraph function 
  source("code/thirdparty/openGraphSaveGraph.R")
  
  graph.path <- file.path(OUTPUT_DIR, "graphics")
  
  ## Map lq.hbwci
  ## -------------------------------------------
  
  # Set up plot layout
  par(mfrow=c(1,1))
  ## map 
  choropleth(TazPoly, ci.zipr[, "lq.hbwci"], TazIndex, "RdYlBu",
             breaks=c(0,0.8,2), LegendSize=0.5, PlotRef=FALSE,
             main="", LegendTitle="Centers", LegendText=c("non-ci", "ci"))
  
  mtext("Centers based on trips lq.hbwci", cex=1)
  
  saveGraph(filename=file.path(graph.path, "Centers_trips_lq_hbwci"), type="pdf")
  
  ## Map lq.hbsci
  ## -------------------------------------------
  
  # Set up plot layout
  par(mfrow=c(1,1))
  ## map 
  choropleth(TazPoly, ci.zipr[, "lq.hbsci"], TazIndex, "RdYlBu",
             breaks=c(0,0.8,2), LegendSize=0.5, PlotRef=FALSE,
             main="", LegendTitle="Centers", LegendText=c("non-ci", "ci"))
  
  mtext("Centers based on trips lq.hbsci", cex=1)
  
  saveGraph(filename=file.path(graph.path, "Centers_trips_lq_hbsci"), type="pdf")
  
  ## Map lq.hbrci
  ## -------------------------------------------
  
  # Set up plot layout
  par(mfrow=c(1,1))
  ## map 
  choropleth(TazPoly, ci.zipr[, "lq.hbrci"], TazIndex, "RdYlBu",
             breaks=c(0,0.8,2), LegendSize=0.5, PlotRef=FALSE,
             main="", LegendTitle="Centers", LegendText=c("non-ci", "ci"))
  
  mtext("Centers based on trips lq.hbrci", cex=1)
  
  saveGraph(filename=file.path(graph.path, "Centers_trips_lq_hbrci"), type="pdf")
  
  ## Map lq.hboci
  ## -------------------------------------------
  
  # Set up plot layout
  par(mfrow=c(1,1))
  ## map 
  choropleth(TazPoly, ci.zipr[, "lq.hboci"], TazIndex, "RdYlBu",
             breaks=c(0,0.8,2), LegendSize=0.5, PlotRef=FALSE,
             main="", LegendTitle="Centers", LegendText=c("non-ci", "ci"))
  
  mtext("Centers based on trips lq.hboci", cex=1)
  
  saveGraph(filename=file.path(graph.path, "Centers_trips_lq_hboci"), type="pdf")
  