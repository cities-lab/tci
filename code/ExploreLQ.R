# Load the rhdf5 library
  library(rhdf5)

# load trips omx file
  
source("code/thirdparty/omx.r")
in.file_tripdistribution <- file.path(INPUT_DIR, 'TDM/TripDistribution.omx')
listOMX(in.file_tripdistribution)
  

  # calculate hbw trips 
  hbwhighIncDist <- readMatrixOMX(in.file_tripdistribution, "hbwmidIncDist")
  hbwhighIncDist_colsum <- colSums(hbwhighIncDist)

  hbwmidIncDist <- readMatrixOMX(in.file_tripdistribution, "hbwmidIncDist")
  hbwmidIncDist_colsum <- colSums(hbwmidIncDist)

  hbwlowIncDist <- readMatrixOMX(in.file_tripdistribution, "hbwlowIncDist")
  hbwlowIncDist_colsum <- colSums(hbwlowIncDist)
  
  hbwtrips <- hbwhighIncDist_colsum + hbwmidIncDist_colsum + hbwlowIncDist_colsum
  
  
  # calculate hbs trips 
  hbshighIncDist <- readMatrixOMX(in.file_tripdistribution, "hbsmidIncDist")
  hbshighIncDist_colsum <- colSums(hbshighIncDist)
  
  hbsmidIncDist <- readMatrixOMX(in.file_tripdistribution, "hbsmidIncDist")
  hbsmidIncDist_colsum <- colSums(hbsmidIncDist)
  
  hbslowIncDist <- readMatrixOMX(in.file_tripdistribution, "hbslowIncDist")
  hbslowIncDist_colsum <- colSums(hbslowIncDist)
  
  hbstrips <- hbshighIncDist_colsum + hbsmidIncDist_colsum + hbslowIncDist_colsum
  
  # calculate hbr trips 
  hbrhighIncDist <- readMatrixOMX(in.file_tripdistribution, "hbrmidIncDist")
  hbrhighIncDist_colsum <- colSums(hbrhighIncDist)
  
  hbrmidIncDist <- readMatrixOMX(in.file_tripdistribution, "hbrmidIncDist")
  hbrmidIncDist_colsum <- colSums(hbrmidIncDist)
  
  hbrlowIncDist <- readMatrixOMX(in.file_tripdistribution, "hbrlowIncDist")
  hbrlowIncDist_colsum <- colSums(hbrlowIncDist)
  
  hbrtrips <- hbrhighIncDist_colsum + hbrmidIncDist_colsum + hbrlowIncDist_colsum
  
  # calculate hbo trips 
  hbohighIncDist <- readMatrixOMX(in.file_tripdistribution, "hbomidIncDist")
  hbohighIncDist_colsum <- colSums(hbohighIncDist)
  
  hbomidIncDist <- readMatrixOMX(in.file_tripdistribution, "hbomidIncDist")
  hbomidIncDist_colsum <- colSums(hbomidIncDist)
  
  hbolowIncDist <- readMatrixOMX(in.file_tripdistribution, "hbolowIncDist")
  hbolowIncDist_colsum <- colSums(hbolowIncDist)
  
  hbotrips <- hbohighIncDist_colsum + hbomidIncDist_colsum + hbolowIncDist_colsum
  
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
  
  # save 
  if (SAVE.INTERMEDIARIES) {
    intm.file <- file.path(INTERMEDIATE_DIR, "trips.lq.centers.RData")
    save(ci.zipr, file=intm.file)
  }
  
  
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
  
  
  
## Use OHAS HBO trips destination taz to identify HBO cecnters
  
  # Load required packages
  require(dplyr)
  require(tidyr)
  
  ## read OHAS activity, household, trip table
  load(file.path(INPUT_DIR, "OHAS_PDX.RData"))
  load(file.path(INPUT_DIR, "Zi.RData"))
  
  activity <- activity %>%
    dplyr::select(sampn, perno, plano, thisaggact, tpurp, mode, trpdur, dtaz) %>%
    arrange(sampn, perno, plano) %>%
    group_by(sampn, perno) %>%
    mutate(maxAct=n(), actNo=1:n())
  
  linked <- activity %>%
    # remove the Change-Of-Mode trips (tpurp==7) in the middle of the day and
    # the first and last 'trip'
    filter(tpurp!=7 & actNo!=1 & actNo != maxAct) %>%
    arrange(sampn, perno, plano) %>%
    group_by(sampn, perno) %>%
    mutate(actNo=1:n(), maxAct=n(),
           lastaggact=lag(thisaggact),
           last_tpurp=lag(tpurp) # last tpurp
    )
  
  # identify home-based trips
  #* hba (home-based all); all home-based trips
  #* hbw (home-based work); work, not including all other activities at work and work related activities
  #* hbs (home-based shopping); both routine and special shopping
  #* hbr (home-based recreation); both outdoor and indoor recreation
  #* hbo (home-based other); other home-based trips
  linked <- mutate(linked, 
                   h2o = ifelse(actNo!=1&(lastaggact=='Home'|lastaggact=='WorkAtHome')&(thisaggact!='Home'&thisaggact!='WorkAtHome'), 1, 0),
                   o2h = ifelse(actNo!=1&(lastaggact!='Home'&lastaggact!='WorkAtHome')&(thisaggact=='Home'|thisaggact=='WorkAtHome'), 1, 0),
                   hba = ifelse(h2o==1|o2h==1, 1, 0))
  
  
  # identify trip purposes 
  linked <- mutate(linked, 
                   hbw = ifelse((h2o==1&tpurp==3)|(o2h==1&last_tpurp==3), 1, 0),
                   hbs = ifelse((h2o==1&(tpurp==13|tpurp==14))|(o2h==1&(last_tpurp==13|last_tpurp==14)), 1, 0),
                   hbr = ifelse((h2o==1&(tpurp==20|tpurp==21))|(o2h==1&(last_tpurp==20|last_tpurp==21)), 1, 0),
                   hbo = ifelse((h2o==1&(tpurp!=3&tpurp!=5&tpurp!=6&tpurp!=13&tpurp!=14&tpurp!=20&tpurp!=21))
                                |(o2h==1&(last_tpurp!=3&tpurp!=5&tpurp!=6&last_tpurp!=13&last_tpurp!=14&last_tpurp!=20&last_tpurp!=21)), 1, 0))
  
  # use tidyr to create a character trip purpose column tpurp.ch
  linked <- linked %>% gather(tpurp.ch, flag, hbw:hbo) %>% filter(flag==1) %>% dplyr::select(-flag)
  
  linked[1:10,]
  
### Calculate hbo destination taz frequency 
  
  linkedhbo <- linked %>% 
    filter(tpurp.ch=="hbo")  
  
  hbo.freq<- linkedhbo %>% 
    group_by(dtaz) %>%
    summarise(frequency=n())
  
  plot(density(hbo.freq$frequency), main="HBO Trips destination TAZ frequency")
  
  # Summary hbo destination taz frequency
  summary(hbo.freq$frequency)
  nrow(hbo.freq)
  
  hbo.freq.freq<- hbo.freq %>% 
    group_by(frequency) %>%
    summarise(freq.freq=n())
  hbo.freq.freq[11:34,]
  hbo.freq.freq <- as.data.frame(hbo.freq.freq)
  # 
  taz.hbo.freq <- merge(TAZ, hbo.freq, by.x="TAZ", by.y="dtaz", all.x=TRUE)
  
  
  
### Calculate hbw destination taz frequency 
  
  linkedhbw <- linked %>% 
    filter(tpurp.ch=="hbw")  
  
  hbw.freq<- linkedhbw %>% 
    group_by(dtaz) %>%
    summarise(frequency=n())
  
  plot(density(hbw.freq$frequency), main="HBW Trips destination TAZ frequency")
  
  # Summary hbw destination taz frequency
  summary(hbw.freq$frequency)
  nrow(hbw.freq)
  
  hbw.freq.freq<- hbw.freq %>% 
    group_by(frequency) %>%
    summarise(freq.frqq=n())
  
  # 
  taz.hbw.freq <- merge(TAZ, hbw.freq, by.x="TAZ", by.y="dtaz", all.x=TRUE)
  
  
### Calculate hbs destination taz frequency 
  
  linkedhbs <- linked %>% 
    filter(tpurp.ch=="hbs")  
  
  hbs.freq<- linkedhbs %>% 
    group_by(dtaz) %>%
    summarise(frequency=n())
  
  plot(density(hbs.freq$frequency), main="HBS Trips destination TAZ frequency")

  # Summary hbs destination taz frequency
  summary(hbs.freq$frequency)
  nrow(hbs.freq)
  
  hbs.freq.freq<- hbs.freq %>% 
    group_by(frequency) %>%
    summarise(freq.frqq=n())
  
  # 
  taz.hbs.freq <- merge(TAZ, hbs.freq, by.x="TAZ", by.y="dtaz", all.x=TRUE)
  
### Calculate hbr destination taz frequency 
  
  linkedhbr <- linked %>% 
    filter(tpurp.ch=="hbr")  
  
  hbr.freq<- linkedhbr %>% 
    group_by(dtaz) %>%
    summarise(frequency=n())
  
  plot(density(hbr.freq$frequency), main="HBR Trips destination TAZ frequency")
  
  # Summary hbr destination taz frequency
  summary(hbr.freq$frequency)
  nrow(hbr.freq)
  
  hbr.freq.freq<- hbr.freq %>% 
    group_by(frequency) %>%
    summarise(freq.freq=n())
  
  # 
  taz.hbr.freq <- merge(TAZ, hbr.freq, by.x="TAZ", by.y="dtaz", all.x=TRUE)
  
  
  ## Map the results 
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
  
  # write a function to plot taz values as a choropleth map
  choropleth <- function(geo=TazPoly, data, DataIndex=TazIndex, palette="Blues", breaks,
                         LegendSize=1, PlotRef=TRUE, LegendOffset=c(1,1),
                         LegendTitle=NULL, RefColor="red", ...){
    DataCut <- cut(data[DataIndex], breaks, include.lowest=TRUE, labels=FALSE)
    ColorPalette <- brewer.pal(length(breaks)-1, palette)
    colors <- ColorPalette[DataCut]
    plot(geo, col=colors, xaxt="n", yaxt="n", border=NA, ...)
    if(PlotRef) points(RefZoneCent[1], RefZoneCent[2], pch=1, col=RefColor, cex=2, lwd=2)
    LegendText <- paste(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)], sep=" - ")
    if(LegendSize != 0){
      legend(7404637,611959, legend=LegendText,
             title=LegendTitle, cex=LegendSize, fill=ColorPalette)
    }
    if(PlotRef){
      points(7410262, 543378, pch=1, col=RefColor, cex=2, lwd=2)
      text(7415412, 543378, "Reference Zone", pos=4)
    }
  }
  
  
  # Names 
  load(file.path(INPUT_DIR, "Zi.RData"))
  
## Map hbo trips destination TAZ frequency
  new.taz.hbo.freq <- taz.hbo.freq$frequency
  names(new.taz.hbo.freq) <- Zi
  
  new.taz.hbo.freq[is.na(new.taz.hbo.freq)] <- 0
  
  # Set up plot layout
  par(mfrow=c(1,1))
  
  ## map 
  choropleth(TazPoly, new.taz.hbo.freq, TazIndex, "RdYlBu",
             breaks=c(0,0.01,1,3,7,16,41), LegendSize=0.2, PlotRef=FALSE,
             main="", LegendTitle="Centers")
  
  mtext("OHAS hbo trips destination TAZ frequency", cex=1)
  
  saveGraph(filename=file.path(graph.path, "Centers_trips_OHAS_hbo_DTAZ"), type="pdf")
  
#### Map hbw trips destination TAZ frequency
  new.taz.hbw.freq <- taz.hbw.freq$frequency
  names(new.taz.hbw.freq) <- Zi
  
  new.taz.hbw.freq[is.na(new.taz.hbw.freq)] <- 0
  
  # Set up plot layout
  par(mfrow=c(1,1))
  
  ## map 
  choropleth(TazPoly, new.taz.hbw.freq, TazIndex, "RdYlBu",
             breaks=c(0,0.01,1,2,3,4,8), LegendSize=0.2, PlotRef=FALSE,
             main="", LegendTitle="Centers")
  
  mtext("OHAS hbw trips destination TAZ frequency", cex=1)
  
  saveGraph(filename=file.path(graph.path, "Centers_trips_OHAS_hbw_DTAZ"), type="pdf")
  
#### Map hbs trips destination TAZ frequency
  new.taz.hbs.freq <- taz.hbs.freq$frequency
  names(new.taz.hbs.freq) <- Zi
  
  new.taz.hbs.freq[is.na(new.taz.hbs.freq)] <- 0
  
  # Set up plot layout
  par(mfrow=c(1,1))
  
  ## map 
  choropleth(TazPoly, new.taz.hbs.freq, TazIndex, "RdYlBu",
             breaks=c(0,0.01,1,3,6,8,23), LegendSize=0.2, PlotRef=FALSE,
             main="", LegendTitle="Centers")
  
  mtext("OHAS hbs trips destination TAZ frequency", cex=1)
  
  saveGraph(filename=file.path(graph.path, "Centers_trips_OHAS_hbs_DTAZ"), type="pdf")
  
#### Map hbr trips destination TAZ frequency
  new.taz.hbr.freq <- taz.hbr.freq$frequency
  names(new.taz.hbr.freq) <- Zi
  
  new.taz.hbr.freq[is.na(new.taz.hbr.freq)] <- 0
  
  # Set up plot layout
  par(mfrow=c(1,1))
  
  ## map 
  choropleth(TazPoly, new.taz.hbr.freq, TazIndex, "RdYlBu",
             breaks=c(0,0.01,1,3,7,11,20), LegendSize=0.2, PlotRef=FALSE,
             main="", LegendTitle="Centers")
  
  mtext("OHAS hbr trips destination TAZ frequency", cex=1)
  
  saveGraph(filename=file.path(graph.path, "Centers_trips_OHAS_hbr_DTAZ"), type="pdf")
  
  
  