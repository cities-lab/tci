## This script aggregates LEHD employment from block level into TAZ level,
## calculates employment and sizeterms density for each TAZ.
## The calculation results are added to shapefile, then identify clusters
  # read four counties blockgroup level LEHD data, with a tazindex column identifying
  # the taz id the block group is in 
    FourCouLEHD <- read.csv("CenRdata/LEHD/FourCouLEHD.csv", header=TRUE)
    
  # load required package
    require(dplyr)
  # rename column name ##TODO: use rename() instead
    FourCouLEHD <- dplyr::rename(FourCouLEHD, TAZ = tazindex)
  # get total employment by group 
    emp.by.taz <- summarise(group_by(FourCouLEHD, TAZ),
                          tot.emp=sum(C000,  na.rm=TRUE),
                          gvt.emp=sum(CNS20, na.rm=TRUE),
                          ret.emp=sum(CNS07, CNS18, na.rm=TRUE),
                          fin.emp=sum(CNS10, CNS11, CNS13, na.rm=TRUE),
                          svc.emp=sum(CNS09, CNS12, CNS14, CNS15, CNS16, CNS17, CNS19, na.rm=TRUE)
                         ) 

  # get number of Household in each taz

    HHoldTable <- read.csv("CenRdata/csv/2010_hia.csv", header=TRUE)
    HHoldTable$HHold <- rowSums(HHoldTable[,2:64]) 
    HHold <- subset(HHoldTable, select=c(taz, HHold))
    HHold$HHold <- round(HHold$HHold)
    colnames(HHold) <- c("TAZ", "HHold")

  # get park acres in each taz
    ParkAcres <- read.csv("CenRdata/csv/ParkAc.csv", header=TRUE)
  
  # Merge Employment and HHold, ParkAcres, area of TAZ
  
    # merge Employment and HHOld 
      EmpHHold <- merge(emp.by.taz, HHold, by="TAZ")
    
    # merge ParkAcre 
      EmpHHoldParkAcre <- merge(EmpHHold, ParkAcres, by="TAZ")

    # load required package
      require(maptools)
      require(rgeos)
      require(rgdal)

      TAZPoly <- readShapePoly("gisserver/taz/TAZ.shp",
                               proj4string=CRS("+init=epsg:2913"))
                               # proj4string=CRS("+proj=lcc +lat_1=46 +lat_2=44.33333333333334 
                               # +lat_0=43.66666666666666 +lon_0=-120.5 +x_0=2500000.0001424 +y_0=0 +ellps=GRS80 
                               # +to_meter=0.3048 +no_defs"))
    
      TAZData <- TAZPoly@data
      Area <- subset(TAZData, select=c(newtaz,f_area))
      colnames(Area)<- c("TAZ", "Area")

              
      # Merge Area and EmpHHoldParkAcre
      Area$seq <- 1:nrow(Area)
      TotalData <- merge(Area,EmpHHoldParkAcre,  by="TAZ",all.x=TRUE)
     
      # re-order and then remove seq
      TotalData <- TotalData[with(TotalData, order(seq)), -c(which(names(Area)=='seq'))]
      row.names(TotalData) <- NULL

      SQFT.KM2 <- 0.092903/1000000 # define a constant for converting sqft to sqkm
    
      TotalData <- mutate(TotalData,
                          non.ret = tot.emp - ret.emp,
                          non.retsvcgvt = non.ret - svc.emp - gvt.emp,
                          st.hbs = ret.emp + .008396 * non.ret + .022126*HHold,
                          st.hbr = tot.emp + 1.278 * HHold + 4.6833 *ParkAcres,
                          st.hbo = 0.2393 * HHold + ret.emp + 0.6419 * svc.emp + 0.6109 * gvt.emp + 0.06802 * non.retsvcgvt,
                          totemp.den = tot.emp / (Area * SQFT.KM2),
                          st.hbs.den = st.hbs / (Area * SQFT.KM2),
                          st.hbr.den = st.hbr / (Area * SQFT.KM2),
                          st.hbo.den = st.hbo / (Area * SQFT.KM2))

    TAZPoly@data <- dplyr::select(TotalData, TAZ, Area, tot.emp, totemp.den, st.hbs, st.hbs.den, st.hbr,st.hbr.den, st.hbo,st.hbo.den)
    out.dir <- "gisserver/tazden/"
    out.shp <- "tazden.*"
    unlink(paste(out.dir, out.shp, sep=""))
    writeOGR(TAZPoly, "gisserver/tazden", "tazden",driver="ESRI Shapefile") #need to save?
    
    # eliminate TAZ with null value
    TAZPloyNoNA <- TAZPoly[!is.na(TAZPoly@data$totemp.den),]
    
    # load identify_centers function
    source("/home/yanghuajie/tci/code/cluster/3_identify_clusters.R")
    
    
    # identify hbw tazs of centers
    hbwci <- identify_centers(TAZPloyNoNA, "totemp.den", 2500, dist=1.0, sum.col="tot.emp", sum.cutoff=1000)
    hbwci <- dplyr::select (hbwci, TAZ)
    hbwci <- arrange(hbwci, TAZ)
    save(hbwci, file="CenRdata/hbwci.RData")
    
    # identify hbs tazs of centers 
    hbsci <- identify_centers(TAZPloyNoNA, "st.hbs.den", 102, dist=1.0, sum.col="st.hbs", sum.cutoff=1000)
    hbsci <- dplyr::select (hbsci, TAZ)
    hbsci <- arrange(hbsci, TAZ)
    save(hbsci, file="CenRdata/hbwci.RData")
    
    # identify hbr tazs of centers 
    hbrci <- identify_centers(TAZPloyNoNA, "st.hbr.den", 1763, dist=1.0, sum.col="st.hbr", sum.cutoff=1000)
    hbrci <- dplyr::select (hbrci, TAZ)
    hbrci <- arrange(hbrci, TAZ)
    save(hbrci, file="CenRdata/hbrci.RData")
    
    # identify hbo tazs of centers 
    hboci <- identify_centers(TAZPloyNoNA, "st.hbo.den", 495, dist=1.0, sum.col="st.hbo", sum.cutoff=1000)
    hboci <- dplyr::select (hboci, TAZ)
    hboci <- arrange(hboci, TAZ)
    save(hboci, file="CenRdata/hboci.RData")
    
