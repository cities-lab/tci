# Load required packages
  require(dplyr)
  require(tidyr)
  require(maptools)
  require(tools)
  require(rhdf5)
  require(data.table)
  source("code/thirdparty/omx.r")

# Settings    
  setwd("~/tci") ## now set in code/settings.R
  method.name <- "cluster"
  project.name <- "Corvallis"
  year <- ""
  unit.name <- 'minutes'  
  #unit.name <- 'dolloars'
  
  # scenario.names <- c('2010')
  #scenario.names <- c('2010', '2030Preferred', '2030Preferred_Scen1')
  scenario.names <- c()
  
#   #default settings
#   source("code/settings.R")
#   #  Inc levels
#   #Ic <- c("lowInc", "midInc", "highInc")
#   # Trip Purpose
#   #Pr <- c("hbw", "hbs", "hbr", "hbo")
#   # Define the travel modes
#   #Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")
#   # Define time period 
#   Tp <- c("peak", "offPeak")
#   
#   # modes used for distance skims
#   Md.tdist <- c("auto", "auto", "auto", "tran", "tran", "bike", "walk")
#   names(Md.tdist) <- Md
  
  prepare_data <- function() {
    
    ## TAZ shp file
    path.shp <- file.path(INPUT_DIR, "TAZ")
    dir.create(file.path(OUTPUT_DIR, 'shp'), recursive = TRUE, showWarnings = FALSE)
    files.shp <- Sys.glob(file.path(path.shp, "TAZ*"))
    for (file.from in files.shp) {
      file.to <- file.path(OUTPUT_DIR, 'shp', paste0('TAZ.', tolower(file_ext(file.from))))
      file.copy(file.from, file.to)
    }
    TAZ.id_name <- "TAZ"
    TAZPoly <- readShapePoly(file.path(OUTPUT_DIR, 'shp/TAZ.shp'),
                             proj4string=CRS("+init=epsg:2992"))
    
    # init Zi
    Zi <- as.character(sort(TAZPoly@data[, TAZ.id_name]))
    
    # Get both external and internal TAZs 
    # file.path(INPUT_DIR, paste(substr(scenario.name, 1, 4), "Dist Mat.csv", sep=" "))
    
    tripDist.df <- read.csv(file.path(INPUT_DIR, paste(substr(scenario.name, 1, 4), "Dist Mat.csv", sep=" ")), 
                            sep=",", header=FALSE, skip=5)
    
    
    # tripDist.df <- read.csv(file.path(INPUT_DIR, "2010 Dist Mat.csv"), sep=",", header=FALSE, skip=5)
    colnames(tripDist.df) <- c("OTAZ", "DTAZ", "Dist")
    Zo <- as.character(unique(tripDist.df$OTAZ))  
    
    # Transform trip distance data.frame into matrix  
    tripDist <- matrix(tripDist.df[ , 3], nrow=length(Zo), dimnames=list(Zo, Zo))
    tripDist <- tripDist[Zi, Zi]
    #alternatively
    #     #Get Zi from matrix dimnames
    #     pr <- 'hbw'; ic <- 'lowInc'; tp <- 'peak'
    #     obj.name <- paste0(tp, "Trips")
    #     file.mtx <-
    #       file.path(INPUT_DIR, "modec", pr, ic, paste0(obj.name, ".RData"))
    #     load(file.mtx)
    #     obj.data <- get(obj.name)
    #     Zi <- dimnames(obj.data)[[1]]
    
    Zi.n <- length(Zi)
    save(Zi, file = file.path(OUTPUT_DIR, "Zi.RData"))
    
    ## District shp file (for creating maps)
    ## District mapping from TAZ
    districts <- TAZPoly@data[, c("TAZ", "DISTRICT")] %>%
      arrange(TAZ) #%>%
    #      dplyr::rename(zone=TAZ,
    #                    ugb=DISTRICT)
    
    save(districts, file=file.path(OUTPUT_DIR, "districts.RData"))
    
    
#     ## hhs by TAZ & income level
#     taz_census <-
#       read.csv(file.path("data/Corvallis_cluster", "input/taz_census.csv"), header = TRUE, sep =
#                  ",")
#     
#     hhs.ZiIc <- taz_census %>%
#       filter(TAZ %in% as.numeric(Zi)) %>%
#       mutate(lowInc = HHI1BASE,
#              midInc = HHI2BASE + HHI3BASE,
#              highInc = HHI4BASE)  %>%
#       mutate(htaz = TAZ) %>%
#       dplyr::select(htaz, lowInc, midInc, highInc) %>%
#       gather(ic, hhs, lowInc:highInc,-htaz)
#     
#     head(hhs.ZiIc)
    
    load(file.path(INPUT_DIR, "inputs/RData/hiazAry.RData"))
    hhs.ZiIc <- apply(hiazAry, c(4, 2), sum) %>% 
                as.data.frame() %>%
                mutate(htaz = Zo) %>%
                filter(htaz %in% Zi) %>%
                mutate(lowInc = i1,
                       midInc = i2 + i3, 
                       highInc = i4,
                       htaz=as.numeric(htaz)) %>%
                dplyr::select(htaz, lowInc, midInc, highInc) %>% 
                gather(ic, hhs, lowInc:highInc,-htaz)
    
    save(hhs.ZiIc, file = file.path(OUTPUT_DIR, "hhs.ZiIc.RData"))
    
    ## for cluster identification
    ## TAZ tot.emp, AFREMP + MINEMP + CONEMP + MFGEMP + TCPEMP + WSTEMP + RETEMP + FINEMP + SVCEMP + GVTEMP
    ## ParkAcres
    
    ## TDM
    dir.create(file.path(OUTPUT_DIR, 'TDM'), recursive = TRUE, showWarnings = FALSE)
    
    # optional period factors (optional, if get.PA.from.OD == True)
    #   period.factors <- read.csv(file.path(INPUT_DIR, "inputs/busPeriodFactors.csv"))
    #
    #   period.factors <- period.factors %>%
    #     separate(purpose, into=c("purpose", "direction"), sep=-3) %>%
    #     rename(peak=pkad, offPeak=opad)
    #     #%>%
    #     #mutate(mode='bus')
    #
    #   write.csv(period.factors, file.path(OUTPUT_DIR, "TDM/period_factors.csv"), row.names = F)
    
    # Load trips by Trips by purpose, income, tod & Mode
    out.file <- file.path(OUTPUT_DIR, "TDM/PrIcTpMdPATrips.omx")
    createFileOMX(out.file, Zi.n, Zi.n, Replace = TRUE)
    
    for (pr in Pr) {
      for (ic in Ic) {
        for (tp in Tp) {
          # Trips by purpose, income & mode & tod
          obj.name <- paste0(tp, "Trips")
          file.name <-
            file.path(INPUT_DIR, "modec", pr, ic, paste0(obj.name, ".RData"))
          load(file.name)
          obj.data <- get(obj.name)
          
          # Subset trip matrix 
          dimnames(obj.data) <- list(Zo, Zo, Md)
          obj.data <- obj.data[Zi, Zi, ]
          
          for (md in Md) {
            print(paste(pr, ic, tp, md, sep = "->"))
            mtx.name <- paste0(pr, ic, tp, md, "Trips")
            mtx.descr <- paste("Trips for", pr, ic, tp, md, sep = " ")
            #writeMatrixOMX(out.file, obj.data[Zi, Zi, md], mtx.name, Description=mtx.descr)
            writeMatrixOMX(out.file, obj.data[, , md], mtx.name, Description =
                             mtx.descr)
          }
          #rm(list=c(obj.name, file.name, obj.data, mtx.name, mtx.descr))
        }
      }
    }
    
    #rm(out.file)
    
    #Load travel time skims
    #somehow travel time skims doesn't have dimnames defined
    path.skims <- file.path(INPUT_DIR, "inputs/RData/")
    
    out.file <- file.path(OUTPUT_DIR, "TDM/travelTimeSkims.omx")
    createFileOMX(out.file, Zi.n, Zi.n, Replace = TRUE)
    
    for (md in Md) {
      #certain modes or components of a mode may not differetiate b/w peak and offPeak
      #special handling of bikeTime file as the file name doesn't follow the same naming convention for other modes.
      obj.name.suffix1 <-
        ifelse(md == "bike", paste0(md, "Time"), paste0("Time", md))
      file.ttcomps <-
        Sys.glob(file.path(path.skims, paste0("*", obj.name.suffix1, ".RData")))
      for (tp in Tp) {
        obj.name.suffix2 <- paste0("Time", tp, md)
        file.ttcomps_tp <-
          Sys.glob(file.path(path.skims, paste0("*", obj.name.suffix2, ".RData")))
        file.ttcomps_tp <- c(file.ttcomps, file.ttcomps_tp)
        
        ttskims <- NULL
        for (file.ttcomp in file.ttcomps_tp) {
          obj.name <- file_path_sans_ext(basename(file.ttcomp))
          load(file.ttcomp)
          obj.data <- get(obj.name)
          ttskims <-
            if (is.null(ttskims))
              obj.data
          else
            ttskims + obj.data
        }
        
        # Subset trip matrix 
        dimnames(ttskims) <- list(Zo, Zo)
        ttskims <- ttskims[Zi, Zi]
        
        mtx.name <- paste0(tp, md, "Time")
        mtx.descr <- paste(tp, "Travel Time Skims for", md, sep = " ")
        writeMatrixOMX(out.file, ttskims, mtx.name, Description = mtx.descr)
        
        #rm(list=c(obj.name, ttskims, mtx.name, mtx.descr))
      }
    }
    
    #file.remove(file.bikeTime_new)
    
    #load travel distance skim
    #travel distance could potentially differ by time period?
    path.skims <- file.path(INPUT_DIR, "inputs/RData/")
    
    out.file <- file.path(OUTPUT_DIR, "TDM/travelDistanceSkims.omx")
    createFileOMX(out.file, Zi.n, Zi.n, Replace = TRUE)
    
    for (md in Md) {
      obj.name <- paste0(Md.tdist[[md]], "Dist")
#       file.name <- file.path(path.skims, paste0(obj.name, ".RData"))
#       load(file.name)
#       obj.data <- get(obj.name)
      obj.data <- tripDist
      mtx.name <- paste0(md, "Distance")
      mtx.descr <- paste("Travel distance skims for", md, sep = " ")
      writeMatrixOMX(out.file, obj.data, mtx.name, Description = mtx.descr)
    }
  }
  
## Directories
  for (scenario.name in scenario.names) {
    INPUT_DIR <- file.path("data/raw/", project.name, scenario.name)
    OUTPUT_DIR <- file.path("data/", project.name, scenario.name)
    dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
    
    prepare_data()
  }
  
## create artificial scenarios for CALM (scenario A - halve driving time; scenario B - halve transit travel time)
  create.hypothetical.scenarios <- FALSE
  if (create.hypothetical.scenarios) {
    base.dir <- file.path('data', project.name, "2010/")
    
    scenA.dir <- file.path('data', project.name, "scenarioA")
    dir.create(scenA.dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(base.dir, scenA.dir, recursive = TRUE)
    
    scenB.dir <- file.path('data', project.name, "scenarioB")
    dir.create(scenB.dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(base.dir, scenB.dir, recursive = TRUE)
    
    halve_skims <- function(skims.path, Md.subset) {
      omx <- listOMX(skims.path)
      mtx.info <- omx$Matrices
      require(stringr)
      for (mtx.i in 1:nrow(mtx.info)) {
        mtx.name <- mtx.info$name[mtx.i]
        mtx.descr <- mtx.info$description[mtx.i]
        mtx <- readMatrixOMX(skims.path, mtx.name)
        if (any(str_detect(mtx.name, Md.subset))) {
          print(mtx.name)
          mtx <- mtx / 2
        }
        writeMatrixOMX(skims.path, mtx, mtx.name, Description = mtx.descr, Replace = TRUE)
      }
    }
    
    Md.drive <- c("driveAlone", "drivePass", "pass")
    Md.transit <- c("busWalk", "parkAndRideBus")
    # skims.path.scenA <- file.path(scenA.dir, "TDM/travelTimeSkims.omx")
    skims.path.scenA <- file.path(scenA.dir, "2010/TDM/travelTimeSkims.omx")
    # skims.path.scenB <- file.path(scenB.dir, "TDM/travelTimeSkims.omx")
    skims.path.scenB <- file.path(scenB.dir, "2010/TDM/travelTimeSkims.omx")
    
    halve_skims(skims.path.scenA, Md.drive)
    halve_skims(skims.path.scenB, Md.transit)
  }
  
  ## prepare taz.shp for clustering of activity centers
  SQFT.KM2 <- 0.092903/1000000
  
  taz.data <- taz@data %>% 
    dplyr::select_("TAZ", area.sqft=taz.area) %>%
    mutate(seq=1:n(), area.km2 = area.sqft * SQFT.KM2)
  
  inputs.path <- file.path(INPUT_DIR, 'inputs')
  load(file.path(inputs.path, "hhs.RData"))
  load(file.path(inputs.path, "parkAcres.RData"))
  load(file.path(inputs.path, "totalEmp.RData"))
  load(file.path(inputs.path, "afrEmp.RData"))
  load(file.path(inputs.path, "minEmp.RData"))
  load(file.path(inputs.path, "conEmp.RData"))
  load(file.path(inputs.path, "mfgEmp.RData"))
  load(file.path(inputs.path, "tcpEmp.RData"))
  load(file.path(inputs.path, "wstEmp.RData"))
  load(file.path(inputs.path, "retEmp.RData"))
  load(file.path(inputs.path, "finEmp.RData"))
  load(file.path(inputs.path, "svcEmp.RData"))
  load(file.path(inputs.path, "gvtEmp.RData"))
  
  taz.idx.inc <- 9:length(hhs) #the first 8 TAZs are external ones
  
  add.data <- data.frame(hhs=hhs, parkAcres=parkAcres[1, ],
                         totalEmp=totalEmp,
                         afrEmp=afrEmp,
                         minEmp=minEmp,
                         conEmp=conEmp,
                         mfgEmp=mfgEmp,
                         tcpEmp=tcpEmp,
                         wstEmp=wstEmp,
                         retEmp=retEmp,
                         finEmp=finEmp,
                         svcEmp=svcEmp,
                         gvtEmp=gvtEmp)
  
  add.data <- add.data[taz.idx.inc, ]
  add.data$TAZ <- as.numeric(Zi)
  
  # Merge Area and EmpHHoldParkAcre
  AllData <- left_join(taz.data, add.data,  by="TAZ")
  
  # Calculate densities
  AllData <- AllData %>% 
    mutate(
          st.hbw = totalEmp,
          st.hbs=0.028 * hhs + retEmp + 0.01 * svcEmp + 0.024 *(totalEmp - retEmp - svcEmp),
          st.hbr=1.969 * hhs + 5.15 * parkAcres + retEmp,
          st.hbo=0.413 * hhs + retEmp + 0.739 * svcEmp + 0.868 * gvtEmp + 0.108 * (totalEmp - retEmp - svcEmp - gvtEmp),
          den.hbw = st.hbw / area.km2,
          den.hbs = st.hbs / area.km2,
          den.hbr = st.hbr / area.km2,
          den.hbo = st.hbo / area.km2)
  
  # re-order by the original row order (seq), as shp file depends on the order
  AllData <- AllData %>%
    arrange(seq) %>%
    dplyr::select(TAZ, area.km2, st.hbw, den.hbw, st.hbs, den.hbs, st.hbr, den.hbr, st.hbo, den.hbo)
  
  taz@data <- left_join(taz@data, AllData, by="TAZ")
  # eliminate TAZ with null value (external TAZs)
  taz <- taz[!is.na(taz@data$den.hbw), ]
  
  if (SAVE.INTERMEDIARIES) {
    out.shp <- "tazden"
    unlink(file.path(INTERMEDIATE_DIR, paste(out.shp, ".*", sep="")))
    writeOGR(taz, INTERMEDIATE_DIR, out.shp, driver="ESRI Shapefile")
  }
  
  basket.args$taz <- taz