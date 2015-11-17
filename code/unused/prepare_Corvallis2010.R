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
  project.name <- "CALM"
  year <- ""
  unit.name <- 'minutes'  
  
  scenario.names <- c('2010')
  #scenario.names <- c('2010', '2030Preferred', '2030Preferred_Scen1')
  
  #default settings
  source("code/settings.R")
  #  Inc levels
  #Ic <- c("lowInc", "midInc", "highInc")
  # Trip Purpose
  #Pr <- c("hbw", "hbs", "hbr", "hbo")
  # Define the travel modes
  #Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")
  # Define time period 
  Tp <- c("peak", "offPeak")
  
  # modes used for distance skims
  Md.tdist <- c("auto", "auto", "auto", "tran", "tran", "bike", "walk")
  names(Md.tdist) <- Md
  
  prepare_data <- function() {
    # init Zi
    #Zi <- as.character(sort(TAZPoly@data[, TAZ.id_name]))
    
    #alternatively
    
    #Get Zi from matrix dimnames
    pr <- 'hbw'; ic <- 'lowInc'; tp <- 'peak'
    obj.name <- paste0(tp, "Trips")
    file.mtx <-
      file.path(INPUT_DIR, "modec", pr, ic, paste0(obj.name, ".RData"))
    load(file.mtx)
    obj.data <- get(obj.name)
    Zi <- dimnames(obj.data)[[1]]
    
    Zi.n <- length(Zi)
    save(Zi, file = file.path(OUTPUT_DIR, "Zi.RData"))
    
    ## District mapping from TAZ
    #  districts <- TAZPoly@data[, c("TAZ", "DISTRICT")] %>%
    #    arrange(TAZ) %>%
    #    dplyr::rename(zone=TAZ,
    #                  ugb=DISTRICT)
    
    #  save(districts, file=file.path(OUTPUT_DIR, "districts.RData"))
    
    ## District shp file (for creating maps)
    
    ## hhs by TAZ & income level
    taz_census <-
      read.csv(file.path(INPUT_DIR, "inputs/taz_census.csv"), header = TRUE, sep =
                 ",")
    
    hhs.ZiIc <- taz_census %>%
      filter(TAZ %in% as.numeric(Zi)) %>%
      mutate(lowInc = HHI1BASE,
             midInc = HHI2BASE + HHI3BASE,
             highInc = HHI4BASE)  %>%
      mutate(htaz = TAZ) %>%
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
    out.file <- file.path(OUTPUT_DIR, "TDM/PrIcTpMdTrips.omx")
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
      file.name <- file.path(path.skims, paste0(obj.name, ".RData"))
      load(file.name)
      obj.data <- get(obj.name)
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
