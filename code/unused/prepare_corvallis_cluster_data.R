# This script prepare the data for applying cluster-based approach in Corvallis 

# Set workplace
#setwd("~/tci") ## now set in code/settings.R
var_list.0 <- ls()

# # Setting 
# INPUT_DIR <- 'data/Corvallis_cluster'
# SAVE.INTERMEDIARIES <- TRUE
# INTERMEDIATE_DIR <- "output/intermediate/cluster/corvallis"
# OUTPUT_DIR <- 'output/cluster/corvallis'
# dir.create(file.path(OUTPUT_DIR), recursive = TRUE, showWarnings = FALSE)


approach.name <- "Cluster-based"
data.source <- "Corvallis"
#unit.name <- "minutes"
unit.name <- "dollars"

source("code/settings.R")  #source project level settings first

INPUT_DIR <- 'data/Corvallis_cluster'
unitcosts <- OHAS.unitcosts.list[[unit.name]]


# Define cutoffs 
#cutoffs <- data.frame(hbw=c(2500, 1000), hbs=c(102, 1000), hbr=c(1763, 1000), hbo=c(495, 1000))
#row.names(cutoffs)=c("cutoff.val", 'sum.cutoff.val')
## alternatively set percentile cutoffs
cutoffs <- data.frame(hbw=c(.50, .50), hbs=c(.50, .50), hbr=c(.50, .50), hbo=c(.50, .50))
row.names(cutoffs) <- c("cutoff.percentile", 'sum.cutoff.percentile')

# Source functions
source("code/functions.R")

# Load required packages 
require(foreign)
require(dplyr)
require(maptools)
require(rgeos)
require(rgdal)
require(SDMTools)

# identify_centers.R
    
    # define a constant for converting sqft to sqkm
    SQFT.KM2 <- 0.092903/1000000 
    
    # Load shapefile 
    TAZPoly <- readShapePoly(file.path("data/shp/Corvallis/TAZ.shp"),
                             proj4string=CRS("+init=epsg:2992"))
    # Define TAZ index 
    Zi <- as.character(sort(TAZPoly@data$TAZ))
    save(Zi, file="data/Corvallis_cluster/Zi.RData")
    max.taz_id <- length(Zi)
    
    districts <- TAZPoly@data[, c("TAZ", "DISTRICT")] %>% 
                 arrange(TAZ) %>%
                 dplyr::rename(zone=TAZ,
                               ugb=DISTRICT)
    
    save(districts, file="data/Corvallis_cluster/districts.RData")
    
    Area <- TAZPoly@data %>% 
            dplyr::select(TAZ, Area=f_area) %>%
            mutate(seq=1:n()) 
   # Load household and park acrec data   
   taz_census <- read.csv(file.path(INPUT_DIR, "input/taz_census.csv"), header=TRUE, sep=",")
    
   Hh.ZiIc <- taz_census %>% 
      filter(TAZ %in% as.numeric(Zi)) %>%
      mutate(lowInc=HHI1BASE,
             midInc=HHI2BASE + HHI3BASE, 
             highInc=HHI4BASE)  %>%
      dplyr::select(lowInc, midInc, highInc)
    
    rownames(Hh.ZiIc) <- Zi      
    Hh.ZiIc <- as.matrix(Hh.ZiIc)
    save(Hh.ZiIc, file=file.path(INPUT_DIR, "Hh.ZiIc.RData"))
    
    # rcode/tripdist/tripdistribution.r: 
    # RemainingEmp created by subtracting the employment of each loaded employment sector from the total employment.
    # destination utilities calculation formulas 
    # input/utilities/distUtil.csv
    # purpose	utility
    # hbwlowInc   	1.108 * logSum + log( 2.872 * (afrEmp + minEmp) + remainingEmp )
    # hbwmidInc	    0.94 * logSum + log( retEmp + 1.177 * mfgEmp + 1.154 * remainingEmp )
    # hbwhighInc	  1.0 * logSum + log( retEmp + 2.2 * finEmp + 1.407 * svcEmp + 1.585 * remainingEmp )
    # hbs	          1.92 * logSum + log( 0.028 * hhs + retEmp + 0.01 * svcEmp + 0.024 * remainingEmp )
    # hbr	          1.365 * logSum + log( 1.969 * hhs + 5.15 * parkAcres + retEmp )
    # hbo	          1.677 * logSum + log( 0.413 * hhs + retEmp + 0.739 * svcEmp + 0.868 * gvtEmp + 0.108 * remainingEmp )
    
    # Load employment data 
    AllData <- Area %>%
              left_join(taz_census, by="TAZ") %>%
              mutate(hhs=HHBASE) %>%
              mutate(tot.emp = AFREMP + MINEMP + CONEMP + MFGEMP + TCPEMP + WSTEMP + RETEMP + FINEMP + SVCEMP + GVTEMP, 
                     st.hbs=0.028 * hhs + RETEMP + 0.01 * SVCEMP + 0.024 *(tot.emp-RETEMP-SVCEMP),
                     st.hbr=1.969 * hhs + 5.15 * parkAcres + RETEMP,
                     st.hbo=0.413 * hhs + RETEMP + 0.739 * SVCEMP + 0.868 * GVTEMP + 0.108 * (tot.emp-RETEMP-SVCEMP-GVTEMP), 
                     totemp.den = tot.emp / (Area * SQFT.KM2),
                     st.hbs.den = st.hbs / (Area * SQFT.KM2),
                     st.hbr.den = st.hbr / (Area * SQFT.KM2),
                     st.hbo.den = st.hbo / (Area * SQFT.KM2)
                    )
    
    
    # re-order by the original row order (seq), as shp file depends on the order
    AllData <- AllData %>% 
               arrange(seq) %>% 
               dplyr::select(TAZ, Area, tot.emp, totemp.den, st.hbs, st.hbs.den, st.hbr, st.hbr.den, st.hbo, st.hbo.den)
    
    TAZPoly@data <- AllData
    
    if (SAVE.INTERMEDIARIES) {
      out.shp <- "corvallis_tazden"
      unlink(file.path(INTERMEDIATE_DIR, paste(out.shp, ".*", sep="")))
      writeOGR(TAZPoly, INTERMEDIATE_DIR, out.shp, driver="ESRI Shapefile")
    }
    
    # eliminate TAZ with null value
    TAZPloyNoNA <- TAZPoly[!is.na(TAZPoly@data$totemp.den),]
    
    # identify hbw tazs of centers
    #hbwci <- identify_centers(TAZPloyNoNA, "totemp.den", cutoff.val=cutoff['cutoff.val', 'hbw'], dist=1.0, sum.col="tot.emp", sum.cutoff.val=cutoff['sum.cutoff.val', 'hbw'])
    hbwci <- identify_centers(TAZPloyNoNA, "totemp.den", dist=1.0, sum.col="tot.emp", cutoffs=split(cutoffs[, 'hbw'], rownames(cutoffs)))
    hbwci <- hbwci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
    
    # identify hbs tazs of centers 
    hbsci <- identify_centers(TAZPloyNoNA, "st.hbs.den", dist=1.0, sum.col="st.hbs", cutoffs=split(cutoffs[, 'hbs'], rownames(cutoffs)))
    hbsci <- hbsci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
    
    # identify hbr tazs of centers 
    hbrci <- identify_centers(TAZPloyNoNA, "st.hbr.den", dist=1.0, sum.col="st.hbr", cutoffs=split(cutoffs[, 'hbr'], rownames(cutoffs)))
    hbrci <- hbrci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
    
    # identify hbo tazs of centers 
    hboci <- identify_centers(TAZPloyNoNA, "st.hbo.den", dist=1.0, sum.col="st.hbo", cutoffs=split(cutoffs[, 'hbo'], rownames(cutoffs)))
    hboci <- hboci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
    
    if (SAVE.INTERMEDIARIES) {
      intm.file <- file.path(INTERMEDIATE_DIR, "centers.RData")
      save(hbwci, hbsci, hbrci, hboci, file=intm.file)
    }
    

# Comput_md_prob_trips.R 
  source("code/cluster/compute_md_prob_trips.R") 

# compute_tcost.R 
    #Begin iteration by trip purpose
    #-------------------------------  
    for (pr in Pr) {
      CentersObjName <- paste(pr, "ci", sep="")
      Centers <- get(CentersObjName); rm(CentersObjName)
      
      # Begin iteration by income group
      for (ic in Ic) {
        # Begin iteration by time period
        for ( tp in Tp){
          TTimecost.ZiMdCm <- array(0, dim=(c(length(Zi), length(Md), length(Cm))), 
                                    dimnames=list(Zi,Md,Cm))
          
          TOperationcost.ZiMdCm <- array(0, dim=(c(length(Zi), length(Md), length(Cm))), 
                                         dimnames=list(Zi,Md,Cm))
          
          # Begin iteration by mode 
          for (md in Md ) {
            # load trip matrices
            TripsMdObjName <- paste(pr, ic, md, "trips", sep="")
            if (! exists(TripsMdObjName)) {
              TripsMd <- readMatrixOMX(file.path(INTERMEDIATE_DIR, "ModeTrips.omx"), TripsMdObjName)
              dimnames(TripsMd) <- list(Zi, Zi)
              assign(TripsMdObjName, TripsMd)
            }
            
            TDistanceObjName <- paste(md, "Distance", sep="")
            
            if ((md == "bike")|(md=="walk")) {
              # load travel time for bike and walk 
              ##TODO: how were these data processed? why bike & walk travel vary by purpose?
              TTimeObjName <- paste(md, "Time", sep="")
        
              
              # End calculate travel time for walk and bike 
            } else {          
              # load travel time for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
              TTimeObjName <- paste(md, tp, "Time", sep = "")
              
            }
            
            if (! exists(TTimeObjName)) {
              TTime.mx <- readMatrixOMX(file.path(INPUT_DIR, "TDM/Skims.omx"), TTimeObjName)
              dimnames(TTime.mx) <- list(Zi, Zi)
              assign(TTimeObjName, TTime.mx)
            }        
            
            if (! exists(TDistanceObjName)) {
              TDistance.mx <- readMatrixOMX(file.path(INPUT_DIR, "TDM/DistanceSkims.omx"), TDistanceObjName)
              assign(TDistanceObjName, TDistance.mx)
              dimnames(TDistance.mx) <- list(Zi, Zi)
            } 
            
            for (cm in Cm) {
              func <- get(paste(cm, '_tt', sep=""))
              TTime <- func(as.character(Centers$TAZ), TTime.mx, TripsMd)
              TCost <- TTime * unitcosts[md, "VOT"] /60
              paste("min", pr, ic, md, "time", sep="")
              
              TDistance <- func(as.character(Centers$TAZ), TDistance.mx, TripsMd)
              TOperationcost <- TDistance*unitcosts[md, "mcpm"]
              # TOperationcost <- TDistance*unitcosts[md, "mcpm"]/(wage.Ic[ic])
              
              TTimecost.ZiMdCm[, md, cm] <- TCost
              TOperationcost.ZiMdCm[, md, cm] <- TOperationcost
            }
            
          } # End loop through mode
          
          Fullcost.ZiMdCm <- TTimecost.ZiMdCm + TOperationcost.ZiMdCm
          
          if (SAVE.INTERMEDIARIES) {
            intm.path <- file.path(INTERMEDIATE_DIR, 'costs')
            dir.create(intm.path, recursive = TRUE, showWarnings = FALSE)
            obj.name <- paste(pr, ic, tp, "FullCost.ZiMdCm", sep="")
            assign(obj.name, Fullcost.ZiMdCm)
            intm.file <- file.path(intm.path, paste(obj.name, ".RData", sep=""))
            save(list=c(obj.name), file=intm.file)
          }
          
        } # End loop through time period
      } # End loop through income group 
    } # End loop through purpose


# aggrregate_tcost.R
  ## This script aggregates cost by purpose and income group
  
  ## Combine the number of trips produced by income group and purpose into array 
  
  TripProd.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))
  
  for (pr in Pr) {
    for (ic in Ic) {
      TripsObjName <- paste(pr, ic, "Dist", sep="")
      in.file <- file.path(INPUT_DIR, "tripdist", paste(TripsObjName, ".RData", sep=""))
      load(in.file); rm(in.file)
      
      # Get trip matrix 
      Trips.ZiZi <- get(TripsObjName)
      Trips.ZiZi <- Trips.ZiZi[Zi, Zi]
      TripProd.ZiIcPr[, ic, pr] <- rowSums(Trips.ZiZi) 
    }
  }
  
  TripProd.ZiIc <-  apply(TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))
  TripProd.ZiPr <-  apply(TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))
  TripProd.Zi <-  apply(TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))
  
  #### aggregate the cost into array by purpose, income, calculate method and time period 
  
  # Begin iteration by calculate method 
  for (cm in Cm) {
    
    # Begin iteration by time period 
    for (tp in Tp) {
      
      AggCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))
      
      #:: Begin iteration by trip purpose 
      for (pr in Pr) { 
        
        #peakAggCost.ZiIcPr.name <- paste(cm, "peak", "AggCost.ZiIcPr", sep="")
        #load(file.path(OUTPUT_DIR, paste("aggcostCmTp/",peakAggCost.ZiIcPr.name, ".RData", sep="" )))
        #peakAggCost.ZiIcPr <- get(peakAggCost.ZiIcPr.name)
        
        #offpeakAggCost.ZiIcPr.name <- paste(cm, "offpeak", "AggCost.ZiIcPr", sep="")
        #load(file.path(OUTPUT_DIR, paste("aggcostCmTp/",offpeakAggCost.ZiIcPr.name, ".RData", sep="" )))
        #offpeakAggCost.ZiIcPr <- get(offpeakAggCost.ZiIcPr.name)      
        # Begin iteration by income group
        for (ic in Ic) {
          
          #get trips array 
          TotTripsArray.name <- paste(pr, ic, "TotTrips.ZiMd", sep="")
          if (!exists(c(TotTripsArray.name)))
            load(file.path(INTERMEDIATE_DIR, paste("trips/", TotTripsArray.name, ".RData", sep="")))
          TotTripsArray <- get(TotTripsArray.name)
          
          # calculate row sum of trips trip array 
          #TotTripsArraySum <- rowSums(TotTripsArray, na.rm=TRUE)
          
          #get full travel cost attay
          FullCostArray.name <- paste(pr, ic, tp, "FullCost.ZiMdCm", sep="")
          if (!exists(c(FullCostArray.name)))
            load(file.path(INTERMEDIATE_DIR, paste("costs/", FullCostArray.name, ".RData", sep="")))
          FullCostArray<- get(FullCostArray.name)
          
          # aggregate cost weighted by trips
          AggCost.ziicpr <- nd.weighted.mean(FullCostArray[, , cm], TotTripsArray, dims=1)
          
          #AggCost.name <- paste(cm, pr, ic, tp,"AggCost.Zi", sep="")
          #assign(AggCost.name, AggCost.Zi)
          #if (SAVE.INTERMEDIARIES) {
          #  intm.file <- file.path(INTERMEDIATE_DIR, 'aggcostcmprictp/', paste(AggCost.name, ".RData"))
          #  save(list=AggCost.name, file=intm.file)
          #}
          
          # combine cost into array
          AggCost.ZiIcPr[,ic,pr] <- AggCost.ziicpr;
          
          rm(AggCost.ziicpr, TotTripsArray, FullCostArray)
          
          
        } # End loop by income group 
        
      } # End loop through purpose
      
      AggCost.ZiIcPr.name <- paste(cm, tp, "AggCost.ZiIcPr", sep="")
      assign(AggCost.ZiIcPr.name, AggCost.ZiIcPr)
      
      output.path <- file.path(OUTPUT_DIR, 'aggcostCmTp/')
      dir.create(output.path, recursive = TRUE, showWarnings = FALSE)
      output.file <- file.path(output.path, paste(AggCost.ZiIcPr.name, ".RData", sep=""))
      save(list=AggCost.ZiIcPr.name, file=output.file)
      
      
      #rm(AggCost.ZiIcPr)
      
      AggCost.ZiIc <- nd.weighted.mean(AggCost.ZiIcPr, TripProd.ZiIcPr, dims=c(1,2))
      AggCost.ZiIc.name <- paste(cm, tp, 'AggCost.ZiIc', sep="")
      assign(AggCost.ZiIc.name, AggCost.ZiIc)
      
      AggCost.ZiPr <- nd.weighted.mean(AggCost.ZiIcPr, TripProd.ZiIcPr, dims=c(1,3))
      AggCost.ZiPr.name <- paste(cm, tp, 'AggCost.ZiPr', sep="")
      assign(AggCost.ZiPr.name, AggCost.ZiPr)
      
      AggCost.Zi <- nd.weighted.mean(AggCost.ZiIcPr, TripProd.ZiIcPr, dims=1)
      AggCost.Zi.name <- paste(cm, tp, 'AggCost.Zi', sep="")
      assign(AggCost.Zi.name, AggCost.Zi)
      
      #save output
      output.path <- file.path(OUTPUT_DIR, 'aggcostCmTp/')
      dir.create(output.path, recursive = TRUE, showWarnings = FALSE)
      output.file <- file.path(output.path, paste(AggCost.Zi.name, ".RData", sep=""))
      save(list=c(AggCost.ZiIc.name, AggCost.ZiPr.name, AggCost.Zi.name), file=output.file)
      
    } # End loop by time period
  } # End loop by calculate method 


# aggregate_by_geo.R
    source("code/cluster/aggregate_by_geo.R")

# plot.R 
 # source("code/cluster/plot.R")

# post_process.R
    require(ggplot2)
    require(ggmap)
    require(reshape2)
    require(magrittr)
  
  # load saveGraph function
  source("code/thirdparty/openGraphSaveGraph.R")
  
    #plot results from cluster-based approach
    dtcost.htaz_tpurp_inc <- melt(weightedpeakAggCost.ZiIcPr, 
                                  varnames = c('htaz', 'inc.level', 'TripPurpose'),
                                  value.name = "tcost")
    
    dtrips.htaz_tpurp_inc <- melt(TripProd.ZiIcPr,
                                  varnames = c('htaz', 'inc.level', 'TripPurpose'),
                                  value.name = "trips")
    
    dhhs.inc <- melt(Hh.ZiIc,
                     varnames = c('htaz', 'inc.level'),
                     value.name = "hhs")
    
    dwtcost.htaz_tpurp_inc <- dtcost.htaz_tpurp_inc %>%
      left_join(dtrips.htaz_tpurp_inc) %>%
      left_join(dhhs.inc) %>%
      mutate(tcost.wt = tcost * trips / hhs,
             tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt)
      )
    
  
    # dwtcost.htaz_tpurp_inc %<>% 
    #   mutate(inc.level = factor(inc.level, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc")),
    #          TripPurpose = factor(TripPurpose, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other")))
    
    dwtcost.htaz_inc <- dwtcost.htaz_tpurp_inc %>%
      group_by(htaz, inc.level) %>%
      summarize(tcost.wt=sum(tcost * trips) / mean(hhs),
                hhs = first(hhs)) %>%
      as.data.frame() %>%
      mutate(tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt))
    
    dwtcost.htaz_tpurp <- dwtcost.htaz_tpurp_inc %>%
      group_by(htaz, TripPurpose) %>%
      summarize(tcost.wt=mean(tcost.wt)) %>%
      as.data.frame() %>%
      mutate(tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt))
    
    dwtcost.htaz <- dwtcost.htaz_inc %>%
      group_by(htaz) %>%
      summarize(tcost.wt=wt.mean(tcost.wt, hhs)) %>%
      as.data.frame() %>%
      mutate(tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt))
    
    # prepare data for plotting
#     pden.inc <- ggplot(dwtcost.htaz_inc, aes(x = tcost.wt, colour=inc.level, group=inc.level)) +
#       geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 200) +
#       scale_colour_discrete(name = 'Income Level')+
#       ggtitle("Household-level travel cost by income groups") +
#       theme(plot.title = element_text(face="bold", size=12, vjust=1))
    
    pden.inc <- pden.inc.f(plot.data=dwtcost.htaz_inc)
    
    pden.inc
    output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_inc.png")
    ggsave(pden.inc, file=output_file, type="cairo-png")
    
#     pden.tpurp <- ggplot(dwtcost.htaz_tpurp, aes(x = tcost.wt, colour=TripPurpose, group=TripPurpose)) +
#       geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 100) +
#       scale_colour_discrete(name = 'Trip Purpose')+
#       ggtitle("Household-level travel cost by trip purposes") +
#       theme(plot.title = element_text(face="bold", size=12, vjust=1))
    
    pden.tpurp <- pden.tpurp.f(plot.data=dwtcost.htaz_tpurp)
    pden.tpurp
    output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_tpurp.png")
    ggsave(pden.tpurp, file=output_file, type="cairo-png")
    
#     boxp.tpurp_inc <- ggplot(dwtcost.htaz_tpurp_inc, aes(x=TripPurpose, y=tcost.wt, fill=inc.level)) +
#       geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
#       scale_fill_discrete(name = 'Income Level') +
#       ggtitle("Household-level travel cost by trip purposes and income levels") +
#       theme(plot.title = element_text(face="bold", size=12, vjust=1))

    boxp.tpurp.inc <-boxp.tpurp.inc.f(plot.data=dwtcost.htaz_tpurp_inc) 
    boxp.tpurp.inc
    output_file = file.path(OUTPUT_DIR, "boxplot_tcost.hh_by_tpurp.inc.png")
    ggsave(boxp.tpurp.inc, file=output_file, type="cairo-png")
    
    
    dwtcost.htaz_inc %<>% select(-hhs)
    
   # transform data for plotting maps
#     tcost.htaz_all <- dwtcost.htaz_tpurp_inc %>%
#       select(-c(trips, hhs, tcost)) %>%
#       mutate(inc.level=as.character(inc.level),
#              TripPurpose=as.character(TripPurpose)
#       )  %>%
#       union(mutate(ungroup(dwtcost.htaz_tpurp), inc.level="All Households", TripPurpose=as.character(TripPurpose))) %>%
#       union(mutate(ungroup(dwtcost.htaz_inc), inc.level=as.character(inc.level), TripPurpose="All Trips")) %>%
#       union(mutate(ungroup(dwtcost.htaz), inc.level="All Households", TripPurpose="All Trips"))  # %>%
#     #right_join(expand.grid(htaz=1:max.taz_id, TripPurpose=c('All', Pr), inc.level=c(Ic, 'All'), stringsAsFactors = F))
#     
#     tcost.htaz_all <- tcost.htaz_all%>% 
#       mutate(id=as.character(htaz), 
#              value = tcost.wt,
#              inc.level = factor(inc.level, levels=c(levels(inc.level), 'All '), labels=c(labels(), "All Households")),
#              TripPurpose = factor(TripPurpose, levels=c('All', Pr), labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))
#       )
#     
      
    tcost.htaz_tpurp.inc <- expand.grid(htaz=as.numeric(Zi), TripPurpose=c(Pr), inc.level=c(Ic), stringsAsFactors = F) %>% 
      left_join(dwtcost.htaz_tpurp_inc %>% ungroup() %>% mutate(inc.level=as.character(inc.level), TripPurpose=as.character(TripPurpose))) %>%
      dplyr::select(htaz, TripPurpose, inc.level, tcost.wt)
    
    tcost.htaz_tpurp.all <- expand.grid(htaz=as.numeric(Zi), TripPurpose=c(Pr), inc.level=c("All"), stringsAsFactors = F) %>%
      left_join(mutate(ungroup(dwtcost.htaz_tpurp), inc.level="All", TripPurpose=as.character(TripPurpose))) 
    
    tcost.htaz_all.inc <- expand.grid(htaz=as.numeric(Zi), TripPurpose=c("All"), inc.level=c(Ic), stringsAsFactors = F) %>%
      left_join(mutate(ungroup(dwtcost.htaz_inc), inc.level=as.character(inc.level), TripPurpose="All")) 
      
    tcost.htaz_all.all <- expand.grid(htaz=as.numeric(Zi), TripPurpose=c("All"), inc.level=c("All"), stringsAsFactors = F) %>%
      left_join(mutate(ungroup(dwtcost.htaz), inc.level="All", TripPurpose="All")) 
    
    tcost.htaz_all <- rbind(tcost.htaz_tpurp.inc, tcost.htaz_tpurp.all, tcost.htaz_all.inc, tcost.htaz_all.all) %>%
      right_join(expand.grid(htaz=as.numeric(Zi), TripPurpose=c('All', Pr), inc.level=c(Ic, 'All'), stringsAsFactors = F))
    
    #prepare data for plotting
    tcost.htaz_all <-  tcost.htaz_all %>%
                       mutate(id=as.character(htaz), 
                              value = tcost.wt)
                       
#                        ,
#              inc.level = factor(inc.level, levels=c("Low Inc", "Mid Inc", "High Inc", 'All Households')), #labels=c(labels(), "All Households")),
#              TripPurpose = factor(TripPurpose, levels=c('All Trips', "HBW", "HB Shopping", "HB Recreation", "HB Other") )#, labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))         
#       )
    
#     tcost.htaz_all <- tcost.htaz_all %>%
#       mutate(id=as.character(htaz), 
#              value = tcost.wt,
#              inc.level = factor(inc.level, levels=c(levels(inc.level), 'All '), labels=c(labels(), "All Households")),
#              TripPurpose = factor(TripPurpose, levels=c('All', Pr), labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))
#       )
    

    require(ggmap)
    require(scales)
    
    
    taz <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "TAZ")
    taz <- fortify(taz, region="TAZ")
    
    #taz.data <- tcost.distr %>% mutate(id = as.character(district.id),
    #                                         value = tcost.wtavg)
    #taz <- left_join(taz, taz.data)
    
#     plot_map <- function(plot.Data) {
#       p <- ggplot() +
#         geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
#                      color = NA, size = 0.1) +
#         scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), limits = c(0, 200), 
#                              name = "Travel Costs\n(Minutes)", na.value = "grey80") +
#         guides(fill = guide_legend(reverse = TRUE)) +
#         theme_nothing(legend = TRUE)
#     }
    
    plot.data <- full_join(taz, tcost.htaz_all)
    maps <- plot_map(plot.Data=plot.data) + facet_grid(TripPurpose~inc.level)
    maps
    output_file = file.path(OUTPUT_DIR, "map_taz_all.png")
    ggsave(maps, file = output_file, width = 8.5, height = 11, type = "cairo-png")

    # Plot districts 
    
    
#     # Plot centers     
#     if (!exists(paste(Pr, "ci", sep="")))
#       load(file.path(INTERMEDIATE_DIR, "centers.RData"))
#     
#     for (pr in Pr) {
#       CentersObjName <- paste(pr, "ci", sep="")
#       Centers <- get(CentersObjName); rm(CentersObjName)
#       Centers$TripPurpose <- pr
#     }  
#     
#     plot_map2 <- function(plot.Data) {
#       p <- ggplot() +
#         geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
#                      color = "black", size = 0.1) +
#         #scale_fill_distiller(palette = "YlOrRd", na.value = "gray80") + 
#         scale_fill_identity(na.value = "gray80") +
#         #guides(fill = guide_legend(reverse = TRUE)) +
#         theme_nothing(legend = FALSE)
#     }
#     
#     centers <- mutate(hbwci, TripPurpose="HBW") %>%
#       union(mutate(hbsci, TripPurpose="HB Shopping")) %>%
#       union(mutate(hbrci, TripPurpose="HB Recreation")) %>%
#       union(mutate(hboci, TripPurpose="HB Other")) %>%
#       mutate(id = as.character(TAZ),
#              is.center = 1, 
#              value = is.center)
#     
#     taz <- taz %>%
#       mutate(TAZ = as.integer(id),
#              seqid=1:n())
#     
#     #taz$TAZ <- as.integer(taz$id)
#     #taz$seq <- 1:nrow(taz)
#     
#     plot.centers <- left_join(taz, hbwci) %>% mutate(TripPurpose="HBW") %>%
#       union( left_join(taz, hbsci) %>% mutate(TripPurpose="HB Shopping") ) %>%
#       union( left_join(taz, hbrci) %>% mutate(TripPurpose="HB Recreation") ) %>%
#       union( left_join(taz, hboci) %>% mutate(TripPurpose="HB Other") ) %>%
#       mutate(TripPurpose = factor(TripPurpose, levels=c("HBW", "HB Shopping", "HB Recreation", "HB Other")),
#              is.center = as.integer(!is.na(center.id))
#       ) %>%
#       arrange(TripPurpose, seqid)
#     
#     #plot.centers <- full_join(taz, centers)
#     
#     plot_map2 <- function(plot.Data) {
#       p <- ggplot() +
#         geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = center.id),
#                      color = "grey70", size = 0.25) +
#         #scale_fill_manual(values=cbPalette) +
#         scale_fill_identity(na.value = "grey70") +
#         #guides(fill = guide_legend(reverse = TRUE)) +
#         theme_nothing(legend = TRUE)
#     }
#     
#     map_centers <- plot_map2(plot.centers) + facet_wrap(~TripPurpose, ncol=2, scales="free") #facet_grid(TripPurpose~inc.level)
#     map_centers
#     output_file = file.path(OUTPUT_DIR, "map_centers.png")
#     ggsave(map_centers, file = output_file, type = "cairo-png")
#     
#     ggplot(nmmaps, aes(date,temp))+geom_point(color="chartreuse4")+
#       facet_wrap(~year, ncol=2, scales="free")

# Remove items 
    
    var_list.1 <- ls()
    rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
    rm(var_list.1)