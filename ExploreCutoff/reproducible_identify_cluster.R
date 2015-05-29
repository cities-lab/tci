# This scripts identify percentile cutoffvalues automatically 

# Set workspace
setwd("~/tci")
var_list.0 <- ls()

source("code/cluster/settings.R")

# Load required functions
source("code/cluster/def_functions.R")


## This script aggregates LEHD employment from block level into TAZ level,
## calculates employment and sizeterms density for each TAZ. The calculation 
## results are appended to taz shapefile, then centers are identified
## based on clustering

#my.dir <- dirname(sys.frame(1)$ofile)
#source(file.path(my.dir,'def_functions.R'))

# load required package
require(dplyr)

# read four counties blockgroup level LEHD data, with a tazindex column identifying
# the taz id the block group is in 
lehd <- read.csv(file.path(INPUT_DIR, "LEHD.csv"), header=TRUE)

emp.by.taz <- lehd %>%
  dplyr::rename(TAZ = tazindex) %>%
  group_by(TAZ) %>%
  summarise(tot.emp=sum(C000,  na.rm=TRUE),
            gvt.emp=sum(CNS20, na.rm=TRUE),
            ret.emp=sum(CNS07, CNS18, na.rm=TRUE),
            fin.emp=sum(CNS10, CNS11, CNS13, na.rm=TRUE),
            svc.emp=sum(CNS09, CNS12, CNS14, CNS15, CNS16, CNS17, CNS19, na.rm=TRUE)
  )

# get number of Household in each taz
hh.by.taz <- read.csv(file.path(INPUT_DIR, "HIA_2010.csv"), header=TRUE)
hh.by.taz$HHold <- rowSums(hh.by.taz[, 2:64]) 
hh.by.taz <- hh.by.taz %>%
  dplyr::select(taz, HHold) %>%
  mutate(HHold=round(HHold)) %>%
  rename(TAZ=taz)

# get park acres in each taz
ParkAcres <- read.csv(file.path(INPUT_DIR, "ParkAcres.csv"), header=TRUE)

# Merge Employment and HHold, ParkAcres, area of TAZ
EmpHHold <- left_join(emp.by.taz, hh.by.taz, by="TAZ")
EmpHHoldParkAcre <- left_join(EmpHHold, ParkAcres, by="TAZ")

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
AllData <- left_join(Area, EmpHHoldParkAcre,  by="TAZ")
SQFT.KM2 <- 0.092903/1000000 # define a constant for converting sqft to sqkm

# Calculate densities
AllData <- AllData %>% 
  mutate(non.ret = tot.emp - ret.emp,
         non.retsvcgvt = non.ret - svc.emp - gvt.emp,
         st.hbs = ret.emp + .008396 * non.ret + .022126*HHold,
         st.hbr = tot.emp + 1.278 * HHold + 4.6833 *ParkAcres,
         st.hbo = 0.2393 * HHold + ret.emp + 0.6419 * svc.emp + 0.6109 * gvt.emp + 0.06802 * non.retsvcgvt,
         totemp.den = tot.emp / (Area * SQFT.KM2),
         st.hbs.den = st.hbs / (Area * SQFT.KM2),
         st.hbr.den = st.hbr / (Area * SQFT.KM2),
         st.hbo.den = st.hbo / (Area * SQFT.KM2),
         lq.hbw = (tot.emp/(tot.emp + st.hbs + st.hbr + st.hbo))/(sum(tot.emp,na.rm=TRUE)/(sum(tot.emp,na.rm=TRUE)+sum(st.hbs,na.rm=TRUE)+sum(st.hbr,na.rm=TRUE)+sum(st.hbo,na.rm=TRUE))),
         lq.hbs = (st.hbs/(tot.emp + st.hbs + st.hbr + st.hbo))/(sum(st.hbs,na.rm=TRUE)/(sum(tot.emp,na.rm=TRUE)+sum(st.hbs,na.rm=TRUE)+sum(st.hbr,na.rm=TRUE)+sum(st.hbo,na.rm=TRUE))),
         lq.hbr = (st.hbr/(tot.emp + st.hbs + st.hbr + st.hbo))/(sum(st.hbr,na.rm=TRUE)/(sum(tot.emp,na.rm=TRUE)+sum(st.hbs,na.rm=TRUE)+sum(st.hbr,na.rm=TRUE)+sum(st.hbo,na.rm=TRUE))),
         lq.hbo = (st.hbo/(tot.emp + st.hbs + st.hbr + st.hbo))/(sum(st.hbo,na.rm=TRUE)/(sum(tot.emp,na.rm=TRUE)+sum(st.hbs,na.rm=TRUE)+sum(st.hbr,na.rm=TRUE)+sum(st.hbo,na.rm=TRUE))))

# re-order by the original row order (seq), as shp file depends on the order
AllData <- AllData %>% 
  arrange(seq) %>% 
  dplyr::select(TAZ, Area, tot.emp, totemp.den, st.hbs, st.hbs.den, st.hbr,st.hbr.den, st.hbo, st.hbo.den,lq.hbw,lq.hbs,lq.hbr,lq.hbo)

TAZPoly@data <- AllData

if (SAVE.INTERMEDIARIES) {
  out.shp <- "tazden"
  unlink(file.path(INTERMEDIATE_DIR, paste(out.shp, ".*", sep="")))
  writeOGR(TAZPoly, INTERMEDIATE_DIR, out.shp, driver="ESRI Shapefile")
}

# eliminate TAZ with null value
TAZPloyNoNA <- TAZPoly[!is.na(TAZPoly@data$totemp.den),]


# calculate density percentile values
hbw.den.cutoff.values <- quantile(TAZPloyNoNA@data[,"totemp.den"], c(0.5,0.6,0.7,0.8,0.9,0.95))

hbs.den.cutoff.values <- quantile(TAZPloyNoNA@data[,"st.hbs.den"], c(0.5,0.6,0.7,0.8,0.9,0.95))

hbr.den.cutoff.values <- quantile(TAZPloyNoNA@data[,"st.hbr.den"], c(0.5,0.6,0.7,0.8,0.9,0.95))

hbo.den.cutoff.values <- quantile(TAZPloyNoNA@data[,"st.hbo.den"], c(0.5,0.6,0.7,0.8,0.9,0.95))

den.cutoff.values <- data.frame(hbw.den.cutoff.values, hbs.den.cutoff.values, hbr.den.cutoff.values, hbo.den.cutoff.values, 
                                row.names=c("50","60","70","80","90","95"))
colnames(den.cutoff.values ) <- c("hbw", "hbs", "hbr", "hbo")


# inentify clusters
data.df <- TAZPloyNoNA@data

# identify hbw clusters 
hbwfilter50 <- data.df$totemp.den >= den.cutoff.values["50","hbw"]
hbwcluster50 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter50, dist=1.0)

hbwfilter60 <- data.df$totemp.den >= den.cutoff.values["60","hbw"]
hbwcluster60 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter60, dist=1.0)

hbwfilter70 <- data.df$totemp.den >= den.cutoff.values["70","hbw"]
hbwcluster70 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter70, dist=1.0)


hbwfilter80 <- data.df$totemp.den >= den.cutoff.values["80","hbw"]
hbwcluster80 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter80, dist=1.0)


hbwfilter90 <- data.df$totemp.den >= den.cutoff.values["90","hbw"]
hbwcluster90 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter90, dist=1.0)

hbwfilter95 <- data.df$totemp.den >= den.cutoff.values["95","hbw"]
hbwcluster95 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter95, dist=1.0)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbwclusters.RData")
  save(hbwcluster50, hbwcluster60, hbwcluster70, hbwcluster80, hbwcluster90, hbwcluster95, file=intm.file)
}


# identify hbs clusters 
hbsfilter50 <- data.df$st.hbs.den >= den.cutoff.values["50","hbs"]
hbscluster50 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter50, dist=1.0)

hbsfilter60 <- data.df$st.hbs.den >= den.cutoff.values["60","hbs"]
hbscluster60 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter60, dist=1.0)

hbsfilter70 <- data.df$st.hbs.den >= den.cutoff.values["70","hbs"]
hbscluster70 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter70, dist=1.0)


hbsfilter80 <- data.df$st.hbs.den >= den.cutoff.values["80","hbs"]
hbscluster80 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter80, dist=1.0)


hbsfilter90 <- data.df$st.hbs.den >= den.cutoff.values["90","hbs"]
hbscluster90 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter90, dist=1.0)

hbsfilter95 <- data.df$st.hbs.den >= den.cutoff.values["95","hbs"]
hbscluster95 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter95, dist=1.0)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbsclusters.RData")
  save(hbscluster50, hbscluster60, hbscluster70, hbscluster80, hbscluster90, hbscluster95, file=intm.file)
}


# identify hbr clusters 
hbrfilter50 <- data.df$st.hbr.den >= den.cutoff.values["50","hbr"]
hbrcluster50 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter50, dist=1.0)

hbrfilter60 <- data.df$st.hbr.den >= den.cutoff.values["60","hbr"]
hbrcluster60 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter60, dist=1.0)

hbrfilter70 <- data.df$st.hbr.den >= den.cutoff.values["70","hbr"]
hbrcluster70 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter70, dist=1.0)


hbrfilter80 <- data.df$st.hbr.den >= den.cutoff.values["80","hbr"]
hbrcluster80 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter80, dist=1.0)


hbrfilter90 <- data.df$st.hbr.den >= den.cutoff.values["90","hbr"]
hbrcluster90 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter90, dist=1.0)

hbrfilter95 <- data.df$st.hbr.den >= den.cutoff.values["95","hbr"]
hbrcluster95 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter95, dist=1.0)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbrclusters.RData")
  save(hbrcluster50, hbrcluster60, hbrcluster70, hbrcluster80, hbrcluster90, hbrcluster95, file=intm.file)
}

# identify hbo clusters 
hbofilter50 <- data.df$st.hbo.den >= den.cutoff.values["50","hbo"]
hbocluster50 <- identify_clusters(TAZPloyNoNA, filter=hbofilter50, dist=1.0)

hbofilter60 <- data.df$st.hbo.den >= den.cutoff.values["60","hbo"]
hbocluster60 <- identify_clusters(TAZPloyNoNA, filter=hbofilter60, dist=1.0)

hbofilter70 <- data.df$st.hbo.den >= den.cutoff.values["70","hbo"]
hbocluster70 <- identify_clusters(TAZPloyNoNA, filter=hbofilter70, dist=1.0)


hbofilter80 <- data.df$st.hbo.den >= den.cutoff.values["80","hbo"]
hbocluster80 <- identify_clusters(TAZPloyNoNA, filter=hbofilter80, dist=1.0)


hbofilter90 <- data.df$st.hbo.den >= den.cutoff.values["90","hbo"]
hbocluster90 <- identify_clusters(TAZPloyNoNA, filter=hbofilter90, dist=1.0)

hbofilter95 <- data.df$st.hbo.den >= den.cutoff.values["95","hbo"]
hbocluster95 <- identify_clusters(TAZPloyNoNA, filter=hbofilter95, dist=1.0)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hboclusters.RData")
  save(hbocluster50, hbocluster60, hbocluster70, hbocluster80, hbocluster90, hbocluster95, file=intm.file)
}

