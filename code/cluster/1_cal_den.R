## This script aggregates LEHD employment from block level into TAZ level,
## calculates employment and sizeterms density for each TAZ.
## The calculation results are added to a shapefile, then centers are identified
## based on clusters

my.dir <- dirname(sys.frame(1)$ofile)
source(file.path(my.dir,'def_functions.R'))

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
         st.hbo.den = st.hbo / (Area * SQFT.KM2))

# re-order by the original row order (seq), as shp file depends on the order
AllData <- AllData %>% 
  arrange(seq) %>% 
  dplyr::select(TAZ, Area, tot.emp, totemp.den, st.hbs, st.hbs.den, st.hbr,st.hbr.den, st.hbo, st.hbo.den)

TAZPoly@data <- AllData

if (SAVE.INTERMEDIARIES) {
  out.shp <- "tazden"
  unlink(file.path(INTERMEDIATE_DIR, paste(out.shp, ".*", sep="")))
  writeOGR(TAZPoly, INTERMEDIATE_DIR, out.shp, driver="ESRI Shapefile")
}

# eliminate TAZ with null value
TAZPloyNoNA <- TAZPoly[!is.na(TAZPoly@data$totemp.den),]

thresholds <- list(totemp.den=2500, st.hbs.den=102, st.hbr.den=1763, st.hbo.den=495)

# identify hbw tazs of centers
hbwci <- identify_centers(TAZPloyNoNA, "totemp.den", 2500, dist=1.0, sum.col="tot.emp", sum.cutoff=1000)
hbwci <- hbwci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

# identify hbs tazs of centers 
hbsci <- identify_centers(TAZPloyNoNA, "st.hbs.den", 102, dist=1.0, sum.col="st.hbs", sum.cutoff=1000)
hbsci <- hbsci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

# identify hbr tazs of centers 
hbrci <- identify_centers(TAZPloyNoNA, "st.hbr.den", 1763, dist=1.0, sum.col="st.hbr", sum.cutoff=1000)
hbrci <- hbrci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

# identify hbo tazs of centers 
hboci <- identify_centers(TAZPloyNoNA, "st.hbo.den", 495, dist=1.0, sum.col="st.hbo", sum.cutoff=1000)
hboci <- hboci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

if (SAVE.INTERMEDIARIES) {
  out.file <- file.path(INTERMEDIATE_DIR, "centers.RData")
  save(hbwci, hbsci, hbrci, hboci, file=out.file)
}