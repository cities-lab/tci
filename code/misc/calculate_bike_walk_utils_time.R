# Calculate bike & walk mode utilities by trip purpose
source("code/cluster/settings.R")

# According to Metro's 2013 Trip-Based Travel Demand Model Methodology Report
# HBW
#Bike
#U = exp (-3.03 – 0.8*Cbdist – 3.650*Cbcost + 0.1279*ln(MixTotA) ) 
#
#Walk
#U = exp (-5.07 – 4.307*ln(Wdist) + 0.3345*ln(MixRetP))

#HBS(hop), HBR, HBO
#Bike
#U = exp (-1.26*Shop – 0.11*Rec – 1.49*Oth – 0.9*Nbdist – 0.22*Nbcost ) 
#
#Walk
#U = exp (-2.82*Shop – 1.80*Rec – 2.76*Oth – 2.466*ln(Wdist)+ 0.1248*ln(MixRetP) + 0.5997*LrgHH)
#where
#Cbdist  =  Bicycle Commute Trip Distance
#Cbcost  =  Bicycle Commute Route Attractiveness
#Nbdist  =  Bicycle Non-commute Trip Distance
#Nbcost	=  Bicycle Non-commute Route Attractiveness
#MixTotA  =  Total employment access within ½ mile of attraction zone (see Section A.1.b)
#MixRetP	=  Retail employment access within ½ mile of production zone (see Section A.1.b)
#LrgHH  =  1 if large household (3+ persons)

# Bike
bikeUtil.c  <- read.csv(file.path(INPUT_DIR, 'TDM/mf201.csv'), header=F)  #Utils for commuting trips (HBWork, HBCollege)
bikeUtil.nc <- read.csv(file.path(INPUT_DIR, 'TDM/mf203.csv'), header=F)  #Utils for non-commuting trips
utilbikehbw <- matrix(bikeUtil.c[, 3], nrow=max.taz_id, byrow=TRUE,
                      dimnames=list(Zi, Zi))
# utilbikehbw <- acast(bikeUtil.c, V1~V2, value.var="V3") ##safer but slower, can handle non-continous/missing zone ids
dimnames(utilbikehbw) <- list(Zi, Zi)
utilbikehbs <- utilbikehbr <- utilbikehbo <- matrix(bikeUtil.nc[, 3], nrow=max.taz_id, 
                                                    byrow=TRUE, dimnames=list(Zi, Zi))
# utilbikehbs <- utilbikehbr <- utilbikehbo <- acast(bikeUtil.nc, V1~V2, value.var="V3") ##safer but slower, can handle non-continous/missing zone ids

# bike travel time
# mf202.csv is the bike commute distance from MetroSkims2010
# mf204.csv is the bike non-commute distance from MetroSkims2010
bikeDist.c <- read.csv(file.path(INPUT_DIR, 'TDM/mf202.csv'), header=FALSE)
bikeDist.nc <- read.csv(file.path(INPUT_DIR, 'TDM/mf204.csv'), header=FALSE)

all.equal(bikeDist.c, bikeDist.nc)  # Commute and noncommute distance are the same
bikeTime <- matrix(bikeDist.c[, 3], nrow=max.taz_id, byrow=TRUE,
                   dimnames=list(Zi, Zi))


# Walk  
wdist.df <- read.table(file.path(INPUT_DIR, 'TDM/2010_wdist.csv'), sep=",", row.names=1, skip=1) #distance matrix used for calculating walk time
wdist.mx <- as.matrix(wdist.df, dimnames=list(Zi, Zi))
utilwalkhbw <- -5.07 - 4.307 * log(wdist.mx) # + 0.3345 * log(MixRetP) 
utilwalkhbs <- -2.82 - 2.466 * log(wdist.mx) #+ 0.1248*ln(MixRetP) + 0.5997*LrgHH)
utilwalkhbr <- -1.80 - 2.466 * log(wdist.mx) #+ 0.1248*ln(MixRetP) + 0.5997*LrgHH)
utilwalkhbo <- -2.76 - 2.466 * log(wdist.mx) #+ 0.1248*ln(MixRetP) + 0.5997*LrgHH)
walkTime <- wdist.mx / walk.speed

