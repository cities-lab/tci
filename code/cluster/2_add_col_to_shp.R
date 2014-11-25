# This script isn't needed anymore
# This script adds employment density and sizeterms density calculated in 2_Cal_Den.R to shape file 

# read dbf file 
require(foreign)
tazshp <- read.dbf("gisserver/taz/TAZ.dbf") 

# load employdensity and sizeterms density for each 
load("results/TotalData.RData")

# add data to shape file 
tazshp$seq <- 1:nrow(tazshp)
tt <- merge(tazshp, TotalData, by.x="newtaz", by.y="TAZ", all.x=TRUE)

# re-order and then remove seq
tt <- tt[with(tt, order(seq)), -c(which(names(tazshp)=='seq'))]
row.names(tt) <- NULL

write.dbf(tt, "gisserver/tazden/tazden.dbf")
