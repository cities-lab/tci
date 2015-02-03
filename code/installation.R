
install.pakcages(c("dplyr", "tidyr", "reshape2",
                   "maptools", "RColorBrewer",
                   "rgeos", "rgdal", 
                   "mlogit", 
                   ))

#install dependencies "rhdf5" package for omx.r
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
