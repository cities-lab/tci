
install.packages(c("dplyr",
                   "ggmap", "ggplot2",
                   "maptools", "mlogit", 
                   "pscl",
                   "RColorBrewer", "reshape2", "rgdal", "rgeos", 
                   "stargazer",
                   "tidyr"
                   ))

#install dependencies "rhdf5" package for omx.r
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
