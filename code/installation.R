
install.packages(c(
                   "devtools", "dplyr",
                   "ggmap", "ggplot2",
                   "maptools", "mlogit", 
                   "pscl",
                   "RColorBrewer", "reshape2", "rgdal", "rgeos", 
                   "SDMTools", "stargazer",
                   "testthat", "tidyr"
                   ))

#install dependencies "rhdf5" package for omx.r
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

#install packages from github
require(devtools)
install_github("krlmlr/kimisc")
