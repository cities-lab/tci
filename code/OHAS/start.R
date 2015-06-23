# This script prepares the workspace and file directories for calculating 
# travel time and travel cost with OHAS data

# Set workspace
setwd("~/tci")

var_list.0 <- ls()

##source common settings, which may be overrided below
source("code/settings.R")

## settings
INPUT_DIR <- 'data/'
OUTPUT_DIR <- 'output/OHAS'
dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
# whether to save intermediate results
SAVE.INTERMEDIARIES <- TRUE
INTERMEDIATE_DIR <- "output/intermediate/OHAS"
dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)

# make names for household income groups, trip purpose and calculation method
IcNames <- c("Low Income", "Mid Income", "High Income")
Ic <- c("lowInc", "midInc", "highInc")
names(IcNames) <- Ic

PrNames <- c("Work", "Shopping", "Recreation", "Other")
Pr <- c("hbw", "hbs", "hbr", "hbo")
names(PrNames) <- Pr

CmNames <- c("mintcost", "avgtcost", "maxtcost")
Cm <- c("min", "avg", "max")
names(CmNames) <- Cm  

##start scripts
source("code/OHAS/compute_tcost.R")
source("code/OHAS/plot_tcost.R")

##clean up
var_list.1 <- ls()
rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
rm(var_list.1)
