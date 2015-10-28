# This script prepares the workspace and file directories for calculating travel time 
# and travel cost with employment centres methods 

# Set workspace
setwd("~/tci")
var_list.0 <- ls()

source("code/settings.R")  #source project level settings first
source("code/cluster/settings.R")

# Load required functions
source("code/functions.R")
#this step below takes a long time; skip it if its intermediate results have been saved
#source("code/cluster/identify_centers.R") 
source("code/cluster/compute_md_prob_trips.R")
source("code/cluster/compute_tcost.R")
source("code/cluster/aggregate_tcost.R")
source("code/cluster/aggregate_by_geo.R")
source("code/cluster/plot.R")

var_list.1 <- ls()
rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
rm(var_list.1)
