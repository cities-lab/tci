#===========
#tci_setup.R
#===========

#:author: Martin Mann
#:date: 2/08/07
#:contact: brian.j.gregor@odot.state.or.us
#:copyright: Oregon Department of Transportation
#:license: GPL2

#Description
#===========

#This script prepares the workspace and file directories for calculating the Travel Cost Index 
#and related measures. The script needs to be executed at the top level of a JEMnR model structure.


codeLoc<-getwd()
#Load Network Script
load(paste(codeLoc,"/performancemeasures/rcode/network/RVMPO_mobility_measures_Peak.R",sep=""))
