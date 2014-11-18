#===========
#tci_setup.R
#===========

#:author: Brian Gregor
#:date: 9/26/05
#:contact: brian.j.gregor@odot.state.or.us
#:copyright: Oregon Department of Transportation
#:license: GPL2

#Description
#===========

#This script prepares the workspace and file directories for calculating the Travel Cost Index and related measures. The script needs to be executed at the top level of a JEMnR model structure.
 setwd("C:/Users/huajie/Desktop/TCIPortland50")
 codeLoc<-getwd()
 setwd(paste(codeLoc,"/performancemeasures/",sep=""))
 codeLoc<-getwd()
#Load generic JEMnR functions
#============================         
     source("rcode/jemnr/PPM_jemnrFunctions.R")
     source("rcode/jemnr/access/PPM_accessUtilities.R")
     source("rcode/jemnr/access/PPM_accessLogSum.R")
     attach(fun) 

#Make directories if they don't exist
#===================================
     if(!file.exists("intm_output/access")) dir.create("intm_output/access")
     if(!file.exists("intm_output/sizevars")) dir.create("intm_output/sizevars")
     if(!file.exists("final_output/tci")) dir.create("final_output/tci")
     if(!file.exists("final_output/tci/graphics")) dir.create("final_output/tci/graphics")
     
#Load Variable Definitions
#=========================

     # Define income group abbreviation
     Ic <- c("lowInc", "midInc", "highInc")
     
     # Define zone abbreviation and limit to internal zones
     load(paste(codeLoc,"/inputs/Rdata/Zi.RData",sep=""))
     load(paste(codeLoc,"/inputs/Rdata/Zo.RData",sep=""))
     
     # Define trip purpose abbreviation
     # The purposes for this study (now) are limited to the home-based trips
     # They exclude nonhome-based trips, school trips and college trips
     Pr <- c("hbw", "hbs", "hbr", "hbo")
     
     # Define the travel modes
     Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")

     # Define time and cost coefficients to convert logsums into time and cost equivalents
     IvTimeCoeff.Pr <- c(hbw=-0.03528, hhs=-0.02275, hhr=-0.02275, hho=-0.02275)
     OpCostCoeff.PrIc <- rbind(
                    hbw=c(lowInc=-0.5417, midInc=-0.5417, highInc=-0.5417),
                    hbs=c(lowInc=-0.4033, midInc=-0.4033, highInc=-0.4033),
                    hbr=c(lowInc=-0.4033, midInc=-0.4033, highInc=-0.4033),
                    hbo=c(lowInc=-0.4033, midInc=-0.4033, highInc=-0.4033))

     # Define bike and walk access utility coefficients
     BikeAccessCoeff.Pr <- c(hbw=-3.217, hbs=-1.839, hbr=-1.839, hbo=-1.839)
     WalkAccessCoeff.Pr <- c(hbw=-4.389, hbs=-2.532, hbr=-2.532, hbo=-2.532)
          
#Calculate and Save the Size Variables for each Trip Purpose
#===========================================================
     # Load the utility descriptions
     sizeVarUtils <- readUtils(paste(getwd(),"/inputs/sizeVarUtils.csv",sep=""))

     # Load function for calculating the size variables and saving them to disk
     source("rcode/nonnetwork/tci/Pcalc_size_vars.R")
     
     for(pr in Pr){
          for(ic in Ic){
               CombinedPrIc <- paste(pr, ic, sep="")
               calcSizeVars(CombinedPrIc)
               }
          }

#Calculate utilities and logsums
#===============================
hbwPeakFactor <- 0.6058
hbsPeakFactor <- 0.2997
hbrPeakFactor <- 0.3085
hboPeakFactor <- 0.3766

#read in accessUtils by trip purpose eg. performancemeasures/inputs/jemnr_inputs/hbwAccessUtils.csv
for(pr in Pr) {
       load(paste(codeLoc,"/inputs/Rdata/", pr, "AccessUtils.RData", sep=""))
}
       
      for(pr in Pr){
            accessUtilities(pr)
            }
                  
      for(pr in Pr){
            accessLogSum(pr)
            }


