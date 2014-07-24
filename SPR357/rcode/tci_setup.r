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

#Load generic JEMnR functions
#============================

#::

     codeLoc <- "rcode"
     source(paste(codeLoc, "/jemnrFunctions.R", sep=""))
     attach(fun) #Attach function list fun to workspace

#Make tci directory if doesn't exist
#===================================
#::

     if(!file.exists("tci")) dir.create("tci")
     if(!file.exists("tci/graphics")) dir.create("tci/graphics")

#Load Variable Definitions
#=========================
#::

     # Define income group abbreviation
     Ic <- c("lowInc", "midInc", "highInc")
     
     # Define zone abbreviation and limit to internal zones
     load("inputs/RData/districts.RData")
     externalZones <- districts$zone[districts$zone<100]
     zoneNames <- districts$zone
     Zo <- as.character(zoneNames)
     IsInternal <- !(Zo %in% as.character(externalZones))
     Zi <- Zo[IsInternal]; rm(IsInternal)
     
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
#::

     # Load the utility descriptions
     sizeVarUtils <- readUtils("inputs/sizeVarUtils.csv")
     verifyVarName("sizeVarUtils",varDictionary)

     # Load function for calculating the size variables and saving them to disk
     source("rcode/tci/calc_size_vars.R")

     for(pr in Pr){
          for(ic in Ic){
               CombinedPrIc <- paste(pr, ic, sep="")
               calcSizeVars(CombinedPrIc)
               }
          }

#Calculate utilities and logsums
#===============================
#::

      for(pr in Pr){
            accessUtilities(pr)
            }
      for(pr in Pr){
            accessLogSum(pr)
            }




