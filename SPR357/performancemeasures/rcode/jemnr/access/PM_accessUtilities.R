#/
#@filename accessUtilities.R
#@author Ben Stabler, benjamin.stabler@odot.state.or.us
#@version 1.2
#@date 4/6/04
#
#Calculate multimodal accessibility (logsums)
#Created 1/20/04 benjamin.stabler@odot.state.or.us
#Revised 1/26/04 benjamin.stabler@odot.state.or.us
#Revised 2/2/04 benjamin.stabler@odot.state.or.us
#Revised 2/27/04 Ben Stabler, to work with NAs in inputs
#Revised 3/22/04 Ben Stabler, to work with flexible
#utility definitions and to calculate an average daily travel time 
#before exp() the utility for all modes not just auto modes.
#Revised 4/6/04 to average peak and offpeak utilities before
# being exp() (average the whole utility not just individual inputs).
#Variable names cannot have numbers in them - replace 
# numbers with letter 1 to A, 2 to B, etc.

#This script calculates logsums by first reading in
#required inputs for a choice for all purposes.  Then it calculates
#a utility for each choice for each purpose and saves the 
#results.  The next script calculates the log sum of the choices.
#
#Inputs are various RData files depending on the choice and purpose
#returns utility by income for each choice for each purpose
# naming convention is util<mode><purpose>.RData
#
#The utility structure depends on the variables specified in the
#input coefficient matrices.  This revision allows utilities to be changed
#by simply changing the column (variable) names in the coefficient input
#CSV file.
#
#See pages 15-19 of the "Metro Travel Forecasting 2003 Trip-Based 
#Demand Model Methodology Report DRAFT 2/12/03" for a description.
#/

cat("Calculate multimodal accessibility\n\n")
   
fun$accessUtilities <- function(purpose) {
    ##/
    #Read in accessibility measures calculated in pre-gen, log, and rep to full matrix
    #@param mixTotA - total employment access within 1/2 mile of attraction zone
    #@param mixRetP - total retail employment access within 1/2 mile of production zone
    #@return mixTotA - total employment access within 1/2 mile of attraction zone replicated to full matrix
    #@return mixRetP - total retail employment access within 1/2 mile of production zone replicated to full matrix
    #@return choiceUtils - choice utility defintions for purpose
    ##/
    load(paste(codeLoc,"/pregen/mixrhm.RData",sep=""))
    load(paste(codeLoc,"/pregen/mixthm.RData",sep=""))
    mixRetP <- matrix(log(mixrhm),length(mixrhm),length(mixrhm))
    mixTotA <- matrix(log(mixthm),length(mixthm),length(mixthm), byrow=T)
    rm(mixrhm, mixthm)
   
    choiceUtils <- get(paste(purpose,"AccessUtils",sep=""))
    #eg hboAccessUtils returns
      #with purpose = "hbo"  returns utility equations for "hbo" (choiceUtils):
          #driveAlonelowInc
          #driveAlonehighInc
          #drivePasslowInc
          #drivePassmidInc
          #drivePasshighInc
          #passlowInc
          #passmidInc
          #passhighInc
          #busWalklowInc
          #busWalkmidInc
          #busWalkhighInc
          #parkAndRideBuslowInc
          #parkAndRideBusmidInc
          #parkAndRideBushighInc
          #bike
          #walk

    ##/
    #Function to calculate utility for each choice
    #This function calculates a utility based on the variables input
    #in the utility definition file for the purpose.  Variable names
    #must be the same as the saved inputs processed in inputsSave.R.  
    #Also, for mode choices that differ by income, the appropriate way to 
    #specify the choice is <mode><lowInc|midInc|highInc> depending
    #on the income class.  If the utility is a weighted average of
    #peak and offPeak variables then the <purpose>PeakFactors input in
    #inputs.R should be referred to in the utility definitions.
    #
    #The utilities are exp() and NAs are replaced with zeros
    #@param choiceUtils - choice utilities for purpose
    #@param choice - current choice
    #@return util - utilities matrix
    ##/
    calcAccessUtil <- function(choiceUtils, choice) {
         
         ##/
         #Get unique variable names in choiceUtils and check to see if any of
         #the variable names conflict with objects already in workspace.
         #@param choiceUtils - choice utility definitions
         #@param choice - current choice
         #@return varNames - names of variables used in distribution
         #@return choiceOnly - choice without income group label
         #@return varsToLoad - variables to load
         ##/
         varNames <- getVarNames(choiceUtils[choice])
         varNames <- unique(varNames)
         varsToLoad <- varNames[!sapply(varNames,exists)]
         #The parent.frame(3) searches for variables three levels up in the call stack
         #This allows inputs common to a purpose to be loaded once
         #One level up is function(x), two up is sapply(), and three up is parent of calcAccessUtil
         varsToLoad <- varsToLoad[!sapply(varsToLoad, function(x) exists(x, where=parent.frame(3)))]
         incomes <- c("lowInc","midInc","highInc")
         incomeSpecificChoice <- as.logical(length(grep("lowInc|midInc|highInc", choice)))
         if(incomeSpecificChoice) {
             choiceOnly <- gsub("lowInc|midInc|highInc", "", choice)
         } else {
             choiceOnly <- choice
         }
         ##/
         #Load inputs specified in choiceUtil (for both peak and offPeak periods if applicable)
         #@param varsToLoad - variables to load
         #@param choiceOnly - choice without income group label
         #@return <variable> - variable input in choiceUtil
         ##/
         #Load inputs
         for(mtx in varsToLoad) {
             
              #Check to see if input varies
              #The resulting matrix will just be the mtx name (no purpose or mode label)
              if(file.exists(paste(codeLoc,"/inputs/RData/", mtx, ".RData", sep=""))) {
                   load(paste(codeLoc,"/inputs/RData/", mtx, ".RData", sep=""))
                
              } else if(file.exists(paste(codeLoc,"/inputs/RData/", mtx, choiceOnly, ".RData", sep=""))) {
                   #Check to see if input varies by choice
                   load(paste(codeLoc,"/inputs/RData/", mtx, choiceOnly, ".RData", sep=""))
                   assign(mtx, get(paste(mtx, choiceOnly, sep="")))
                   rm(list=paste(mtx, choiceOnly, sep=""))
                   
              } else if(file.exists(paste(codeLoc,"/inputs/RData/", mtx, choiceOnly, purpose, ".RData", sep=""))) {
                   #Check to see if input varies by choice and purpose
                   load(paste(codeLoc,"/inputs/RData/", mtx, choiceOnly, purpose, ".RData", sep=""))
                   assign(mtx, get(paste(mtx, choiceOnly, purpose, sep="")))
                   rm(list=paste(mtx, choiceOnly, purpose, sep=""))
              } else {
                   #Matrix not found
                   stop(paste("matrix ", mtx, " not found", sep=""))
              }
         }

       ##/
       #Parse and evaluate the utility defintion input
       #@param choiceUtils - choice utility definition
       #@param choice - choice
       #@return util - utility for choice
       ##/  
       
       #Evaulate utility definition for purpose and turn off warnings (for log(-Inf))
       options(warn=-1)
       util <- eval(parse(text=choiceUtils[choice]))
       options(warn=0)
       #Take exponent of utility and replace NAs with 0
       util <- exp(util)
       util[is.na(util)] <- 0 
       util
    }
    
     #/
     #Calculate choice utilities for each purpose
     #/
     
     ##/
     #Calculate <mode> daily utility for each income class
     #@param <various> - various inputs depending on the purpose
     #@return <various> - saves results to a <mode><purpose>RData file
     #Because of function namespaces (separate locations for variables
     # defined within functions - local versus global variables) a variable
     # named ivTime is remove when the function finishes calculating the
     # utility and a new ivTime is loaded for the next choice.
     ##/

	#Call function, assign to applicable name, save and remove object
	cModes <- names(choiceUtils)
	for(rMode in cModes) {
	
	    cat("Current Purpose: ", purpose, "\tCurrent Choice: ", rMode, "\n")
	
	    #Call function for mode choice
	    util <- calcAccessUtil(choiceUtils, rMode)
	    
	    #Save and remove objects
	    assign(paste("util", rMode, purpose, sep=""), util)
	    save(list=paste("util", rMode, purpose, sep=""), 
		file=paste("intm_output/access/","util", rMode, purpose, ".RData", sep=""))
	    rm(list=c(paste("util", rMode, purpose, sep=""), "util"))
	}
     
}

