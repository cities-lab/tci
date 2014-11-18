#/
#@filename accessLogSum.R
#@author Ben Stabler, benjamin.stabler@odot.state.or.us
#@version 1.2
#@date 4/6/04
#
#Calculate multimodal accessibility (daily logsums)
#This script sums the log of all choices for each purpose.  
#Created 1/20/04 benjamin.stabler@odot.state.or.us
#Revised 2/2/04 benjamin.stabler@odot.state.or.us
#Revised 3/22/04 to work with util<mode><purpose> inputs
#Revised 4/6/04 to work with util<modeIncludingIncome><purpose> inputs
#
#See pages 15-19 of the "Metro Travel Forecasting 2003 Trip-Based 
#Demand Model Methodology Report DRAFT 2/12/03" for a description.
#/

cat("Calculate multimodal accessibility (daily logsum) \n\n")

fun$accessLogSum <- function(purpose) {

    #/
    #Calculate log sums
    #/
    
    ##/
    #Calculate daily log sums for each purpose
    #Each mode's exp(utilities) is read in and added to the 
    #running total utility by purpose by income.  After all the modes have been
    #added in, the total utility is logged, with infinities replaced with zero.
    #The resulting log sum (a list of three log sums by income) is then saved out.
    #Logsums that do not vary by income are output in this format as well for
    #code generalization purposes.
    #@param <various> - various util<mode><purpose> utilities by income for each choice for each purpose
    #@return hbwlogSum - saves hbw log sum to RData file
    #@return hbslogSum - saves hbw log sum to RData file
    #@return hbrlogSum - saves hbr log sum to RData file
    #@return hbologSum - saves hbo log sum to RData file
    #@return nhbwlogSum - saves nhbv log sum to RData file
    #@return nhbnwlogSum - saves NHBNW log sum to RData file
    #@return hbcolllogSum - saves HBCOLL log sum to RData file
    ##/

    #Print message to console to info user
    cat("\nCalculating logSum for ", purpose, "multimodal accessibility\n")

    #Create object to store sum of all choices
    logSum <- list("lowInc"=0, "midInc"=0, "highInc"=0)

    #Get choices for trip purpose and add together by income
    #Collapse purposes by income to purposes to output a list of three incomes for each purpose
    choices <- names(get(paste(purpose, "AccessUtils", sep="")))
    incomes <- c("lowInc","midInc","highInc")
    choicesNoIncome <- unique(gsub("lowInc|midInc|highInc", "", choices))

    for(choice in choicesNoIncome) {
	    cat("Current Purpose: ", purpose, "\tCurrent Choice: ", choice, "\n")
	    
         if(choice %in% choices) {
              #If true then choice does not vary by income
              load(paste("intm_output/access/", "util", choice, purpose, ".RData", sep=""))
              logSum$lowInc <- logSum$lowInc + get(paste("util", choice, purpose, sep=""))
              logSum$midInc <- logSum$midInc + get(paste("util", choice, purpose, sep=""))
              logSum$highInc <- logSum$highInc + get(paste("util", choice, purpose, sep=""))
              rm(list=paste("util", choice, purpose, sep=""))
         } else {
              #Choice varies by income
              load(paste("intm_output/access/", "util", choice, "lowInc", purpose, ".RData", sep=""))
              load(paste("intm_output/access/", "util", choice, "midInc", purpose, ".RData", sep=""))
              load(paste("intm_output/access/", "util", choice, "highInc", purpose, ".RData", sep=""))
              logSum$lowInc <- logSum$lowInc + get(paste("util", choice, "lowInc", purpose, sep=""))
              logSum$midInc <- logSum$midInc + get(paste("util", choice, "midInc", purpose, sep=""))
              logSum$highInc <- logSum$highInc + get(paste("util", choice, "highInc", purpose, sep=""))
              rm(list=c(paste("util", choice, "lowInc", purpose, sep=""),
                    paste("util", choice, "midInc", purpose, sep=""),
                    paste("util", choice, "highInc", purpose, sep="")))
         }
    }

    #Take log of total choices to get log sum by purpose and save results
    logSum <- lapply(logSum, log)
    logSum <- lapply(logSum, function(x) { x[is.infinite(x)] <- 0; x } )
    assign(paste("logSum", purpose, sep=""), logSum)
    rm(logSum)
    save(list=paste("logSum", purpose, sep=""), file=paste("intm_output/access/","logSum", purpose, ".RData", sep=""))
    rm(list=paste("logSum", purpose, sep=""))
}


