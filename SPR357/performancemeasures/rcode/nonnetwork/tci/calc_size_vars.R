#/
#@filename calc_size_vars.R
#@author Brian Gregor
#@version 1.0
#@date 6/04/05
#
#Calculates unlogged size terms of destination choice utilities for hbw, hbs, hbr, hbo
#and saves to disk for use in transportation cost index calculations
#
#Adapts portions of tripDistribution function by Ben Stabler last revised on 5/26/04
#/

cat("Calculate Size Variables\n\n")

calcSizeVars <- function(purpose) {

    cat(paste("Calculate ", purpose, " size variables\n\n", sep=""))

    #/
    #Loads inputs specified in distribution utility size variables defintion csv file
    #/
    
    ##/
    #Get variable names in sizeVarUtils
    #@param sizeVarUtil - distribution utility definitions
    #@return varNames - names of variables used in distribution
    #@return districts - district defintions
    #@return tripProdAry - purpose specific trip productions array
    #@return purposeOnly - purpose without income group label
    ##/
    varNames <- getVarNames(sizeVarUtils[purpose])
    incomes <- c("lowInc","midInc","highInc")
    incomeSpecificPurpose <- as.logical(length(grep("lowInc|midInc|highInc", purpose)))

    ##/
    #Load employment numbers by zone
    #Replicate by row to create full matrices.  Only those sectors 
    #that are included in the sizeVarUtils definition file are loaded
    #and removed from totalEmp to create remainingEmp. "afr" is 
    #agriculture/forestry and "min" is mining.  These are grouped 
    #in the bank data so they split by an arbitrary percent since 
    #they are aggregated in calculations anyone.  They are split 
    #here just to account for inputs. Also, removes "remainingEmp" 
    #from inputs to read in.
    #
    #This code section also creates remainingEmp by subtracting the
    #employment of each loaded employment sector from the total 
    #employment.
    #@param varNames - names of variables used in distribution
    #@return <employment sector>Emp - employment matrix
    #@return totalEmp - total employment matrix
    #@return remainingEmp - remaining employment matrix
    ##/
    
    #Load total employment
    #Convert remainingEmp to a full matrix filling by row
    load(paste(codeLoc,"/inputs/Rdata/totalEmp.RData",sep=""))
    remainingEmp <- totalEmp
    remainingEmp <- matrix(remainingEmp, length(remainingEmp), length(remainingEmp), byrow=T)
   
    #Read in employment categories and subtract employment
    #from remaining employment
    empCats <- varNames[grep("Emp",varNames)]
    empCats <- empCats[empCats != "remainingEmp"]
    for(emp in empCats) {
        load(paste(codeLoc,"/inputs/Rdata/", emp, ".RData", sep=""))
        assign(emp, matrix(get(emp), length(get(emp)), length(get(emp)), byrow=T))
        remainingEmp <- remainingEmp - get(emp)
    }
    rm(emp)

     #Load number of households
     load(paste(codeLoc,"/inputs/Rdata/hhs.RData",sep=""))
     hhs <- matrix(hhs, length(hhs), length(hhs), byrow=TRUE)
     
     #Load park acres. Already in matrix form
     load(paste(codeLoc,"/inputs/Rdata/parkAcres.RData",sep=""))


    ##/
    #Boost Retail Employment to Shopping Center Employment if applicable
    #Adjust totalEmp to reflect change  (if necessary)
    #@param shsqft - Shopping center square footage
    #@param retEmp - retail employment matrix
    #@param totalEmp - total employment matrix
    #@return totalEmp - boosted total employment
    #@return retEmp -boosted retail employment
    ##/
    if(exists("retEmp") & exists("totalEmp")) {
         load(paste(codeLoc,"/inputs/Rdata/shsqft.RData",sep=""))
         shsqft <- matrix(shsqft, length(shsqft), length(shsqft), byrow=T)
         shemp <- shsqft / 1000 * 3
         totalEmp <- totalEmp - retEmp
         retEmp <- (retEmp * (retEmp >= shemp) + shemp * (retEmp < shemp))
         totalEmp <- totalEmp + retEmp
         rm(shsqft)
    }

       #Evaulate size variable definition for purpose and save to disk
	 options(warn=-1)
	 SizeVarDataName <- paste("sizeVar", purpose, sep="")
	 SizeVarFileName <- paste("intm_output/sizevars/", SizeVarDataName, ".RData", sep="")
	 assign(paste("sizeVar", purpose, sep=""), eval(parse(text=sizeVarUtils[purpose])))
	 #remove "-inf" replace with zero -Martin 01/12/07	
   Sizetemp <- get(SizeVarDataName)
   Sizetemp[is.infinite(Sizetemp)] <- -10
   assign( paste("sizeVar", purpose, sep=""), Sizetemp)
   #save result as RData file
   save(list=eval(SizeVarDataName), file=SizeVarFileName)
   rm(list=ls()[ls()==SizeVarDataName], SizeVarDataName, SizeVarFileName)
	 options(warn=0)

#End of calcSizeVars function
} 


