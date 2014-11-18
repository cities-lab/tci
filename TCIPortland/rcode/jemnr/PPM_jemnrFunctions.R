#/
#@filename jemnrFunctions.R
#@author Ben Stabler benjamin.stabler@odot.state.or.us
#@date 4/16/04
#@version 1.2
#Functions for JEM-n-R
#Revised 1/26/04 benjamin.stabler@odot.state.or.us
#Added readCoeffs on 3/25/04
#Revised to work with the fun list and to source in
#all the other module functions defined in other files.
#
#Copyright (C) 2004  Oregon Department of Transportation
#
#This program is free software; you can redistribute it and/or
#modify it under the terms of the GNU General Public License
#as published by the Free Software Foundation; either version 2
#of the License, or (at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
#/

cat("JEM-n-R Function Definitions\n\n")

#/
#Function Defintions
#This section of code defines common functions used in JEM-n-R
#/

##/
#Define a fun list to store all model functions.
#This list is then attached to the workspace so the functions
#can be references without refering to the list.
#The list is attach() to the workspace in runModel.R since it
#must be attached every R session.
#@param null
#@return fun - list of functions (currently null)
##/
fun <- list()

##/ 
#sumary.array - summary function to sum by dimension names
#This function is a custom method for the summary function for arrays
#It is used to summarize arrays by a specified dimension such as workers
#@param object - array object to summarize
#@param dimensions - string representations of the dimensions to summarize on
#@return null
##/
fun$summary.array <- function(object, dimensions) {
    if(!all(dimensions %in% names(dimnames(object)))) { stop("Dimension not found") }

    dimNums <- match(dimensions, names(dimnames(object)))
    result <- apply(object, dimNums, sum)
    total <- sum(object)

    if(is.null(dim(result))) {
        result <- c(result, total)
        names(result)[length(result)] <- "total"
    } else {
        result <- list(summary = result, total = total)
    }
    result
}

##/
#getDims - function to get the dimension number for an array dimension by name
#Used in conjunction with summary.array() this function allows arrays to summarized
#according to a specified dimension by name (as opposed to by number, which is the
#R default).
#@param object - R object to search for dimension names
#@param dimensions - string representations of dimensions to return dimension number
#@return dimNumbers dimension numbers as a vector
##/
fun$getDims <- function(object, dimensions) {
    if(!all(dimensions %in% names(dimnames(object)))) { stop("Dimension not found") }
    match(dimensions, names(dimnames(object)))
}

##/
#distsum - function to create district summaries
# distsum.R - creates 8-district summary - WRS 12/04/02
# adapted from collapse.v2 by Brian Gregor (ODOT)
# revisions 1/23/04 by Ben Stabler (ODOT)
#   1) new district input format,
#   2) system independent (no system commands)
#   3) R only code (removed awk script)
#   4) added districts parameter
# revised 3/10/04 Ben Stabler to work with 3D array
#   assumes 3rd dimension is dimension to loop over so
#   it collapsed the 1st and 2nd dimension for each 3rd
#   dimension element
#@param mat - matrix name
#@param desc - matrix description
#@param ens - ensemble or district code
#@param digits - maximum decimal points output
#@param rpt - report file name
#@param proj - project name
#@param init  - user initials
#@param districts - districts definition object
#@return null
##/
fun$distsum <- function (mat, desc, ens, digits, rpt, proj, init, districts) {

     # INPUT    DESCRIPTION          EXAMPLE
     # mat      matrix name          "mf.colldt"
     # desc     matrix description   "College Distribution"
     # ens      ensemble             "ga"
     # digits   max decimal points    3
     # rpt      report file name     "dist"
     # proj     project name*        "R Test"
     # init     user initials*       "wrs"
     # districts district definitions districts
     # example:  
     # distsum ("mf.colldt", "College Distribution", "ga", 3, "dist", project, initials)

     # Only works with vectors, 2D matrices, and 3D arrays
       if(is.vector(get(mat, envir=sys.parent()))) {
         type <- "1d"
       } else if(is.matrix(get(mat, envir=sys.parent()))) {
         type <- "2d"
       } else if(length(dim(get(mat, envir=sys.parent()))) == 3) {
         type <- "3d"
       } else {
         stop("ERROR:  unsupported data type (not a one, two or three dimensional object)")
       }
     
     # Set output width
       oldOptions <- options()
       options('width'=80) #80 character page width
     
     # Print header lines
       if (file.exists(paste(rpt, ".rpt", sep=""))) {
         outfile <- file(paste(rpt, ".rpt", sep=""), "a+")
         writeLines ("\n\f\n", con=outfile, sep="")
       } else {
         outfile <- file(paste(rpt, ".rpt", sep=""), "a+")
       }
     
       writeLines(proj, con=outfile, sep="\n")
       writeLines(paste("JEM-n-R (", toupper(init), ") - ", date(), sep=""),
         con=outfile, sep="\n")
       writeLines(paste(mat, desc, sep=" - "), con=outfile, sep="\n")
     
     # Load ensemble
       enssize <- nrow(districts)
       numdist <- length(unlist(unique(districts[ens])))
       dist.name <- sort(unlist(unique(districts[ens])))
     
       for(x in 1:numdist) {
         if(x < 10) {
             dist.name[x] <- paste(ens, "0", dist.name[x], sep="")
         } else {
             dist.name[x] <- paste(ens, dist.name[x], sep="")
         }
       }
       dist.name <- append(dist.name, "SUM")
     
     # Test for number of zones
       if(type == "2d") {
         numzones <- sqrt(length(get(mat, envir=sys.parent())))
       } else if(type == "3d") {
         numzones <- dim(get(mat, envir=sys.parent()))[1]
       } else {
         numzones <- length(get(mat, envir=sys.parent()))
       }
       if(enssize != numzones) {
         stop("ERROR:  ensemble and matrix sizes do not match")
       }
     
     # Process 3d array of full matrices
       if(type == "3d") {
     
         #Loop through all elements of 3rd dimension
         for(i in 1:dim(get(mat, envir=sys.parent()))[3]) {
     
             collapse.row <- apply(get(mat, envir=sys.parent())[,,i], 2,
                 function(x) tapply(x, districts[ens], sum))
             dist.sum <- t(apply(t(collapse.row), 2, function(x)
                 tapply(x, districts[ens], sum)))
     
             ma.rowsum <- append(apply(dist.sum, 1, sum), sum(dist.sum))
             ma.colsum <- apply(dist.sum, 2, sum)
             dist.sum <- cbind(rbind(dist.sum, ma.colsum), ma.rowsum)
     
             dist.sum <- data.frame(dist.sum)
             names(dist.sum) <- row.names(dist.sum) <- dist.name
             dist.sum <- round(dist.sum, digits)
     
             writeLines(paste("\n", dimnames(get(mat, envir=sys.parent()))[[3]][i],
                 "              destination groups", sep=""), con=outfile)
             writeLines("origin", con=outfile)
             writeLines("groups", con=outfile)
     
             sink(outfile, append=T)
             print(dist.sum)
             sink()
         }
       }
     
     # Process 2d full matrix
       if(type == "2d") {
     
         collapse.row <- apply(get(mat, envir=sys.parent()), 2,
             function(x) tapply(x, districts[ens], sum))
         dist.sum <- t(apply(t(collapse.row), 2,
             function(x) tapply(x, districts[ens], sum)))
     
         ma.rowsum <- append(apply(dist.sum, 1, sum), sum(dist.sum))
         ma.colsum <- apply(dist.sum, 2, sum)
         dist.sum <- cbind(rbind(dist.sum, ma.colsum), ma.rowsum)
     
         dist.sum <- data.frame(dist.sum)
         names(dist.sum) <- row.names(dist.sum) <- dist.name
         dist.sum <- round(dist.sum, digits)
     
         writeLines("\n              destination groups", con=outfile)
         writeLines("origin", con=outfile)
         writeLines("groups", con=outfile)
     
         sink(outfile, append=T)
         print(dist.sum)
         sink()
     
       }
     
     # Process 1d mo/md (array)
       if (type == "1d") {
     
         foo <- data.frame(get(mat, envir=sys.parent()))
         goo <- data.frame(districts[ens])
         dist.sum <- apply(foo, 2, function(x) tapply (x, goo, sum))
     
         ms.totsum <- sum(dist.sum)
         dist.sum <- rbind(dist.sum, ms.totsum)
     
         dist.sum <- data.frame(dist.sum)
         row.names(dist.sum) <- dist.name
         names(dist.sum) <- "ALL"
     
         dist.sum <- round(dist.sum, digits)
         dist.sum <- round(dist.sum, digits)
     
         writeLines("\nzones", con=outfile)
         sink(outfile, append=T)
         print(dist.sum)
         sink()
     
       }
     
       #Close output file and reset output width
       close(outfile)
       options(oldOptions)

}

##/
#balance - ipf 2D balancing function from Metro
#This function uses a balancing scheme similar to the one in Emme2
#@param mfMat - matrix to balance
#@param moTarget - row target margins
#@param mdTarget - column target margins
#@param error - acceptable error convergence factor
#@param lmax - maximum number of iterations
#@return mfMat - adjusted (balanced) matrix
##/
fun$balance <- function(mfMat, moTarget, mdTarget, error, lmax) {
    iter <- 1
    checksum <- 1
    while(checksum >= error || iter <= lmax) {
        moRowSum <- apply(mfMat, 1, sum)
        moColSum <- apply(mfMat, 2, sum)
        rowFactor <- moTarget/moRowSum
        colFactor <- mdTarget/moColSum
        rowFactor <- matrix(rowFactor, length(rowFactor), length(rowFactor))
        colFactor <- matrix(colFactor, length(colFactor), length(colFactor), byrow=T)
        mfMat <- mfMat * rowFactor * colFactor
        mfMat[is.na(mfMat)] <- 0
        moRowSum <- apply(mfMat, 1 ,sum)
        checksum <- (moTarget - moRowSum) / moTarget
        checksum[is.na(checksum)] <- 0
        iter <- iter + 1
    }
    mfMat
}

##/
#rDoc - function to parse R code
#Ben Stabler, 4/27/04, benjamin.stabler@odot.state.or.us
#Fixed levelOne and levelTwo regular expressions to work with tabs as well
#Fixed 4/27/04 a whitespacing issue for file level parameters.
#@param inFileName - input R file to parse for rDoc info
#@param outFileName - html file to save rDoc descriptive output to
#@return null
##/
fun$rDoc <- function(inFileName, outFileName) {
    #Read in file
    input <- scan(inFileName, what="", sep="\n", quiet=T)

    #Replace < and > with &lsaquo; and &rsaquo; for HTML
    #Remove whitespace and tab at beginning and end of line
    input <- gsub("<","&lt;", input)
    input <- gsub(">","&gt;", input)
    input <- gsub("^ +| +$", "", input)
    input <- gsub("^\t+|\t+$", "", input)

    #Get line number of symbols
    regular <- grep("^ *#|^\t#", input)
    levelOne <- grep("^ *#/|^\t#/", input)
    levelTwo <- grep("^ *##/|^\t##/", input)

    #Determine if document is well formed
    levelOneWellFormed <- !as.logical(length(levelOne)%%2)
    levelTwoWellFormed <- !as.logical(length(levelTwo)%%2)
    if(levelOneWellFormed | levelTwoWellFormed) {
        #Document is well formed

        outFile <- file(outFileName, "wt")
        writeLines("<html>", outFile)
        writeLines("<head>", outFile)
        writeLines("<title>rDoc Output</title>", outFile)
        writeLines("<meta name=\"author\" content=\"Ben Stabler, ODOT\">", outFile)
        writeLines("</head>", outFile)
        #Begin visible part of file
        writeLines("<body bgcolor=\"#FFFFFF\" text=\"#000000\">", outFile)

        #HEADER
        if(length(levelOne) > 1) {
            #filename
            fNameLine <- grep("@filename", input)
            baF <- strsplit(input[fNameLine], "@filename")[[1]]
            fName <- strsplit(baF[2], " +")[[1]]
            fName <- paste(fName[2:length(fName)], collapse=" ")
            fName <- paste("<b>", fName, "</b>", collapse="")
            writeLines(fName, outFile, sep="<br>\n")
            #author
            authorLine <- grep("@author", input)
            baA <- strsplit(input[authorLine], "@author")[[1]]
            author <- strsplit(baA[2], " +")[[1]]
            author <- paste(author[2:length(author)], collapse=" ")
            writeLines(author, outFile, sep="<br>\n")
            #version and date
            versionLine <- grep("@version", input)
            baV <- strsplit(input[versionLine], "@version")[[1]]
            version <- strsplit(baV[2], " +")[[1]]
            dateLine <- grep("@date", input)
            baD <- strsplit(input[dateLine], "@date")[[1]]
            dateL <- strsplit(baD[2], " +")[[1]]
            version <- paste("version: ", version[2:length(version)], collapse=" ")
            version <- paste(version, ", " , dateL[2:length(dateL)], sep="", collapse="")
            writeLines(version, outFile, sep="<br>\n")

            #Blank line, header comments, blank line
            writeLines(paste("rDoc file created: ", date()), outFile, sep="<br>\n")
            writeLines("", outFile, sep="<br>\n")
            comments <- input[(levelOne[1]+1):(levelOne[2]-1)]
            comments <- comments[- (grep("@author|@date|@version|@filename|@param|@return",comments))]
            comments <- gsub("#","",comments)
            comments <- paste(comments, "<br>", sep="")
            writeLines(comments, outFile)
            writeLines("", outFile, sep="<br>\n")
        }

        #Start LEVEL ONE loop
        oneStarts <- levelOne[seq(1,length(levelOne),by=2)]
        oneEnds <- levelOne[seq(2,length(levelOne),by=2)]

        if(length(levelOne) > 3) {
            for(i in 2:(length(levelOne)/2)) {
                currentLines <- input[(oneStarts[i]+1):(oneEnds[i]-1)]
                currentLines <- currentLines[grep("#", currentLines)]
                currentLines <- gsub("#/","", currentLines)
                currentLines <- gsub("#", "", currentLines)
                currentLines[1] <- paste("<h2>", currentLines[1], "</h2>", collapse="")
                writeLines("<hr>", outFile, sep="\n")
                writeLines(currentLines, outFile, sep="\n")
                writeLines("<br>", outFile, sep="<br>\n")

                    #Start LEVEL TWO LOOP
                    cLevelTwos <- levelTwo[levelTwo > oneEnds[i] &
                        ((levelTwo < oneStarts[i+1]) | (oneEnds[i] == levelOne[length(levelOne)])) ]

                    if(length(cLevelTwos) > 0) {
                        twoStarts <- cLevelTwos[seq(1,length(cLevelTwos),by=2)]
                        twoEnds <- cLevelTwos[seq(2,length(cLevelTwos),by=2)]

                        for(j in 1:length(twoStarts)) {
                            currentLines <- input[(twoStarts[j]+1):(twoEnds[j]-1)]
                            currentLines <- currentLines[grep("#", currentLines)]
                            currentLines <- gsub("##/","", currentLines)
                            currentLines <- gsub("#", "", currentLines)

                            #param and return lines
                            paramLines <- currentLines[grep("@param", currentLines)]
                            returnLines <- currentLines[grep("@return", currentLines)]
                            paramLines <- gsub("@param", "", paramLines)
                            returnLines <- gsub("@return", "", returnLines)

                            #Remove param and return lines
                            if(length(paramLines) > 0) {
                                currentLines <- currentLines[- grep("@param", currentLines)]
                            }
                            if(length(returnLines) > 0) {
                                currentLines <- currentLines[- grep("@return", currentLines)]
                            }
                            currentLines[1] <- paste("<b>", currentLines[1], "</b>", collapse="")
                            writeLines(currentLines, outFile, sep="<br>\n")
                            writeLines("<br><i>Parameters:</i><blockquote>", outFile, sep="\n")
                            writeLines(paramLines, outFile, sep="<br>\n")
                            writeLines("</blockquote>", outFile, sep="\n")
                            writeLines("<i>Return:</i><blockquote>", outFile, sep="\n")
                            writeLines(returnLines, outFile, sep="<br>\n")
                            writeLines("</blockquote>", outFile, sep="\n")
                        }
                    }
            }

        }
        #End output
        writeLines("</table></html>", outFile)

        #Function does not return anything.  Instead it saves a rDoc file
        close(outFile)
        cat(paste("\nrDoc file created for: ", inFileName, "\n\n", sep=""))

    } else {
        #Can't create documentation since rDoc not well formed
        stop("Can't create documentation since rDoc not well formed")
    }

#End of rDoc function
}

##/
#rDocRecursive function
#Function to apply rDoc to all .R files in all
#the subdirectories and the top level directory.
#rDoc output files are named by replacing .R with .html
#@param folder - root directory reference
#@return null
##/
fun$rDocRecursive <- function(folder=getwd(), rDocFun=rDoc) {
     rFiles <- dir(folder, recursive=T)
     rFiles <- rFiles[grep(".R$", rFiles)]
     cat("Files to rDoc:\n\n")
     print(rFiles)
     invisible(sapply(rFiles, function(x) rDocFun(x, gsub(".R$", ".html", x))))
}

##/
#Function to read utility defintion csv file
#Ben Stabler, 3/29/04, benjamin.stabler@odot.state.or.us
#@param inFileName - input csv file
#@return  inFileName - returns object to workspace with same name as input file
#The format of the CSV file is as follows:
#Rows are by purpose and there are two columns:
#    the first is "purpose" and is the trip purpose name
#    the second is "utility" and is the utility definition
#The variables identified in the utility definitions must be named the same
#as the inputs read in in inputsSave.R or as those created by the model such as
#logSum.
##/
fun$readUtils <- function(inFileName) {
     utils <- read.csv(inFileName)
     #Create a vector of named utility definitions
     result <- as.character(utils[,2])
     result <- gsub(" +|\t", " ", result)
     names(result) <- utils[,1]
     result
}

##/
#Function to parse variable names from a utility definition as an R expression stored as a string
#Variable names cannot have numbers in them - replace numbers with letter 1 to A, 2 to B, etc.
#@param rString - R expression stored as a string
#@return words - variable names in expression
##/
fun$getVarNames <- function(rString) {
     #Replace punctuation characters such as +,(,^ with " "
     rString <- gsub("[+]|[*]|[(]|[)]|[-]|[@^]|[!]", " ", rString)
     #Remove all numbers including decimals
     rString <- gsub("[0-9]+.[0-9]+|[0-9]+", "", rString)
     #Split on white space
     words <- strsplit(rString, "[[:space:]]")[[1]]
     words <- words[words != ""]
     #Check if object exists and if it does check to see if it is a function
     objExist <- sapply(words, exists)
     #Test if object is a function so it is not loaded
     objFunction <- sapply(words[objExist], function(x) is.function(get(x)))
     objFunction <- objFunction[objFunction == T]
     #Return variable names
     words[!(words %in% names(objFunction))]
}

##/
#Function to read market segment coefficient matrix
#Ben Stabler, 3/30/04, benjamin.stabler@odot.state.or.us
#@param inFileName - input csv file
#@return  inFileName - returns object to workspace with same name as input file
#The format of the CSV file is as follows:
#The first row contains the column names which are:
#choice,cval0,cval1,cval2,cval3,h1,h2,h3,h4,w1,w2,w3,w4
#The remaining rows represent the coefficients for each mode or purpose.
##/
fun$readCoeffs <- function(inFileName) {
     #read in the data and the first two rows separately
     coeffs <- read.csv(inFileName)
     #name the rows with the values of column one and remove column one
     rownames(coeffs) <- coeffs[,1]
     coeffs <- coeffs[,2:ncol(coeffs)]
     coeffs
}

##/
#Function to verify variable names
#Ben Stabler, 4/16/04, benjamin.stabler@odot.state.or.us
#@param utilDefVectorName - string name of utility defintion object
#@param varDictionary - variable dinctionary object
#@return  null
#Function call example: verifyVarName("hbwModeUtils",varDictionary)
#The format of the CSV file is as follows:
#The first row is column names - "variable" and "description"
#The remaining rows are variable names and descriptions.
##/
fun$verifyVarName <- function(utilDefVectorName, varDictionary) {

     varNames <- sapply(get(utilDefVectorName), getVarNames)
     varNames <- unique(unlist(varNames))

     varDefined <- varNames[varNames %in% varDictionary$variable]
     varUndefined <- varNames[!(varNames %in% varDictionary$variable)]

     if(length(varUndefined) > 0) {
          cat(paste("\nThe following variables defined in ", utilDefVectorName,
               " were not found in the variable dictionary\n\n", sep=""))
          cat(paste(paste(varUndefined, "\n", collapse="", sep=""), "\n"))
          stop("Variables not found",call.=FALSE)

      } else {

          cat(paste("\nAll variables in ", utilDefVectorName,
               " found in the variable dictionary\n\n", sep=""))
      }
}

##/
#Function to verify mode names
#Ben Stabler, 4/16/04, benjamin.stabler@odot.state.or.us
#@param utilDefVectorName - string name of utility defintion object
#@param modes - available modes for model
#@return  null
#Function call example: verifyModeName("hbwModeUtils",modes)
#Modes can be specified in the utility files by income -
#for example driveAlonelowInc.  The function removes
#lowInc|midInc|highInc from the name before checking mode
#names thus supporting mode names by income
##/
fun$verifyModeName <- function(utilDefVectorName, modes) {

     modeNames <- names(get(utilDefVectorName))
     #Drops the income class subtitle
     modeNames <- gsub("lowInc|midInc|highInc","", modeNames)
     modeNames <- unique(unlist(modeNames))

     modeDefined <- modeNames[modeNames %in% modes$mode]
     modeUndefined <- modeNames[!(modeNames %in% modes$mode)]

     if(length(modeUndefined) > 0) {
          cat(paste("\nThe following modes defined in ", utilDefVectorName,
               " were not found in the modes vector\n\n", sep=""))
          cat(paste(paste(modeUndefined, "\n", collapse="", sep=""), "\n"))
          stop("Modes not found",call.=FALSE)

      } else {

          cat(paste("\nAll modes in ", utilDefVectorName,
               " found in the modes vector\n\n", sep=""))
      }
}

##/
 #Function to read in EMME/2 matrix
 #This reads EMME/2 zone-to-zone batch out file in the following format
 #from to value
 #  x  y: zzzz
 #After reading the file, a matrix with the from zones as rows and to zones as columns
 #Adapted from OSUM Code, 11/10/03
 #Requires flat.to.mat function
 #@param emme2.file - EMME/2 file name
 #@return value.matrix - data in matrix format
##/
fun$readEmme2 <- function(emme2.file) {
    emme2.in <- scan(emme2.file, skip=4, what = list(0, "", 0))
    value.by.orig<-tapply(emme2.in[[3]],emme2.in[[1]],function(x) x)
    
    #Does file have all OD pairs (including intrazonal)?
    if( all(unlist(lapply(value.by.orig, length))==length(names(value.by.orig))) ) { 
        #Yes
        numZones <- length(value.by.orig)
        value.matrix <- matrix(unlist(value.by.orig), numZones, numZones, byrow=T)
        rownames(value.matrix) <- colnames(value.matrix) <- unique(emme2.in[[1]])
    } else {
        #No
        rows <- length(emme2.in[[1]])
        emme2Df <- data.frame(From=rep("", rows), To=rep("", rows), Value=rep(0, rows))
        emme2Df$From <- as.character(emme2.in[[1]])
        emme2Df$To <- gsub(":","",as.character(emme2.in[[2]]))
        emme2Df$Value <- emme2.in[[3]]
        value.matrix <- flat.to.mat(emme2Df, names(value.by.orig), 0)
    }
    value.matrix
}

##/
# Convert a data frame in the form of "from zone", "to zone", "value" into a square matrix
# @param input.frame - a data frame with three columns
# @param dists.or.zones - a vector of names of the districts or zones for the matrix
# @param empty - default empty cell value (usually zero or one)
# @return out.mat - data as a matrix
# For argument one the data frame must have the following columns:
#   first column is the from zone or district name (a character type)
#   second column is the to zone or district name (a character type)
#   third column is the values for the corresponding cells of the matrix.
# Source: OSUM Code, 11/10/03
##/
fun$flat.to.mat <- function(input.frame, dists.or.zones, empty){
    rowcol.names <- as.character(sort(dists.or.zones))
    mat.dim <- length(dists.or.zones)
    out.mat <- matrix(empty, mat.dim, mat.dim)
    rownames(out.mat) <- colnames(out.mat) <- rowcol.names
    if(nrow(input.frame)!=0) apply(input.frame, 1, function(x) out.mat[x[1], x[2]] <<- as.numeric(x[3]))
    out.mat
}




