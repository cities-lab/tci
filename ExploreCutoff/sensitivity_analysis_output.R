# This script prepares the workspace and file directories for calculating travel time 
# and travel cost with employment centres methods 

# Set workspace
setwd("~/tci")
var_list.0 <- ls()

source("code/cluster/settings.R")


# define density cutoff and total cutoff

  den.cutoff <- c(50,60,70,80,90,95)
  tot.cutoff <- c(25,50,75)
  all.df <- data.frame(c(1:2162))
  
for (dc in den.cutoff) {
  
  for (tc in tot.cutoff) {
    
    # load results of specific cutoffs
    den.tot.cutoff <-  paste(dc, tc, sep="_")
    load(file.path(INTERMEDIATE_DIR, paste("results", den.tot.cutoff, ".RData", sep="")))
    
    
    # rename the calculation results with specific cutoffs 
    minpeakAggCost.Zi.name <- paste("minpeakAggCost", den.tot.cutoff, ".Zi", sep="")
    assign(minpeakAggCost.Zi.name, minpeakAggCost.Zi)
    
    minoffpeakAggCost.Zi.name <- paste("minoffpeakAggCost", den.tot.cutoff, ".Zi", sep="")
    assign(minoffpeakAggCost.Zi.name, minoffpeakAggCost.Zi)
    
    minAggTpCost.Zi.name <- paste("minAggCost", den.tot.cutoff, ".Zi", sep="")
    assign(minAggTpCost.Zi.name, minAggTpCost.Zi)
    
    weightedpeakAggCost.Zi.name <- paste("weightedpeakAggCost", den.tot.cutoff, ".Zi", sep="")
    assign(weightedpeakAggCost.Zi.name, weightedpeakAggCost.Zi)
    
    weightedoffpeakAggCost.Zi.name <- paste("weightedoffpeakAggCost", den.tot.cutoff, ".Zi", sep="")
    assign(weightedoffpeakAggCost.Zi.name, weightedoffpeakAggCost.Zi)
    
    weightedAggTpCost.Zi.name <- paste("weightedAggCost", den.tot.cutoff, ".Zi", sep="")
    assign(weightedAggTpCost.Zi.name, weightedAggTpCost.Zi)
    
    # convert array into data.frame
    
    df <- data.frame(minpeakAggCost.Zi, minoffpeakAggCost.Zi, minAggTpCost.Zi, 
                     weightedpeakAggCost.Zi, weightedoffpeakAggCost.Zi, weightedAggTpCost.Zi)
    
    colnames(df) <- c(minpeakAggCost.Zi.name, minoffpeakAggCost.Zi.name, minAggTpCost.Zi.name, 
                      weightedpeakAggCost.Zi.name, weightedoffpeakAggCost.Zi.name, weightedAggTpCost.Zi.name)
    
    df.name <- paste("df", den.tot.cutoff, sep="")
    assign(df.name, df)
    
    remove(minpeakAggCost.ZiIcPr, minpeakAggCost.Zi, minoffpeakAggCost.ZiIcPr, minoffpeakAggCost.Zi, 
           weightedpeakAggCost.ZiIcPr, weightedpeakAggCost.Zi, weightedoffpeakAggCost.ZiIcPr, weightedoffpeakAggCost.Zi,
           minAggTpCost.Zi, weightedAggTpCost.Zi)
    
    # store all data.frame into one data.frame
    if (dc!=95) {
      x <- (dc - 50)/10
    }
    else{
      
      x <- 5
    }
    y <- tc/25
    col.index <- x*18 + y*6 + 1
    all.df[, (col.index-5) : col.index] <- df  
  
  }
  
}

# delete the first colum of data frame
  all.df <- all.df[,-1]
  str(all.df)

# use stargazer function to export out put 
require(stargazer)
intm.file <- file.path(INTERMEDIATE_DIR, "sensiti_analy_output.txt")

stargazer(all.df, type = "text", title="Descriptive statistics", 
          summary.stat = c("n","min", "p25", "median", "mean","sd", "p75", "max"), digits=4, out=intm.file)


var_list.1 <- ls()
rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
rm(var_list.1) 

