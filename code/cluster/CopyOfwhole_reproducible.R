# This scripts automatically generate the results of sensitivity  analysis
# and travel cost with employment centres methods 

# Set workspace
setwd("~/tci")
var_list.0 <- ls()

source("code/cluster/settings.R")
source("code/cluster/def_functions.R")

# Load required functions

for (cutoff.p in c(.5, .6, .7, .8, .9,  .95) ){
  for (sum.cutoff.p in c(.25, .5, .75)) {
    cutoffs <- data.frame(hbw=c(cutoff.p, sum.cutoff.p), 
                          hbs=c(cutoff.p, sum.cutoff.p), 
                          hbr=c(cutoff.p, sum.cutoff.p), 
                          hbo=c(cutoff.p, sum.cutoff.p))
    row.names(cutoffs) <- c("cutoff.percentile", 'sum.cutoff.percentile')
    
    # change the output and intermediate directory 
    OUTPUT_DIR <- paste("output/cluster/cutoffs/", "cutoffp", cutoff.p, "sum.cutoff.p", sum.cutoff.p, sep="")
    dir.create(file.path(OUTPUT_DIR), recursive = TRUE, showWarnings = FALSE)
        
    INTERMEDIATE_DIR <- paste("output/intermediate/cluster/cutoffs/",  cutoff.p, "sum.cutoff.p", sum.cutoff.p, sep="")
    dir.create(file.path(INTERMEDIATE_DIR), recursive = TRUE, showWarnings = FALSE)
    
    #source("code/cluster/identify_centers.R")
    source("code/cluster/compute_md_prob_trips.R")
    source("code/cluster/compute_tcost.R")
    source("code/cluster/aggregate_tcost.R")
    
    
    # convert array into data.frame
    
    df <- data.frame(minpeakAggCost.Zi, minoffpeakAggCost.Zi, minAggTpCost.Zi, 
                     weightedpeakAggCost.Zi, weightedoffpeakAggCost.Zi, weightedAggTpCost.Zi)
    
    title.name <- paste("density.cutoff = ", dc, "and total.cutoff = ", tc, sep=" ")
    
    
    
    ds <- stargazer(df, type = "text", title=title.name, 
                    summary.stat = c("n","min", "p25", "median", "mean","sd", "p75", "max"), digits=4)
    
    
    #df.name <- paste("df", den.tot.cutoff, sep="")
    #assign(df.name, df)
    
    remove(minpeakAggCost.ZiIcPr, minpeakAggCost.Zi, minoffpeakAggCost.ZiIcPr, minoffpeakAggCost.Zi, 
           weightedpeakAggCost.ZiIcPr, weightedpeakAggCost.Zi, weightedoffpeakAggCost.ZiIcPr, weightedoffpeakAggCost.Zi,
           minAggTpCost.Zi, weightedAggTpCost.Zi)
    
    df <- data.frame(ds)
    colnames(df) <- "Descriptive statistics"
    
    # store all data.frame into one data.frame
    
    if(is.null(all.df)) {
      all.df <- df
    }
    
    else{ 
      all.df <- cbind(all.df, df)
      
    }
    
  }
  
}


OUTPUT_DIR <- 'output/cluster'
output.file <- file.path(OUTPUT_DIR, 'all_df_putty.RData')
save(all.df, file=output.file)




#var_list.1 <- ls()
#rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
#rm(var_list.1)
