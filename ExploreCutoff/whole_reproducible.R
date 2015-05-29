# This scripts automatically generate the results of sensitivity  analysis
# and travel cost with employment centres methods 

# Set workspace
setwd("~/tci")
var_list.0 <- ls()

source("code/cluster/settings.R")

# Load required functions

for (cutoff.p in c(.5, .75, .95) {
  sum.cutoff.p in c(.5, .75, .95){
    cutoffs <- data.frame(hbw=c(cutoff.p, sum.cutoff.p), 
                          hbs=c(cutoff.p, sum.cutoff.p), 
                          hbr=c(cutoff.p, sum.cutoff.p), 
                          hbo=c(cutoff.p, sum.cutoff.p))
    row.names(cutoffs) <- c("cutoff.percentile", 'sum.cutoff.percentile')
    
    OUTPUT_DIR <- paste("output/cluster/", "cutoffp", cuttoff.p, "sum.cutoff.p", sum.cutoff.p)
    
    source("ExploreCutoff/cluster/identify_centers.R")
    source("ExploreCutoff/cluster/compute_md_prob_trips.R")
    source("ExploreCutoff/cluster/compute_tcost.R")
    source("ExploreCutoff/cluster/aggregate_tcost.R")
    
  }
  
}




# combine all total cutoff values into array 
den.cutoff.values <- data.frame(hbw=c(170, 316,529,1010,2249,4138),hbs=c(30,54,102,218,545,1060),
                                hbr=c(1085,1375,1761,2238,3372,5494), hbo=c(251,353,496,766,1473,2553), row.names=c("50","60","70","80","90","95"))


str(den.cutoff.values)
den.cutoff.values["50","hbw"]*5


# combine all total cutoff values into array

hbw.tot.cutoff.values <- rbind(
  "50"=c("25"=667, "50"=2229, "75"=5285),
  "60"=c("25"=1550, "50"=3903, "75"=14188),
  "70"=c("25"=2534, "50"=4819, "75"=9672),
  "80"=c("25"=2840, "50"=5165, "75"=12083),
  "90"=c("25"=3780, "50"=6584, "75"=10805),
  "95"=c("25"=8171, "50"=7805, "75"=9899))


hbs.tot.cutoff.values <- rbind(
  "50"=c("25"=600, "50"=1100, "75"=1594),
  "60"=c("25"=322, "50"=815, "75"=1660),
  "70"=c("25"=458, "50"=1134, "75"=3240),
  "80"=c("25"=593, "50"=1030, "75"=2616),
  "90"=c("25"=955, "50"=1845, "75"=3828),
  "95"=c("25"=694, "50"=1725, "75"=3124))


hbr.tot.cutoff.values <- rbind(
  "50"=c("25"=4567, "50"=7723, "75"=22201),
  "60"=c("25"=3777, "50"=7306, "75"=13519),
  "70"=c("25"=3239, "50"=6483, "75"=13077),
  "80"=c("25"=2850, "50"=6128, "75"=11056),
  "90"=c("25"=2746, "50"=6278, "75"=13772),
  "95"=c("25"=6848, "50"=9089, "75"=12932))


hbo.tot.cutoff.values <- rbind(
  "50"=c("25"=1433, "50"=2209, "75"=7268),
  "60"=c("25"=1395, "50"=2439, "75"=6592),
  "70"=c("25"=1874, "50"=3532, "75"=9055),
  "80"=c("25"=1348, "50"=3399, "75"=7554),
  "90"=c("25"=2089, "50"=3390, "75"=6886),
  "95"=c("25"=2768, "50"=3684, "75"=6601))

tot.cutoff.values <- array(c(hbw.tot.cutoff.values, hbs.tot.cutoff.values, hbr.tot.cutoff.values, hbo.tot.cutoff.values),
                           dim=c(6,3,4), dimnames=list(c("50","60","70","80","90","95"), c("25","50","75"), c("hbw","hbs", "hbr", "hbo")))

# definde the percentile of cutoffs
den.cutoff.percentage <- c("50", "60", "70", "80", "90","95")
tot.cutoff.percentage <- c("25","50","75")
all.df <- NULL

for (dc in den.cutoff.percentage) {
  
  for (tc in tot.cutoff.percentage) {
    
    den.tot.cutoff.percentage <- paste(dc, tc, sep="_")
    
    source("ExploreCutoff/cluster/identify_centers.R")
    source("ExploreCutoff/cluster/compute_md_prob_trips.R")
    source("ExploreCutoff/cluster/compute_tcost.R")
    source("ExploreCutoff/cluster/aggregate_tcost.R")
    
    if (SAVE.INTERMEDIARIES) {
      intm.file <- file.path(INTERMEDIATE_DIR, paste("results", den.tot.cutoff.percentage, ".RData", sep=""))
      save(minpeakAggCost.Zi, minoffpeakAggCost.Zi, minAggTpCost.Zi, 
           weightedpeakAggCost.Zi, weightedoffpeakAggCost.Zi, weightedAggTpCost.Zi, file=intm.file)
    }
    
    
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


print（all.df）

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "all_df.RData")
  save(all.df,file=intm.file)
}



var_list.1 <- ls()
rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
rm(var_list.1)
