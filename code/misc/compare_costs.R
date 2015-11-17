# This script investigates whether the costs calculated by different methods are different with paired t-test,
# and generate plots. 

# Set workspace
  setwd("~/tci")

# load cost 
#::

# Original TCI cost
  load("data/TCIPortland50/performancemeasures/final_output/tci/AveMarketCost.Zi.RData")
  load("data/TCIPortland50/performancemeasures/final_output/tci/BestMarketCost.Zi.RData")
  load("data/TCIPortland50/performancemeasures/final_output/tci/CompMarketCost.Zi.RData")

# OHAS cost
  load("data/OHASTTime/results/Cost.Zi.RData")
  OHASCost.Zi <- Cost.Zi

# Load employment centers cost
  load("data/CenTTime/results/aggcostcmtp/weightedoffpeakAggCost.Zi.RData")
  load("data/CenTTime/results/aggcostcmtp/weightedpeakAggCost.Zi.RData")



 
# Calculate WeightedAggCost.Zi. Current trip data do not distinguish peak and offprak trips, 
# WeightedAggCost.Zi is averge of weightedoffpeakAggCost.Zi and weightedpeakAggCost.Zi
  WeightedAggCost.Zi <- (weightedoffpeakAggCost.Zi+weightedpeakAggCost.Zi)/2


# Paired t-test 
# Weighted represents WeightedAggCost.Zi; OHAS represents OHASCost.Zi; 
# Ave represents AveMarketCost.Zi; Best represents BestMarketCost.Zi. 
  
  Weighted_OHAS <- t.test(WeightedAggCost.Zi, OHASCost.Zi,paired=TRUE)
  Weighted_Ave <- t.test(WeightedAggCost.Zi, AveMarketCost.Zi, paired=TRUE)
  Weighted_Best <- t.test(WeightedAggCost.Zi,  BestMarketCost.Zi, paired=TRUE)
  OHAS_Ave <- t.test(OHASCost.Zi, AveMarketCost.Zi,paired=TRUE)
  OHAS_Best <- t.test(OHASCost.Zi, BestMarketCost.Zi, paired=TRUE)
  Ave_Best <- t.test(AveMarketCost.Zi, BestMarketCost.Zi, paired=TRUE)

# Visualization 
  # Load saveGraph function 
    source("code/thirdparty/openGraphSaveGraph.R")
  

  # create required data frame

    AllCost.df <- data.frame(cost = c(WeightedAggCost.Zi,OHASCost.Zi, AveMarketCost.Zi,BestMarketCost.Zi,CompMarketCost.Zi),
                         method=factor(rep(c("Weighted", "OHAS", "Ave", "Best","Comp"),rep(2162,5))))

    AllCost.df$cost[is.na(AllCost.df$cost)] <- 0
     
  # reoder factor
  #http://stackoverflow.com/questions/18413756/re-ordering-factor-levels-in-data-frame
    AllCost.df$method <- factor(AllCost.df$method, levels=c("Weighted", "OHAS", "Ave", "Best","Comp"))
  
    attach(AllCost.df)

  # compare cost with density line plot 
  # http://www.statmethods.net/graphs/density.html
    # create value labels
    method.f <- factor(method, levels= c("Weighted", "OHAS", "Ave", "Best","Comp"), 
                   labels=c("WeightedAggCost", "OHASCost", "AveMarketCost", "BestMarketCost", "CompMarketCost"))
  # load required package
    library(sm)
  
  # plot densities
    sm.density.compare(cost, method, xlab="Cost")
    title(main="Cost distribution")
    
  # add legend via mouse click
    colfill <- c(2:(2+length(levels(method.f))))
    legend(locator(1), levels(method.f), fill=colfill)
   
    saveGraph(filename="graphics/comparecost/densityline", type="pdf")

    detach(AllCost.df)
 
  # boxplot 
   boxplot(cost~method,data=AllCost.df, main="Boxplot of cost", xlab="Method", ylab="Cost", cex.lab=1.2)
   
   saveGraph(filename="graphics/comparecost/boxplot", type="pdf")
  
  # violin plot 

  WeightedAggCost.Zi[is.na(WeightedAggCost.Zi)] <- 0
  OHASCost.Zi[is.na(OHASCost.Zi)] <- 0
  AveMarketCost.Zi[is.na(AveMarketCost.Zi)] <- 0
  BestMarketCost.Zi[is.na(BestMarketCost.Zi)] <- 0
  CompMarketCost.Zi[is.na(CompMarketCost.Zi)] <- 0

  library(vioplot)
  
  vioplot(WeightedAggCost.Zi, OHASCost.Zi,AveMarketCost.Zi, BestMarketCost.Zi,CompMarketCost.Zi, 
        names=c("Market", "OHAS", "Ave", "Best", "Comp"), col="gold")
  
  title("Violin plots of cost")
  
  saveGraph(filename="graphics/comparecost/violinplot", type="pdf")
  
  
