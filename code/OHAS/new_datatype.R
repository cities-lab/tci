# This script reorganize data type of RData calculated by summarize_test.R

# Load required package 
require(dplyr)

# Define income group abbreviation
Ic <- c("lowInc", "midInc", "highInc")

# Define trip purpose abbreviation
# The purposes for this study (now) are limited to the home-based trips
# They exclude nonhome-based trips, school trips and college trips
Pr <- c("hbw", "hbs", "hbr", "hbo")

# define calculate method 

Cm <- c("min", "avg", "max")

# Load zone abbreviation 
load("data/CommonData/Zi.RData")

#### reorganize tcost.htaz.tpurp.inc

### get min tcost 

## hbw
# hbw lowInc
hbwlowIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbw", inc.level=="lowInc")

# hbw midInc 
hbwmidIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbw", inc.level=="midInc")

# hbw highInc 
hbwhighIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbw", inc.level=="highInc")
## hbs 
# hbs lowInc
hbslowIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbs", inc.level=="lowInc")

# hbs midInc 
hbsmidIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbs", inc.level=="midInc")

# hbs highInc 
hbshighIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbs", inc.level=="highInc")

## hbr 
# hbr lowInc
hbrlowIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbr", inc.level=="lowInc")

# hbr midInc 
hbrmidIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbr", inc.level=="midInc")

# hbr highInc 
hbrhighIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbr", inc.level=="highInc")

## hbo 
# hbo lowInc
hbolowIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbo", inc.level=="lowInc")

# hbo midInc 
hbomidIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbo", inc.level=="midInc")

# hbo highInc 
hbohighIncmintcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.min) %>%
  filter(tpurp.ch=="hbo", inc.level=="highInc")

### get avg tcost 

## hbw
# hbw lowInc
hbwlowIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbw", inc.level=="lowInc")

# hbw midInc 
hbwmidIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbw", inc.level=="midInc")

# hbw highInc 
hbwhighIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbw", inc.level=="highInc")
## hbs 
# hbs lowInc
hbslowIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbs", inc.level=="lowInc")

# hbs midInc 
hbsmidIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbs", inc.level=="midInc")

# hbs highInc 
hbshighIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbs", inc.level=="highInc")

## hbr 
# hbr lowInc
hbrlowIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbr", inc.level=="lowInc")

# hbr midInc 
hbrmidIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbr", inc.level=="midInc")

# hbr highInc 
hbrhighIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbr", inc.level=="highInc")

## hbo 
# hbo lowInc
hbolowIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbo", inc.level=="lowInc")

# hbo midInc 
hbomidIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbo", inc.level=="midInc")

# hbo highInc 
hbohighIncavgtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.avg) %>%
  filter(tpurp.ch=="hbo", inc.level=="highInc")


### get max tcost 

## hbw
# hbw lowInc
hbwlowIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbw", inc.level=="lowInc")

# hbw midInc 
hbwmidIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbw", inc.level=="midInc")

# hbw highInc 
hbwhighIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbw", inc.level=="highInc")
## hbs 
# hbs lowInc
hbslowIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbs", inc.level=="lowInc")

# hbs midInc 
hbsmidIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbs", inc.level=="midInc")

# hbs highInc 
hbshighIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbs", inc.level=="highInc")

## hbr 
# hbr lowInc
hbrlowIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbr", inc.level=="lowInc")

# hbr midInc 
hbrmidIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbr", inc.level=="midInc")

# hbr highInc 
hbrhighIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbr", inc.level=="highInc")

## hbo 
# hbo lowInc
hbolowIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbo", inc.level=="lowInc")

# hbo midInc 
hbomidIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbo", inc.level=="midInc")

# hbo highInc 
hbohighIncmaxtcost <- tcost.htaz.tpurp.inc %>% 
  dplyr::select(htaz,tpurp.ch, inc.level, tcost.max) %>%
  filter(tpurp.ch=="hbo", inc.level=="highInc")


## add cost into array 

for (cm in Cm){
  
  Cost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))
  
  #:: Begin iteration by trip purpose 
  for (pr in Pr) { 
    
    # Begin iteration by income group
    for (ic in Ic) {
      
      
      # load trips array 
      Cost <- get(paste(pr, ic, cm, "tcost", sep=""))
      Cost <- as.data.frame(Cost)
      rowindex <- c(1:nrow(Cost))
      
      Cost.Zi <- matrix(0, nrow=2162, ncol=1)
      for (i in rowindex) {
        
        Cost.Zi[Cost[i, 1],1] <- Cost[i,4]
        
      }
      
      # Add cost to array 
      
      Cost.ZiIcPr[,ic,pr] <- Cost.Zi; rm(Cost, Cost.Zi)
      
      # End loop through income group     
    }
    
    # End loop through trip purpose 
  }
  
  
  assign(paste(cm, "ttcost.ZiIcPr", sep=""), Cost.ZiIcPr); rm(Cost.ZiIcPr)
  
       
  # End loop through calculation method 
} 

print("minttcost.ZiIcPr[1:5,,]")
print(minttcost.ZiIcPr[1:5,,])

print("avgttcost.ZiIcPr[1:5,,]")
print(avgttcost.ZiIcPr[1:5,,])

print("maxttcost.ZiIcPr[1:5,,]")
print(maxttcost.ZiIcPr[1:5,,])


## reorganize tcost.htaz.inc

# Order by htaz 
tcost.htaz.inc <- arrange(tcost.htaz.inc, htaz)
tcost.htaz.inc[1:10,]

### calculate min cost 

## lowInc  

lowIncmintcost <- tcost.htaz.inc %>% 
  dplyr::select(htaz, inc.level, tcost.min) %>% 
  filter(inc.level=="lowInc")
## midInc  

midIncmintcost <- tcost.htaz.inc %>% 
  dplyr::select(htaz, inc.level, tcost.min) %>% 
  filter(inc.level=="midInc")
## highInc  

highIncmintcost <- tcost.htaz.inc %>% 
  dplyr::select(htaz, inc.level, tcost.min) %>% 
  filter(inc.level=="highInc")

### calculate avg cost 

## lowInc  

lowIncavgtcost <- tcost.htaz.inc %>% 
  dplyr::select(htaz, inc.level, tcost.avg) %>% 
  filter(inc.level=="lowInc")
## midInc  

midIncavgtcost <- tcost.htaz.inc %>% 
  dplyr::select(htaz, inc.level, tcost.avg) %>% 
  filter(inc.level=="midInc")
## highInc  

highIncavgtcost <- tcost.htaz.inc %>% 
  dplyr::select(htaz, inc.level, tcost.avg) %>% 
  filter(inc.level=="highInc")
### calculate max cost 

## lowInc  

lowIncmaxtcost <- tcost.htaz.inc %>% 
  dplyr::select(htaz, inc.level, tcost.max) %>% 
  filter(inc.level=="lowInc")
## midInc  

midIncmaxtcost <- tcost.htaz.inc %>% 
  dplyr::select(htaz, inc.level, tcost.max) %>% 
  filter(inc.level=="midInc")
## highInc  

highIncmaxtcost <- tcost.htaz.inc %>% 
  dplyr::select(htaz, inc.level, tcost.max) %>% 
  filter(inc.level=="highInc")



# combine the cost into array 

for (cm in Cm){
  
  Cost.ZiIc <- array(0, dim=c(length(Zi), length(Ic)), dimnames=list(Zi,Ic))
  
  
  # Begin iteration by income group
  for (ic in Ic) {
    
    
    # load trips array 
    Cost <- get(paste(ic, cm, "tcost", sep=""))
    Cost <- as.data.frame(Cost)
    rowindex <- c(1:nrow(Cost))
    
    Cost.Zi <- matrix(0, nrow=2162, ncol=1)
    for (i in rowindex) {
      
      Cost.Zi[Cost[i, 1],1] <- Cost[i,3]
      
    }
    
    # Add cost to array 
    
    Cost.ZiIc[,ic] <- Cost.Zi; rm(Cost, Cost.Zi)
    
    # End loop through income group     
  }
  
  assign(paste(cm, "hhtcost.ZiIc", sep=""), Cost.ZiIc); rm(Cost.ZiIc)
  
  # End loop through calculation method 
}

print("minhhtcost.ZiIc[1:5,] ")
print(minhhtcost.ZiIc[1:5,])

print("avghhtcost.ZiIc[1:5,] ")
print(avghhtcost.ZiIc[1:5,]) 

print("maxhhtcost.ZiIc[1:5,]")
print(maxhhtcost.ZiIc[1:5,])


## reorganize tcost.htaz 

# get tcost of different method 

mintcost <- dplyr::select(tcost.htaz, htaz, tcost.min)

avgtcost <- dplyr::select(tcost.htaz, htaz, tcost.avg)

maxtcost <- dplyr::select(tcost.htaz, htaz, tcost.max)


# combine the cost into array 

# initialize an array to store cost 
hhCost.ZiCm <- array(0, dim=c(length(Zi), length(Cm)), dimnames=list(Zi, Cm))


for (cm in Cm){     
  # load trips array 
  Cost <- get(paste(cm, "tcost", sep=""))
  Cost <- as.data.frame(Cost)
  rowindex <- c(1:nrow(Cost))
  
  Cost.Zi <- matrix(0, nrow=2162, ncol=1)
  for (i in rowindex) {
    
    Cost.Zi[Cost[i, 1],1] <- Cost[i,2]
    
  }
  
  # Add cost to array 
  
  hhCost.ZiCm[,cm] <- Cost.Zi; rm(Cost, Cost.Zi)
  
  # End loop through calculation method 
} 

print("hhCost.ZiCm[1:5,]")
print(hhCost.ZiCm[1:5,]) 

# save data 

if(SAVE.INTERMEDIARIES) {
  intm_file = file.path(INTERMEDIATE_DIR, "newtcost.RData")
  save(minttcost.ZiIcPr, avgttcost.ZiIcPr, maxttcost.ZiIcPr, minhhtcost.ZiIc, 
       avghhtcost.ZiIc, maxhhtcost.ZiIc,hhCost.ZiCm, file=intm_file)
}




