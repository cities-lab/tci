# This scripts identify linked trips, not place-based trips, by income groups and trip purposes

# Load required packages
require(dplyr)
require(tidyr)
require(reshape2)
require(SDMTools)

tcost.trip <- tcost.trip %>% 
  left_join(unitcosts) %>%                    #append unit travel cost by mode (and potentially by inc.level)
  mutate(t.cost=VOT*tripdur.hours,            #time costs
         m.cost=mcpm*tripdist.miles,          #monetary costs
         tcost= constant + t.cost + m.cost) %>%        #total costs
  na.omit()                                   #exclude rows with unknown HTAZ, tpurp, or inc.level


# calculate household-level travel cost
tcost.hh <- tcost.trip %>%
  group_by(SAMPN) %>%
  summarise(tcost=sum(tcost),
            HTAZ=first(HTAZ),                 #retain HTAZ, inc.level and HHWGT
            HHSIZ=first(HHSIZ),
            inc.level=first(inc.level),
            INCOME=first(INCOME),
            HHWGT=first(HHWGT),
            district.id=first(district.id)
  )

tcost.hh.tpurp <- tcost.trip %>%
  group_by(SAMPN, TripPurpose) %>%
  summarise(tcost=sum(tcost),
            HTAZ=first(HTAZ),
            HHSIZ=first(HHSIZ),
            inc.level=first(inc.level),
            HHWGT=first(HHWGT),
            district.id=first(district.id)
  )


# summarize trip-level tcost
tcost.HTAZ.tpurp.inc <- compute_tcost(tcost.hh.tpurp, by=c("HTAZ", "TripPurpose", "inc.level"), summarize_tcost)
print(tcost.HTAZ.tpurp.inc)

tcost.tpurp.inc <- compute_tcost(tcost.hh.tpurp, by=c("TripPurpose", "inc.level"), summarize_tcost, w="HHWGT")
print(tcost.tpurp.inc)

tcost.tpurp <- compute_tcost(tcost.hh.tpurp, by="TripPurpose", summarize_tcost, w="HHWGT")
print(tcost.tpurp)

# summarize household-level travel cost by taz and/or income level
tcost.HTAZ.inc <- compute_tcost(tcost.hh, by=c("HTAZ", "inc.level"), summarize_tcost, w="HHWGT")
print(tcost.HTAZ.inc)

tcost.HTAZ <- compute_tcost(tcost.hh, by=c("HTAZ"), summarize_tcost, w="HHWGT")
print(tcost.HTAZ)

tcost.inc <- compute_tcost(tcost.hh, by="inc.level", summarize_tcost, w="HHWGT")
print(tcost.inc)


# summarize household-level travel cost by district, TripPurpose and income level
tcost.distr.tpurp.inc <- compute_tcost(tcost.hh.tpurp, by=c("district.id", "TripPurpose", "inc.level"), summarize_tcost, w="HHWGT")
print(tcost.distr.tpurp.inc)

tcost.distr.tpurp <- compute_tcost(tcost.hh.tpurp, by=c("district.id", "TripPurpose"), summarize_tcost, w="HHWGT")
print(tcost.distr.tpurp)

tcost.distr.inc <- compute_tcost(tcost.hh, by=c("district.id", "inc.level"), summarize_tcost, w="HHWGT")
print(tcost.distr.inc)

# summarize household-level travel cost by district
tcost.distr <- compute_tcost(tcost.hh, by="district.id", summarize_tcost, w="HHWGT")
print(tcost.distr)

# summarize overall household-level travel cost  
tcost.all <- compute_tcost(tcost.hh %>% mutate(all=1), by=c("all"), summarize_tcost, w="HHWGT")
print(tcost.all)

output.file <- file.path(OUTPUT_DIR, "tcost.RData")
save(tcost.HTAZ.tpurp.inc, tcost.hh, tcost.HTAZ.inc, tcost.HTAZ, tcost.distr, tcost.all, tcost.hh.tpurp, 
     tcost.tpurp.inc,tcost.trip, tcost.distr.tpurp.inc, tcost.distr.tpurp, tcost.distr.inc, file=output.file)

#reshape data frame into arrays for plotting
#tcost by HTAZ, inc.level, and tpurp
mintcost.ZiIcPr <- acast(tcost.HTAZ.tpurp.inc, HTAZ~inc.level~TripPurpose, value.var="tcost.min")
avgtcost.ZiIcPr <- acast(tcost.HTAZ.tpurp.inc, HTAZ~inc.level~TripPurpose, value.var="tcost.avg")
maxtcost.ZiIcPr <- acast(tcost.HTAZ.tpurp.inc, HTAZ~inc.level~TripPurpose, value.var="tcost.max")

#tcost by HTAZ, inc.level
minhhtcost.ZiIc <- acast(tcost.HTAZ.inc, HTAZ~inc.level, value.var="tcost.min")
avghhtcost.ZiIc <- acast(tcost.HTAZ.inc, HTAZ~inc.level, value.var="tcost.avg")
maxhhtcost.ZiIc <- acast(tcost.HTAZ.inc, HTAZ~inc.level, value.var="tcost.max")

#tcost by HTAZ
flat.tcost.HTAZ <- dplyr::select(tcost.HTAZ, HTAZ, min=tcost.min, avg=tcost.avg, max=tcost.max) %>%
                   gather(func, value, -HTAZ)
hhCost.ZiCm <- acast(flat.tcost.HTAZ, HTAZ~func, value.var="value") #could use spread
