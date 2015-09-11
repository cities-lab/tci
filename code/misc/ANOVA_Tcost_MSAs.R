# This script conducts ANOVA test to compare the travel costs of OHAS-Portland, NHTS-Portland, NHTS-TampaBay, NHTS-Salt Lake City   

# Set workplace 
 setwd("~/tci")
 
 # make names for household income groups, trip purpose and calculation method
 IcNames <- c("Low Income", "Mid Income", "High Income")
 Ic <- c("lowInc", "midInc", "highInc")
 names(IcNames) <- Ic
 
 PrNames <- c("Work", "Shopping", "Recreation", "Other")
 Pr <- c("hbw", "hbs", "hbr", "hbo")
 names(PrNames) <- Pr
 
 CmNames <- c("mintcost", "avgtcost", "maxtcost")
 Cm <- c("min", "avg", "max")
 names(CmNames) <- Cm  

# Load required packages
 require(ggplot2)
 
# Load data 
   # Load OHAS data 
   load("output/OHAS/tcost.RData")
   op.tcost.trip <- tcost.trip
   op.tcost.hh.tpurp <- tcost.hh.tpurp 
   remove(tcost.all, tcost.distr, tcost.hh, tcost.hh.tpurp, tcost.HTAZ.inc, 
          tcost.HTAZ.tpurp.inc, tcost.tpurp.inc, tcost.trip, tcost.HTAZ)
   
   # Load NHTS-Portland data 
   load("output/NHTS09/Portland/tcost.RData")
   np.tcost.trip <- tcost.trip
   np.tcost.hh.tpurp <- tcost.hh.tpurp 
   remove(tcost.all, tcost.distr, tcost.hh, tcost.hh.tpurp, tcost.HTAZ.inc, 
          tcost.HTAZ.tpurp.inc, tcost.tpurp.inc, tcost.trip, tcost.HTAZ)
   
   # Load NHTS-Tampa Bay data 
   load("output/NHTS09/TampaBay/tcost.RData")
   nt.tcost.trip <- tcost.trip
   nt.tcost.hh.tpurp <- tcost.hh.tpurp 
   remove(tcost.all, tcost.distr, tcost.hh, tcost.hh.tpurp, tcost.HTAZ.inc, 
          tcost.HTAZ.tpurp.inc, tcost.tpurp.inc, tcost.trip, tcost.HTAZ)
   
   # Load NHTS-Salt Lake City data 
   load("output/NHTS09/SaltLakeCity/tcost.RData")
   ns.tcost.trip <- tcost.trip
   ns.tcost.hh.tpurp <- tcost.hh.tpurp 
   remove(tcost.all, tcost.distr, tcost.hh, tcost.hh.tpurp, tcost.HTAZ.inc, 
          tcost.HTAZ.tpurp.inc, tcost.tpurp.inc, tcost.trip, tcost.HTAZ)
 
# Trip-level travel cost  
  # One-way ANOVA for overall travel cost in different MSAs
     # Combine trip-level tcost data 
     op.tcost.trip.sub <- data.frame(op.tcost.trip[, c("tcost", "TripPurpose", "inc.level")], msa="op")
     np.tcost.trip.sub <- data.frame(np.tcost.trip[, c("tcost", "TripPurpose", "inc.level")], msa="np")
     nt.tcost.trip.sub <- data.frame(nt.tcost.trip[, c("tcost", "TripPurpose", "inc.level")], msa="nt")
     ns.tcost.trip.sub <- data.frame(ns.tcost.trip[, c("tcost", "TripPurpose", "inc.level")], msa="ns")
     
     tcost.trip.msas <- rbind(op.tcost.trip.sub, np.tcost.trip.sub, nt.tcost.trip.sub, ns.tcost.trip.sub)
     tcost.trip.msas$TripPurpose <- as.factor(tcost.trip.msas$TripPurpose)
     # Combine data 
     ggplot(tcost.trip.msas, aes(x = msa , y = tcost, fill=msa)) +
       geom_boxplot() +
       scale_x_discrete() + xlab("MSAs") +
       ylab("Trip-level travel cost") +ylim(0,100)
     
     # ANOVA test
     tcost.trip.msas.aov <- aov(tcost ~ msa, data=tcost.trip.msas)
     summary(tcost.trip.msas.aov)
     TukeyHSD(tcost.trip.msas.aov)
     
  # One-way ANOVA test for travel cost by same income group in diffent areas 
     # hbw
     tcost.trip.msas.hbw <- aov(tcost ~ msa, 
                                data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbw"),])
     summary(tcost.trip.msas.hbw)
     TukeyHSD(tcost.trip.msas.hbw)
     
     # hbs
     tcost.trip.msas.hbs <- aov(tcost ~ msa, 
                                data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbs"),])
     summary(tcost.trip.msas.hbs)
     TukeyHSD(tcost.trip.msas.hbs)
     
     # hbr
     tcost.trip.msas.hbr <- aov(tcost ~ msa, 
                                data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbr"),])
     summary(tcost.trip.msas.hbr)
     TukeyHSD(tcost.trip.msas.hbr)
     
     # hbo
     tcost.trip.msas.hbo <- aov(tcost ~ msa, 
                                data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbo"),])
     summary(tcost.trip.msas.hbo)
     TukeyHSD(tcost.trip.msas.hbo)
     
  # One-way ANOVA test for travel cost for same trip purpose in diffent areas   
     # lowInc
     tcost.trip.msas.lowInc <- aov(tcost ~ msa, 
                                   data=tcost.trip.msas[which(tcost.trip.msas$inc.level=="lowInc"),])
     summary(tcost.trip.msas.lowInc)
     TukeyHSD(tcost.trip.msas.lowInc)
     
     # midInc
     tcost.trip.msas.midInc <- aov(tcost ~ msa, 
                                   data=tcost.trip.msas[which(tcost.trip.msas$inc.level=="midInc"),])
     summary(tcost.trip.msas.midInc)
     TukeyHSD(tcost.trip.msas.midInc)
     
     # highInc
     tcost.trip.msas.highInc <- aov(tcost ~ msa, 
                                    data=tcost.trip.msas[which(tcost.trip.msas$inc.level=="highInc"),])
     summary(tcost.trip.msas.highInc)
     TukeyHSD(tcost.trip.msas.highInc)
     
  # One-way ANOVA test for travel cost by same income group for same trip purpose in diffent areas 
     # hbw-lowInc
     tcost.trip.msas.hbw.lowInc <- aov(tcost ~ msa, 
                                       data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbw"&tcost.trip.msas$inc.level=="lowInc"),])
     summary(tcost.trip.msas.hbw.lowInc)
     TukeyHSD(tcost.trip.msas.hbw.lowInc)
     
     # hbw-midInc
     tcost.trip.msas.hbw.midInc <- aov(tcost ~ msa, 
                                       data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbw"&tcost.trip.msas$inc.level=="midInc"),])
     summary(tcost.trip.msas.hbw.midInc)
     TukeyHSD(tcost.trip.msas.hbw.midInc)
     
     # hbw-highInc
     tcost.trip.msas.hbw.highInc <- aov(tcost ~ msa, 
                                        data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbw"&tcost.trip.msas$inc.level=="highInc"),])
     summary(tcost.trip.msas.hbw.highInc)
     TukeyHSD(tcost.trip.msas.hbw.highInc)
     
     # hbs-lowInc
     tcost.trip.msas.hbs.lowInc <- aov(tcost ~ msa, 
                                       data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbs"&tcost.trip.msas$inc.level=="lowInc"),])
     summary(tcost.trip.msas.hbs.lowInc)
     TukeyHSD(tcost.trip.msas.hbs.lowInc)
     
     # hbs-midInc
     tcost.trip.msas.hbs.midInc <- aov(tcost ~ msa, 
                                       data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbs"&tcost.trip.msas$inc.level=="midInc"),])
     summary(tcost.trip.msas.hbs.midInc)
     TukeyHSD(tcost.trip.msas.hbs.midInc)
     
     # hbs-highInc
     tcost.trip.msas.hbs.highInc <- aov(tcost ~ msa, 
                                        data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbs"&tcost.trip.msas$inc.level=="highInc"),])
     summary(tcost.trip.msas.hbs.highInc)
     TukeyHSD(tcost.trip.msas.hbs.highInc)
     
     # hbr-lowInc
     tcost.trip.msas.hbr.lowInc <- aov(tcost ~ msa, 
                                       data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbr"&tcost.trip.msas$inc.level=="lowInc"),])
     summary(tcost.trip.msas.hbr.lowInc)
     TukeyHSD(tcost.trip.msas.hbr.lowInc)
     
     # hbr-midInc
     tcost.trip.msas.hbr.midInc <- aov(tcost ~ msa, 
                                       data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbr"&tcost.trip.msas$inc.level=="midInc"),])
     summary(tcost.trip.msas.hbr.midInc)
     TukeyHSD(tcost.trip.msas.hbr.midInc)
     
     # hbr-highInc
     tcost.trip.msas.hbr.highInc <- aov(tcost ~ msa, 
                                        data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbr"&tcost.trip.msas$inc.level=="highInc"),])
     summary(tcost.trip.msas.hbr.highInc)
     TukeyHSD(tcost.trip.msas.hbr.highInc)
     
     # hbo-lowInc
     tcost.trip.msas.hbo.lowInc <- aov(tcost ~ msa, 
                                       data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbo"&tcost.trip.msas$inc.level=="lowInc"),])
     summary(tcost.trip.msas.hbo.lowInc)
     TukeyHSD(tcost.trip.msas.hbo.lowInc)
     
     # hbo-midInc
     tcost.trip.msas.hbo.midInc <- aov(tcost ~ msa, 
                                       data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbo"&tcost.trip.msas$inc.level=="midInc"),])
     summary(tcost.trip.msas.hbo.midInc)
     TukeyHSD(tcost.trip.msas.hbo.midInc)
     
     # hbo-highInc
     tcost.trip.msas.hbo.highInc <- aov(tcost ~ msa, 
                                        data=tcost.trip.msas[which(tcost.trip.msas$TripPurpose=="hbo"&tcost.trip.msas$inc.level=="highInc"),])
     summary(tcost.trip.msas.hbo.highInc)
     TukeyHSD(tcost.trip.msas.hbo.highInc)
     
     
# Two-way ANOVA test for each MSA
   # OHAS-Portland 
     # Transform TripPurpose as factor    
     op.tcost.trip$TripPurpose <- as.factor(op.tcost.trip$TripPurpose)
     
     # Plot travel cost 
     boxp.op.tcost.trip <- ggplot(op.tcost.trip, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
       geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
       scale_fill_discrete(name = 'Income Level') + 
       ggtitle("OHAS-Portland trip-level travel cost ") +
       theme(plot.title = element_text(face="bold", size=12, vjust=1))
     boxp.op.tcost.trip
     
     # Two-way ANOVA test 
     op.tcost.trip.PrIc <- aov(tcost ~ TripPurpose*inc.level, data=op.tcost.trip)
     summary(op.tcost.trip.PrIc)
     
     TukeyHSD(op.tcost.trip.PrIc, which="TripPurpose")
     TukeyHSD(op.tcost.trip.PrIc, which="inc.level")

     # One-way ANOVA test of same trip purpose 
       # hbw
       op.tcost.trip.hbw <- aov(tcost ~ inc.level, data=op.tcost.trip[which(op.tcost.trip$TripPurpose=="hbw"),])
       summary(op.tcost.trip.hbw)
       TukeyHSD(op.tcost.trip.hbw)
       
       # hbs
       op.tcost.trip.hbs <- aov(tcost ~ inc.level, data=op.tcost.trip[which(op.tcost.trip$TripPurpose=="hbs"),])
       summary(op.tcost.trip.hbs)
       TukeyHSD(op.tcost.trip.hbs)
     
       # hbr
       op.tcost.trip.hbr <- aov(tcost ~ inc.level, data=op.tcost.trip[which(op.tcost.trip$TripPurpose=="hbr"),])
       summary(op.tcost.trip.hbr)
       TukeyHSD(op.tcost.trip.hbr)
       
       # hbo
       op.tcost.trip.hbo <- aov(tcost ~ inc.level, data=op.tcost.trip[which(op.tcost.trip$TripPurpose=="hbo"),])
       summary(op.tcost.trip.hbo)
       TukeyHSD(op.tcost.trip.hbo)
     
     # One-way ANOVA test of same income group 
       # lowInc
       op.tcost.trip.lowInc <- aov(tcost ~ TripPurpose, data=op.tcost.trip[which(op.tcost.trip$inc.level=="lowInc"),])
       summary(op.tcost.trip.lowInc)
       TukeyHSD(op.tcost.trip.lowInc)
       
       # midInc
       op.tcost.trip.midInc <- aov(tcost ~ TripPurpose, data=op.tcost.trip[which(op.tcost.trip$inc.level=="midInc"),])
       summary(op.tcost.trip.midInc)
       TukeyHSD(op.tcost.trip.midInc)
       
       # highInc
       op.tcost.trip.highInc <- aov(tcost ~ TripPurpose, data=op.tcost.trip[which(op.tcost.trip$inc.level=="highInc"),])
       summary(op.tcost.trip.highInc)
       TukeyHSD(op.tcost.trip.highInc)
       
    # NHTS-Portland 
       # Transform TripPurpose as factor    
       np.tcost.trip$TripPurpose <- as.factor(np.tcost.trip$TripPurpose)
       
       # Plot travel cost 
       boxp.np.tcost.trip <- ggplot(np.tcost.trip, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
         geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
         scale_fill_discrete(name = 'Income Level') + 
         ggtitle("OHAS-Portland trip-level travel cost ") +
         theme(plot.title = element_text(face="bold", size=12, vjust=1))
       boxp.np.tcost.trip
       
       # Two-way ANOVA test 
       np.tcost.trip.PrIc <- aov(tcost ~ TripPurpose*inc.level, data=np.tcost.trip)
       summary(np.tcost.trip.PrIc)
       
       TukeyHSD(np.tcost.trip.PrIc, which="TripPurpose")
       TukeyHSD(np.tcost.trip.PrIc, which="inc.level")
       
       # One-way ANOVA test of same trip purpose 
       # hbw
       np.tcost.trip.hbw <- aov(tcost ~ inc.level, data=np.tcost.trip[which(np.tcost.trip$TripPurpose=="hbw"),])
       summary(np.tcost.trip.hbw)
       TukeyHSD(np.tcost.trip.hbw)
       
       # hbs
       np.tcost.trip.hbs <- aov(tcost ~ inc.level, data=np.tcost.trip[which(np.tcost.trip$TripPurpose=="hbs"),])
       summary(np.tcost.trip.hbs)
       TukeyHSD(np.tcost.trip.hbs)
       
       # hbr
       np.tcost.trip.hbr <- aov(tcost ~ inc.level, data=np.tcost.trip[which(np.tcost.trip$TripPurpose=="hbr"),])
       summary(np.tcost.trip.hbr)
       TukeyHSD(np.tcost.trip.hbr)
       
       # hbo
       np.tcost.trip.hbo <- aov(tcost ~ inc.level, data=np.tcost.trip[which(np.tcost.trip$TripPurpose=="hbo"),])
       summary(np.tcost.trip.hbo)
       TukeyHSD(np.tcost.trip.hbo)
       
       # One-way ANOVA test of same income group 
       # lowInc
       np.tcost.trip.lowInc <- aov(tcost ~ TripPurpose, data=np.tcost.trip[which(np.tcost.trip$inc.level=="lowInc"),])
       summary(np.tcost.trip.lowInc)
       TukeyHSD(np.tcost.trip.lowInc)
       
       # midInc
       np.tcost.trip.midInc <- aov(tcost ~ TripPurpose, data=np.tcost.trip[which(np.tcost.trip$inc.level=="midInc"),])
       summary(np.tcost.trip.midInc)
       TukeyHSD(np.tcost.trip.midInc)
       
       # highInc
       np.tcost.trip.highInc <- aov(tcost ~ TripPurpose, data=np.tcost.trip[which(np.tcost.trip$inc.level=="highInc"),])
       summary(np.tcost.trip.highInc)
       TukeyHSD(np.tcost.trip.highInc)
    # NHTS-Tampa Bay 
       # Transform TripPurpose as factor    
       nt.tcost.trip$TripPurpose <- as.factor(nt.tcost.trip$TripPurpose)
       
       # Plot travel cost 
       boxp.nt.tcost.trip <- ggplot(nt.tcost.trip, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
         geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
         scale_fill_discrete(name = 'Income Level') + 
         ggtitle("OHAS-Portland trip-level travel cost ") +
         theme(plot.title = element_text(face="bold", size=12, vjust=1))
       boxp.nt.tcost.trip
       
       # Two-way ANOVA test 
       nt.tcost.trip.PrIc <- aov(tcost ~ TripPurpose*inc.level, data=nt.tcost.trip)
       summary(nt.tcost.trip.PrIc)
       
       TukeyHSD(nt.tcost.trip.PrIc, which="TripPurpose")
       TukeyHSD(nt.tcost.trip.PrIc, which="inc.level")
       
       # One-way ANOVA test of same trip purpose 
       # hbw
       nt.tcost.trip.hbw <- aov(tcost ~ inc.level, data=nt.tcost.trip[which(nt.tcost.trip$TripPurpose=="hbw"),])
       summary(nt.tcost.trip.hbw)
       TukeyHSD(nt.tcost.trip.hbw)
       
       # hbs
       nt.tcost.trip.hbs <- aov(tcost ~ inc.level, data=nt.tcost.trip[which(nt.tcost.trip$TripPurpose=="hbs"),])
       summary(nt.tcost.trip.hbs)
       TukeyHSD(nt.tcost.trip.hbs)
       
       # hbr
       nt.tcost.trip.hbr <- aov(tcost ~ inc.level, data=nt.tcost.trip[which(nt.tcost.trip$TripPurpose=="hbr"),])
       summary(nt.tcost.trip.hbr)
       TukeyHSD(nt.tcost.trip.hbr)
       
       # hbo
       nt.tcost.trip.hbo <- aov(tcost ~ inc.level, data=nt.tcost.trip[which(nt.tcost.trip$TripPurpose=="hbo"),])
       summary(nt.tcost.trip.hbo)
       TukeyHSD(nt.tcost.trip.hbo)
       
       # One-way ANOVA test of same income group 
       # lowInc
       nt.tcost.trip.lowInc <- aov(tcost ~ TripPurpose, data=nt.tcost.trip[which(nt.tcost.trip$inc.level=="lowInc"),])
       summary(nt.tcost.trip.lowInc)
       TukeyHSD(nt.tcost.trip.lowInc)
       
       # midInc
       nt.tcost.trip.midInc <- aov(tcost ~ TripPurpose, data=nt.tcost.trip[which(nt.tcost.trip$inc.level=="midInc"),])
       summary(nt.tcost.trip.midInc)
       TukeyHSD(nt.tcost.trip.midInc)
       
       # highInc
       nt.tcost.trip.highInc <- aov(tcost ~ TripPurpose, data=nt.tcost.trip[which(nt.tcost.trip$inc.level=="highInc"),])
       summary(nt.tcost.trip.highInc)
       TukeyHSD(nt.tcost.trip.highInc)
       
    # NHTS-Salt Lake City 
       # Transform TripPurpose as factor    
       ns.tcost.trip$TripPurpose <- as.factor(ns.tcost.trip$TripPurpose)
       
       # Plot travel cost 
       boxp.ns.tcost.trip <- ggplot(ns.tcost.trip, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
         geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
         scale_fill_discrete(name = 'Income Level') + 
         ggtitle("OHAS-Portland trip-level travel cost ") +
         theme(plot.title = element_text(face="bold", size=12, vjust=1))
       boxp.ns.tcost.trip
       
       # Two-way ANOVA test 
       ns.tcost.trip.PrIc <- aov(tcost ~ TripPurpose*inc.level, data=ns.tcost.trip)
       summary(ns.tcost.trip.PrIc)
       
       TukeyHSD(ns.tcost.trip.PrIc, which="TripPurpose")
       TukeyHSD(ns.tcost.trip.PrIc, which="inc.level")
       
       # One-way ANOVA test of same trip purpose 
         # hbw
         ns.tcost.trip.hbw <- aov(tcost ~ inc.level, data=ns.tcost.trip[which(ns.tcost.trip$TripPurpose=="hbw"),])
         summary(ns.tcost.trip.hbw)
         TukeyHSD(ns.tcost.trip.hbw)
         
         # hbs
         ns.tcost.trip.hbs <- aov(tcost ~ inc.level, data=ns.tcost.trip[which(ns.tcost.trip$TripPurpose=="hbs"),])
         summary(ns.tcost.trip.hbs)
         TukeyHSD(ns.tcost.trip.hbs)
         
         # hbr
         ns.tcost.trip.hbr <- aov(tcost ~ inc.level, data=ns.tcost.trip[which(ns.tcost.trip$TripPurpose=="hbr"),])
         summary(ns.tcost.trip.hbr)
         TukeyHSD(ns.tcost.trip.hbr)
         
         # hbo
         ns.tcost.trip.hbo <- aov(tcost ~ inc.level, data=ns.tcost.trip[which(ns.tcost.trip$TripPurpose=="hbo"),])
         summary(ns.tcost.trip.hbo)
         TukeyHSD(ns.tcost.trip.hbo)
       
       # One-way ANOVA test of same income group 
         # lowInc
         ns.tcost.trip.lowInc <- aov(tcost ~ TripPurpose, data=ns.tcost.trip[which(ns.tcost.trip$inc.level=="lowInc"),])
         summary(ns.tcost.trip.lowInc)
         TukeyHSD(ns.tcost.trip.lowInc)
         
         # midInc
         ns.tcost.trip.midInc <- aov(tcost ~ TripPurpose, data=ns.tcost.trip[which(ns.tcost.trip$inc.level=="midInc"),])
         summary(ns.tcost.trip.midInc)
         TukeyHSD(ns.tcost.trip.midInc)
         
         # highInc
         ns.tcost.trip.highInc <- aov(tcost ~ TripPurpose, data=ns.tcost.trip[which(ns.tcost.trip$inc.level=="highInc"),])
         summary(ns.tcost.trip.highInc)
         TukeyHSD(ns.tcost.trip.highInc)
       
# Compare household-level travel cost 
    # One-way ANOVA for overall travel cost in different MSAs
         # Combine trip-level tcost data 
         op.tcost.hh.tpurp.sub <- data.frame(op.tcost.trip[, c("tcost", "TripPurpose", "inc.level")], msa="op")
         np.tcost.hh.tpurp.sub <- data.frame(np.tcost.trip[, c("tcost", "TripPurpose", "inc.level")], msa="np")
         nt.tcost.hh.tpurp.sub <- data.frame(nt.tcost.trip[, c("tcost", "TripPurpose", "inc.level")], msa="nt")
         ns.tcost.hh.tpurp.sub <- data.frame(ns.tcost.trip[, c("tcost", "TripPurpose", "inc.level")], msa="ns")
         
         tcost.hh.tpurp.msas <- rbind(op.tcost.hh.tpurp.sub, np.tcost.hh.tpurp.sub, nt.tcost.hh.tpurp.sub, ns.tcost.hh.tpurp.sub)
         tcost.hh.tpurp.msas$TripPurpose <- as.factor(tcost.hh.tpurp.msas$TripPurpose)
         # Combine data 
         ggplot(tcost.hh.tpurp.msas, aes(x = msa , y = tcost, fill=msa)) +
           geom_boxplot() +
           scale_x_discrete() + xlab("MSAs") +
           ylab("Trip-level travel cost") +ylim(0,100)
         
         # ANOVA test
         tcost.hh.tpurp.msas.aov <- aov(tcost ~ msa, data=tcost.hh.tpurp.msas)
         summary(tcost.hh.tpurp.msas.aov)
         TukeyHSD(tcost.hh.tpurp.msas.aov)
         
    # One-way ANOVA test for travel cost by same income group in diffent areas 
         # hbw
         tcost.hh.tpurp.msas.hbw <- aov(tcost ~ msa, 
                                        data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbw"),])
         summary(tcost.hh.tpurp.msas.hbw)
         TukeyHSD(tcost.hh.tpurp.msas.hbw)
         
         # hbs
         tcost.hh.tpurp.msas.hbs <- aov(tcost ~ msa, 
                                        data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbs"),])
         summary(tcost.hh.tpurp.msas.hbs)
         TukeyHSD(tcost.hh.tpurp.msas.hbs)
         
         # hbr
         tcost.hh.tpurp.msas.hbr <- aov(tcost ~ msa, 
                                        data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbr"),])
         summary(tcost.hh.tpurp.msas.hbr)
         TukeyHSD(tcost.hh.tpurp.msas.hbr)
         
         # hbo
         tcost.hh.tpurp.msas.hbo <- aov(tcost ~ msa, 
                                        data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbo"),])
         summary(tcost.hh.tpurp.msas.hbo)
         TukeyHSD(tcost.hh.tpurp.msas.hbo)
         
    # One-way ANOVA test for travel cost for same trip purpose in diffent areas   
         # lowInc
         tcost.hh.tpurp.msas.lowInc <- aov(tcost ~ msa, 
                                           data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$inc.level=="lowInc"),])
         summary(tcost.hh.tpurp.msas.lowInc)
         TukeyHSD(tcost.hh.tpurp.msas.lowInc)
         
         # midInc
         tcost.hh.tpurp.msas.midInc <- aov(tcost ~ msa, 
                                           data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$inc.level=="midInc"),])
         summary(tcost.hh.tpurp.msas.midInc)
         TukeyHSD(tcost.hh.tpurp.msas.midInc)
         
         # highInc
         tcost.hh.tpurp.msas.highInc <- aov(tcost ~ msa, 
                                            data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$inc.level=="highInc"),])
         summary(tcost.hh.tpurp.msas.highInc)
         TukeyHSD(tcost.hh.tpurp.msas.highInc)
         
    # One-way ANOVA test for travel cost by same income group for same trip purpose in diffent areas 
         # hbw-lowInc
         tcost.hh.tpurp.msas.hbw.lowInc <- aov(tcost ~ msa, 
                                               data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbw"&tcost.hh.tpurp.msas$inc.level=="lowInc"),])
         summary(tcost.hh.tpurp.msas.hbw.lowInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbw.lowInc)
         
         # hbw-midInc
         tcost.hh.tpurp.msas.hbw.midInc <- aov(tcost ~ msa, 
                                               data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbw"&tcost.hh.tpurp.msas$inc.level=="midInc"),])
         summary(tcost.hh.tpurp.msas.hbw.midInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbw.midInc)
         
         # hbw-highInc
         tcost.hh.tpurp.msas.hbw.highInc <- aov(tcost ~ msa, 
                                                data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbw"&tcost.hh.tpurp.msas$inc.level=="highInc"),])
         summary(tcost.hh.tpurp.msas.hbw.highInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbw.highInc)
         
         # hbs-lowInc
         tcost.hh.tpurp.msas.hbs.lowInc <- aov(tcost ~ msa, 
                                               data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbs"&tcost.hh.tpurp.msas$inc.level=="lowInc"),])
         summary(tcost.hh.tpurp.msas.hbs.lowInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbs.lowInc)
         
         # hbs-midInc
         tcost.hh.tpurp.msas.hbs.midInc <- aov(tcost ~ msa, 
                                               data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbs"&tcost.hh.tpurp.msas$inc.level=="midInc"),])
         summary(tcost.hh.tpurp.msas.hbs.midInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbs.midInc)
         
         # hbs-highInc
         tcost.hh.tpurp.msas.hbs.highInc <- aov(tcost ~ msa, 
                                                data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbs"&tcost.hh.tpurp.msas$inc.level=="highInc"),])
         summary(tcost.hh.tpurp.msas.hbs.highInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbs.highInc)
         
         # hbr-lowInc
         tcost.hh.tpurp.msas.hbr.lowInc <- aov(tcost ~ msa, 
                                               data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbr"&tcost.hh.tpurp.msas$inc.level=="lowInc"),])
         summary(tcost.hh.tpurp.msas.hbr.lowInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbr.lowInc)
         
         # hbr-midInc
         tcost.hh.tpurp.msas.hbr.midInc <- aov(tcost ~ msa, 
                                               data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbr"&tcost.hh.tpurp.msas$inc.level=="midInc"),])
         summary(tcost.hh.tpurp.msas.hbr.midInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbr.midInc)
         
         # hbr-highInc
         tcost.hh.tpurp.msas.hbr.highInc <- aov(tcost ~ msa, 
                                                data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbr"&tcost.hh.tpurp.msas$inc.level=="highInc"),])
         summary(tcost.hh.tpurp.msas.hbr.highInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbr.highInc)
         
         # hbo-lowInc
         tcost.hh.tpurp.msas.hbo.lowInc <- aov(tcost ~ msa, 
                                               data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbo"&tcost.hh.tpurp.msas$inc.level=="lowInc"),])
         summary(tcost.hh.tpurp.msas.hbo.lowInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbo.lowInc)
         
         # hbo-midInc
         tcost.hh.tpurp.msas.hbo.midInc <- aov(tcost ~ msa, 
                                               data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbo"&tcost.hh.tpurp.msas$inc.level=="midInc"),])
         summary(tcost.hh.tpurp.msas.hbo.midInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbo.midInc)
         
         # hbo-highInc
         tcost.hh.tpurp.msas.hbo.highInc <- aov(tcost ~ msa, 
                                                data=tcost.hh.tpurp.msas[which(tcost.hh.tpurp.msas$TripPurpose=="hbo"&tcost.hh.tpurp.msas$inc.level=="highInc"),])
         summary(tcost.hh.tpurp.msas.hbo.highInc)
         TukeyHSD(tcost.hh.tpurp.msas.hbo.highInc)
         
         
    # Two-way ANOVA test for each MSA
      # OHAS-Portland 
         # Transform TripPurpose as factor    
         op.tcost.trip$TripPurpose <- as.factor(op.tcost.trip$TripPurpose)
         
         # Plot travel cost 
         boxp.op.tcost.trip <- ggplot(op.tcost.trip, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
           geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
           scale_fill_discrete(name = 'Income Level') + 
           ggtitle("OHAS-Portland trip-level travel cost ") +
           theme(plot.title = element_text(face="bold", size=12, vjust=1))
         boxp.op.tcost.trip
         
         # Two-way ANOVA test 
         op.tcost.hh.tpurp.PrIc <- aov(tcost ~ TripPurpose*inc.level, data=op.tcost.trip)
         summary(op.tcost.hh.tpurp.PrIc)
         
         TukeyHSD(op.tcost.hh.tpurp.PrIc, which="TripPurpose")
         TukeyHSD(op.tcost.hh.tpurp.PrIc, which="inc.level")
         
         # One-way ANOVA test of same trip purpose 
         # hbw
         op.tcost.hh.tpurp.hbw <- aov(tcost ~ inc.level, data=op.tcost.trip[which(op.tcost.trip$TripPurpose=="hbw"),])
         summary(op.tcost.hh.tpurp.hbw)
         TukeyHSD(op.tcost.hh.tpurp.hbw)
         
         # hbs
         op.tcost.hh.tpurp.hbs <- aov(tcost ~ inc.level, data=op.tcost.trip[which(op.tcost.trip$TripPurpose=="hbs"),])
         summary(op.tcost.hh.tpurp.hbs)
         TukeyHSD(op.tcost.hh.tpurp.hbs)
         
         # hbr
         op.tcost.hh.tpurp.hbr <- aov(tcost ~ inc.level, data=op.tcost.trip[which(op.tcost.trip$TripPurpose=="hbr"),])
         summary(op.tcost.hh.tpurp.hbr)
         TukeyHSD(op.tcost.hh.tpurp.hbr)
         
         # hbo
         op.tcost.hh.tpurp.hbo <- aov(tcost ~ inc.level, data=op.tcost.trip[which(op.tcost.trip$TripPurpose=="hbo"),])
         summary(op.tcost.hh.tpurp.hbo)
         TukeyHSD(op.tcost.hh.tpurp.hbo)
         
         # One-way ANOVA test of same income group 
         # lowInc
         op.tcost.hh.tpurp.lowInc <- aov(tcost ~ TripPurpose, data=op.tcost.trip[which(op.tcost.trip$inc.level=="lowInc"),])
         summary(op.tcost.hh.tpurp.lowInc)
         TukeyHSD(op.tcost.hh.tpurp.lowInc)
         
         # midInc
         op.tcost.hh.tpurp.midInc <- aov(tcost ~ TripPurpose, data=op.tcost.trip[which(op.tcost.trip$inc.level=="midInc"),])
         summary(op.tcost.hh.tpurp.midInc)
         TukeyHSD(op.tcost.hh.tpurp.midInc)
         
         # highInc
         op.tcost.hh.tpurp.highInc <- aov(tcost ~ TripPurpose, data=op.tcost.trip[which(op.tcost.trip$inc.level=="highInc"),])
         summary(op.tcost.hh.tpurp.highInc)
         TukeyHSD(op.tcost.hh.tpurp.highInc)
         
    # NHTS-Portland 
         # Transform TripPurpose as factor    
         np.tcost.trip$TripPurpose <- as.factor(np.tcost.trip$TripPurpose)
         
         # Plot travel cost 
         boxp.np.tcost.trip <- ggplot(np.tcost.trip, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
           geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
           scale_fill_discrete(name = 'Income Level') + 
           ggtitle("OHAS-Portland trip-level travel cost ") +
           theme(plot.title = element_text(face="bold", size=12, vjust=1))
         boxp.np.tcost.trip
         
         # Two-way ANOVA test 
         np.tcost.hh.tpurp.PrIc <- aov(tcost ~ TripPurpose*inc.level, data=np.tcost.trip)
         summary(np.tcost.hh.tpurp.PrIc)
         
         TukeyHSD(np.tcost.hh.tpurp.PrIc, which="TripPurpose")
         TukeyHSD(np.tcost.hh.tpurp.PrIc, which="inc.level")
         
         # One-way ANOVA test of same trip purpose 
         # hbw
         np.tcost.hh.tpurp.hbw <- aov(tcost ~ inc.level, data=np.tcost.trip[which(np.tcost.trip$TripPurpose=="hbw"),])
         summary(np.tcost.hh.tpurp.hbw)
         TukeyHSD(np.tcost.hh.tpurp.hbw)
         
         # hbs
         np.tcost.hh.tpurp.hbs <- aov(tcost ~ inc.level, data=np.tcost.trip[which(np.tcost.trip$TripPurpose=="hbs"),])
         summary(np.tcost.hh.tpurp.hbs)
         TukeyHSD(np.tcost.hh.tpurp.hbs)
         
         # hbr
         np.tcost.hh.tpurp.hbr <- aov(tcost ~ inc.level, data=np.tcost.trip[which(np.tcost.trip$TripPurpose=="hbr"),])
         summary(np.tcost.hh.tpurp.hbr)
         TukeyHSD(np.tcost.hh.tpurp.hbr)
         
         # hbo
         np.tcost.hh.tpurp.hbo <- aov(tcost ~ inc.level, data=np.tcost.trip[which(np.tcost.trip$TripPurpose=="hbo"),])
         summary(np.tcost.hh.tpurp.hbo)
         TukeyHSD(np.tcost.hh.tpurp.hbo)
         
         # One-way ANOVA test of same income group 
         # lowInc
         np.tcost.hh.tpurp.lowInc <- aov(tcost ~ TripPurpose, data=np.tcost.trip[which(np.tcost.trip$inc.level=="lowInc"),])
         summary(np.tcost.hh.tpurp.lowInc)
         TukeyHSD(np.tcost.hh.tpurp.lowInc)
         
         # midInc
         np.tcost.hh.tpurp.midInc <- aov(tcost ~ TripPurpose, data=np.tcost.trip[which(np.tcost.trip$inc.level=="midInc"),])
         summary(np.tcost.hh.tpurp.midInc)
         TukeyHSD(np.tcost.hh.tpurp.midInc)
         
         # highInc
         np.tcost.hh.tpurp.highInc <- aov(tcost ~ TripPurpose, data=np.tcost.trip[which(np.tcost.trip$inc.level=="highInc"),])
         summary(np.tcost.hh.tpurp.highInc)
         TukeyHSD(np.tcost.hh.tpurp.highInc)
    
    # NHTS-Tampa Bay 
         # Transform TripPurpose as factor    
         nt.tcost.trip$TripPurpose <- as.factor(nt.tcost.trip$TripPurpose)
         
         # Plot travel cost 
         boxp.nt.tcost.trip <- ggplot(nt.tcost.trip, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
           geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
           scale_fill_discrete(name = 'Income Level') + 
           ggtitle("OHAS-Portland trip-level travel cost ") +
           theme(plot.title = element_text(face="bold", size=12, vjust=1))
         boxp.nt.tcost.trip
         
         # Two-way ANOVA test 
         nt.tcost.hh.tpurp.PrIc <- aov(tcost ~ TripPurpose*inc.level, data=nt.tcost.trip)
         summary(nt.tcost.hh.tpurp.PrIc)
         
         TukeyHSD(nt.tcost.hh.tpurp.PrIc, which="TripPurpose")
         TukeyHSD(nt.tcost.hh.tpurp.PrIc, which="inc.level")
         
      # One-way ANOVA test of same trip purpose 
         # hbw
         nt.tcost.hh.tpurp.hbw <- aov(tcost ~ inc.level, data=nt.tcost.trip[which(nt.tcost.trip$TripPurpose=="hbw"),])
         summary(nt.tcost.hh.tpurp.hbw)
         TukeyHSD(nt.tcost.hh.tpurp.hbw)
         
         # hbs
         nt.tcost.hh.tpurp.hbs <- aov(tcost ~ inc.level, data=nt.tcost.trip[which(nt.tcost.trip$TripPurpose=="hbs"),])
         summary(nt.tcost.hh.tpurp.hbs)
         TukeyHSD(nt.tcost.hh.tpurp.hbs)
         
         # hbr
         nt.tcost.hh.tpurp.hbr <- aov(tcost ~ inc.level, data=nt.tcost.trip[which(nt.tcost.trip$TripPurpose=="hbr"),])
         summary(nt.tcost.hh.tpurp.hbr)
         TukeyHSD(nt.tcost.hh.tpurp.hbr)
         
         # hbo
         nt.tcost.hh.tpurp.hbo <- aov(tcost ~ inc.level, data=nt.tcost.trip[which(nt.tcost.trip$TripPurpose=="hbo"),])
         summary(nt.tcost.hh.tpurp.hbo)
         TukeyHSD(nt.tcost.hh.tpurp.hbo)
         
    # One-way ANOVA test of same income group 
         # lowInc
         nt.tcost.hh.tpurp.lowInc <- aov(tcost ~ TripPurpose, data=nt.tcost.trip[which(nt.tcost.trip$inc.level=="lowInc"),])
         summary(nt.tcost.hh.tpurp.lowInc)
         TukeyHSD(nt.tcost.hh.tpurp.lowInc)
         
         # midInc
         nt.tcost.hh.tpurp.midInc <- aov(tcost ~ TripPurpose, data=nt.tcost.trip[which(nt.tcost.trip$inc.level=="midInc"),])
         summary(nt.tcost.hh.tpurp.midInc)
         TukeyHSD(nt.tcost.hh.tpurp.midInc)
         
         # highInc
         nt.tcost.hh.tpurp.highInc <- aov(tcost ~ TripPurpose, data=nt.tcost.trip[which(nt.tcost.trip$inc.level=="highInc"),])
         summary(nt.tcost.hh.tpurp.highInc)
         TukeyHSD(nt.tcost.hh.tpurp.highInc)
         
    # NHTS-Salt Lake City 
         # Transform TripPurpose as factor    
         ns.tcost.trip$TripPurpose <- as.factor(ns.tcost.trip$TripPurpose)
         
         # Plot travel cost 
         boxp.ns.tcost.trip <- ggplot(ns.tcost.trip, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
           geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 100)  +
           scale_fill_discrete(name = 'Income Level') + 
           ggtitle("OHAS-Portland trip-level travel cost ") +
           theme(plot.title = element_text(face="bold", size=12, vjust=1))
         boxp.ns.tcost.trip
         
      # Two-way ANOVA test 
         ns.tcost.hh.tpurp.PrIc <- aov(tcost ~ TripPurpose*inc.level, data=ns.tcost.trip)
         summary(ns.tcost.hh.tpurp.PrIc)
         
         TukeyHSD(ns.tcost.hh.tpurp.PrIc, which="TripPurpose")
         TukeyHSD(ns.tcost.hh.tpurp.PrIc, which="inc.level")
         
      # One-way ANOVA test of same trip purpose 
         # hbw
         ns.tcost.hh.tpurp.hbw <- aov(tcost ~ inc.level, data=ns.tcost.trip[which(ns.tcost.trip$TripPurpose=="hbw"),])
         summary(ns.tcost.hh.tpurp.hbw)
         TukeyHSD(ns.tcost.hh.tpurp.hbw)
         
         # hbs
         ns.tcost.hh.tpurp.hbs <- aov(tcost ~ inc.level, data=ns.tcost.trip[which(ns.tcost.trip$TripPurpose=="hbs"),])
         summary(ns.tcost.hh.tpurp.hbs)
         TukeyHSD(ns.tcost.hh.tpurp.hbs)
         
         # hbr
         ns.tcost.hh.tpurp.hbr <- aov(tcost ~ inc.level, data=ns.tcost.trip[which(ns.tcost.trip$TripPurpose=="hbr"),])
         summary(ns.tcost.hh.tpurp.hbr)
         TukeyHSD(ns.tcost.hh.tpurp.hbr)
         
         # hbo
         ns.tcost.hh.tpurp.hbo <- aov(tcost ~ inc.level, data=ns.tcost.trip[which(ns.tcost.trip$TripPurpose=="hbo"),])
         summary(ns.tcost.hh.tpurp.hbo)
         TukeyHSD(ns.tcost.hh.tpurp.hbo)
         
    # One-way ANOVA test of same income group 
         # lowInc
         ns.tcost.hh.tpurp.lowInc <- aov(tcost ~ TripPurpose, data=ns.tcost.trip[which(ns.tcost.trip$inc.level=="lowInc"),])
         summary(ns.tcost.hh.tpurp.lowInc)
         TukeyHSD(ns.tcost.hh.tpurp.lowInc)
         
         # midInc
         ns.tcost.hh.tpurp.midInc <- aov(tcost ~ TripPurpose, data=ns.tcost.trip[which(ns.tcost.trip$inc.level=="midInc"),])
         summary(ns.tcost.hh.tpurp.midInc)
         TukeyHSD(ns.tcost.hh.tpurp.midInc)
         
         # highInc
         ns.tcost.hh.tpurp.highInc <- aov(tcost ~ TripPurpose, data=ns.tcost.trip[which(ns.tcost.trip$inc.level=="highInc"),])
         summary(ns.tcost.hh.tpurp.highInc)
         TukeyHSD(ns.tcost.hh.tpurp.highInc)