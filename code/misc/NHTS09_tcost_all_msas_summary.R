# This script generate travel cost summary table and boxplot for all MSAs 

# setting
OUTPUT_DIR <- file.path("output/Survey/NHTS09/summary")
dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)

HHC_MSAs <- c("Atlanta, GA"= "0520", "Austin--San Marcos, TX" = "0640", "Boston--Worcester--Lawrence, MA--NH--ME--CT"= "1122",
              "Buffalo--Niagara Falls, NY"="1280", "Charlotte--Gastonia--Rock Hill, NC--SC" = "1520", 
              "Chicago--Gary--Kenosha, IL--IN--WI"= "1602", "Cincinnati--Hamilton, OH--KY--IN"="1642", "Cleveland--Akron, OH"="1692", 
              "Columbus, OH" = "1840", "Dallas--Fort Worth, TX" = "1922", "Denver--Boulder--Greeley, CO" = "2082", 
              "Detroit--Ann Arbor--Flint, MI" = "2162", "Grand Rapids--Muskegon--Holland, MI" = "3000", 
              "Greensboro--Winston-Salem--High Point, NC" = "3120",  "Hartford, CT" = "3280", "Houston--Galveston--Brazoria, TX" = "3362", 
              "Indianapolis, IN" = "3480", "Jacksonville, FL" = "3600", "Kansas City, MO--KS" = "3760", "Las Vegas, NV--AZ" = "4120", 
              "Los Angeles--Riverside--Orange County, CA"="4472", "Louisville, KY--IN"="4520", "Memphis, TN--AR--MS"="4920", 
              "Miami--Fort Lauderdale, FL" = "4992", "Milwaukee--Racine, WI" = "5082", "Minneapolis--St. Paul, MN--WI" = "5120", 
              "Nashville, TN" = "5360", "New Orleans, LA" = "5560", "New York--Northern New Jersey--Long Island,NY--NJ--CT--PA" = "5602", 
              "Norfolk--Virginia Beach--Newport News, VA--NC" = "5720", "Oklahoma City, OK" = "5880", "Orlando, FL" = "5960",
              "Philadelphia--Wilmington--Atlantic City,PA--NJ--DE--MD" = "6162", "Phoenix--Mesa, AZ" = "6200", "Pittsburgh, PA" = "6280", 
              "Portland--Salem, OR--WA" = "6442", "Providence--Fall River--Warwick, RI--MA" = "6480", "Raleigh--Durham--Chapel Hill, NC" = "6640", 
              "Rochester, NY" = "6840", "Sacramento--Yolo, CA"= "6922", "St. Louis, MO--IL" = "7040", 
              "Salt Lake City--Ogden, UT" = "7160", "San Antonio, TX" = "7240", "San Diego, CA" = "7320", 
              "San Francisco--Oakland--San Jose, CA"= "7362", "Seattle--Tacoma--Bremerton, WA" = "7602",
              "Tampa--St. Petersburg--Clearwater, FL" = "8280", 
              "Washington--Baltimore, DC--MD--VA--WV" = "8872", "West Palm Beach--Boca Raton, FL" = "8960"
)  

unit.names <- c("minutes", "dollars") 

# load required packages
require(dplyr)
require(dplyr)

# Combine household level total travel cost 

for (unit.name in unit.names ) {
  
  is.first.msa <- TRUE
  
  for (msa.id in HHC_MSAs) {
    
    # Set output directory 
    OUTPUT_DIR <- file.path("output/Survey/NHTS09", names(HHC_MSAs)[HHC_MSAs==msa.id], unit.name)
    load(file.path(OUTPUT_DIR, "tcost.RData"))
    
    tcost.hh.msa <- tcost.hh %>% 
      dplyr::select(SAMPN, tcost) %>%
      mutate(msa.name=names(HHC_MSAs)[HHC_MSAs==msa.id])
    
    tcost.hh.lowInc.msa <- tcost.hh %>% 
      filter(inc.level=="lowInc")  %>% 
      dplyr::select(SAMPN, tcost) %>%
      mutate(msa.name=names(HHC_MSAs)[HHC_MSAs==msa.id])
    
    rm(tcost.hh)
    
    if (is.first.msa) {
      tcost.hh.lowInc.msas <- tcost.hh.lowInc.msa
      tcost.hh.msas <- tcost.hh.msa
      is.first.msa <- FALSE
      
    } else {
      tcost.hh.lowInc.msas <- rbind(tcost.hh.lowInc.msas, tcost.hh.lowInc.msa)
      tcost.hh.msas <- rbind(tcost.hh.msas, tcost.hh.msa)
    }
  } 
  
  tcost.hh.lowInc.msas.name <- paste("tcost.hh.lowInc.msas", unit.name, sep=".")
  assign(tcost.hh.lowInc.msas.name, tcost.hh.lowInc.msas)
  
  tcost.hh.msas.name <- paste("tcost.hh.msas", unit.name, sep=".")
  assign(tcost.hh.msas.name, tcost.hh.msas)
  
  rm(tcost.hh.msas, tcost.hh.lowInc.msas)
  
}


# Generate travel cost summary table for all MSAs 

tcost.hh.msas.minutes.sum <- tcost.hh.msas.minutes %>% 
  group_by(msa.name) %>%
  summarise(tcost.25percentile = quantile(tcost, 0.25), 
            tcost.mean=mean(tcost),
            tcost.median=median(tcost),
            tcost.75percentile = quantile(tcost, 0.75)) %>%
  as.data.frame()

output_file <- file.path(OUTPUT_DIR, "tcost_hh_msas_minutes_sum.csv")
write.csv(tcost.hh.msas.minutes.sum, file=output_file, row.names=FALSE) 


tcost.hh.msas.dollars.sum <- tcost.hh.msas.dollars %>% 
    group_by(msa.name) %>%
    summarise(tcost.25percentile = quantile(tcost, 0.25), 
              tcost.mean=mean(tcost),
              tcost.median=median(tcost),
              tcost.75percentile = quantile(tcost, 0.75)) %>%
    as.data.frame()

output_file <- file.path(OUTPUT_DIR, "tcost_hh_msas_dollars_sum.csv")
write.csv(tcost.hh.msas.dollars.sum, file=output_file, row.names=FALSE) 


tcost.hh.lowInc.msas.minutes.sum <- tcost.hh.lowInc.msas.minutes %>% 
  group_by(msa.name) %>%
  summarise(tcost.25percentile = quantile(tcost, 0.25), 
            tcost.mean=mean(tcost),
            tcost.median=median(tcost),
            tcost.75percentile = quantile(tcost, 0.75)) %>%
  as.data.frame()

output_file <- file.path(OUTPUT_DIR, "tcost_hh_lowInc_msas_minutes_sum.csv")
write.csv(tcost.hh.lowInc.msas.minutes.sum, file=output_file, row.names=FALSE) 


tcost.hh.lowInc.msas.dollars.sum <- tcost.hh.lowInc.msas.dollars %>% 
  group_by(msa.name) %>%
  summarise(tcost.25percentile = quantile(tcost, 0.25), 
            tcost.mean=mean(tcost),
            tcost.median=median(tcost),
            tcost.75percentile = quantile(tcost, 0.75)) %>%
  as.data.frame()

output_file <- file.path(OUTPUT_DIR, "tcost_hh_lowInc_msas_dollars_sum.csv")
write.csv(tcost.hh.lowInc.msas.dollars.sum, file=output_file, row.names=FALSE)

# Generate boxplots
boxp.msas.allInc.minutes <- ggplot(tcost.hh.msas.minutes[tcost.hh.msas.minutes$msa.name %in% names(HHC_MSAs), ], 
                                 aes(x=msa.name, y=tcost, fill=msa.name)) +
                                 geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + 
                                 xlab("MSAs") + ylim(0, 450) + coord_flip() +
                                 ggtitle("All income groups household-level total travel cost by MSAs") +
                                 theme(plot.title = element_text(face="bold", size=12, vjust=1),
                                        legend.position="none") 
                        
output_file <- file.path(OUTPUT_DIR, "boxp.msas.allInc.minutes.png")
ggsave(boxp.msas.allInc.minutes, file= output_file)


boxp.msas.allInc.dollars <- ggplot(tcost.hh.msas.dollars[tcost.hh.msas.dollars$msa.name %in% names(HHC_MSAs), ], 
                                   aes(x=msa.name, y=tcost, fill=msa.name)) +
  geom_boxplot() + labs(y="Generalized Travel Costs (dollars)") + 
  xlab("MSAs") + ylim(0, 200) + coord_flip() +
  ggtitle("All income groups household-level total travel cost by MSAs") +
  theme(plot.title = element_text(face="bold", size=12, vjust=1),
        legend.position="none") 

output_file <- file.path(OUTPUT_DIR, "boxp.msas.allInc.dollars.png")
ggsave(boxp.msas.allInc.dollars, file= output_file)


boxp.msas.lowInc.minutes <- ggplot(tcost.hh.lowInc.msas.minutes[tcost.hh.lowInc.msas.minutes$msa.name %in% names(HHC_MSAs), ], 
                                   aes(x=msa.name, y=tcost, fill=msa.name)) +
  geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + 
  xlab("MSAs") + ylim(0, 450) + coord_flip() +
  ggtitle("low income groups household-level total travel cost by MSAs") +
  theme(plot.title = element_text(face="bold", size=12, vjust=1),
        legend.position="none") 

output_file <- file.path(OUTPUT_DIR, "boxp.msas.lowInc.minutes.png")
ggsave(boxp.msas.lowInc.minutes, file= output_file)


boxp.msas.lowInc.dollars <- ggplot(tcost.hh.lowInc.msas.dollars[tcost.hh.lowInc.msas.dollars$msa.name %in% names(HHC_MSAs), ], 
                                   aes(x=msa.name, y=tcost, fill=msa.name)) +
  geom_boxplot() + labs(y="Generalized Travel Costs (dollars)") + 
  xlab("MSAs") + ylim(0, 200) + coord_flip() +
  ggtitle("Low income groups household-level total travel cost by MSAs") +
  theme(plot.title = element_text(face="bold", size=12, vjust=1),
        legend.position="none") 

output_file <- file.path(OUTPUT_DIR, "boxp.msas.lowInc.dollars.png")
ggsave(boxp.msas.lowInc.dollars, file= output_file)
