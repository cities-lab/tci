#This should be in a data file
INPUT_DIR <- 'data/NHTS09'
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

HHC_MSAs.short <- c("Atlanta, GA"= "0520", "Austin, TX" = "0640", "Boston, MA-NH-ME-CT"= "1122",
              "Buffalo, NY"="1280", "Charlotte, NC--SC" = "1520",
              "Chicago, IL-IN-WI"= "1602", "Cincinnati, OH-KY-IN"="1642", "Cleveland, OH"="1692",
              "Columbus, OH" = "1840", "Dallas, TX" = "1922", "Denver, CO" = "2082",
              "Detroit, MI" = "2162", "Grand Rapids, MI" = "3000",
              "Greensboro, NC" = "3120",  "Hartford, CT" = "3280", "Houston, TX" = "3362",
              "Indianapolis, IN" = "3480", "Jacksonville, FL" = "3600", "Kansas City, MO-KS" = "3760", "Las Vegas, NV-AZ" = "4120",
              "Los Angeles, CA"="4472", "Louisville, KY-IN"="4520", "Memphis, TN-AR-MS"="4920",
              "Miami, FL" = "4992", "Milwaukee, WI" = "5082", "Minneapolis, MN" = "5120",
              "Nashville, TN" = "5360", "New Orleans, LA" = "5560", "New York, NY-NJ-CT-PA" = "5602",
              "Norfolk, VA-NC" = "5720", "Oklahoma City, OK" = "5880", "Orlando, FL" = "5960",
              "Philadelphia,PA-NJ-DE-MD" = "6162", "Phoenix, AZ" = "6200", "Pittsburgh, PA" = "6280",
              "Portland, OR-WA" = "6442", "Providence, RI-MA" = "6480", "Raleigh, NC" = "6640",
              "Rochester, NY" = "6840", "Sacramento, CA"= "6922", "St. Louis, MO-IL" = "7040",
              "Salt Lake City, UT" = "7160", "San Antonio, TX" = "7240", "San Diego, CA" = "7320",
              "San Francisco, CA"= "7362", "Seattle, WA" = "7602",
              "Tampa, FL" = "8280",
              "Washington, DC-MD-VA-WV" = "8872", "West Palm Beach, FL" = "8960"
)

fileConn<-file.path(INPUT_DIR, "MSA_names.tab")
lines <- c(paste("FIPS", "full.name", "short.name", sep="\t"))

for (i in 1:length(HHC_MSAs)) {
  stopifnot(HHC_MSAs[i]==HHC_MSAs.short[i])
  lines <- c(lines, paste(paste0('"', HHC_MSAs[i], '"'), names(HHC_MSAs)[i], names(HHC_MSAs.short)[i], sep="\t"))
}

cat(lines, sep="\n", file=fileConn)
