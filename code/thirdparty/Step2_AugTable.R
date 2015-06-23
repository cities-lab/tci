
#Create TOUR/TRIP Table for Oregon Household Activity Survey
#Ben Stabler, stabler@pbworld.com, 06/22/09
#requires R in the environment %PATH% variable to run R
#required the RODBC library be installed as well
#command line call to execute script is:
#R --no-save < createTourTable.R > log_Region3.txt DB=OHAS_Region3_FinalData.mdb

#Revised 01/25/10 BTS
#1) WorkAtHome coded separately now
#2) added TOURHASBIKE, TOURHASDRIVE, TOURHASPNR, TOURFIRSTMODE, TOURACCESSMODETOFIRSTTRANSIT, TOUREGRESSMODETOLASTTRANSIT, TOURCUMDUR
#3) added same additional fields for work sub tours as well
#4) changed TOURDUR to include travel time
#5) code trips before a tour or work subtour as -2 (for better sorting)

#Revised 02/17/10 BTS
#1) uses PLACE fields XCORD and YCORD instead of XCORD2 and YCORD2
#2) calculate distance based on lat/long coordinates
#3) added AGGACT 99 (Other)

#Revised 07/08/10 BTS
#1) Added route distance calculated from ROUTE table

#Revised 11/30/10 AOB
#1) Added the ability to account for Trips where the first leg of the tour is on transit (they don't walk to the bus it picks them up at their front door)

#Revised 02/22/11 Alex and Joe
#1) Added 3 lines in function "addWorkSubTourFields" to assign the MODE, MODENAME and MODENAMECODE to "NA" for the first record of the subtour; It is because the MODE for the first record is the mode coming to the first place of the subtour. 

#Revised 07/08/11 BTS
#PLACE records sorted at start in case not sorted, TRIP table keep all trips not just valid tour trips, 
#"WorkAtHome" record now treated like a "Home" record, fixed case where some open ended tours were coded but should be set to -1, 
#added tour distance fields (TOURDIST, TOURDISTROUTE, WORKSUBTOURDIST, WORKSUBTOURDISTROUTE), removed left over duplicate 
#records from data processing, added Keith's linkedtrip table which appends ChangeMode data to next record and removes ChangeMode records

#Revised 08/01/11 BTS
#Optionally join hh, person, and veh fields
#User specified fields to write out for the trip, linkedtrip, and tour tables
#Fixed ChangeMode coding to skip if no trip record after a ChangeMode record

#Revised 09/12/11 BTS
#Fixed TOURHAS<MODE> fields so the previous tour return home mode is ignored and so PNR must include drive after last transit record as well

#Revised 10/31/11 BTS
#Update Last<Fields> in linkedTrips table for record after ChangeMode record to refer to place before ChangeMode record

#Revised 02/03/12 BTS
#Added ESCORTPURP field for trip purpose of person (acutally 1st additional person in the car) being escorted

#Revised 02/29/12 BTS
#Revised ESCORTPURP field to code purpose 9 ?Picked up Passenger? in addition to 8 ?Dropped off Passenger?

#Revised 08/03/12 BTS
#LINKEDTRIP table fields NextAGGACT, NextXCORD, NextYCORD, NextMODE, Distance, DistanceRoute, ThisMODE, and LastMODE updated.

#Revised 1/26/13 EW
#LINKEDTRIP table fields NumTransfers, LastPlaceNo, LastCity, NextCity added
#LINKEDTRIP table field linked trip mode calculation updated so that main mode is selected, rather than concatenating all modes 
#required the doParallel library to be added

#Revised 2/15/13 EW
#LINKEDTRIP table edited to add fuel cost calculation for drive or passenger segments of trip.  

#Revised 3/15/13 EW
#LINKEDTRIP table edited to add PNR and KNR trip modes to hierarchy
#Fuel Cost added to vehicle table
# Added shell command to get number of processors on machine
# Added TripPurpose field to linkedTrip table, contains the following purposes: HBW, HBO, HBShop, HBSch, NHBW, NHBNW HBWEsc, 
#    HBOEsc, HBShopEsc, HBSchEsc, NHBWEsc, NHBNWEsc, and by Production-Attraction Direction. 

# Revised 3/26/13 EW
# Added sort for all tables as they are read in from Access database
# Added sort for all tables that are output before they are exported to Access database

#Revised 4/4/13 BTS
# Fixed place table dopar call so it correctly returns all persons and tours
# Fixed a small bug in addWorkSubTourFields related to field naming - WORKSUBTOURNUMSTOPS

#Revised 5/1/13 EW
# Fixed issue with loop trips by dropping them as soon as place table is read in, then renumbering PLACENO field
# Added methodology to calculate the OTPURP field for 'trip to airport' and 'trip from airport' 
# If either 'trip to airport' or 'trip from airport' are coded as TPURP 7 (change mode), they are recoded to the purpose of the portion of trip on Airplane
# Added O_TPURP to output tables so 'trip to airport' and 'trip from airport' records will be identifiable

#Revised 5/29/13 EW
# Added line to recode original PLANO field to PLANO_Orig
# Added line to reset PLANO to PLANO_Orig before tables are export, this maintains original PLANO for each record

#Revised 6/21/13 EW
# Added LastDEP_HR and LastDEP_MIN to trip and linkedtrip tables

#Revised 9/12/13 AB
# Now reads cleaned R datasets and exports Rdatasets in 64-bit as opposed to Acces in 32-bit

#Revised 10/22/13 AB
# Small revistions for statewide dataset - no changes are made to the place table, gas price appended to household table

#Revised 10/23/13 BTS
# Revised AirportTripsRecode function to only apply to change mode trips

# Revised 11/1/2013 AB
# Small revisions to make consistent with Step 1 (Metro and RTC now seperate datasets)

# Revised 11/5/2013 AB
# Small revisions to spatially tag counties and cities

#Revised 02/26/13 BTS
# Revised coding of linked trip purpose

#Revised 03/11/14 BTS
# For linkedtrips, Fixed re-coding of next and last fields when only 1 linkedtrip per person
# For linkedtrips, Escort purpose was included with the Other purpose and now it is not
# The remaining trips with purpose=NA at the end of the procedure are either Home->Esc or Esc->Home and 
#   are coded HBOEsc or trips that do not have a Home end are now coded NHBEsc

# Revised 04/23/14 AB
# Records were not being properly ordered in last step - now corrected

# Revised 06/10/14 AB
# Added a mpo tag call at the end

# Revised 07/28/2014 AB
# Appended household and person weights and expansion factors to the linkedTrip table


######################################################
#Read in files
######################################################

# datasets and lcoations
#dbNames = list.files(path ="Step1_MoveDatatoR", pattern=".Ddata")
#dbNames <- c("Metro.Rdata","RTC.Rdata","MWVCOG.Rdata","LCOG.Rdata","RVMPO.Rdata","Bend.Rdata",               
#             "Region2.Rdata",   "Region3.Rdata",   "Region4.Rdata",   "Region5.Rdata") 

dbNames <- c("OHAS_PDX.RData")
LoadLocation <- "data/"
SaveLocation <- "output/OHAS"
refFileLocation <- "output/OHAS/augRefFiles/"

#activity aggregation coding file
activitiesFileName = paste(refFileLocation, "activities.csv", sep="")

#mode coding file
modesFileName = paste(refFileLocation, "modes.csv", sep="")

# Vehicle database from ODOT
vehicleDatabaseFileName = paste(refFileLocation, "Fuel_Economy.RData", sep="")

# Fuel Price Lookup
fuelPriceFileName = paste(refFileLocation, "FuelPriceUpdated.csv", sep="")

######################################################
#Parameters
######################################################

#remove temp file after written to DB
removeTempFile = FALSE

#join hh and person fields
joinHHPerData = FALSE

# Set number of processors to be used by doParallel package below
clusternumber =  as.integer(Sys.getenv("NUMBER_OF_PROCESSORS")) - 1

# Default MPG for vehicles prior to 1975
defaultMPG <- 15

#output table fields to write
tripFields = c("SAMPN","PERNO","PLANO","BEGTOURNUM","BEGWORKSUBTOURNUM","PNAME","TPURP","O_TPURP","MODE","VEHNO",
	"ARR_HR","ARR_MIN","DEP_HR","DEP_MIN","LastDEP_HR","LastDEP_MIN","TRPDUR","ACTDUR","ADDR","CITY","STATE","ZIP","XCORD","YCORD",
	"AGGACT","AGGACTCODE","MODENAME","MODENAMECODE","ESCORTPURP","ThisAGGACT","ThisXCORD","ThisYCORD","ThisMODE","LastAGGACT",
	"LastXCORD","LastYCORD","LastMODE","LastPLANO","LastCITY","NextAGGACT","NextXCORD","NextYCORD","NextMODE","NextCITY","Distance","DistanceRoute")

# 2-27-2014 AB Revised linkedTrip fields to include "LOCTYPE" from the place table
linkedTripFields = c("SAMPN","PERNO","PLANO","BEGTOURNUM","BEGWORKSUBTOURNUM","PNAME","TPURP","O_TPURP","MODE","VEHNO",
	"ARR_HR","ARR_MIN","DEP_HR","DEP_MIN","LastDEP_HR","LastDEP_MIN","TRPDUR","ACTDUR","ADDR","CITY","STATE","ZIP","XCORD","YCORD",
	"AGGACT","AGGACTCODE","MODENAME","MODENAMECODE","ESCORTPURP","ThisAGGACT","ThisXCORD","ThisYCORD","ThisMODE","NumTransfers","LastAGGACT",
	"LastXCORD","LastYCORD","LastMODE","LastPLANO","LastCITY","NextAGGACT","NextXCORD","NextYCORD","NextMODE","NextCITY","Distance","DistanceRoute",
	"CMNMTRPDUR","CMMOTTRPDUR","CMNMACTDUR","CMMOTACTDUR","CMMODENAMES","CMNMDIST","CMMOTDIST","CMNMDISTROUTE",
	"CMMOTDISTROUTE","CMHASTRANSIT","CMAUTODIST","FuelCost","TripPurpose","TripPurposePA","LOCTYPE")
	
tourFields = c("SAMPN","PERNO","PLANO","BEGTOURNUM","BEGWORKSUBTOURNUM","PNAME","ADDR","CITY","STATE","ZIP",
	"XCORD","YCORD","TOURHASTRANSIT","TOURHASBIKE","TOURHASDRIVE","TOURHASPNR","TOURMODES","TOURFIRSTMODE",
	"TOURACCESSMODETOFIRSTTRANSIT","TOUREGRESSMODETOLASTTRANSIT","TOURMANDACT","TOURACTS","TOURMANDPRIMXCORD",
	"TOURMANDPRIMYCORD","TOURDUR","TOURDEP_HR","TOURDEP_MIN","TOURNUMSTOPS","TOURDIST","TOURDISTROUTE",
	"WORKSUBTOURHASTRANSIT","WORKSUBTOURHASBIKE","WORKSUBTOURHASDRIVE","WORKSUBTOURHASPNR","WORKSUBTOURMODES",
	"WORKSUBTOURFIRSTMODE","WORKSUBTOURACCESSMODETOFIRSTTRANSIT","WORKSUBTOUREGRESSMODETOLASTTRANSIT","WORKSUBTOURACTS",
	"WORKSUBTOURDUR","WORKSUBTOURCUMDUR","WORKSUBTOURDEP_HR","WORKSUBTOURDEP_MIN","WORKSUBTOURNUMSTOPS",
	"WORKSUBTOURDIST","WORKSUBTOURDISTROUTE")

######################################################
#Function Definitions
######################################################

#distance function - lat/long
distance = function(x1,x2,y1,y2) { 

  dLat = (y2-y1)/180*pi
  dLon = (x2-x1)/180*pi
  a = sin(dLat/2) * sin(dLat/2) + cos(y1/180*pi) * cos(y2/180*pi) * sin(dLon/2) * sin(dLon/2)
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  d = 3958.82 * c; #(earth radius in miles) * c
  return(d * 5280) #distance feet
 }
 
#add escort purpose field
addEscortPurpose = function(placeIn) {

	#add field
	placeIn$ESCORTPURP = ""

	#get hh
	df = placeIn
	
	#drop off
	if("Escort" %in% df$AGGACT & 8 %in% df$TPURP) {

		y = df[df$AGGACT=="Escort" & df$TPURP == 8,c("PER1","ARR_HR","ARR_MIN")] #drop off
		y$ESCORTPURP = ""
		
		#change time to decimal
		y[,"ARR_TIME"] = y[,"ARR_HR"] + y[,"ARR_MIN"]/60
		df_arr_time = df$ARR_HR + df$ARR_MIN/60
		
		for (i in 1:nrow(y)) {
		
			#must have a PER1 value
			if(!is.na(y[i,"PER1"])) {
				
				#match for four minute window pre/post driver arrive time
				timeMatch = df_arr_time >= ( y[i,"ARR_TIME"] - 4/60) & df_arr_time <= ( y[i,"ARR_TIME"] + 4/60)
				matches = (df$PERNO == y[i,"PER1"] & timeMatch)
				if(any(matches)) {
					y$ESCORTPURP[i] = as.character(df$AGGACT[matches])[1] #take 1st
				} else {
					y$ESCORTPURP[i] = ""
				}
			}
		}
		
		df$ESCORTPURP[df$AGGACT=="Escort" & df$TPURP == 8] = y$ESCORTPURP
	}

	#pick up	
	if("Escort" %in% df$AGGACT & 9 %in% df$TPURP) {

		#get next record
		escort_records = which(df$AGGACT=="Escort" & df$TPURP == 9)
		y = df[escort_records + 1,c("PER1","DEP_HR","DEP_MIN")] #pick up
		y$ESCORTPURP = ""
		
		#change time to decimal
		y[,"DEP_TIME"] = y[,"DEP_HR"] + y[,"DEP_MIN"]/60
		df_dep_time = df$DEP_HR + df$DEP_MIN/60
		
		for (i in 1:nrow(y)) {
		
			#must have a PER1 value
			if(!is.na(y[i,"PER1"])) {
				
				#match for four minute window pre/post driver arrive time
				timeMatch = df_dep_time >= ( y[i,"DEP_TIME"] - 4/60) & df_dep_time <= ( y[i,"DEP_TIME"] + 4/60)
				matches = (df$PERNO == y[i,"PER1"] & timeMatch)
				if(any(matches)) {
					previous = which(matches) - 1 #previous record
					y$ESCORTPURP[i] = as.character(df$AGGACT[previous])[1] #take 1st
				} else {
					y$ESCORTPURP[i] = ""
				}
			}
		}
		
		df$ESCORTPURP[escort_records] = y$ESCORTPURP
	}

	#set field in master table
	placeIn$ESCORTPURP = df$ESCORTPURP
	
	return(placeIn)
}


#add This,Next,Last to each activity for activity, TAZ, and mode
addThisNextLast = function(df) {

    #This Activity,TAZ (X and Y),Mode
    df$ThisAGGACT = df$AGGACT
    df$ThisXCORD = df$XCORD
    df$ThisYCORD = df$YCORD
    df$ThisMODE = df$MODE
    
    #Last (Previous) Activity, TAZ, Mode, Place number, City
    lastPLANO = match(df$PLANO -1,df$PLANO)
    if(length(df$AGGACT) > 1) {
      df$LastAGGACT = df$AGGACT[lastPLANO]
      df$LastXCORD = df$XCORD[lastPLANO]
      df$LastYCORD = df$YCORD[lastPLANO]
      df$LastMODE = df$MODE[lastPLANO]
	  df$LastPLANO = df$PLANO[lastPLANO]
	  df$LastCITY = df$CITY[lastPLANO]  
	  df$LastDEP_HR = df$DEP_HR[lastPLANO]
	  df$LastDEP_MIN = df$DEP_MIN[lastPLANO]
    } else {
      df$LastAGGACT = NA
      df$LastXCORD = NA
      df$LastYCORD = NA
      df$LastMODE = NA
	  df$LastPLANO = NA
	  df$LastCITY = NA
	  df$LastDEP_HR = NA
	  df$LastDEP_MIN = df$DEP_MIN[lastPLANO]
    }
    
    #Next Activity, TAZ, Mode, City
    nextPLANO = match(df$PLANO +1,df$PLANO)
    
    if(length(df$AGGACT) > 1) {
      df$NextAGGACT = df$AGGACT[nextPLANO]
      df$NextXCORD = df$XCORD[nextPLANO]
      df$NextYCORD = df$YCORD[nextPLANO]
      df$NextMODE = df$MODE[nextPLANO]
	  df$NextCITY = df$CITY[nextPLANO]
    } else {
      df$NextAGGACT = NA
      df$NextXCORD = NA
      df$NextYCORD = NA
      df$NextMODE = NA
	  df$NextCITY = NA
    }
    
    #Trip straight line distance (ft)
    df$Distance = distance(df$LastXCORD, df$ThisXCORD, df$LastYCORD, df$ThisYCORD)
    
    #Trip route distance (ft) from ROUTE table
    df$DistanceRoute = routeDistance[paste(df$SAMPN, df$PERNO, df$PLANO-1, df$PLANO)]

    return(df)
}

#recalculate Next,Last fields for LINKEDTRIPS table
calcThisNextLastLinked = function(df) {

    #Last (Previous) Activity, TAZ, Mode
    lastIndex = 1:(nrow(df)-1)
    if(length(df$AGGACT) > 1) {
      df$LastAGGACT[2:nrow(df)] = df$AGGACT[lastIndex]
      df$LastXCORD[2:nrow(df)] = df$XCORD[lastIndex]
      df$LastYCORD[2:nrow(df)] = df$YCORD[lastIndex]
      df$LastMODE[2:nrow(df)] = df$ThisMODE[lastIndex]
	  df$LastPLANO[2:nrow(df)] = df$PLANO[lastIndex]
	  df$LastCITY[2:nrow(df)] = df$CITY[lastIndex]
	  df$LastDEP_HR[2:nrow(df)] = df$DEP_HR[lastIndex]
	  df$LastDEP_MIN[2:nrow(df)] = df$DEP_MIN[lastIndex]	  
    } 
    
    #Next Activity, TAZ, Mode
    nextIndex = c(2:nrow(df),NA)
    if(length(df$AGGACT) > 1) {
      df$NextAGGACT = df$AGGACT[nextIndex]
      df$NextXCORD = df$XCORD[nextIndex]
      df$NextYCORD = df$YCORD[nextIndex]
      df$NextMODE = df$ThisMODE[nextIndex]
	  df$NextCITY = df$CITY[nextIndex]
    } 
    
    return(df)
}

#add Change Mode Fields in order to remove change mode records later if needed
addChangeModeFields = function(df) {

	 #add change mode fields - non-motorized and motorized
	 df$CMNMTRPDUR = 0
	 df$CMMOTTRPDUR = 0
	 df$CMNMACTDUR = 0
	 df$CMMOTACTDUR = 0
	 df$CMMODENAMES = ""
	 df$CMNMDIST = 0
	 df$CMMOTDIST = 0
	 df$CMNMDISTROUTE = 0
	 df$CMMOTDISTROUTE = 0
	 df$CMHASTRANSIT = F
     df$AUTODIST = ifelse(df$MODENAME%in%c("DRIVER","PASSENGER"),df$Distance,0)
	 df$CMAUTODIST = 0 
	 df$CMVEHNO = ifelse(df$MODENAME%in%c("DRIVER","PASSENGER"),df$VEHNO,0)
	 
	 #identify change mode records and what record each change mode record needs to be copied to
	 if(any(df$AGGACT == "ChangeMode")) {
		 
		 changeModeRecords = which(df$AGGACT == "ChangeMode")
		 tripRecords = which(df$AGGACT != "ChangeMode")
		 changeModeTripRecords = sapply(changeModeRecords, function(x) tripRecords[which(x < tripRecords)[1]])
	
		 #code change mode fields only if there are trip records after the change mode records
		 if(!all(is.na(changeModeTripRecords))) {

				for(i in 1:length(changeModeRecords)) {
					
					changeModeRecord = changeModeRecords[i]
					targetRecord = changeModeTripRecords[i]
	
					#add up mode names
					df$CMMODENAMES[targetRecord] = paste(df$CMMODENAMES[targetRecord], as.character(df$MODENAME[changeModeRecord]), collapse=" ")
										
					#is this a transit trip
					isNA = is.na(df$MODENAME) #replace NA with OTHER temporarily
					df$MODENAME[is.na(df$MODENAME)] = "OTHER" 
					if(df$MODENAME[changeModeRecord] == "TRANSIT" | df$CMMODENAMES[targetRecord] == "TRANSIT") {
						df$CMHASTRANSIT[targetRecord] = T 
					}
					df$MODENAME[isNA] = NA #replace with NA
					
					#add up duration and distance
					if(df$MODENAME[changeModeRecord] %in% c("WALK","BIKE")) {
						
						#set non-motorized change mode fields	
						df$CMNMTRPDUR[targetRecord] = df$CMNMTRPDUR[targetRecord] + df$TRPDUR[changeModeRecord]
						df$CMNMACTDUR[targetRecord] = df$CMNMACTDUR[targetRecord] + df$ACTDUR[changeModeRecord]
						df$CMNMDIST[targetRecord] = df$CMNMDIST[targetRecord] + df$Distance[changeModeRecord]
						df$CMNMDISTROUTE[targetRecord] = df$CMNMDISTROUTE[targetRecord] + df$DistanceRoute[changeModeRecord]
					} else {
					
						#set motorized change mode fields	
						df$CMMOTTRPDUR[targetRecord] = df$CMMOTTRPDUR[targetRecord] + df$TRPDUR[changeModeRecord]
						df$CMMOTACTDUR[targetRecord] = df$CMMOTACTDUR[targetRecord] + df$ACTDUR[changeModeRecord]
						df$CMMOTDIST[targetRecord] = df$CMMOTDIST[targetRecord] + df$Distance[changeModeRecord]
						df$CMMOTDISTROUTE[targetRecord] = df$CMMOTDISTROUTE[targetRecord] + df$DistanceRoute[changeModeRecord]
						}
				
				if(df$MODENAME[changeModeRecord] %in% c("DRIVER","PASSENGER")){
				    df$CMAUTODIST[targetRecord] = df$AUTODIST[targetRecord]+df$AUTODIST[changeModeRecord]
				    df$VEHNO[is.na(df$VEHNO)] = 0
					df$CMVEHNO[targetRecord] = max(df$VEHNO[targetRecord],df$VEHNO[changeModeRecord],na.rm=TRUE)

					}
				 
				}
				
			}

		}

   return(df)
}

#identify tours function
identifyTours = function(df) {

    #home-based tour number
    df$BEGTOURNUM = cumsum(as.integer(df$AGGACT == "Home" | df$AGGACT == "WorkAtHome"))
    df$ENDTOURNUM = df$BEGTOURNUM - c(0,diff(df$BEGTOURNUM))
    
   #add extra records for tour end and tour start activities
    extraRows = df[df$BEGTOURNUM != df$ENDTOURNUM,]
    extraRows$BEGTOURNUM = extraRows$ENDTOURNUM
    df = rbind(df, extraRows)
    df = df[order(df$SAMPN,df$PERNO,df$PLANO,df$BEGTOURNUM),]
    
    #NA (set to -2) invalid start tours
    if(any(df$BEGTOURNUM == 1)) {
      firstValidIndex = which(df$BEGTOURNUM == 1)[1]
      if(firstValidIndex>1) {
        df$BEGTOURNUM[1:(firstValidIndex-1)] = -2
        df$ENDTOURNUM[1:(firstValidIndex-1)] = -2
      }
    }  
    
    #if last activity is the only with that code then set to -1
    countTourNums = table(df$BEGTOURNUM)
    for(tourNum in names(countTourNums)) {
      if(countTourNums[as.character(tourNum)] <= 2 & tourNum > 0) {
        df$BEGTOURNUM[df$BEGTOURNUM == tourNum] = -1
        df$ENDTOURNUM[df$ENDTOURNUM == tourNum] = -1
      }
    }
    
    #if last activity is not HOME then set to -1
    lastTourNum = df$ENDTOURNUM[nrow(df)]
    if(!(df$AGGACT[nrow(df)] %in% c("Home","WorkAtHome"))) {
    	df$BEGTOURNUM[df$BEGTOURNUM == lastTourNum] = -1
    	df$ENDTOURNUM[df$ENDTOURNUM == lastTourNum] = -1
    }
    
    return(df)
}

#add tour fields
addTourFields = function(df) { 

    #identify if transit on tour
    df$TOURHASTRANSIT = FALSE
    if(any(df$MODENAME[2:nrow(df)] == "TRANSIT", na.rm=T)) {
      df$TOURHASTRANSIT = TRUE
    }
    
    #identify if bike on tour
    df$TOURHASBIKE = FALSE
    if(any(df$MODENAME[2:nrow(df)] == "BIKE", na.rm=T)) {
      df$TOURHASBIKE = TRUE
    }
 
     #identify if drive on tour
    df$TOURHASDRIVE = FALSE
    if(any(df$MODENAME[2:nrow(df)] == "DRIVER" | df$MODENAME[2:nrow(df)] == "PASSENGER", na.rm=T)) {
      df$TOURHASDRIVE = TRUE
    }
 
     #identify if park and ride on tour - both HASDRIVE and HASTRANSIT and Drive before transit and Drive after transit
    df$TOURHASPNR = FALSE
    if(any(df$TOURHASDRIVE, na.rm=T) & any(df$TOURHASTRANSIT, na.rm=T)) {
  
    		driveRecords = which(df$MODENAME == "DRIVER" | df$MODENAME == "PASSENGER")
      	transitRecords = which(df$MODENAME == "TRANSIT")
      	
      if(driveRecords[1] < transitRecords[1]) { #before
      	if(driveRecords[length(driveRecords)] > transitRecords[length(transitRecords)]) { #after
        	df$TOURHASPNR = TRUE
        }
      }
    }
    
    #concat all tour modes
    df$TOURMODES = paste(df$MODENAMECODE[2:nrow(df)], collapse=" ")

    #identify first mode on tour
    df$TOURFIRSTMODE = df$MODENAMECODE[2]

    #identify access mode to first transit
    df$TOURACCESSMODETOFIRSTTRANSIT = NA
    if(any(df$TOURHASTRANSIT, na.rm=T)) {
    
      transitRecNum = min(which(df$MODENAME == "TRANSIT"))
      
      # extra if statement to handel cases where there is no walk trip to transit
      if(transitRecNum == 1){  
        df$TOURACCESSMODETOFIRSTTRANSIT = NA
      } else {
      if(df$AGGACT[transitRecNum - 1] == "ChangeMode") {
        df$TOURACCESSMODETOFIRSTTRANSIT = df$MODENAMECODE[transitRecNum - 1]
      } else {
        df$TOURACCESSMODETOFIRSTTRANSIT = NA #should have a ChangeMode record before
      }
      }
    
    }
    
    #identify egress mode to last transit
    df$TOUREGRESSMODETOLASTTRANSIT = NA
    if(any(df$TOURHASTRANSIT, na.rm=T)) {
    
      transitRecNum = max(which(df$MODENAME == "TRANSIT"))
      
      if(df$AGGACT[transitRecNum] == "ChangeMode") { #transit trip is ChangeMode trip and then get next mode
        df$TOUREGRESSMODETOLASTTRANSIT = df$MODENAMECODE[transitRecNum + 1]
      } else {
        df$TOUREGRESSMODETOLASTTRANSIT = NA #should have a ChangeMode record after
      }
      
    }
      
    #tour primary activity
    rowNum = -1
    if(any(df$AGGACT == "Work")) {
      rowNum = which(df$AGGACT  == "Work")[1]
      df$TOURMANDACT = "Work"
    } else if (any(df$AGGACT == "School")) {
      rowNum = which(df$AGGACT  == "School")[1]
      df$TOURMANDACT = "School"
    } else {
      df$TOURMANDACT = NA
    }
    
    #tour activities
    df$TOURACTS = paste(df$AGGACTCODE[2:(nrow(df)-1)], collapse=" ")
    
    #mandatory tour primary destination
    if(rowNum > 0) {
      df$TOURMANDPRIMXCORD = df$XCORD[rowNum]
      df$TOURMANDPRIMYCORD = df$YCORD[rowNum]
    } else {
      df$TOURMANDPRIMXCORD = NA
      df$TOURMANDPRIMYCORD = NA
    }
    
    #tour duration (includes travel time)
    dur = sum(df$ACTDUR[2:(length(df$ACTDUR)-1)] + df$TRPDUR[2:(length(df$TRPDUR)-1)])
    df$TOURDUR = dur + df$TRPDUR[nrow(df)]
    
    #duration from tour start time (includes travel time)
    cumdur = cumsum(df$ACTDUR[2:(length(df$ACTDUR)-1)] + df$TRPDUR[2:(length(df$TRPDUR)-1)])
    df$TOURCUMDUR = c(0, cumdur, (max(cumdur) + df$TRPDUR[nrow(df)])) #c(0, trp+act, final trp)
    
    #tour start hr and min
    df$TOURDEP_HR = df$DEP_HR[1]
    df$TOURDEP_MIN = df$DEP_MIN[1]
    
    #tour number of stops
    df$TOURNUMSTOPS = nrow(df) - 2 #(Home,X,X,Home)
    
    #tour distance
    df$TOURDIST = sum(df$Distance[2:nrow(df)])
    
    #tour route distance
    df$TOURDISTROUTE = sum(df$DistanceRoute[2:nrow(df)])
    
    return(df)
}

#identify work subtours
identifyWorkSubTour = function(df) {

  df$BEGWORKSUBTOURNUM = -1
  df$ENDWORKSUBTOURNUM = -1

  #only if more than 2 Work codes
  if("Work" %in% df$AGGACT) {
    if(sum(as.integer(df$AGGACT == "Work")) > 1) {
    
      df$BEGWORKSUBTOURNUM = cumsum(as.integer(df$AGGACT == "Work"))
      
      #NA before Work activity
      df$BEGWORKSUBTOURNUM[df$BEGWORKSUBTOURNUM == 0] = -1
      
      #NA the last work start codes if there isn't a work end code as well
      numPossibleWorkSubtours = (length(unique(df$BEGWORKSUBTOURNUM[df$BEGWORKSUBTOURNUM>0]))-1)
      if(numPossibleWorkSubtours %% 2) { 
        df$BEGWORKSUBTOURNUM[df$BEGWORKSUBTOURNUM == df$BEGWORKSUBTOURNUM[length(df$BEGWORKSUBTOURNUM)]] = 9999
      }
      df$ENDWORKSUBTOURNUM = df$BEGWORKSUBTOURNUM - c(0,diff(df$BEGWORKSUBTOURNUM))
    
      #add extra records for work sub tour end and tour start activities
      index = df$BEGWORKSUBTOURNUM != df$ENDWORKSUBTOURNUM
      index[is.na(index)] = FALSE
      extraRows = df[index,]
      extraRows$BEGWORKSUBTOURNUM = extraRows$ENDWORKSUBTOURNUM
      
      df = rbind(df, extraRows)
      df = df[order(df$SAMPN,df$PERNO,df$PLANO,df$BEGWORKSUBTOURNUM),]
      df$BEGWORKSUBTOURNUM[df$BEGWORKSUBTOURNUM == 9999] = -1
      df$ENDWORKSUBTOURNUM[df$ENDWORKSUBTOURNUM == 9999] = -1
      
      #NA (set to -2) out invalid start work subtours
      firstValidIndex = which(df$BEGWORKSUBTOURNUM == 1)[1]
      if(firstValidIndex>1) {
        df$BEGWORKSUBTOURNUM[1:(firstValidIndex-1)] = -2
        df$ENDWORKSUBTOURNUM[1:(firstValidIndex-1)] = -2
      }
      
      #if last activity is the only with that code then set to -1
      countTourNums = table(df$BEGWORKSUBTOURNUM)
      for(tourNum in names(countTourNums)) {
        if(countTourNums[as.character(tourNum)] <= 2 & tourNum > 0) {
          df$BEGWORKSUBTOURNUM[df$BEGWORKSUBTOURNUM == tourNum] = -1
          df$ENDWORKSUBTOURNUM[df$ENDWORKSUBTOURNUM == tourNum] = -1
        }
      }
      
    }
  }
   
  return(df)
}

#add work subtour fields
addWorkSubTourFields = function(df) {

    #Alex and Joe Added the following 3 lines because the MODE for the
    #first record is the mode coming to the first place of the subtour. 
    df$MODENAME[1] <- NA
    df$MODE[1] <- NA
    df$MODENAMECODE[1]<- NA

    #work sub tour has transit
    df$WORKSUBTOURHASTRANSIT = FALSE
    if(any(df$MODENAME[2:nrow(df)] == "TRANSIT", na.rm=T)) {
      df$WORKSUBTOURHASTRANSIT = TRUE
    }
    
    #identify if bike on tour
    df$WORKSUBTOURHASBIKE = FALSE
    if(any(df$MODENAME[2:nrow(df)] == "BIKE", na.rm=T)) {
      df$WORKSUBTOURHASBIKE = TRUE
    }
 
    #identify if drive on tour
    df$WORKSUBTOURHASDRIVE = FALSE
    if(any(df$MODENAME[2:nrow(df)] == "DRIVER" | df$MODENAME[2:nrow(df)] == "PASSENGER", na.rm=T)) {
      df$WORKSUBTOURHASDRIVE = TRUE
    }
 
    #identify if park and ride on tour - both HASDRIVE and HASTRANSIT and Drive before transit
    df$WORKSUBTOURHASPNR = FALSE
    if(any(df$WORKSUBTOURHASDRIVE, na.rm=T) & any(df$WORKSUBTOURHASTRANSIT, na.rm=T)) {
    
    	driveRecords = which(df$MODENAME == "DRIVER" | df$MODENAME == "PASSENGER")
      transitRecords = which(df$MODENAME == "TRANSIT")
    
    	if(driveRecords[1] < transitRecords[1]) { #before
      	if(driveRecords[length(driveRecords)] > transitRecords[length(transitRecords)]) { #after
        	df$WORKSUBTOURHASPNR = TRUE
        }
      }    
    }    
    
    #work sub tour modes
    df$WORKSUBTOURMODES = paste(df$MODENAMECODE[2:nrow(df)], collapse=" ")
    
    #identify first mode on tour
    df$WORKSUBTOURFIRSTMODE = df$MODENAMECODE[2]
    
    #identify access mode to first transit
    df$WORKSUBTOURACCESSMODETOFIRSTTRANSIT = NA
    if(any(df$WORKSUBTOURHASTRANSIT, na.rm=T)) {
    
      transitRecNum = min(which(df$MODENAME == "TRANSIT"))
      
      if(transitRecNum>1) {
	      if(df$AGGACT[transitRecNum - 1] == "ChangeMode") {
	        df$WORKSUBTOURACCESSMODETOFIRSTTRANSIT = df$MODENAMECODE[transitRecNum - 1]
	      } else {
	        df$WORKSUBTOURACCESSMODETOFIRSTTRANSIT = NA #should have a ChangeMode record before
	      }
      } else {
      		df$WORKSUBTOURACCESSMODETOFIRSTTRANSIT = NA #should have a ChangeMode record before
      }
      
    }
    
    #identify egress mode to last transit
    df$WORKSUBTOUREGRESSMODETOLASTTRANSIT = NA
    if(any(df$WORKSUBTOURHASTRANSIT, na.rm=T)) {
    
      transitRecNum = max(which(df$MODENAME == "TRANSIT"))
      
      if(transitRecNum < nrow(df)) {
	      if(df$AGGACT[transitRecNum] == "ChangeMode") { #transit trip is ChangeMode trip and then get next mode
	        df$WORKSUBTOUREGRESSMODETOLASTTRANSIT = df$MODENAMECODE[transitRecNum + 1]
	      } else {
	        df$WORKSUBTOUREGRESSMODETOLASTTRANSIT = NA #should have a ChangeMode record after
	      }
      } else {
        df$WORKSUBTOUREGRESSMODETOLASTTRANSIT = NA #should have a ChangeMode record after
      }
      
    }
    
    #work tour activities
    df$WORKSUBTOURACTS = paste(df$AGGACTCODE[2:(nrow(df)-1)], collapse=" ")
    
    #work sub tour duration
    dur = sum(df$ACTDUR[2:(length(df$ACTDUR)-1)] + df$TRPDUR[2:(length(df$TRPDUR)-1)])
    df$WORKSUBTOURDUR = dur + df$TRPDUR[nrow(df)]

    #work tour cumulative duration (includes travel time)
    cumdur = cumsum(df$ACTDUR[2:(length(df$ACTDUR)-1)] + df$TRPDUR[2:(length(df$TRPDUR)-1)])
    df$WORKSUBTOURCUMDUR = c(0, cumdur, (max(cumdur) + df$TRPDUR[nrow(df)])) #c(0, trp+act, final trp)
    
    #work sub tour start hr and min
    df$WORKSUBTOURDEP_HR = df$DEP_HR[1]
    df$WORKSUBTOURDEP_MIN = df$DEP_MIN[1]
    
    #tour number of stops
    df$WORKSUBTOURNUMSTOPS = nrow(df) - 2 #(Work,X,X,Work)
    
    #work sub tour distance
    df$WORKSUBTOURDIST = sum(df$Distance[2:nrow(df)])
    
    #work sub tour route distance
    df$WORKSUBTOURDISTROUTE = sum(df$DistanceRoute[2:nrow(df)])
    
    return(df)
}

#create linked trip table - drop change mode records
createLinkedTripTable = function(trip, sampn) {

  #copy table
  linkedTrip = trip
     
	#code 1st trip Last fields before removing ChangeMode records
	changeModeRecords = which(trip$ThisAGGACT == "ChangeMode")
	tripRecords = which(trip$ThisAGGACT != "ChangeMode")
	changeModeRecord = rep(0, length(tripRecords))
	for(i in 1:length(tripRecords)) {
	  if(i==1) {
	  	changeModeRecord[i] = (changeModeRecords[tripRecords[i] > changeModeRecords][1])
	  } else {
			changeModeRecord[i] = (changeModeRecords[tripRecords[i] > changeModeRecords & changeModeRecords > tripRecords[i-1]][1])
		}
	}
	tripRecords = tripRecords[!is.na(changeModeRecord)]
	changeModeRecord = changeModeRecord[!is.na(changeModeRecord)]
	linkedTrip$LastAGGACT[tripRecords] = linkedTrip$LastAGGACT[changeModeRecord]
	linkedTrip$LastXCORD[tripRecords] = linkedTrip$LastXCORD[changeModeRecord]
	linkedTrip$LastYCORD[tripRecords] = linkedTrip$LastYCORD[changeModeRecord]
	linkedTrip$LastMODE[tripRecords] = linkedTrip$LastMODE[changeModeRecord]
	linkedTrip$LastPLANO[tripRecords] = linkedTrip$LastPLANO[changeModeRecord]
	linkedTrip$LastCITY[tripRecords] = linkedTrip$LastCITY[changeModeRecord]
	linkedTrip$LastCITY[tripRecords] = linkedTrip$LastCITY[changeModeRecord]
    linkedTrip$LastDEP_HR[tripRecords] = linkedTrip$LastDEP_HR[changeModeRecord]
    linkedTrip$LastDEP_MIN[tripRecords] = linkedTrip$LastDEP_MIN[changeModeRecord]
		
	#distances set to change mode distances + final record distance
	#this mode set to change modes + final record mode
	linkedTrip = linkedTrip[linkedTrip$ThisAGGACT!="ChangeMode",]
	linkedTrip$Distance = linkedTrip$Distance + linkedTrip$CMNMDIST + linkedTrip$CMMOTDIST 
	linkedTrip$DistanceRoute = linkedTrip$DistanceRoute + linkedTrip$CMNMDISTROUTE + linkedTrip$CMMOTDISTROUTE
 	linkedTrip$ThisMODE = gsub("^ ","",paste(linkedTrip$CMMODENAMES, linkedTrip$MODENAME))
    
	# linked trip mode is initially a concatenation of all modes on trips, use field to determine transit transfers
	linkedTrip$NumTransfers = linkedTrip$ThisMODE
	linkedTrip$NumTransfers = gsub(".*TRANSIT TRANSIT TRANSIT.*",2,linkedTrip$NumTransfers)	
	linkedTrip$NumTransfers = gsub(".*TRANSIT .* TRANSIT TRANSIT.*",2,linkedTrip$NumTransfers)
	linkedTrip$NumTransfers = gsub(".*TRANSIT TRANSIT .* TRANSIT.*",2,linkedTrip$NumTransfers)
	linkedTrip$NumTransfers = gsub(".*TRANSIT .* TRANSIT .* TRANSIT.*",2,linkedTrip$NumTransfers)	
	linkedTrip$NumTransfers = gsub(".*TRANSIT TRANSIT.*",1,linkedTrip$NumTransfers)
	linkedTrip$NumTransfers = gsub(".*TRANSIT .* TRANSIT.*",1,linkedTrip$NumTransfers)
	
	linkedTrip$NumTransfers = ifelse(linkedTrip$NumTransfers%in%c("1","2"),linkedTrip$NumTransfers,0)
	
	#Combinations of modes are replaced by the highest mode in the hierarchy. Hierarchy is as listed below. 
	linkedTrip$ThisMODE = gsub("DRIVER .*TRANSIT.*","PNR",linkedTrip$ThisMODE) # Combination starts with Drive, coded as PNR
	linkedTrip$ThisMODE = gsub(".*TRANSIT.* DRIVER","PNR",linkedTrip$ThisMODE) # Combination ends with Drive, coded as PNR
	linkedTrip$ThisMODE = gsub("PASSENGER .*TRANSIT.*","KNR",linkedTrip$ThisMODE) # Combination starts with Passenger, coded as KNR
	linkedTrip$ThisMODE = gsub(".*TRANSIT.* PASSENGER","KNR",linkedTrip$ThisMODE) # Combination ends with Passenger, coded as KNR
	linkedTrip$ThisMODE = gsub("TAXI .*TRANSIT.*","KNR",linkedTrip$ThisMODE) # Combination starts with Passenger, coded as KNR
	linkedTrip$ThisMODE = gsub(".*TRANSIT.* TAXI","KNR",linkedTrip$ThisMODE) # Combination ends with Passenger, coded as KNR
	linkedTrip$ThisMODE = gsub("PARATRANSIT .*TRANSIT.*","KNR",linkedTrip$ThisMODE) # Combination starts with PARATRANSIT, coded as KNR
	linkedTrip$ThisMODE = gsub(".*TRANSIT.* PARATRANSIT","KNR",linkedTrip$ThisMODE) # Combination ends with PARATRANSIT, coded as KNR
	linkedTrip$ThisMODE = gsub("CAR/VANPOOL .*TRANSIT.*","KNR",linkedTrip$ThisMODE) # Combination starts with CAR/VANPOOL, coded as KNR
	linkedTrip$ThisMODE = gsub(".*TRANSIT.* CAR/VANPOOL","KNR",linkedTrip$ThisMODE) # Combination ends with CAR/VANPOOL, coded as KNR
	linkedTrip$ThisMODE = gsub(".*TRANSIT.*","TRANSIT",linkedTrip$ThisMODE) # Combination contains transit, but no auto access, coded as transit
	linkedTrip$ThisMODE = gsub(".*SCHOOLBUS.*","SCHOOLBUS",linkedTrip$ThisMODE) # combination contains school bus, coded as schoolbus
	linkedTrip$ThisMODE = gsub(".*CAR/VANPOOL.*","CAR/VANPOOL",linkedTrip$ThisMODE) # Combination contains CAR/VANPOOL but not Transit, coded as CAR/VANPOOL
	linkedTrip$ThisMODE = gsub(".*PARATRANSIT.*","PARATRANSIT",linkedTrip$ThisMODE) # combination contains PARATRANSIT but does not include transit, coded as PARATRANSIT
	linkedTrip$ThisMODE = gsub(".*DRIVER.*","DRIVER",linkedTrip$ThisMODE) # combination contains DRIVER but not transit, coded as DRIVER
	linkedTrip$ThisMODE = gsub(".*PASSENGER.*","PASSENGER",linkedTrip$ThisMODE) # combination contains PASSENGER but not transit, coded as PASSENGER
	linkedTrip$ThisMODE = gsub(".*TAXI.*","TAXI",linkedTrip$ThisMODE) # combination contains TAXI but not transit, coded as TAXI
	linkedTrip$ThisMODE = gsub(".*BIKE.*","BIKE",linkedTrip$ThisMODE) # This logic needs updated, to reflect that if the Bike distance is longer than motorized, then it should be bike, otherwise motorized
	linkedTrip$ThisMODE = gsub(".*WALK.*","WALK",linkedTrip$ThisMODE)
	linkedTrip$ThisMODE = gsub(".*OTHER.*","OTHER",linkedTrip$ThisMODE)
	
	#loop through sampns and perno and update next, last fields
	for(i in 1:length(sampn)) {
	  
	  #get perno for sampn
	  if(sampn[i] %in% linkedTrip$SAMPN) {
	  	perno = unique(linkedTrip$PERNO[linkedTrip$SAMPN == sampn[i]])
	
	  	#loop through perno
	  	for(j in 1:length(perno)) {
	  	
	  	 #print current sampn and perno being processed
	     cat(paste("sampn (", i, "of", length(sampn), "):", sampn[i], ", perno:", perno[j], "\n"))
	    
	    	#get rows
	    	linkedTrip2 = linkedTrip[linkedTrip$SAMPN == sampn[i] & linkedTrip$PERNO == perno[j],]
	    
	    	#re-calculate fields
	    	linkedTrip2 = calcThisNextLastLinked(linkedTrip2)
	    	linkedTrip[linkedTrip$SAMPN == sampn[i] & linkedTrip$PERNO == perno[j],] = linkedTrip2
	  	}
	  }
	}

	linkedTrip$ThisAGGACT=as.character(linkedTrip$ThisAGGACT)
	linkedTrip$NextAGGACT=as.character(linkedTrip$NextAGGACT)
	linkedTrip$LastAGGACT=as.character(linkedTrip$LastAGGACT)
	
	#code ESCORTPURP for AP as well
  linkedTrip$ESCORTPURP[linkedTrip$LastAGGACT=="Escort"] = linkedTrip$ESCORTPURP[which(linkedTrip$LastAGGACT=="Escort")-1]
  linkedTrip$ESCORTPURP[is.na(linkedTrip$ESCORTPURP)] = ""
  linkedTrip$ESCORTPURP[linkedTrip$ESCORTPURP=="Escort"] = ifelse(linkedTrip$ThisAGGACT[linkedTrip$ESCORTPURP=="Escort"]=="Escort",
  linkedTrip$LastAGGACT[linkedTrip$ESCORTPURP=="Escort"],linkedTrip$ThisAGGACT[linkedTrip$ESCORTPURP=="Escort"])
		
	#code PA/AP
	linkedTrip$ProdAttr=ifelse(linkedTrip$LastAGGACT=="Home","PA","AP")
	linkedTrip$ProdAttr=ifelse(linkedTrip$AGGACT=="Home" & linkedTrip$LastAGGACT=="Home","PA",linkedTrip$ProdAttr)

  #purpose codes
  homeLabels = c("Home","WorkAtHome")
  workLabels = c("Work","WorkRelated")
  othLabels  = c("EatOut","PersonalBus","SocialRec","Health","Other")
  shopLabels = c("Shopping")
  recLabels  = c("Recreation")
  schLabels  = c("School","SchoolRelated")
  notWorkLabels = c(othLabels,shopLabels,recLabels,schLabels)
  
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% workLabels & linkedTrip$LastAGGACT %in% homeLabels,"HBW","NA")
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% othLabels  & linkedTrip$LastAGGACT %in% homeLabels,"HBO",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% shopLabels & linkedTrip$LastAGGACT %in% homeLabels,"HBShp",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% recLabels  & linkedTrip$LastAGGACT %in% homeLabels,"HBRec",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% schLabels  & linkedTrip$LastAGGACT %in% homeLabels,"HBSch",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% workLabels & !(linkedTrip$LastAGGACT %in% homeLabels),"NHBW",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% notWorkLabels & !(linkedTrip$LastAGGACT %in% homeLabels),"NHBNW",linkedTrip$TripPurpose)
	
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$LastAGGACT %in% workLabels,"HBW",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$LastAGGACT %in% othLabels,"HBO",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$LastAGGACT %in% shopLabels,"HBShp",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$LastAGGACT %in% recLabels,"HBRec",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$LastAGGACT %in% schLabels,"HBSch",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% !(linkedTrip$LastAGGACT %in% homeLabels) & linkedTrip$LastAGGACT %in% workLabels,"NHBW",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% !(linkedTrip$LastAGGACT %in% homeLabels) & linkedTrip$LastAGGACT %in% notWorkLabels,"NHBNW",linkedTrip$TripPurpose)
 
	# Specify Escort Trip Purposes
	linkedTrip$TripPurpose=ifelse(linkedTrip$ESCORTPURP %in% workLabels & linkedTrip$LastAGGACT %in% homeLabels,"HBWEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$ESCORTPURP %in% othLabels  & linkedTrip$LastAGGACT %in% homeLabels,"HBOEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$ESCORTPURP %in% shopLabels & linkedTrip$LastAGGACT %in% homeLabels,"HBShpEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$ESCORTPURP %in% recLabels  & linkedTrip$LastAGGACT %in% homeLabels,"HBRecEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$ESCORTPURP %in% schLabels  & linkedTrip$LastAGGACT %in% homeLabels,"HBSchEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$ESCORTPURP %in% workLabels & !(linkedTrip$LastAGGACT %in% homeLabels),"NHBWEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$ESCORTPURP %in% notWorkLabels & !(linkedTrip$LastAGGACT %in% homeLabels),"NHBNWEsc",linkedTrip$TripPurpose)

  linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$ESCORTPURP %in% workLabels,"HBWEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$ESCORTPURP %in% othLabels,"HBOEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$ESCORTPURP %in% shopLabels,"HBShpEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$ESCORTPURP %in% recLabels,"HBRecEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% homeLabels & linkedTrip$ESCORTPURP %in% schLabels,"HBSchEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% !(linkedTrip$ESCORTPURP %in% homeLabels) & linkedTrip$ESCORTPURP %in% workLabels,"NHBWEsc",linkedTrip$TripPurpose)
	linkedTrip$TripPurpose=ifelse(linkedTrip$AGGACT %in% !(linkedTrip$ESCORTPURP %in% homeLabels) & linkedTrip$ESCORTPURP %in% notWorkLabels,"NHBNWEsc",linkedTrip$TripPurpose)
	
	#Recode miscoded Escort trips as HBO or NHB
	linkedTrip$TripPurpose[linkedTrip$TripPurpose=="NA"] = ifelse(linkedTrip$AGGACT[linkedTrip$TripPurpose=="NA"] %in% homeLabels | 
	linkedTrip$LastAGGACT[linkedTrip$TripPurpose=="NA"] %in% homeLabels,"HBOEsc","NHBEsc")

  #Trip Purpose + PA field 
  linkedTrip$TripPurposePA=paste(linkedTrip$TripPurpose,linkedTrip$ProdAttr,sep="")

	#return linked trip table
	return(linkedTrip)
}

#Function to Assign mpg to each record in the vehicles table
#===========================================================

assignMPG <- function(veh, vehNames){

  # prep data by assigning names and pulling out correct fields
  names(veh) <- vehNames
  veh$O_MAKE=as.character(veh$O_MAKE)
  veh$MODEL=as.character(veh$MODEL)		
  Make <- MakeCW[MakeCW[,1]==as.integer(veh["MAKE"]),2]
  O.Make <- veh["O_MAKE"]
  Model <- veh["MODEL"]
  if(is.na(Model)) Model = "DK/RF"
  veh <- as.numeric(veh[c("SAMPN", "VEHNO", "YEAR", "MAKE")])
  names(veh) <- c("SAMPN", "VEHNO", "YEAR", "MAKE")         

  #Step 1. Pull out all vehicles that match the Make 

  # Flag to let last check know if Make can be trusted
  trustMake=T
  # And record of how many times the Model name went through the "fuzzy" matching logic
  fuzzy=0

  # Reduce list to just vehicles of the correct Make
  if(veh["MAKE"] < 54){
     MakeSub.. <- MPG..[MakeMatch[[Make]],]
  } else {     
     # Other make not in the 53 predefined
     if(veh["MAKE"] == 97){
        test = 0
        x=0
        # search for make match from the "other" Make field
        while(test == 0 & x < .31){
           MakeSub.. <- MPG..[agrep(O.Make, MPG..$Make, max=x, ignore.case=T),]
           x=x+.1
           test <- nrow(MakeSub..)
        }   
        # if no match for the Make, all vehicles are possible
        if(test==0){
           MakeSub.. <- MPG..
           trustMake=F
        }   
        rm(test,x)
     # if respondent didn't know the make, all cars are available
     } else {
        MakeSub.. <- MPG..
        trustMake=F
     }         
  }
  
  #Step 2. Pull out all vehicles for the correct Year 

  # Reduce vehicles to just those of the correct year, or closest year
  if(sum(MakeSub..$Year==veh["YEAR"])==0){
     # If they didn't know the year, all years for that make are available
     if(veh["YEAR"] == 9998) {
        YrSub.. <- MakeSub..
     # if there is no information for that Make for that Year, get the closest year
     } else {
        YrSub.. <- MakeSub..[abs(MakeSub..$Year-veh["YEAR"])==min(abs(MakeSub..$Year-veh["YEAR"])),]
     }
  # else create a subset for that Make and the specific Year
  } else {
     YrSub.. <- MakeSub..[MakeSub..$Year==veh["YEAR"],]
  }

  #Step 3. Choose the vehicle model, uses fuzzy logic to match names

  # setting the test varible to indicate if a Model match was found (0 = no match)
  test=0

  # this first if statement limits the search to only records where the owner knew the model name        
  if(Model != "DK/RF"){
     # setting the fuzzy logic to a perfect name match for starters (0 = no deviation)
     x=0
     while(test == 0 & x < .31){
        ModelSub.. <- YrSub..[agrep(Model, YrSub..$Model, max=x, ignore.case=T),]
        x=x+.1
        test <- nrow(ModelSub..)
     }
     # keeping track of how many times the fuzzy logic was needed to match the model name for the specific record
     fuzzy <- fuzzy+(x-0.1)

     # If no matches exist, search so that Model searches across the Make name (using fuzzy logic)
     if(test==0){
        x=0
        while(test == 0 & x < .31){
           ModelSub.. <- YrSub..[agrep(Model, YrSub..$Make, max=x, ignore.case=T),]
           x=x+.1
           test <- nrow(ModelSub..)
        }   
        fuzzy <- fuzzy+(x-0.1)
     }

     # If no matches exist, open search so that Model searches across all vehciles (using fuzzy logic)
     # This is then reduced by the closest year
     if(test==0){
        x=0
        while(test == 0 & x < .31){
           ModelSub.. <- MPG..[agrep(Model, MPG..$Model, max=x, ignore.case=T),]
           x=x+.1
           test <- nrow(ModelSub..)
        }
        fuzzy <- fuzzy+(x-0.1)   
        if(test>0){
           ModelSub.. <- ModelSub..[abs(ModelSub..$Year-veh["YEAR"])==min(abs(ModelSub..$Year-veh["YEAR"])),]
           test <- nrow(ModelSub..)
        }
     }
     # the model fuzzy logic is complete at this step
     rm(x)
  } 

  #Step 4. Handling cases where the owner didn't know the Model of the vehicle, or no model match has been made yet

  #In some cases, model numbers match up poorly, in this case go with the Make
  if(test == 0){
     # Get the Make name for reporting purposes
     # (up to this point the Make has been kept as a reference number)
     Make <- ifelse(veh["MAKE"] == 97, O.Make ,Make)
     # If the Make is trusted (the trustMake flag was not switched to False in the Make search)
     # then the make mpg is used for the the unknown model
     if(trustMake){
        ModelSub.. <- YrSub..
        test=nrow(ModelSub..)
     } else {
        # Use default value for year, if nothing exists to this point
        if(veh["YEAR"] %in% 1975:2040){
           ModelSub.. <- MPG..[MPG..$Year == veh["YEAR"] & MPG..$Make =="?",]
           test=nrow(ModelSub..)
           # Message to alert the user that the year average mpg was used at this point 
           print(paste("Default Year - SampNo:",veh["SAMPN"], "VehNo:", veh["VEHNO"], " - Year, Make, Model:", veh["YEAR"], Make, Model))
           rm(Make)
        }
     }
  }
  	
  # If no matches exist by this point, the default of 15 is given and reported,
  # Otherwise the efficency of the vehicles remaining in the subset table are averaged     
  if(test==0){
     print(paste("Default 15 - SampNo:",veh["SAMPN"], "VehNo:", veh["VEHNO"], " - Year, Make, Model:", veh["YEAR"], Make, Model))
     resultingMPG <- c(SAMPN=veh[[1]], VEHNO=veh[[2]], cty=defaultMPG, hwy=defaultMPG, cmb=defaultMPG, FuzzyTest=fuzzy*10, NumVehRec=test)
     rm(Make)
  } else {
     resultingMPG <- c(SAMPN=veh[[1]], VEHNO=veh[[2]], cty=mean(ModelSub..$cty, na.rm=T), hwy=mean(ModelSub..$hwy, na.rm=T), cmb=mean(ModelSub..$cmb, na.rm=T), FuzzyTest=fuzzy*10, NumVehRec=test)
     rm(ModelSub..)
  }
  rm(test, YrSub.., MakeSub..)
  round(resultingMPG)
}

######################################################
# Function to calculate the linked trip fuel cost
######################################################

FuelCost <- function(linkedTrip,fuel_price,hh){

    MonthPos <- c(0,31,60,91,121,152,182,213,244,274,305,335)
    names(MonthPos) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

    # Split the fuel cost date into components
    Date <- do.call(rbind,strsplit(gsub(",", "", FuelPrice$Date), " "))
    FuelPrice$Month <- as.character(Date[,1])
    FuelPrice$Day <- as.numeric(Date[,2])
    FuelPrice$Year <- as.numeric(Date[,3])

    # Create a SurveyPos field based on month, date and year
    rm(Date)
    FuelPrice <- FuelPrice[FuelPrice$Year > 2008,]
    FuelPrice$SurveyPos <- 100
    FuelPrice$SurveyPos <- FuelPrice$SurveyPos + (400 * (FuelPrice$Year-2009)) + MonthPos[FuelPrice$Month] + FuelPrice$Day
     
    rownames(hh) <- hh$SAMPN

    # split hh file by Area, then process each area separately 
    hh_area=by(hh,hh$AREA,function(x){x})
    hh=foreach(i=hh_area,.combine=rbind) %dopar% {
        Areaid=unique(i$AREA)	 
        fuelPricesub=subset(FuelPrice,FuelPrice$AREA==Areaid)
        i$GasPrice <- unlist(sapply(i$ASSN, function(x) fuelPricesub$Cost[min(abs(x-fuelPricesub$SurveyPos)) == abs(x-fuelPricesub$SurveyPos)][1]))
        return(i)
    }

    # match the Gas Price and MPG from household and vehicle table to the linked trip table	 
    linkedTrip$GasPrice=hh$GasPrice[match(linkedTrip$SAMPN,hh$SAMPN)]
    linkedTrip$VEHNO=ifelse(linkedTrip$VEHNO==0&linkedTrip$CMVEHNO!="NA",linkedTrip$CMVEHNO,linkedTrip$VEHNO)
    linkedTrip$cmb=veh$cmb[match(linkedTrip$SAMPN+1000000*linkedTrip$VEHNO,(veh$SAMPN+1000000*veh$VEHNO))]

    # Calculate fuel cost 
    linkedTrip$FuelCost=ifelse(linkedTrip$CMAUTODIST>0,(linkedTrip$CMAUTODIST/5280/linkedTrip$cmb*linkedTrip$GasPrice),0)
    linkedTrip$FuelCost=ifelse(linkedTrip$CMAUTODIST==0 & linkedTrip$MODENAME %in%c("DRIVER","PASSENGER"),(linkedTrip$Distance/5280/linkedTrip$cmb*linkedTrip$GasPrice),linkedTrip$FuelCost)

    # Drop unnecessary fields
    drops=c("GasPrice","cmb")
    linkedTrip=linkedTrip[,!(names(linkedTrip) %in% drops)]

	return(linkedTrip)
	
}


FuelCostVehs <- function(vehtable,fuel_price,hh){

    MonthPos <- c(0,31,60,91,121,152,182,213,244,274,305,335)
    names(MonthPos) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

    # Split the fuel cost date into components
    Date <- do.call(rbind,strsplit(gsub(",", "", FuelPrice$Date), " "))
    FuelPrice$Month <- as.character(Date[,1])
    FuelPrice$Day <- as.numeric(Date[,2])
    FuelPrice$Year <- as.numeric(Date[,3])

    # Create a SurveyPos field based on month, date and year
    rm(Date)
    FuelPrice <- FuelPrice[FuelPrice$Year > 2008,]
    FuelPrice$SurveyPos <- 100
    FuelPrice$SurveyPos <- FuelPrice$SurveyPos + (400 * (FuelPrice$Year-2009)) + MonthPos[FuelPrice$Month] + FuelPrice$Day
     
    rownames(hh) <- hh$SAMPN

    # split hh file by Area, then process each area separately 
    hh_area=by(hh,hh$AREA,function(x){x})
    hh=foreach(i=hh_area,.combine=rbind) %dopar% {
        Areaid=unique(i$AREA)	 
        fuelPricesub=subset(FuelPrice,FuelPrice$AREA==Areaid)
        i$GasPrice <- unlist(sapply(i$ASSN, function(x) fuelPricesub$Cost[min(abs(x-fuelPricesub$SurveyPos)) == abs(x-fuelPricesub$SurveyPos)][1]))
        return(i)
    }

    # match the Gas Price and MPG from household and vehicle table to the linked trip table	 
    vehtable$GasPrice=hh$GasPrice[match(vehtable$SAMPN,hh$SAMPN)]

	return(vehtable)
	
}


AirportTripsRecode <- function(place){

    place$O_TPURP=as.character(place$O_TPURP)
    place$O_MODE=as.character(place$O_MODE)
    place$STATE=as.character(place$STATE)

    #save orig change mode code
    place$PREVCMODE = c(0, place$TPURP[1:(nrow(place)-1)]) == 7
    place$NEXTCMODE = c(place$TPURP[2:nrow(place)],0) == 7

    for(i in 2:nrow(place)-1) {
    
        modeValue = place$MODE[i]
        modeString = place$O_MODE[i]
        tpurpPrev = place$TPURP[i-1]
        tpurpNext = place$TPURP[i+1]
        
        #only revise if change mode and other mode AIRPLANE
        if(!is.na(modeValue) && modeValue==97 && modeString=='AIRPLANE' && (tpurpPrev==7 | tpurpNext==7)){
            hhidCur = place$SAMPN[i]
            hhidPrev = place$SAMPN[i-1]
            hhidNext = place$SAMPN[i+1]

            pidCur = place$PERNO[i]
            pidPrev = place$PERNO[i-1]
            pidNext = place$PERNO[i+1]

            state = place$STATE[i-1]

            if(hhidCur==hhidPrev && pidCur==pidPrev && state == 'OR'){
              place$O_TPURP[i-1] = 'Trip to Airport'
              place$TPURP[i-1] = place$TPURP[i]
            }

            modeStringNext = place$O_MODE[i+1]

            if( hhidCur==hhidNext && pidCur==pidNext && !is.na(modeStringNext) && modeStringNext != 'AIRPLANE'){
              place$O_TPURP[i+1] = 'Trip from Airport'
              place$TPURP[i+1] = place$TPURP[i]
            }
        }
    }

    #drop the airplane trips only if revised
    place$O_MODE[is.na(place$O_MODE)]=0
    place=place[!(place$MODE==97 & place$O_MODE=='AIRPLANE' & (place$PREVCMODE | place$NEXTCMODE)),]
    return(place)
}

######################################################



# loop through all the databases
for(dbName in dbNames){

######################################################
#Read tables from database
######################################################

#print database base being processed
print(dbName)

load(paste(LoadLocation, dbName, sep=""))
#retVeh<- vehRet
#rm(vehRet)
placeCopy <- place

###########################################################################
#Add PLANO_Orig field to maintain original PLANO number
###########################################################################

place$PLANO_Orig=place$PLANO

# Drop 'loop trips' in the place table
place$TPURP2[is.na(place$TPURP2)] = 0
place$looptrips=ifelse(place$TPURP2==96 & place$PNAME == "HOME",1,0)
place = subset(place, place$looptrips == 0)
place = subset(place, select = -c(looptrips))

# Recode O_TPURP to either 'trip to airport' or 'trip from airport' if relevant
place = AirportTripsRecode(place)


######################################################
#Read additional tables
######################################################

#activity codes
activities = read.csv(activitiesFileName)

#mode names
modeNames = read.csv(modesFileName)

#read vehicle db
load(vehicleDatabaseFileName)

#read fuel price data
FuelPrice = read.csv(fuelPriceFileName)

######################################################
#Identify Tours and WorkSubTours, Code Tour and WorkSubTour Fields
######################################################

#----------------------------
#DASH Survey Processing Steps
#Items 5 and 6 are not currently done - BTS 083009
#0) Aggregate activities
#1) Create ACTIVITIES Table (UNIQID, HH, PER, DAY, ACT, TOURNO, HASSUBTOUR)
#     keep all fields and add calculated fields
#     Create home-based tours and work-based subtours
#2) Add This,Next,Last to each activity for activity, TAZ, and mode
#3) Add tour fields (duration, mode (1st from home), etc)
#4) Join HH and PER data (up to 9 people)
#5) Each person in HH is then joined to each activty record even if they are
#     not on the tour.  Each person's activity and location at that TOD is added 
#     to tour record at that TOD.  Note this is not each person on the trip.
#6) Then join activities if same TAZ and activity and create
#     new Activity Codes - w/kid, w/adult, escort kid, escort adult. The 
#     key is to identify HH members and non-HH members
#7) Generate TOUR file
#8) Generate TRIP file
#9) Add MPG to vehicles table
#---------------------------

#sort place and route table to ensure proper ordering
hh = hh[order(hh$SAMPN),]
per = per[order(per$SAMPN,per$PERNO),]
place = place[order(place$SAMPN, place$PERNO, place$PLANO),]
veh = veh[order(veh$SAMPN,veh$VEHNO),]
route = route[order(route$SAMPN, route$PERNO, route$TRIPNO, route$OPLANO, route$DPLANO, route$POINTNO),]
if(length(retVeh) > 2) retVeh = retVeh[order(retVeh$SAMPN,retVeh$RVEHNO),]

#Aggregate activities
place$AGGACT = activities$Activity[match(place$TPURP, activities$Code)]
place$AGGACTCODE = activities$AggActCode[match(place$TPURP, activities$Code)]

#add mode names
place$MODENAME = modeNames$MODENAME[match(place$MODE,modeNames$MODECODE)]
place$MODENAMECODE = modeNames$MODENAMECODE[match(place$MODE,modeNames$MODECODE)]

#calculate ROUTE table distances hash table for later
route$LASTXCORD = c(0, route$XCORD[1:(nrow(route)-1)])
route$LASTYCORD = c(0, route$YCORD[1:(nrow(route)-1)])
route$Distance = distance(route$LASTXCORD, route$XCORD, route$LASTYCORD, route$YCORD)
routeDistance = tapply(route$Distance, paste(route$SAMPN, route$PERNO, route$OPLANO, route$DPLANO), 
  function(x) ifelse(length(x)==1,NA,sum(x[2:length(x)])) )

#get unique sampn numbers
sampn = unique(place$SAMPN)

################################################
# Distribute process 
################################################
library(doParallel)
cluster = makeCluster(clusternumber)
registerDoParallel(cluster)

place_hh = by(place, place$SAMPN, function(x) {x})
placeOut = foreach(i=place_hh, .combine="rbind") %dopar% {

  #get perno for sampn
  perno = unique(i$PERNO)
  
  #add ESCORTPURP
  i = addEscortPurpose(i)
 
  #loop through perno
  for(j in 1:length(perno)) {
    
    #print current sampn and perno being processed
    cat(paste("perno:", perno[j], "\n"))
    
    #get rows
    place2 = i[i$PERNO == perno[j],]
    
	#renumber place number, due to records replaced by dropping linked trips
	for (n in 1:nrow(place2)){
	print(n)
	place2[n,]$PLANO=n
	}
	
    #run functions to add calculated fields
    place2 = addThisNextLast(place2)
    place2 = addChangeModeFields(place2)
    place2 = identifyTours(place2)
    
    #get tour numbers
    tourno = unique(place2$BEGTOURNUM)
    
    #flag to indicate if person place records output table created
    createdPerRecTable = FALSE
    for(k in tourno) {
    
      #get tour records
      place3 = place2[place2$BEGTOURNUM == k,]
    
      if(k > 0) { #skip < 0
        
        #run tour functions
        place3 = addTourFields(place3)
        place3 = identifyWorkSubTour(place3)
          
        #get work sub tour numbers
        worktourno = unique(place3$BEGWORKSUBTOURNUM)
        if(length(worktourno) == 0) { worktourno = -1 }
        
        #loop through worktourno
        for(m in worktourno) {

          #get work tour records
          place4 = place3[place3$BEGWORKSUBTOURNUM == m,]

          if(m > 0) { #skip < 0

            #run tour functions
            place4 = addWorkSubTourFields(place4)
            
            #add tour place records to person records
            if(createdPerRecTable) {
              placeReturn = rbind(placeReturn, place4)
            } else {
              placeReturn = place4
              createdPerRecTable = TRUE
            }

          } else {
            #no work subtours
            place4$WORKSUBTOURHASTRANSIT  = NA
            place4$WORKSUBTOURHASBIKE = NA
            place4$WORKSUBTOURHASDRIVE = NA
            place4$WORKSUBTOURHASPNR = NA
            place4$WORKSUBTOURMODES  = NA
            place4$WORKSUBTOURFIRSTMODE = NA
            place4$WORKSUBTOURACCESSMODETOFIRSTTRANSIT = NA
            place4$WORKSUBTOUREGRESSMODETOLASTTRANSIT = NA
            place4$WORKSUBTOURACTS  = NA
            place4$WORKSUBTOURDUR  = NA
            place4$WORKSUBTOURCUMDUR  = NA
            place4$WORKSUBTOURDEP_HR  = NA
            place4$WORKSUBTOURDEP_MIN  = NA
            place4$WORKSUBTOURNUMSTOPS  = NA
            place4$WORKSUBTOURDIST = NA
            place4$WORKSUBTOURDISTROUTE = NA
            
            #add tour place records to person records
            if(createdPerRecTable) {
              placeReturn = rbind(placeReturn, place4)
            } else {
              placeReturn = place4
              createdPerRecTable = TRUE
            }
            
          }
        }
      
      } else {
        #no tours
        place3$TOURHASTRANSIT = NA
        place3$TOURHASBIKE = NA
        place3$TOURHASDRIVE = NA
        place3$TOURHASPNR = NA
        place3$TOURMODES = NA
        place3$TOURFIRSTMODE = NA
        place3$TOURACCESSMODETOFIRSTTRANSIT = NA
        place3$TOUREGRESSMODETOLASTTRANSIT = NA
        place3$TOURMANDACT = NA
        place3$TOURACTS = NA
        place3$TOURMANDPRIMXCORD = NA
        place3$TOURMANDPRIMYCORD = NA
        place3$TOURDUR = NA
        place3$TOURCUMDUR = NA
        place3$TOURDEP_HR = NA
        place3$TOURDEP_MIN = NA
        place3$TOURNUMSTOPS  = NA
        place3$TOURDIST  = NA
        place3$TOURDISTROUTE  = NA
        place3$BEGWORKSUBTOURNUM  = NA
        place3$ENDWORKSUBTOURNUM  = NA
        place3$WORKSUBTOURHASTRANSIT  = NA
        place3$WORKSUBTOURHASBIKE = NA
        place3$WORKSUBTOURHASDRIVE = NA
        place3$WORKSUBTOURHASPNR = NA
        place3$WORKSUBTOURMODES  = NA
        place3$WORKSUBTOURFIRSTMODE = NA
        place3$WORKSUBTOURACCESSMODETOFIRSTTRANSIT = NA
        place3$WORKSUBTOUREGRESSMODETOLASTTRANSIT = NA
        place3$WORKSUBTOURACTS  = NA
        place3$WORKSUBTOURDUR  = NA
        place3$WORKSUBTOURCUMDUR  = NA
        place3$WORKSUBTOURDEP_HR  = NA
        place3$WORKSUBTOURDEP_MIN  = NA
        place3$WORKSUBTOURNUMSTOPS  = NA
        place3$WORKSUBTOURDIST = NA
        place3$WORKSUBTOURDISTROUTE = NA
        
        #add tour place records to person records
        if(createdPerRecTable) {
          placeReturn = rbind(placeReturn, place3)
        } else {
          placeReturn = place3
          createdPerRecTable = TRUE
        }
        
      }
    }
    
    #add person place records to hh person records
    if(j==1) {
      hhPlaceRecords = placeReturn
    } else {
      hhPlaceRecords = rbind(hhPlaceRecords, placeReturn)
    }
    rm(placeReturn)
    
  }
  
  #return all records for all household members
  return(hhPlaceRecords)
}

# create sort order vector for later
placeOut$SORTORDER = 1:nrow(placeOut)

######################################################
#Add MPG to Veh table
######################################################

# Apply assignMPG function to all non-retired vehicles, by combination of SAMPN and VEHNO   
vehspl=by(veh,paste(veh$SAMPN,veh$VEHNO),function(x){x})
vehOut=foreach(i=vehspl) %dopar% {
    assignMPG(i, names(i))
}

# Transpose output table from above step and merge with the vehicle table
vehattscombined=as.data.frame(t(do.call("cbind",vehOut)))	 
veh=merge(veh,vehattscombined,by.x=c("SAMPN","VEHNO"),by.y=c("SAMPN","VEHNO"))

# Prepare the retired vehicles table for processing by replacing names
if(length(retVeh) > 2){
retNames <- gsub("^R", "", names(retVeh))
retNames[grep("^O_", retNames)] <- gsub("^O_R", "O_", retNames[grep("^O_", retNames)])
 
# Apply assignMPG function to all retired vehicles 
retvehspl=by(retVeh,paste(retVeh$SAMPN,retVeh$RVEHNO),function(x){x})
retvehOut=foreach(i=retvehspl) %dopar% {
    assignMPG(i, retNames)
}

# Transpose output table from above step and merge with the retired vehicle table
retvehattscombined=as.data.frame(t(do.call("cbind",retvehOut)))	 
retVeh=merge(retVeh,retvehattscombined,by.x=c("SAMPN","RVEHNO"),by.y=c("SAMPN","VEHNO"))
	 
# Check data
print(mean(retVeh$cmb))
print(mean(veh[veh$SAMPN %in% retVeh$SAMPN, "cmb"]))     
}
######################################################
#Join HH, Person, Veh data
######################################################

if(joinHHPerData) {
	
	#join hh data
	placeOut = merge(placeOut, hh, by="SAMPN", all.x=T, sort=F)
	
	#join select primary person data
	selectPerColumns = c("SAMPN","PERNO","GEND","AGE","RELATE","LIC","TRANS","EMPLY","HOURS","WMODE","EPARK","EDUCA","STUDE","SCHOL","SMODE")
	per = per[,selectPerColumns]
	placeOut = merge(placeOut, per, by=c("SAMPN","PERNO"), all.x=T, sort=F, incomparables =NA)
	
	#join select person data for other persons on the trip with same HH
	tempPer = per
	colnames(tempPer) = paste(colnames(per), "_1", sep="")
	placeOut = merge(placeOut, tempPer, by.x=c("SAMPN","PER1"), by.y=c("SAMPN_1","PERNO_1"), all.x=T, sort=F, incomparables=NA)
	
	colnames(tempPer) = paste(colnames(per), "_2", sep="")
	placeOut = merge(placeOut, tempPer, by.x=c("SAMPN","PER2"), by.y=c("SAMPN_2","PERNO_2"), all.x=T, sort=F, incomparables=NA)
	
	colnames(tempPer) = paste(colnames(per), "_3", sep="")
	placeOut = merge(placeOut, tempPer, by.x=c("SAMPN","PER3"), by.y=c("SAMPN_3","PERNO_3"), all.x=T, sort=F, incomparables=NA)
	
	colnames(tempPer) = paste(colnames(per), "_4", sep="")
	placeOut = merge(placeOut, tempPer, by.x=c("SAMPN","PER4"), by.y=c("SAMPN_4","PERNO_4"), all.x=T, sort=F, incomparables=NA)
	
	colnames(tempPer) = paste(colnames(per), "_5", sep="")
	placeOut = merge(placeOut, tempPer, by.x=c("SAMPN","PER5"), by.y=c("SAMPN_5","PERNO_5"), all.x=T, sort=F, incomparables=NA)
	
	#join select vehicle data
	selectVehColumns = c("SAMPN","VEHNO","YEAR","MAKE","MODEL","BODY","FUEL")
	placeOut = merge(placeOut, veh[,selectVehColumns], by=c("SAMPN","VEHNO"), all.x=TRUE)

}

######################################################
#Create output tables
######################################################

#rearrange column order and sort
keys = c("SAMPN","PERNO","PLANO","BEGTOURNUM","BEGWORKSUBTOURNUM","PNAME")
placeOut = placeOut[,c(keys,colnames(placeOut)[!(colnames(placeOut) %in% keys)])]
placeOut = placeOut[order(placeOut$SORTORDER),]
placeOut = placeOut[,!(colnames(placeOut) %in% "SORTORDER")]

#activity table - remove duplicated PLANO records that were created for tour processing earlier
activity = placeOut[!duplicated(paste(placeOut$SAMPN,placeOut$PERNO,placeOut$PLANO)),]

#trip table - drop 1st record on each tour
trip = placeOut[!duplicated(paste(placeOut$SAMPN,placeOut$PERNO,placeOut$PLANO)),]
trip = trip[duplicated(paste(trip$SAMPN,trip$PERNO)),]

#linked trip table - drop change mode records
linkedTrip = createLinkedTripTable(trip,sampn)

#tour table - take 1st record on each tour or 1st worksubtour record
#remove incomplete tour records from data before processing
tour = placeOut[placeOut$BEGTOURNUM > 0,]
tourID = paste(tour$SAMPN,tour$PERNO,tour$BEGTOURNUM)

tour = by(tour, tourID, function(x) {
  startWorkSubtour = which(x$BEGWORKSUBTOURNUM == 1)[1]
  if(is.na(startWorkSubtour)) {
    result = x[1,] #if NA then take 1st tour record
  } else {
    result = x[startWorkSubtour,] #else take 1st work subtour record
  }
  return(result)
  }
)
tour <- do.call(rbind,tour) #row bind the results

######################################################
# Calculate the linked trip fuel cost
######################################################

linkedTrip = FuelCost(linkedTrip,fuel_price,hh)

# Join Gas Price to veh tables

vehwgasprice=FuelCostVehs(veh,fuel_price,hh)
if(length(retVeh) > 2) retvehwgasprice=FuelCostVehs(retVeh,fuel_price,hh)

######################################################
#Data Summaries
######################################################

#Tours by HH
table(tapply(tour$BEGTOURNUM,tour$SAMPN,function(x) length(unique(x))))

#Tour mandatory activity
table(tour$TOURMANDACT,useNA="always")

#Tour modes top 10 occurences
data.frame(COUNT_TOURMODES=sort(table(tour$TOURMODES),dec=T)[1:10])

#Tour activities top 10 occurences
data.frame(COUNT_TOURACTS=sort(table(tour$TOURACTS),dec=T)[1:10])

#Tour numstops
table(tour$TOURNUMSTOPS)

#Trip mode share
table(trip$MODENAME)

#Trip mode by route distance (ft)
table(trip$MODENAME,cut(trip$DistanceRoute,seq(0,52800,5280)))

#Trip mode by activity
table(trip$MODENAME,trip$AGGACT)

#Linked trip change mode distance and mode
table(linkedTrip$CMMODENAMES,cut(linkedTrip$CMNMDISTROUTE + linkedTrip$CMMOTDISTROUTE,seq(0,52800,5280)))

#Linked trip total trip distance by mode
table(linkedTrip$CMMODENAMES,cut(linkedTrip$DistanceRoute + linkedTrip$CMNMDISTROUTE + + linkedTrip$CMMOTDISTROUTE,seq(0,52800,5280)))

#Linked transit trip total trip distance by change mode names (access or egress)
transitTrips = linkedTrip[linkedTrip$CMHASTRANSIT==T | linkedTrip$MODENAME=="TRANSIT",]
table(transitTrips$CMMODENAMES,cut(transitTrips$DistanceRoute + transitTrips$CMNMDISTROUTE + transitTrips$CMMOTDISTROUTE,seq(0,52800,5280)))

#Linked trip mode tabulation
table(linkedTrip$ThisMODE)

#Work subtours per tour
tourID = paste(activity$SAMPN,activity$PERNO,activity$BEGTOURNUM)
workToursByTour = by(activity, tourID, function(x) { y = unique(x$BEGWORKSUBTOURNUM); length(y[y>0]) } )
table(workToursByTour)

#check tour has transit versus tour access mode to first transit
table(tour$TOURHASTRANSIT,tour$TOURACCESSMODETOFIRSTTRANSIT, useNA="always")

#Average trip cost
mean(linkedTrip$FuelCost,na.rm=TRUE)

#Average trip auto distance
mean(linkedTrip$CMAUTODIST,na.rm=TRUE)

#Linked trips by purpose and PA
x = as.data.frame(table(linkedTrip$TripPurposePA))
x$Percent = round(x$Freq/sum(x$Freq),2)
print(x)

######################################################
# Reset the PLANO field before output
######################################################

trip$PLANO=trip$PLANO_Orig
linkedTrip$PLANO=linkedTrip$PLANO_Orig
tour$PLANO=tour$PLANO_Orig

######################################################
#Write output tables
######################################################

# sort tables
trip=trip[order(trip$SAMPN,trip$PERNO,trip$PLANO), ]
linkedTrip=linkedTrip[order(linkedTrip$SAMPN,linkedTrip$PERNO,linkedTrip$PLANO), ]
tour=tour[order(tour$SAMPN,tour$PERNO,tour$PLANO), ]
vehwgasprice=vehwgasprice[order(vehwgasprice$SAMPN,vehwgasprice$VEHNO), ]
if(length(retVeh) > 2) retvehwgasprice=retvehwgasprice[order(retvehwgasprice$SAMPN,retvehwgasprice$RVEHNO), ]

######################################################
#Script Complete
######################################################

trip <- trip[,tripFields]
linkedTrip <- linkedTrip[,linkedTripFields]
tour <- tour[,tourFields]
veh <- vehwgasprice
vehRet <- retvehwgasprice
place <- placeCopy  # retain orginal(unaltered) place table AB 10-22-13
rm(placeCopy)

# small detail  AB 10-22-13
# move gas price to household table.
hh$GasPrice <- veh[match(hh$SAMPN, veh$SAMPN), "GasPrice"]

# name and order data
rownames(hh) <- hh$SAMPN
hh <- hh[order(hh$SAMPN),]
rownames(per) <- paste(per$SAMPN, per$PERNO, sep="-")
per <- per[order(per$SAMPN, per$PERNO),]
rownames(place) <- paste(place$SAMPN, place$PERNO, place$PLANO, sep="-")
place <- place[order(place$SAMPN, place$PERNO, place$PLANO),]
rownames(route) <- paste(route$SAMPN, route$PERNO, route$TRIPNO, route$POINTNO, sep="-")
route <- route[order(route$SAMPN, route$PERNO, route$TRIPNO, route$POINTNO),]
rownames(veh) <- paste(veh$SAMPN, veh$VEHNO, sep="-")
rownames(vehRet) <- paste(vehRet$SAMPN, vehRet$RVEHNO, sep="-")
rownames(trip) <- paste(trip$SAMPN, trip$PERNO, trip$PLANO, sep="-")
rownames(linkedTrip) <- paste(linkedTrip$SAMPN, linkedTrip$PERNO, linkedTrip$PLANO, sep="-")
rownames(tour) <- paste(tour$SAMPN, tour$PERNO, tour$PLANO, sep="-")
rownames(activity) <- paste(activity$SAMPN, activity$PERNO, activity$PLANO, sep="-")
activity <- activity[order(activity$SAMPN, activity$PERNO, activity$PLANO),]

# 07/28/2014 AB - adding weighting and expansion fields to the linked trip table
linkedTrip$HHWGT <- hh[as.character(linkedTrip$SAMPN),"HHWGT"]
linkedTrip$HHEXP <- hh[as.character(linkedTrip$SAMPN),"SWEXP"]
linkedTrip$PERWGT <- per[paste(linkedTrip$SAMPN, linkedTrip$PERNO, sep="-"),"PERWGT"]
linkedTrip$PEREXP <- per[paste(linkedTrip$SAMPN, linkedTrip$PERNO, sep="-"),"SWEXP"]

# source in location tagger scripts to tag all tables with states counties and cities 
source("Step2_AddAugTables/countyTag.R")
source("Step2_AddAugTables/cityTag.R")
source("Step2_AddAugTables/mpoTag.R")

# save dataset
save(list=c("hh", "per", "place", "route", "veh", "vehRet", "activity", "trip", "linkedTrip", "tour"), file=paste(SaveLocation, dbName, sep=""))
rm(list=c("hh", "per", "place", "route", "veh", "vehRet", "activity", "trip", "linkedTrip", "tour"))
stopCluster(cluster)
gc()

cat(paste("Coded Tour/Trip Fields in", dbName, "Complete", date(), " \n"))
} # end of database loop
