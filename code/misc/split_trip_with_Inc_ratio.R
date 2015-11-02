# This script use data distribution by trip purposes and income groups from Metro
# to calculate trip ratio between income groups. Then, use the ratio to split trip distribution 
# by trip purposes and trip modes from latest emmebank  

# setting 
setwd("~/tci")
source("code/thirdparty/omx.r")
require(rhdf5)

# trip distribution matrices
load("data/Zi.RData")
max.taz_id <- length(Zi)

# settings that are common to all methods

# Define income group abbreviation
Ic <- c("lowInc", "midInc", "highInc")

# Define trip purpose abbreviation
# The purposes for this study (now) are limited to the home-based trips
# They exclude nonhome-based trips, school trips and college trips
Pr <- c("hbw", "hbs", "hbr", "hbo")

# Define the travel modes
Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")

# Calculate trip trip ratio between income groups
TripDist.ZiZiIc <- array(0, dim=c(length(Zi), length(Zi), length(Ic)), 
                         dimnames=list(Zi, Zi, Ic))

for (pr in Pr) {
  for (ic in Ic) {
    # Load trip distribution matrices for each income group
    TripsObjName <- paste(pr, ic, "Dist", sep="")
    in.file <- file.path("data/TDM/tripdist", paste(TripsObjName, ".RData", sep=""))
    load(in.file); rm(in.file)
    # Get trip matrix 
    Trips.ZiZi <- get(TripsObjName)
    TripDist.ZiZiIc[ , ,ic] <- Trips.ZiZi
  }
  TripDistRatio.ZiZiIc <- sweep(TripDist.ZiZiIc, c(1,2),
                                apply(TripDist.ZiZiIc, c(1,2), sum), "/")
  TripDist.array.name <- paste(pr, "TripDist.ZiZiIc", sep="")
  assign(TripDist.array.name, TripDist.ZiZiIc)
  TripDistRatio.array.name <- paste(pr, "TripDistRatio.ZiZiIc", sep="")
  assign(TripDistRatio.array.name, TripDistRatio.ZiZiIc)
  rm(TripDistRatio.ZiZiIc)
}


# Check calculattion result
test <- hbwTripDist.ZiZiIc[1:5,1:5, "lowInc"]/(hbwTripDist.ZiZiIc[1:5,1:5, "lowInc"] 
                                               + hbwTripDist.ZiZiIc[1:5,1:5, "midInc"]
                                               + hbwTripDist.ZiZiIc[1:5,1:5, "highInc"])
stopifnot(all(round(test, 4) == round(hbwTripDistRatio.ZiZiIc[1:5,1:5, "lowInc"], 4)))

# Read emme2011 data
load("data/emme2011.RData")

# rename trips mxtrix in 2040 N scenario
hbwdriveAlone2040N <- mf61
hbwdrivePass2040N <- mf62
hbwpass2040N <- mf63
hbwbusWalk2040N <- mf64
hbwparkAndRideBus2040N <- mf65
hbwbike2040N <- mf66
hbwwalk2040N <- mf67

hbodriveAlone2040N <- mf75
hbodrivePass2040N <- mf76
hbopass2040N <- mf77
hbobusWalk2040N <- mf78
hboparkAndRideBus2040N <- mf79
hbobike2040N <- mf80
hbowalk2040N <- mf81

hbrdriveAlone2040N <- mf82
hbrdrivePass2040N <- mf83
hbrpass2040N <- mf84
hbrbusWalk2040N <- mf85
hbrparkAndRideBus2040N <- mf86
hbrbike2040N <- mf87
hbrwalk2040N <- mf88

hbsdriveAlone2040N <- mf89
hbsdrivePass2040N <- mf90
hbspass2040N <- mf91
hbsbusWalk2040N <- mf92
hbsparkAndRideBus2040N <- mf93
hbsbike2040N <- mf94
hbswalk2040N <- mf95
# Use the ratio between income groups to split trips 
createFileOMX("data/emme2011_2040N.omx", max.taz_id, max.taz_id, Replace=TRUE)

for (pr in Pr) {
  for (md in Md) {
    for (ic in Ic) {
      trip.mx.name <- paste(pr, md, "2040N", sep="")
      trip.mx <- get(trip.mx.name)
      trip.mx <- trip.mx[1:2162, 1:2162]
      
      TripDistRatio.array.name <- paste(pr, "TripDistRatio.ZiZiIc", sep="")
      TripDistRatio.ZiZiIc <- get(TripDistRatio.array.name)
      
      trip.mx.ic <- trip.mx*TripDistRatio.ZiZiIc[ , , ic] 
      trip.mx.ic.name <- paste(pr, ic, md, "trips", sep="") 
      
      mfDiscription <- paste("origin-destination trips table of", ic, "household group for", pr, "purpose by", md, sep=" ")
      writeMatrixOMX("data/emme2011_2040N.omx",  trip.mx.ic, trip.mx.ic.name, Description=mfDiscription)
      
    }
  }
}

# Check the result 
listOMX("data/emme2011_2040N.omx")
test <- readMatrixOMX("data/emme2011_2040N.omx", "hbwhighIncdriveAlonetrips")
test[1:5,1:5]
hbwdriveAlone2040N[1:5,1:5]
hbwTripDistRatio.ZiZiIc[1:5,1:5,"highInc"]
stopifnot(test[1:5,1:5]==hbwdriveAlone2040N[1:5,1:5]* hbwTripDistRatio.ZiZiIc[1:5,1:5,"highInc"])


# rename trips mxtrix in 2040 TFC scenario
hbwdriveAlone2040T <- mf113
hbwdrivePass2040T <- mf114
hbwpass2040T <- mf115
hbwbusWalk2040T <- mf116
hbwparkAndRideBus2040T <- mf117
hbwbike2040T <- mf118
hbwwalk2040T <- mf119

hbodriveAlone2040T <- mf127
hbodrivePass2040T <- mf128
hbopass2040T <- mf129
hbobusWalk2040T <- mf130
hboparkAndRideBus2040T <- mf131
hbobike2040T <- mf132
hbowalk2040T <- mf133

hbrdriveAlone2040T <- mf134
hbrdrivePass2040T <- mf135
hbrpass2040T <- mf136
hbrbusWalk2040T <- mf137
hbrparkAndRideBus2040T <- mf138
hbrbike2040T <- mf139
hbrwalk2040T <- mf140

hbsdriveAlone2040T <- mf141
hbsdrivePass2040T <- mf142
hbspass2040T <- mf143
hbsbusWalk2040T <- mf144
hbsparkAndRideBus2040T <- mf145
hbsbike2040T <- mf146
hbswalk2040T <- mf147


# Use the ratio between income groups to split trips 
createFileOMX("data/emme2011_2040T.omx", max.taz_id, max.taz_id, Replace=TRUE)

for (pr in Pr) {
  for (md in Md) {
    for (ic in Ic) {
      trip.mx.name <- paste(pr, md, "2040T", sep="")
      trip.mx <- get(trip.mx.name)
      trip.mx <- trip.mx[1:2162, 1:2162]
      
      TripDistRatio.array.name <- paste(pr, "TripDistRatio.ZiZiIc", sep="")
      TripDistRatio.ZiZiIc <- get(TripDistRatio.array.name)
      
      trip.mx.ic <- trip.mx*TripDistRatio.ZiZiIc[ , , ic] 
      trip.mx.ic.name <- paste(pr, ic, md, "trips", sep="") 
      
      mfDiscription <- paste("origin-destination trips table of", ic, "household group for", pr, "purpose by", md, sep=" ")
      writeMatrixOMX("data/emme2011_2040T.omx",  trip.mx.ic, trip.mx.ic.name, Description=mfDiscription)
      
    }
  }
}

# Check the result 
listOMX("data/emme2011_2040T.omx")
test <- readMatrixOMX("data/emme2011_2040T.omx", "hbwhighIncdriveAlonetrips")
test[1:5,1:5]
hbwdriveAlone2040T[1:5,1:5]
hbwTripDistRatio.ZiZiIc[1:5,1:5,"highInc"]
stopifnot(test[1:5,1:5]==hbwdriveAlone2040T[1:5,1:5]* hbwTripDistRatio.ZiZiIc[1:5,1:5,"highInc"])


# rename trips mxtrix in 2040 State scenario
hbwdriveAlone2040S <- mf165
hbwdrivePass2040S <- mf166
hbwpass2040S <- mf167
hbwbusWalk2040S <- mf168
hbwparkAndRideBus2040S <- mf169
hbwbike2040S <- mf170
hbwwalk2040S <- mf171

hbodriveAlone2040S <- mf179
hbodrivePass2040S <- mf180
hbopass2040S <- mf181
hbobusWalk2040S <- mf182
hboparkAndRideBus2040S <- mf183
hbobike2040S <- mf184
hbowalk2040S <- mf185

hbrdriveAlone2040S <- mf186
hbrdrivePass2040S <- mf187
hbrpass2040S <- mf188
hbrbusWalk2040S <- mf189
hbrparkAndRideBus2040S <- mf190
hbrbike2040S <- mf191
hbrwalk2040S <- mf192

hbsdriveAlone2040S <- mf193
hbsdrivePass2040S <- mf194
hbspass2040S <- mf195
hbsbusWalk2040S <- mf196
hbsparkAndRideBus2040S <- mf197
hbsbike2040S <- mf198
hbswalk2040S <- mf199

# Use the ratio between income groups to split trips 
createFileOMX("data/emme2011_2040S.omx", max.taz_id, max.taz_id, Replace=TRUE)

for (pr in Pr) {
  for (md in Md) {
    for (ic in Ic) {
      trip.mx.name <- paste(pr, md, "2040S", sep="")
      trip.mx <- get(trip.mx.name)
      trip.mx <- trip.mx[1:2162, 1:2162]
      
      TripDistRatio.array.name <- paste(pr, "TripDistRatio.ZiZiIc", sep="")
      TripDistRatio.ZiZiIc <- get(TripDistRatio.array.name)
      
      trip.mx.ic <- trip.mx*TripDistRatio.ZiZiIc[ , , ic] 
      trip.mx.ic.name <- paste(pr, ic, md, "trips", sep="") 
      
      mfDiscription <- paste("origin-destination trips table of", ic, "household group for", pr, "purpose by", md, sep=" ")
      writeMatrixOMX("data/emme2011_2040S.omx",  trip.mx.ic, trip.mx.ic.name, Description=mfDiscription)
      ls()
    }
  }
}

# Check the result 
listOMX("data/emme2011_2040S.omx")
test <- readMatrixOMX("data/emme2011_2040S.omx", "hbwhighIncdriveAlonetrips")
test[1:5,1:5]
hbwdriveAlone2040S[1:5,1:5]
hbwTripDistRatio.ZiZiIc[1:5,1:5,"highInc"]
stopifnot(test[1:5,1:5]==hbwdriveAlone2040S[1:5,1:5]* hbwTripDistRatio.ZiZiIc[1:5,1:5,"highInc"])




