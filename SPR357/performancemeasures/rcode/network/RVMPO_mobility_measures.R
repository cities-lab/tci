#=============================
#calculate_mobility_measures.R
#=============================

#:Author: Brian Gregor
#:Contact: brian.j.gregor@odot.state.or.us
#:Date: 09/26/05
#:Revisions: Martin Mann
#:Date: 05/16/06
#:License: GPL2
#-------------------------Folder Setup----------------------------------------------------------
# Top Folder: 
# 1."R" shortcut "Start in" folder
#	    Subfolders:
#       1. Emme2
#			      -must contain emme2ban, PMInput.mac, TripLength.mac
#       2. Input:  
#		       -must contain LinkColNames.txt, Setup.csv
#	      3. Output
#		       -will contain Performance Measures and Link data
#             One subfolder:
#               1. pdf
#                -folder contains output from reports from Access database.  This script does not produce
#                 this output but does
#
#*******************************************************************************************************
# Script writes the output Link and summary data to tables in the database.  The link in the reports to the
# graphs needs to be reestablished before printing if the path has been altered.  After running script,
# open the database and print out each of the reports to Adobe Acrobat or PDF995
#*******************************************************************************************************
#--------------------------------------EXTRACT EMME2 DATA--------------------------------------------
#
    setwd(paste(codeLoc,"/performancemeasures/input",sep=""))
    Setup.CM<- read.csv("Setup.csv",header=FALSE,as.is=TRUE)
      Scen <- as.numeric(Setup.CM[1,2])
      ScenYear <- as.numeric(Setup.CM[2,2])
      ScenDescp <- Setup.CM[3,2]
    SelectLink.Lk__ <- read.csv("SelectLinks.csv",header=TRUE,as.is=TRUE)
    LinkTypes.Lk__ <- read.csv("LinkType.csv",header=FALSE,as.is=TRUE)
      
#Change working directory to ...emme2/BaseYear(or ...emme2/FutureYear depending on FOR loop iteration) subfolder
    setwd(paste(codeLoc,"/emme2/",sep=""))
     
#Run Emme2 Macro to create Emme2 Output file
    system(paste("emme2 -m PMInputJem.mac"),
      show.output.on.console=TRUE, invisible=TRUE)

#Read current Emme2 File into R
    Links.Lk__ <- read.table("Emme2_Output.txt", header=TRUE)
    file.remove("Emme2_Output.txt")

#
#-------------------------FIND Average Trip Length FROM EMME2 DATA--------------------
#
#Run macro, find "BaseYear Average Trip Length"(or "FutureYear Average Trip Length")
      system(paste("emme2 -m TripLenJemnR.mac",Scen,sep=" "),
        show.output.on.console=TRUE, invisible=TRUE)

#Extract Average Trip Length value from emme2 file
      TripLenFile <- scan("TripLength.txt", what=" ", sep="\n")
      TripLenFile<-TripLenFile[5]
      TripLenFile<-unlist(strsplit(TripLenFile," "))
      TripLen<-as.numeric(TripLenFile[length(TripLenFile)])
      file.remove("TripLength.txt")
#
#-------------------------FIND Average SL Link Trip Lengths FROM EMME2 DATA--------------------
#
#Run macro, find "BaseYear Average Trip Length"(or "FutureYear Average Trip Length")
       SLTripLen<-c(rep(1,length(rownames(SelectLink.Lk__))))
       for(l in 1:length(SelectLink.Lk__[,1])){
          fnode = SelectLink.Lk__[l,2]
          tnode = SelectLink.Lk__[l,3]
          system(paste("emme2 -m SLTripLenJemnR.mac",fnode,tnode,Scen,sep=" "),
           show.output.on.console=TRUE, invisible=TRUE)

#Extract Average Trip Length value from emme2 file
          SLTripLenFile <- scan("SLTripLength.txt", what=" ", sep="\n")
          SLTripLenFile<-SLTripLenFile[5]
          SLTripLenFile<-unlist(strsplit(SLTripLenFile," "))
          SLTripLen<-as.numeric(SLTripLenFile[length(SLTripLenFile)])
          SelectLink.Lk__[l,paste("SLTripLen",sep="")]<-SLTripLen
          file.remove("SLTripLength.txt")
      }
#
#-------------------------CREATE INPUT FILE FOR PERFORMANCE MEASURES FROM EMME2 DATA--------------------
#
#Set working directory to shortcut's "Start in" location
      setwd(paste(codeLoc,"/performancemeasures",sep=""))
      
#Manipulate Links.Lk__ dataframe, add row and column names
      Links.Lk__<-Links.Lk__[,1:9]
      LinksCol.Lk__ <- read.table(paste("Input/LinkColNames.txt",sep=""),as.is=TRUE)
      colnames(Links.Lk__)<- LinksCol.Lk__[1:9]
      #rownames(Links.Lk__) <- paste(Links.Lk__$Inode, Links.Lk__$Jnode, sep="-")
            
#Define Function to calculate Congestion (volume) delay relative to Free Flow speed (speed limit)
      calcHourlyDelay <- function(Length, Ffs, HourlyVol, Lanes, HourlyCap){
            if(Ffs >= 50){
                   CongDelay <-round(((60 * Length / Ffs) * (2 + (49 * (1 - 1.0168 * (HourlyVol
                   / (Lanes * HourlyCap))) ^ 2 + 1.1735) ^ .5 + 7.1176 * (HourlyVol
                   / (Lanes * HourlyCap)) - 8.083305))-(60 * Length / Ffs),3)
                   CongDelay
            } else {
                   CongDelay <-round(((60 * Length / Ffs) * (2 + (16 * (1 - .8298 * (HourlyVol
                   / (Lanes * HourlyCap))) ^ 2 + 1.361) ^ .5 + 3.3192 * (HourlyVol
                   / (Lanes * HourlyCap)) - 5.1666))-(60 * Length / Ffs),3)
                   CongDelay
            }
      }

#Run Loop to create 24 Hourly Congestion Delay (in minutes) values (relative to FFS) for each link
            Links.Lk__[,"PeakCongDelay"] <-round(apply(Links.Lk__,1,function(x)calcHourlyDelay(x["Length"],
            x["Ffs"],x["Peak")], x["Lanes"], x["Cap"])),5)
            
#Define Function to calculate Congestion (volume) delay relative to LOS D to LOS E speed (speed at V/C of .87)
      calcHourlyDelayDE <- function(CongDelay, Length, Ffs,Lanes){
            VCDE=0.87
            if(Ffs >= 50){
                   CongDelayDE <-round((CongDelay +(60 * Length / Ffs))- ((60 * Length / Ffs) *
                   (2 + (49 * (1 - 1.0168 * VCDE) ^ 2 + 1.1735) ^ .5 + 7.1176 * VCDE - 8.083305)),3)
                   CongDelayDE
            } else {
                   CongDelayDE <-round((CongDelay +(60 * Length / Ffs)) -((60 * Length / Ffs) *
                   (2 + (16 * (1 - .8298 * VCDE) ^ 2 + 1.361) ^ .5 + 3.3192 * VCDE - 5.1666)),3)
                   CongDelayDE
            }
      }
             
#Run Loop to create 24 Hourly Congestion Delay (in minutes) values (relative to LOS D/E) for each link
      for(k in 1:24){
            Links.Lk__[,paste("DelayDE",k,sep="")] <-round(apply(Links.Lk__,1,function(x)calcHourlyDelayDE(x[paste("CongDelay",
            k,sep="")],x["Length"], x["Ffs"], x["Lanes"])),5)
            #Remove negative delay (occurs when actual V/C is less than V/C at LOS D/E)
            Links.Lk__[Links.Lk__[,paste("DelayDE",k,sep="")]<0,paste("DelayDE",k,sep="")]<-0
      }

#create subtables of Links.Lk__ for use in calculating average link delay
      HourlyVol <- Links.Lk__[9:32]
      HourlyCongDelay<- Links.Lk__[33:56]
      HourlyDelayDE<- Links.Lk__[57:80]

 # Create Columns for average Delay relative to FFS for each link
      #Weight each links Hourly delay by the proportion of corresponding Links Hourly volume to Link ADT
      WgtHourlyCongDelay <- HourlyCongDelay*(HourlyVol/Links.Lk__$Adt)
      #Set to zero weighted delay on links with Adt equal to zero
      WgtHourlyCongDelay[is.nan(as.matrix(WgtHourlyCongDelay))] <-  0
      #Sum weighted delays to give average link delay
      Links.Lk__$TotCongDelay<-round(rowSums(WgtHourlyCongDelay),3)
      
# Create Columns for average Delay relative to LOS DE for each link (comments as above)
      WgtHourlyDelayDE <- HourlyDelayDE*(HourlyVol/Links.Lk__$Adt)
      WgtHourlyDelayDE[is.nan(as.matrix(WgtHourlyDelayDE))] <-  0
      Links.Lk__$TotDelayDE<-round(rowSums(WgtHourlyDelayDE),3)
      
#
#-------------------------CALCULATE PERFORMANCE MEASURES---------------------------------------------------
#

# Link types classify all functional classes of interest
      Lt <- c(1,2,3,4,5,30)
      names(Lt) <- c("Freeway", "Principal Arterial", "Minor Arterial","Collector", "Local", "Freeway Ramp")

#Only keep data of these link types
      Links.Lk__ <- Links.Lk__[Links.Lk__$Type %in% Lt,]
      
#Join the capacities data to the link data and calculate volume to capacity ratios for the Peak Hour
      Links.Lk__$PeakVc <- round(Links.Lk__$Hour17Vol / (Links.Lk__$Cap*Links.Lk__$Lanes),2)

#Group Peak Hour V/C by lane miles and Percent VMT
      VCBreaks<-unique(c(seq(0,max(Links.Lk__$PeakVc),0.2),ceiling(max(Links.Lk__$PeakVc)*5)/5))
      VCbyMiles<- tapply(Links.Lk__$Length,cut(Links.Lk__$PeakVc,VCBreaks,right=FALSE),sum)
      VCbyMiles[is.na(as.matrix(VCbyMiles))] <-  0
      VCbyVMT<- tapply(Links.Lk__$Length*Links.Lk__$Hour17Vol,cut(Links.Lk__$PeakVc,VCBreaks,right=FALSE),sum)
      VCbyVMT[is.na(as.matrix(VCbyVMT))] <-  0
      VCbyPercVMT<- round(VCbyVMT/sum(VCbyVMT),3)
      VC<-data.frame(t(rbind(VCbyMiles,VCbyPercVMT)))

#Join the capacities data to the link data and calculate ADT to capacity ratios
      Links.Lk__$AdtC <- round(Links.Lk__$Adt / (Links.Lk__$Cap*Links.Lk__$Lanes),1)
      
  #Group ADT/C by lane miles and Percent VMT
      AdtCBreaks<-unique(c(seq(0,max(Links.Lk__$AdtC),1),ceiling(max(Links.Lk__$AdtC))))
      AdtCbyMiles<- tapply(Links.Lk__$Length,cut(Links.Lk__$AdtC,br= AdtCBreaks,right=FALSE),sum)
      AdtCbyMiles[is.na(as.matrix(AdtCbyMiles))] <-  0
      AdtCbyVMT<- tapply(Links.Lk__$Length*Links.Lk__$Adt,cut(Links.Lk__$AdtC,br= AdtCBreaks,right=FALSE),sum)
      AdtCbyVMT[is.na(as.matrix(AdtCbyVMT))] <-  0
      AdtCbyPercVMT<- round(AdtCbyVMT/sum(AdtCbyVMT),3)
      AC<-data.frame(t(rbind(AdtCbyMiles,AdtCbyPercVMT)))

#Add IncidentDelayRatio to the dataframe
      Links.Lk__$InciDlyRatio <- rep(NA, nrow(Links.Lk__))
      Links.Lk__$InciDlyRatio[Links.Lk__$Type %in% c(1,30)] <- 2.4
      Links.Lk__$InciDlyRatio[Links.Lk__$Type %in% c(2:5)] <- 1.1

#Define several standard parameters
      # Vehicle occupancy
      VehOcc <- 1.25
      # Working days
      WorkDays <- 250
      # Population
      if (Year == "BaseYear") {
            Population <- 105761
      } else {
            Population <- 154962
      }

#Define functions to calculate measures
#======================================

#Define a function to calculate recurring vehicle hours of delay (VHD) by link
	#:parameter: Delay.Lk - vector of link delay
	#:parameter: Vol.Lk - vector of traffic volume by network link
	#:return: RecurVhd.Lk - vector of vehicle-hours of recurring delay per link
      calcRecurVhd <- function(Delay.Lk, Vol.Lk){
            RecurVhd.Lk <- rowSums(Delay.Lk * Vol.Lk / 60)
            RecurVhd.Lk
      }

#Define a function to calculate recurring and incident vehicle hours of delay (VHD) by link
	#:parameter: Delay.Lk - vector of link delay
	#:parameter: Vol.Lk - vector of ADT by network link
	#:parameter: InciDlyRatio.Lk - vector of ratios of incident delay to recurring delay
	#:return: TotVhd.Lk - vector of total minutes of recurring and incident delay per link
      calcTotVhd <- function(Delay.Lk, Vol.Lk, InciDlyRatio.Lk){
            TotVhd.Lk <- rowSums(Delay.Lk * (1 + InciDlyRatio.Lk) * Vol.Lk / 60)
            TotVhd.Lk
      }

#Define a function to calculate link vehicle miles traveled (VMT)
	#:parameter: Length.Lk - link length in miles
	#:parameter: Vol.Lk - link volume in vehicles
	#:return: Vmt.Lk - link vehicle miles traveled
      calcVmt <- function(Length.Lk, Adt.Lk){
            Vmt.Lk <- Length.Lk * Adt.Lk
            Vmt.Lk
      }
         
#Define a function to calculate annual person delay
	#:parameter: TotVhd.Lk - vector of total vehicle hours of delay
	#:parameter: VehOcc - average vehicle occupancy
	#:parameter: WorkDays - number of work days per year
	#:return: TotPhd.Lk - number of annual person hours of delay per year
      calcTotPhd <- function(TotVhd.Lk, VehOcc, WorkDays){
      	    TotPhd.Lk <- TotVhd.Lk * VehOcc * WorkDays
            TotPhd.Lk
      }
   
#Define a function to calculate travel time index by link type (freeflow comparison)
	#:parameter: Length.Lk - link length in miles
	#:parameter: Adt.Lk - link traffic volume
	#:parameter: TotCongDelay.Lk - link Congestion Delay (FFS) (in minutes)
	#:parameter: Ffs.Lk - link free flow speed
	#:parameter: Type.Lk - link type
	#:return: AveTri.Lt - average travel rate index by link type
      calcAveTypeTti1 <- function(Length.Lk, Adt.Lk, TotCongDelay.Lk, Ffs.Lk, InciDlyRatio.Lk, Type.Lk){
            TotDelay.Lk <- TotCongDelay.Lk * (1 + InciDlyRatio.Lk)
            FfsTime.Lk <- (60 * Length.Lk / Ffs.Lk)
            LinkTime.Lk <- FfsTime.Lk + TotDelay.Lk
            Tti.Lk <- LinkTime.Lk / (FfsTime.Lk)
            Vmt.Lk <- calcVmt(Length.Lk, Adt.Lk)
            VmtWtTti.Lk <- Tti.Lk * Vmt.Lk
            SumVmtWtTti.Lt <- tapply(VmtWtTti.Lk, Type.Lk, sum)
            SumVmt.Lt <- tapply(Vmt.Lk, Type.Lk, sum)
            AveTti.Lt <- SumVmtWtTti.Lt / SumVmt.Lt
            AveTti.Lt
      }

#Define a function to calculate travel time index by link type (LOS D/E comparison)
	#:parameter: Length.Lk - link length in miles
	#:parameter: Adt.Lk - link traffic volume
	#:parameter: TotCongDelay.Lk - link Congestion (FFS) Delay (in minutes)
	#:parameter: TotDelayDe.Lk - link Congestion (LOSD/E) Delay (in minutes)
	#:parameter: Ffs.Lk - link free flow speed
	#:parameter: Type.Lk - link type
	#:return: AveTri.Lt - average travel rate index by link type
      calcAveTypeTti2 <- function(Length.Lk, Adt.Lk, TotCongDelay.Lk, TotDelayDe.Lk, Ffs.Lk, InciDlyRatio.Lk, Type.Lk){
	        TotDelay.Lk <- TotCongDelay.Lk * (1 + InciDlyRatio.Lk)
	        FfsTime.Lk <- 60 * Length.Lk / Ffs.Lk
	        RecurCongTime.Lk <- FfsTime.Lk + TotCongDelay.Lk
     	    DeTime.Lk <- RecurCongTime.Lk - TotDelayDe.Lk
     	    TotCongTime.Lk <- FfsTime.Lk + TotDelay.Lk
     	    Tti.Lk <- TotCongTime.Lk / DeTime.Lk
     	    Vmt.Lk <- calcVmt(Length.Lk, Adt.Lk)
     	    VmtWtTti.Lk <- Tti.Lk * Vmt.Lk
     	    SumVmtWtTti.Lt <- tapply(VmtWtTti.Lk, Type.Lk, sum)
     	    SumVmt.Lt <- tapply(Vmt.Lk, Type.Lk, sum)
     	    AveTti.Lt <- SumVmtWtTti.Lt / SumVmt.Lt
     	    AveTti.Lt
      }

#Define a function to calculate the RNCI
	#:parameter: LinkAdt - a vector of link traffic volumes
	#:parameter: LinkLanes - a vector of the number of lanes for each link
	#:return: Rnci - a value of the road network concentration index
      calcRnci <- function(LinkAdt, LinkLanes){
            LaneVol <- sort(LinkAdt / LinkLanes)
            PctLaneVol <- cumsum(LaneVol)/sum(LaneVol)
            PctEqVol <- cumsum(rep(1,length(LaneVol)))/(sum(rep(1,length(LaneVol))))
            Rnci <- (sum(PctEqVol)-sum(PctLaneVol))/sum(PctEqVol)
            Rnci
      }

#Define a function to plot a Lorenz curve
	#:parameter: LinkAdt - a vector of link traffic volumes
	#:parameter: LinkLanes - a vector of the number of lanes for each link
	#:return: none
      plotRnci <- function(LinkAdt, LinkLanes, ...){
            LaneVol <- sort(LinkAdt / LinkLanes)
            Xvals <- 100 * (1:length(LaneVol)/length(LaneVol))
            Yvals <- 100 * cumsum(LaneVol)/sum(LaneVol)
            Rnci <- round(calcRnci(LinkAdt, LinkLanes), 2)
            plot(Xvals, Yvals, type="l", lwd=2,
                 xlab="Percent of Links", ylab="Percent of Lane Volume", ...)
            lines(Xvals, Xvals, lty=2, lwd=2)
            text(0, 80, labels=paste("RNCI =", Rnci), pos=4, cex=1.25)
            legend(0, 100, legend=c("Pct of Total Lane Volume", "Line of Equality"),
                 lty=c(1,2), lwd=2, bty="n")
      }
                 
#Define a function to plot a Lorenz curve
	#:parameter: LinkAdt - a vector of link traffic volumes
	#:parameter: LinkLanes - a vector of the number of lanes for each link
	#:return: none
      plotRnciCap <- function(LinkAdt, LinkLanes, LaneCap, ...){
            LinkCap <- LinkLanes*LaneCap
            LaneVol <- sort(LinkAdt / LinkCap)
            Xvals <- 100 * (1:length(LaneVol)/length(LaneVol))
            Yvals <- 100 * cumsum(LaneVol)/sum(LaneVol)
            Rnci <- round(calcRnci(LinkAdt, LinkCap), 2)
            plot(Xvals, Yvals, type="l", lwd=2,
                 xlab="Percent of Links", ylab="Percent of Lane Volume", ...)
            lines(Xvals, Xvals, lty=2, lwd=2)
            text(0, 80, labels=paste("RNCI =", Rnci), pos=4, cex=1.25)
            legend(0, 100, legend=c("Pct of Total Lane Volume", "Line of Equality"),
                 lty=c(1,2), lwd=2, bty="n")
      }

#Conduct tests of measures
#=========================
      attach(Links.Lk__)

#Calculate average annual delay per person for two reference speeds

      #Restricts Calculations to Freeways, Freeway Ramps, Primary Arterials
      Adt_Art <- Adt[Type %in% c(1,2,30)]
      UmsType <- 1 * (Type %in% c(1,2,30))

      # Freeflow reference speed, all Links
      HourlyVol<-Links.Lk__[,9:32]
      HourlyDelay<-Links.Lk__[,33:56]

      # Calculate daily vehicle hours of delay
      RecurVhdFrfl_All.Lk <- calcRecurVhd(HourlyDelay, HourlyVol)
      TotVhdFrfl_All.Lk <- calcTotVhd(HourlyDelay, HourlyVol, InciDlyRatio)
     
      # Calculate annual person hours of delay
      RecurAnnPhdFrfl_All.Lk <- calcTotPhd(RecurVhdFrfl_All.Lk, VehOcc, WorkDays)
      TotAnnPhdFrfl_All.Lk <- calcTotPhd(TotVhdFrfl_All.Lk, VehOcc, WorkDays)
     
      # Calculate annual hours of delay per capita
      AnnRecurDlyCapFrfl_All <- round(sum(RecurAnnPhdFrfl_All.Lk) / Population,2)
      AnnTotDlyCapFrfl_All <- round(sum(TotAnnPhdFrfl_All.Lk) / Population,2)

#-------------------------------------------------------------------
     # Freeflow reference speed, freeway and principal arterial links
     HourlyVol<-Links.Lk__[Type %in% c(1,2,30),9:32]
     HourlyDelay<-Links.Lk__[Type %in% c(1,2,30),33:56]
     
     # Calculate daily vehicle hours of delay
     RecurVhdFrfl_Art.Lk <- calcRecurVhd(HourlyDelay, HourlyVol)
     TotVhdFrfl_Art.Lk <- calcTotVhd(HourlyDelay, HourlyVol, InciDlyRatio[Type %in% c(1,2,30)])

     # Calculate annual person hours of delay
     RecurAnnPhdFrfl_Art.Lk <- calcTotPhd(RecurVhdFrfl_Art.Lk, VehOcc, WorkDays)
     TotAnnPhdFrfl_Art.Lk <- calcTotPhd(TotVhdFrfl_Art.Lk, VehOcc, WorkDays)

     # Calculate annual hours of delay per capita
     AnnRecurDlyCapFrfl_Art <- round(sum(RecurAnnPhdFrfl_Art.Lk) / Population,2)
     AnnTotDlyCapFrfl_Art <- round(sum(TotAnnPhdFrfl_Art.Lk) / Population,2)

#-------------------------------------------------------------------
     # Moderate flow reference speed, all links
     HourlyVol<-Links.Lk__[,9:32]
     HourlyDelayDE<-Links.Lk__[,57:80]
     
     # Calculate daily vehicle hours of delay
     RecurVhdMdfl_All.Lk <- calcRecurVhd(HourlyDelayDE, HourlyVol)
     TotVhdMdfl_All.Lk <- calcTotVhd(HourlyDelayDE, HourlyVol, InciDlyRatio)

     # Calculate annual person hours of delay
     RecurAnnPhdMdfl_All.Lk <- calcTotPhd(RecurVhdMdfl_All.Lk, VehOcc, WorkDays)
     TotAnnPhdMdfl_All.Lk <- calcTotPhd(TotVhdMdfl_All.Lk, VehOcc, WorkDays)

     # Calculate annual hours of delay per capita
     AnnRecurDlyCapMdfl_All <- round(sum(RecurAnnPhdMdfl_All.Lk) / Population,2)
     AnnTotDlyCapMdfl_All <- round(sum(TotAnnPhdMdfl_All.Lk) / Population,2)
#-------------------------------------------------------------------

     # Moderate flow reference speed, freeway and principal arterial links
     HourlyVol<-Links.Lk__[Type %in% c(1,2,30),9:32]
     HourlyDelayDE<-Links.Lk__[Type %in% c(1,2,30),57:80]
     
     # Calculate daily vehicle hours of delay
     RecurVhdMdfl_Art.Lk <- calcRecurVhd(HourlyDelayDE, HourlyVol)
     TotVhdMdfl_Art.Lk <- calcTotVhd(HourlyDelayDE, HourlyVol, InciDlyRatio[Type %in% c(1,2,30)])

     # Calculate annual person hours of delay
     RecurAnnPhdMdfl_Art.Lk <- calcTotPhd(RecurVhdMdfl_Art.Lk, VehOcc, WorkDays)
     TotAnnPhdMdfl_Art.Lk <- calcTotPhd(TotVhdMdfl_Art.Lk, VehOcc, WorkDays)

     # Calculate annual hours of delay per capita
     AnnRecurDlyCapMdfl_Art <- round(sum(RecurAnnPhdMdfl_Art.Lk) / Population,2)
     AnnTotDlyCapMdfl_Art <- round(sum(TotAnnPhdMdfl_Art.Lk) / Population,2)
 #-------------------------------------------------------------------
#Calculate Travel Time Index
     UmsType <- 1 * (Type %in% c(1,2,30))
     
     # calculate TTI for Scenario A freeflow speed, all classes
     TTIFFAll<-round(calcAveTypeTti1(Length, Adt, TotCongDelay, Ffs, InciDlyRatio, rep(1, length(Length))),2)
     # calculate TTI for Scenario A freeflow speed, freeway and principal arterial
     TTIFFArt<-round(calcAveTypeTti1(Length, Adt, TotCongDelay, Ffs, InciDlyRatio, UmsType),2)
     # calculate TTI for Scenario A modflow speed, all classes
     TTIDEAll<-round(calcAveTypeTti2(Length, Adt, TotCongDelay, TotDelayDE, Ffs, InciDlyRatio,  rep(1, length(Length))),2)
     # calculate TTI for Scenario A modflow speed, freeway and principal arterial
     TTIDEArt<-round(calcAveTypeTti2(Length, Adt, TotCongDelay, TotDelayDE, Ffs, InciDlyRatio,  UmsType),2)
 #-------------------------------------------------------------------
 
 #Output Summary Measures
      SummPerfMeas <-c(AnnRecurDlyCapFrfl_All, AnnTotDlyCapFrfl_All, AnnRecurDlyCapFrfl_Art, AnnTotDlyCapFrfl_Art,
        AnnRecurDlyCapMdfl_All, AnnTotDlyCapMdfl_All, AnnRecurDlyCapMdfl_Art, AnnTotDlyCapMdfl_Art,
        TTIFFAll, TTIFFArt, TTIDEAll, TTIDEArt, TripLen)  #, SLTripLen)

      SummPerfMeasCompare<- c("FreeFlow", "FreeFlow","FreeFlow", "FreeFlow","LOS D/E","LOS D/E","LOS D/E","LOS D/E",
       "FreeFlow", "FreeFlow","FreeFlow","LOS D/E","LOS D/E","LOS D/E","Average Daily")

      SummPerfMeasFC<-c("AllLinks", "AllLinks","Arterials","Arterials","AllLinks","AllLinks", "Arterials","Arterials",
       "AllLinks","NonArterials","Arterials", "AllLinks","NonArterials","Arterials","All Links")

      SumPerfMeastype <- c("Annual Congestion Delay Per Capita (hrs)", "Annual Total Delay Per Capita (hrs)",
       "Annual Congestion Delay Per Capita (hrs)","Annual Total Delay Per Capita (hrs)",
       "Annual Congestion Delay Per Capita (hrs)", "Annual Total Delay Per Capita (hrs)",
       "Annual Congestion Delay Per Capita (hrs)","Annual Total Delay Per Capita (hrs)",
       rep("TravelTimeIndex",6),"TripLength(miles)")

      RVCOGSummPerfMeas<-data.frame(cbind(SumPerfMeastype,SummPerfMeasCompare,SummPerfMeasFC,SummPerfMeas))

#Output Performance Measure Results
      write.table(Links.Lk__,paste("Output\\",Year,"RVCOG_LinkPerformanceMeasures.csv",sep=""),sep=",",row.names=TRUE,col.names=TRUE)
      write.table(RVCOGSummPerfMeas,paste("Output\\",Year,"RVCOG_SummaryPerformanceMeasures.csv",sep=""),sep=",",row.names=TRUE,col.names=TRUE)
      write.table(VC,paste("Output\\",Year,"RVCOG_SummaryVCMeasures.csv",sep=""),sep=",",row.names=TRUE,col.names=TRUE)
       
#Save a copy of current Links.Lk__ dataframe for use in RNCI Comparisons
      if (Year == "BaseYear"){
            BaseYearLinks.Lk__<-Links.Lk__
            SelectLink.Lk__$FC<-paste(LinkTypes.Lk__[Links.Lk__[match(SelectLink.Lk__[,1],rownames(Links.Lk__)),3],2], sep=" ")}
detach(Links.Lk__)

#Plot RNCI for each functional class comparing scenarios A and B
#---------------------------------------------------------------
    for(lt in Lt[c(1:6)]){
          FileName <- paste("Output\\Base_RCNI", lt, ".emf", sep="")
          win.metafile(FileName, width=10, height=7)
          OldPar <- par(mfrow=c(1,2), oma=c(1,1,4,1))
          LinkAdtALt <- BaseYearLinks.Lk__$Adt[BaseYearLinks.Lk__$Type == lt]
          BaseLanes <- BaseYearLinks.Lk__$Lanes[BaseYearLinks.Lk__$Type == lt]
          plotRnci(LinkAdtALt, BaseLanes, main="Base Year")
          mtext(paste(names(Lt)[Lt == lt], "Road Network Concentration Index"),
               line=1, outer=TRUE, cex=2)
          par(OldPar)
          dev.off()
          }
#Calculate a composite RNCI based on capacities
#---------------------------------------------
     win.metafile("Output\\CompositeBase_RNCI.emf", width=10, height=7)
     OldPar <- par(mfrow=c(1,2), oma=c(1,1,4,1))
      IsMajor <- Links.Lk__$Type %in% c(1,2,3,4,5,30)
     plotRnciCap(BaseYearLinks.Lk__$Adt[IsMajor],BaseYearLinks.Lk__$Lanes[IsMajor],BaseYearLinks.Lk__$Cap[IsMajor],
               main="Base Year")
     mtext("Capacity Weighted Average Arterial and Collector\nRoad Network Concentration Index",
          line=0, outer=TRUE, cex=2)
     par(OldPar)
     dev.off()
 #-----------------------------------------------END-----------------------------------------------------------------

