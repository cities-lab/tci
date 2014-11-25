#=============================
#calculate_mobility_measures.R
#=============================

#:Author: Brian Gregor
#:Contact: brian.j.gregor@odot.state.or.us
#:Date: 09/26/05
#:Revisions:
#:License: GPL2


#Read in data and define variables
#=================================

#Read in network data
#--------------------

#::

     Links.Lk__ <- read.table("eug_netdata_rev_names.txt", header=TRUE)
     rownames(Links.Lk__) <- paste(Links.Lk__$Inode, Links.Lk__$Jnode, sep="-")
     Capacities.Lk__ <- read.table("EUG_CAPS.TXT", header=TRUE)
     rownames(Capacities.Lk__) <- paste(Capacities.Lk__$inode, Capacities.Lk__$jnode, sep="-")

#Join the capacities data to the link data and calculate volume to capacity ratios
#---------------------------------------------------------------------------------

#::

     Links.Lk__$Cap <- Capacities.Lk__[rownames(Links.Lk__),"link_cap_per_day"]
     Links.Lk__$VcA <- Links.Lk__$AdtA / Links.Lk__$Cap
     Links.Lk__$VcB <- Links.Lk__$AdtB / Links.Lk__$Cap

#Define link types
#-----------------

#::

     # Link types classify all functional classes of interest
     Lt <- c(1, 2, 3, 4, 5, 6, 7, 8)
     names(Lt) <- c("Freeway", "Principal Arterial", "Major Arterial", "Minor Arterial",
                    "Major Collector", "Neighborhood Collector", "Local",
                    "Freeway Ramp")

#Only keep data of these link types
#----------------------------------

#::

     Links.Lk__ <- Links.Lk__[Links.Lk__$Type %in% Lt,]                    

#Add IncidentDelayRatio to the dataframe
#---------------------------------------

#::

     Links.Lk__$InciDlyRatio <- rep(NA, nrow(Links.Lk__))
     Links.Lk__$InciDlyRatio[Links.Lk__$Type %in% c(1,8)] <- 2.4
     Links.Lk__$InciDlyRatio[Links.Lk__$Type %in% 2:7 ] <- 1.1

#Define several standard parameters
#----------------------------------

#::

     # Vehicle occupancy
     VehOcc <- 1.25

     # Working days
     WorkDays <- 250

     # Population
     Population <- 235000


#Define functions to calculate measures
#======================================

#Define a function to calculate recurring vehicle hours of delay (VHD) by link
#-----------------------------------------------------------------------------

#:parameter: Delay.Lk - vector of link delay
#:parameter: Vol.Lk - vector of traffic volume by network link
#:return: RecurVhd.Lk - vector of vehicle-hours of recurring delay per link

#::

     calcRecurVhd <- function(Delay.Lk, Vol.Lk){
          RecurVhd.Lk <- Delay.Lk * Vol.Lk / 60
          RecurVhd.Lk
          }

#Define a function to calculate recurring and incident vehicle hours of delay (VHD) by link
#------------------------------------------------------------------------------------------

#:parameter: Delay.Lk - vector of link delay
#:parameter: Vol.Lk - vector of ADT by network link
#:parameter: InciDlyRatio.Lk - vector of ratios of incident delay to recurring delay
#:return: TotVhd.Lk - vector of total minutes of recurring and incident delay per link

#::

     calcTotVhd <- function(Delay.Lk, Vol.Lk, InciDlyRatio.Lk){
          TotVhd.Lk <- Delay.Lk * (1 + InciDlyRatio.Lk) * Vol.Lk / 60
          TotVhd.Lk
          }

#Define a function to calculate link vehicle miles traveled (VMT)
#----------------------------------------------------------------

#:parameter: Length.Lk - link length in miles
#:parameter: Vol.Lk - link volume in vehicles
#:return: Vmt.Lk - link vehicle miles traveled

#::

     calcVmt <- function(Length.Lk, Vol.Lk){
          Vmt.Lk <- Length.Lk * Vol.Lk
          Vmt.Lk
          }
     
     
#Define a function to calculate annual person delay
#--------------------------------------------------

#:parameter: TotVhd.Lk - vector of total vehicle hours of delay
#:parameter: VehOcc - average vehicle occupancy
#:parameter: WorkDays - number of work days per year
#:return: TotPhd.Lk - number of annual person hours of delay per year

#::

     calcTotPhd <- function(TotVhd.Lk, VehOcc, WorkDays){
          TotPhd.Lk <- TotVhd.Lk * VehOcc * WorkDays
          TotPhd.Lk
          }

#Define a function to calculate average speed by link type
#---------------------------------------------------------

#:parameter: Length.Lk - link length in miles
#:parameter: Vol.Lk - link traffic volume
#:parameter: Time.Lk - link travel time in minutes
#:parameter: Type.Lk - link type
#:return: AveSpeed.Lt - average speed by link type

#::

     calcAveTypeSpeed <- function(Length.Lk, Vol.Lk, Time.Lk, Type.Lk){
          Vmt.Lk <- calcVmt(Length.Lk, Vol.Lk)
          Speed.Lk <- 60 * Length.Lk / Time.Lk
          VmtWtSpeed.Lk <- Speed.Lk * Vmt.Lk
          SumVmtWtSpeed.Lt <- tapply(VmtWtSpeed.Lk, Type.Lk, sum)
          SumVmt.Lt <- tapply(Vmt.Lk, Type.Lk, sum)
          AveSpeed.Lt <- SumVmtWtSpeed.Lt / SumVmt.Lt
          AveSpeed.Lt
          }
      
#Define a function to calculate travel rate index by link type (freeflow comparison)
#-----------------------------------------------------------------------------------

#:parameter: Length.Lk - link length in miles
#:parameter: Vol.Lk - link traffic volume
#:parameter: Time.Lk - link travel time in minutes
#:parameter: Ffs.Lk - link free flow speed
#:parameter: Type.Lk - link type
#:return: AveTri.Lt - average travel rate index by link type

#::

     calcAveTypeTri1 <- function(Length.Lk, Vol.Lk, Delay.Lk, Ffs.Lk, Type.Lk){
          Vmt.Lk <- calcVmt(Length.Lk, Vol.Lk)
          FfsTime.Lk <- 60 * Length.Lk / Ffs.Lk
          RecurCongTime.Lk <- FfsTime.Lk + Delay.Lk
          Tri.Lk <- RecurCongTime.Lk / FfsTime.Lk
          VmtWtTri.Lk <- Tri.Lk * Vmt.Lk
          SumVmtWtTri.Lt <- tapply(VmtWtTri.Lk, Type.Lk, sum)
          SumVmt.Lt <- tapply(Vmt.Lk, Type.Lk, sum)
          AveTri.Lt <- SumVmtWtTri.Lt / SumVmt.Lt
          AveTri.Lt
          }
     
#Define a function to calculate travel rate index by link type (LOS D/E comparison)
#----------------------------------------------------------------------------------

#:parameter: Length.Lk - link length in miles
#:parameter: Vol.Lk - link traffic volume
#:parameter: Time.Lk - link travel time in minutes
#:parameter: Ffs.Lk - link free flow speed
#:parameter: Type.Lk - link type
#:return: AveTri.Lt - average travel rate index by link type

#::

     calcAveTypeTri2 <- function(Length.Lk, Vol.Lk, Delay.Lk,
                         DelayDe.Lk, Ffs.Lk, Type.Lk){
          Vmt.Lk <- calcVmt(Length.Lk, Vol.Lk)
          FfsTime.Lk <- 60 * Length.Lk / Ffs.Lk
          RecurCongTime.Lk <- FfsTime.Lk + Delay.Lk
          DeTime.Lk <- RecurCongTime.Lk - DelayDe.Lk
          Tri.Lk <- RecurCongTime.Lk / DeTime.Lk
          VmtWtTri.Lk <- Tri.Lk * Vmt.Lk
          SumVmtWtTri.Lt <- tapply(VmtWtTri.Lk, Type.Lk, sum)
          SumVmt.Lt <- tapply(Vmt.Lk, Type.Lk, sum)
          AveTri.Lt <- SumVmtWtTri.Lt / SumVmt.Lt
          AveTri.Lt
          }
      
#Define a function to calculate travel time index by link type (freeflow comparison)
#-----------------------------------------------------------------------------------

#:parameter: Length.Lk - link length in miles
#:parameter: Vol.Lk - link traffic volume
#:parameter: Time.Lk - link travel time in minutes
#:parameter: Ffs.Lk - link free flow speed
#:parameter: Type.Lk - link type
#:return: AveTri.Lt - average travel rate index by link type

#::

     calcAveTypeTti1 <- function(Length.Lk, Vol.Lk, Delay.Lk, Ffs.Lk,
                              InciDlyRatio.Lk, Type.Lk){
          Vmt.Lk <- calcVmt(Length.Lk, Vol.Lk)
          TotDelay.Lk <- Delay.Lk * (1 + InciDlyRatio.Lk)
          FfsTime.Lk <- 60 * Length.Lk / Ffs.Lk
          TotCongTime.Lk <- FfsTime.Lk + TotDelay.Lk
          Tti.Lk <- TotCongTime.Lk / FfsTime.Lk
          VmtWtTti.Lk <- Tti.Lk * Vmt.Lk
          SumVmtWtTti.Lt <- tapply(VmtWtTti.Lk, Type.Lk, sum)
          SumVmt.Lt <- tapply(Vmt.Lk, Type.Lk, sum)
          AveTti.Lt <- SumVmtWtTti.Lt / SumVmt.Lt
          AveTti.Lt
          }
      
#Define a function to calculate travel time index by link type (LOS D/E comparison)
#----------------------------------------------------------------------------------

#:parameter: Length.Lk - link length in miles
#:parameter: Vol.Lk - link traffic volume
#:parameter: Time.Lk - link travel time in minutes
#:parameter: Ffs.Lk - link free flow speed
#:parameter: Type.Lk - link type
#:return: AveTri.Lt - average travel rate index by link type

#::

     calcAveTypeTti2 <- function(Length.Lk, Vol.Lk, Delay.Lk, DelayDe.Lk, Ffs.Lk,
                         InciDlyRatio.Lk,  Type.Lk){
     Vmt.Lk <- calcVmt(Length.Lk, Vol.Lk)
     TotDelay.Lk <- Delay.Lk * (1 + InciDlyRatio.Lk)
     FfsTime.Lk <- 60 * Length.Lk / Ffs.Lk
     RecurCongTime.Lk <- FfsTime.Lk + Delay.Lk
     TotCongTime.Lk <- FfsTime.Lk + TotDelay.Lk
     DeTime.Lk <- RecurCongTime.Lk - DelayDe.Lk
     Tti.Lk <- TotCongTime.Lk / DeTime.Lk
     VmtWtTti.Lk <- Tti.Lk * Vmt.Lk
     SumVmtWtTti.Lt <- tapply(VmtWtTti.Lk, Type.Lk, sum)
     SumVmt.Lt <- tapply(Vmt.Lk, Type.Lk, sum)
     AveTti.Lt <- SumVmtWtTti.Lt / SumVmt.Lt
     AveTti.Lt
     }


#Conduct tests of measures
#=========================

#::

     attach(Links.Lk__)

#Calculate average annual delay per person for two reference speeds and two network scenarios
#--------------------------------------------------------------------------------------------

#Scenario A - with Ferry Street Bridge Link
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#::

     # Freeflow reference speed all Links
     # Calculate daily vehicle hours of delay
     ARecurVhdFreeflow.Lk <- calcRecurVhd(DlyFfsA, AdtA)
     ATotVhdFreeflow.Lk <- calcTotVhd(DlyFfsA, AdtA, InciDlyRatio)
     # Calculate annual person hours of delay
     ARecurAnnPhdFreeflow.Lk <- calcTotPhd(ARecurVhdFreeflow.Lk, VehOcc, WorkDays)
     ATotAnnPhdFreeflow.Lk <- calcTotPhd(ATotVhdFreeflow.Lk, VehOcc, WorkDays)
     # Calculate annual hours of delay per capita
     AAnnRecurDlyCapFreeflow <- sum(ARecurAnnPhdFreeflow.Lk) / Population
     AAnnTotDlyCapFreeflow <- sum(ATotAnnPhdFreeflow.Lk) / Population

     # Freeflow reference speed freeway and principal arterial links
     # Calculate daily vehicle hours of delay
     ARecurVhdFreeflow.Lk <- calcRecurVhd(DlyFfsA[Type %in% c(1,2)], AdtA[Type %in% c(1,2)])
     ATotVhdFreeflow.Lk <- calcTotVhd(DlyFfsA[Type %in% c(1,2)],
                              AdtA[Type %in% c(1,2)], InciDlyRatio[Type %in% c(1,2)])
     # Calculate annual person hours of delay
     ARecurAnnPhdFreeflow.Lk <- calcTotPhd(ARecurVhdFreeflow.Lk, VehOcc, WorkDays)
     ATotAnnPhdFreeflow.Lk <- calcTotPhd(ATotVhdFreeflow.Lk, VehOcc, WorkDays)
     # Calculate annual hours of delay per capita
     AAnnRecurDlyCapFreeflow2 <- sum(ARecurAnnPhdFreeflow.Lk) / Population
     AAnnTotDlyCapFreeflow2 <- sum(ATotAnnPhdFreeflow.Lk) / Population

     # Moderate flow reference speed for all links
     # Calculate daily vehicle hours of delay
     ARecurVhdModflow.Lk <- calcRecurVhd(DlyDeA, AdtA)
     ATotVhdModflow.Lk <- calcTotVhd(DlyDeA, AdtA, InciDlyRatio)
     # Calculate annual person hours of delay
     ARecurAnnPhdModflow.Lk <- calcTotPhd(ARecurVhdModflow.Lk, VehOcc, WorkDays)
     ATotAnnPhdModflow.Lk <- calcTotPhd(ATotVhdModflow.Lk, VehOcc, WorkDays)
     # Calculate annual hours of delay per capita
     AAnnRecurDlyCapModflow <- sum(ARecurAnnPhdModflow.Lk) / Population
     AAnnTotDlyCapModflow <- sum(ATotAnnPhdModflow.Lk) / Population

     # Moderate flow reference speed for freeway and principal arterial links
     # Calculate daily vehicle hours of delay
     ARecurVhdModflow.Lk <- calcRecurVhd(DlyDeA[Type %in% c(1,2)], AdtA[Type %in% c(1,2)])
     ATotVhdModflow.Lk <- calcTotVhd(DlyDeA[Type %in% c(1,2)],
                         AdtA[Type %in% c(1,2)], InciDlyRatio[Type %in% c(1,2)])
     # Calculate annual person hours of delay
     ARecurAnnPhdModflow.Lk <- calcTotPhd(ARecurVhdModflow.Lk, VehOcc, WorkDays)
     ATotAnnPhdModflow.Lk <- calcTotPhd(ATotVhdModflow.Lk, VehOcc, WorkDays)
     # Calculate annual hours of delay per capita
     AAnnRecurDlyCapModflow2 <- sum(ARecurAnnPhdModflow.Lk) / Population
     AAnnTotDlyCapModflow2 <- sum(ATotAnnPhdModflow.Lk) / Population

#Scenario B - without Ferry Street Bridge Link
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#::

     # Freeflow reference speed all Links
     # Calculate daily vehicle hours of delay
     BRecurVhdFreeflow.Lk <- calcRecurVhd(DlyFfsB, AdtB)
     BTotVhdFreeflow.Lk <- calcTotVhd(DlyFfsB, AdtB, InciDlyRatio)
     # Calculate annual person hours of delay
     BRecurAnnPhdFreeflow.Lk <- calcTotPhd(BRecurVhdFreeflow.Lk, VehOcc, WorkDays)
     BTotAnnPhdFreeflow.Lk <- calcTotPhd(BTotVhdFreeflow.Lk, VehOcc, WorkDays)
     # Calculate annual hours of delay per capita
     BAnnRecurDlyCapFreeflow <- sum(BRecurAnnPhdFreeflow.Lk) / Population
     BAnnTotDlyCapFreeflow <- sum(BTotAnnPhdFreeflow.Lk) / Population

     # Freeflow reference speed freeway and principal arterial links
     # Calculate daily vehicle hours of delay
     BRecurVhdFreeflow.Lk <- calcRecurVhd(DlyFfsB[Type %in% c(1,2)], AdtB[Type %in% c(1,2)])
     BTotVhdFreeflow.Lk <- calcTotVhd(DlyFfsB[Type %in% c(1,2)],
                              AdtB[Type %in% c(1,2)], InciDlyRatio[Type %in% c(1,2)])
     # Calculate annual person hours of delay
     BRecurAnnPhdFreeflow.Lk <- calcTotPhd(BRecurVhdFreeflow.Lk, VehOcc, WorkDays)
     BTotAnnPhdFreeflow.Lk <- calcTotPhd(BTotVhdFreeflow.Lk, VehOcc, WorkDays)
     # Calculate annual hours of delay per capita
     BAnnRecurDlyCapFreeflow2 <- sum(BRecurAnnPhdFreeflow.Lk) / Population
     BAnnTotDlyCapFreeflow2 <- sum(BTotAnnPhdFreeflow.Lk) / Population

     # Moderate flow reference speed for all links
     # Calculate daily vehicle hours of delay
     BRecurVhdModflow.Lk <- calcRecurVhd(DlyDeB, AdtB)
     BTotVhdModflow.Lk <- calcTotVhd(DlyDeB, AdtB, InciDlyRatio)
     # Calculate annual person hours of delay
     BRecurAnnPhdModflow.Lk <- calcTotPhd(BRecurVhdModflow.Lk, VehOcc, WorkDays)
     BTotAnnPhdModflow.Lk <- calcTotPhd(BTotVhdModflow.Lk, VehOcc, WorkDays)
     # Calculate annual hours of delay per capita
     BAnnRecurDlyCapModflow <- sum(BRecurAnnPhdModflow.Lk) / Population
     BAnnTotDlyCapModflow <- sum(BTotAnnPhdModflow.Lk) / Population

     # Moderate flow reference speed for freeway and principal arterial links
     # Calculate daily vehicle hours of delay
     BRecurVhdModflow.Lk <- calcRecurVhd(DlyDeB[Type %in% c(1,2)], AdtB[Type %in% c(1,2)])
     BTotVhdModflow.Lk <- calcTotVhd(DlyDeB[Type %in% c(1,2)],
                         AdtB[Type %in% c(1,2)], InciDlyRatio[Type %in% c(1,2)])
     # Calculate annual person hours of delay
     BRecurAnnPhdModflow.Lk <- calcTotPhd(BRecurVhdModflow.Lk, VehOcc, WorkDays)
     BTotAnnPhdModflow.Lk <- calcTotPhd(BTotVhdModflow.Lk, VehOcc, WorkDays)
     # Calculate annual hours of delay per capita
     BAnnRecurDlyCapModflow2 <- sum(BRecurAnnPhdModflow.Lk) / Population
     BAnnTotDlyCapModflow2 <- sum(BTotAnnPhdModflow.Lk) / Population

#Calculate Travel Time Index
#===========================

#::

     UmsType <- 1 * (Type %in% c(1,2))

     # calculate TTI for Scenario A freeflow speed, all classes
     calcAveTypeTti1(Length, AdtA, DlyFfsA, Ffs, InciDlyRatio, rep(1, length(Length)))
     # calculate TTI for Scenario A freeflow speed, freeway and principal arterial
     calcAveTypeTti1(Length, AdtA, DlyFfsA, Ffs, InciDlyRatio, UmsType)
     # calculate TTI for Scenario A modflow speed, all classes
     calcAveTypeTti2(Length, AdtA, DlyFfsA, DlyDeA, Ffs, InciDlyRatio,  rep(1, length(Length)))
     # calculate TTI for Scenario A modflow speed, freeway and principal arterial
     calcAveTypeTti2(Length, AdtA, DlyFfsA, DlyDeA, Ffs, InciDlyRatio,  UmsType)

     # calculate TTI for Scenario B freeflow speed, all classes
     calcAveTypeTti1(Length, AdtB, DlyFfsB, Ffs, InciDlyRatio, rep(1, length(Length)))
     # calculate TTI for Scenario B freeflow speed, freeway and principal arterial
     calcAveTypeTti1(Length, AdtB, DlyFfsB, Ffs, InciDlyRatio, UmsType)
     # calculate TTI for Scenario B modflow speed, all classes
     calcAveTypeTti2(Length, AdtB, DlyFfsB, DlyDeB, Ffs, InciDlyRatio,  rep(1, length(Length)))
     # calculate TTI for Scenario B modflow speed, freeway and principal arterial
     calcAveTypeTti2(Length, AdtB, DlyFfsB, DlyDeB, Ffs, InciDlyRatio,  UmsType)


