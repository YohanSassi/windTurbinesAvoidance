#_______________________________________________________________________________________________________________________
##
##      PROJECT: Avoidance of wind turbines by griffon vultures
##
##      GOAL OF THE SCRIPT:  This script aims to test the method to assess wind turbine avoidance by simulating
##                          individual path with specific avoidance angles and minimum distance to turbine. These
##                          simulations are then used as data set for rotation analysis to see if we detect 
##                          avoidance
##
##
##      CREDIT: Yohan Sassi (yoh.sassi22@gmail.com) & Benjamin Robira (benjamin.robira@normalesup.org)
##
#_______________________________________________________________________________________________________________________

rm(list = ls())

#Spatial
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(sf)
library(maptools)
library(raster)

#Datetime
library(lubridate)

#Rcpp
library(Rcpp)

#Parallelisation 
library(parallel)
library(doParallel)

#Datahandling
library(dplyr)
library(zoo)
library(data.table)
library(zoom)
library(scales)
library(stringr)
library(CircularDDM)
library(geosphere)
library(move)
library(CircStats)

#Homemade functions
source("Functions/df_to_spatial_df_utm.R")#To convert long/lat to UTM
Rcpp::sourceCpp("Functions/discretisePathInSpace2.cpp")




#________________________________________________________________
##
#### Prepare raw data lists to loop on afterwards      ####
##
#________________________________________________________________

# Full list containing Vultures GPS fixes
yearsRawData <- vector("list")


# For year1 = 2019 etc...
yearsRawData[[1]] <- list(name = "2019", rawFilesName = c("Data/RawData/Vulturestracks_Jan_to_May2019.csv",
                                                          "Data/RawData/Vulturestracks_Jun_to_Aug2019.csv",
                                                          "Data/RawData/Vulturestracks_Sep_to_Dec2019.csv"))

yearsRawData[[2]] <- list(name = "2020", rawFilesName = c("Data/RawData/Vulturestracks_Jan_to_May2020.csv",
                                                          "Data/RawData/Vulturestracks_Jun_to_Aug2020.csv",
                                                          "Data/RawData/Vulturestracks_Sep_to_Dec2020.csv"))

yearsRawData[[3]] <- list(name = "2021", rawFilesName = c("Data/RawData/Vulturestracks_Jan_to_May2021.csv",
                                                          "Data/RawData/Vulturestracks_Jun_to_Aug2021.csv",
                                                          "Data/RawData/Vulturestracks_Sep_to_Dec2021.csv"))




# Load wind farms and associated wind turbine lists
load("./Data/RawData/AllWindFarms_Causses.Rdata")
load("./Data/RawData/AllWindTurbines_Causses.Rdata")
load("./Data/RawData/Causses_WindFarmNames_Dict.Rdata")


#________________________________________________________________
##
#### Rediscretisation of paths at 5m         ####
##
#________________________________________________________________

## Load elevation raster from MNT
elevationCaussesRaster <- raster::raster('./Data/RawData/Elevation_Causses.tif') 

# Parameters to rediscretise at constant step length
timeThresholdIndependentMinutes = 0.5 # Time between two location to consider them independent
distanceSamplingInMeter = 5 # Spatial step between two rediscretized location
turblenceBufferInMeter = 15 # Turbulence buffer around turbine (+ GPS vertical error)
nbCoresToSubstract = 2
year <- c("2019","2020","2021")
selectedWindFarms = c(1,4,10,11)



#-- Loop
for (y in 1:3){ # Iterate on each year
  
  ## Initialize a data frame to save results
  summaryRawData<- matrix(nrow = 3*18, ncol=8)
  summaryRawData <- as.data.frame(summaryRawData) # A data frame that will contain summary data of the filtrations
  colnames(summaryRawData ) <- c("year", "fileName", "windFarm", "nbIndInZone", "nbPtInFarmBuffer", "minRiskyAlt","altThreshold","meanDistToClosestTurbine")
  
  c = 0 # Counter
  
  for (p in 1:3){ # Iterate on each file within a specific year
    
    ## Import data
    fileToImport <- yearsRawData[[y]]$rawFilesName[p]
    dataVulture.df <- read.csv(fileToImport,h=T)
    
    # Transform points of paths into UTM coordinates
    df_to_spatial_df_utm(dataframe=dataVulture.df, 
                         column_longitude="location.long", 
                         column_latitude="location.lat", 
                         utm_zone=31, hemisphere="N")
    dataVulture.df.sp <- dataframe.sp_utm
    dataVulture.df.sp@data$rowNumber <- 1:nrow(dataVulture.df)
    
    for (f in selectedWindFarms){ #length(allWindFarms)
      # Counter for rows
      c = c+1
      
      # Start to fill the summary df
      underscoreindices <- str_locate_all(pattern ='_', yearsRawData[[y]]$rawFilesName[p])
      ptindice <- str_locate_all(pattern ='.csv', yearsRawData[[y]]$rawFilesName[p])
      filename <- substr(yearsRawData[[y]]$rawFilesName[p], 
                         as.integer(underscoreindices[[1]][1,1])+1, 
                         as.integer(ptindice[[1]][1,1])-1)
      
      
      summaryRawData[c,1:3] <- c(yearsRawData[[y]]$name,filename,allWindFarms[[f]]$name)
      
      # Points located in the farm's buffer --> reduce the size of the working dataframe
      pointsInFarmBuffer.df.sp <-  dataVulture.df.sp[!is.na(sp::over(dataVulture.df.sp, allWindFarms[[f]]$farmBuffer.sP)),]
      
      #If there are points in the farm buffer:
      if (length(pointsInFarmBuffer.df.sp) > 0) {
        
        # Recreate data frame of vultures' tracks with UTM coordinates
        dataVulture.df_utm <- as.data.frame(cbind(pointsInFarmBuffer.df.sp@coords, pointsInFarmBuffer.df.sp@data))
        dataVulture.df_utm[,c(1,2)] <- round(dataVulture.df_utm[,c(1,2)])
        
        # Segregate date & time
        dataVulture.df_utm$time <- format(as.POSIXct(dataVulture.df_utm$timestamp), format = "%H:%M:%S") ## extract time
        dataVulture.df_utm$date <- as.Date(dataVulture.df_utm$timestamp, format = "%Y-%m-%d")
        
        
        ##---
        ## For later (to filter out points after rediscretisation): calculate time of entrance and exit of the buffer area
        
        # First determine ID of points out of the area
        whichPointsOutBuffer <- which(!(dataVulture.df.sp@data$rowNumber %in% dataVulture.df_utm$rowNumber))
        isInArea <- rep(1, times=length(dataVulture.df.sp@data$rowNumber)) # area = farm buffer ?
        isInArea[whichPointsOutBuffer] <- 0
        
        # Second, determine entrance exit time per individual
        vectorOfIndividuals <- unique(dataVulture.df$individual.local.identifier)
        
        entranceExitBufferArea.list <- lapply(vectorOfIndividuals, function(x){
          dfTransitory <- dataVulture.df[dataVulture.df$individual.local.identifier == x,]
          isInAreaTransitory <- isInArea[dataVulture.df$individual.local.identifier == x]
          
          # Find entrances and exits -> preceded by 0 or followed by 0; correct first point == entrance if 1, last point == exist if 1
          whichPointEntrances <- which(isInAreaTransitory == 1 & dplyr::lag(isInAreaTransitory) == 0)
          if(isInAreaTransitory[1] == 1){
            whichPointEntrances <- c(1, whichPointEntrances)
          }
          
          whichPointExits <- which(isInAreaTransitory == 1 & dplyr::lead(isInAreaTransitory) == 0)
          if(isInAreaTransitory[length(isInAreaTransitory)] == 1){
            whichPointExits <- c(whichPointExits, length(isInAreaTransitory))
          }
          
          return(
            cbind(
              as.character(dfTransitory$timestamp[whichPointEntrances]),
              as.character(dfTransitory$timestamp[whichPointExits]),
              rep(x, time=length(whichPointEntrances))
            )
          )
        }
        )
        
        entranceExitBufferArea.df <- do.call(rbind.data.frame, entranceExitBufferArea.list)
        colnames(entranceExitBufferArea.df) <- c("timestamp_entrance", "timestamp_exit", "individual")
        ##---
        
        # Attribute a height to every points (from MNT) instead of altitude above mean sea level (given by GPS)
        dataVulture.df_utm$height.above.ground <- round(dataVulture.df_utm$height.above.msl - 
                                                          raster::extract(elevationCaussesRaster, pointsInFarmBuffer.df.sp, method='bilinear'))
        
        ##  Filter by ground speed (>4m/s) to be sure they are flying / gps.hdop < 4 for reduced GPS location errors / remove all data below turbines
        # Nathan et al. 2012, D’Eon & Delparte 2005, Martin-Diaz et al. 2020
        minRiskyAlt <- max(unique(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp$ht_max)) - 
          max(unique(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp$diam_rotor))
        
        dataVulture_filtered.df_utm <- dataVulture.df_utm[which(dataVulture.df_utm$ground.speed > 4 & 
                                                                  dataVulture.df_utm$gps.hdop < 4 &
                                                                  dataVulture.df_utm$height.above.ground > minRiskyAlt),]
        
        ## Estimate the distance to the closest turbines (problem if only one row)
        if (nrow(dataVulture_filtered.df_utm) > 1) {
          dataVulture_filtered.df_utm$distToClosestTurbine <- apply(pointDistance(
            spTransform(SpatialPoints(dataVulture_filtered.df_utm[,1:2], proj4string = allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp@proj4string), CRS("+proj=longlat +datum=WGS84")),
            spTransform(SpatialPoints(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, proj4string = allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp@proj4string), CRS("+proj=longlat +datum=WGS84")), 
            lonlat = T, allpairs = T), 1, min)
        } else {
          dataVulture_filtered.df_utm$distToClosestTurbine <- min(pointDistance(
            spTransform(SpatialPoints(dataVulture_filtered.df_utm[,1:2], proj4string = allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp@proj4string), CRS("+proj=longlat +datum=WGS84")),
            spTransform(SpatialPoints(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, proj4string = allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp@proj4string), CRS("+proj=longlat +datum=WGS84")), 
            lonlat = T, allpairs = T))
        }
        
        # Check which individuals crossed the zone (buffer comprised)
        indiv.v <- unique(dataVulture_filtered.df_utm$individual.local.identifier)
        
        # Fill summary with info on negative height
        summaryRawData[c,4:6] <- c(length(indiv.v), nrow(dataVulture_filtered.df_utm),minRiskyAlt)
        
        # Save the filtered raw dataset
        slashindices <- str_locate_all(pattern ='/', yearsRawData[[y]]$rawFilesName[p])
        
        filename <- substr(yearsRawData[[y]]$rawFilesName[p], 
                           as.integer(slashindices[[1]][nrow(slashindices[[1]]),1])+1, 
                           as.integer(ptindice[[1]][1,1])-1)
        
            
        #______________________________________________________________________________
        ##
        #### Rediscretisation of vultures' tracks at a constant step length       ####
        ##
        #______________________________________________________________________________
        
        #Define a height threshold by turbine max height (ht_max) + a buffer of aerial turbulence + the largest negative height value set to 0 (max = 50m)
        altitudeThresholdInMeter <- max(unique(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp$ht_max)) + turblenceBufferInMeter
        summaryRawData[c,7:8] <- c(altitudeThresholdInMeter,mean(dataVulture_filtered.df_utm$distToClosestTurbine))
        
        ##Prepare parallelisation to go faster
        cores=detectCores()
        cl <- makeCluster(cores[1]-nbCoresToSubstract) #not to overload your computer, take maximum number of cores minus 4 #Can be changed
        registerDoParallel(cl)
        registerDoSEQ() #if you opt not to run parallelising -----------> here problem, certainly because of Rcpp that is not correctly loaded... why? I don't know. -> it is common problem with parallelisation. Parallelisation cannot allow us to use of Rcpp
        
        dataVulture.df_utm_rediscretised <- foreach(count=1:length(indiv.v), .packages=c('dplyr', 'lubridate', 'zoo'), .combine = 'c') %dopar% {
          # #Resource Rcpp function in local environment
          # setwd("T:/Saved_PhD/Yohan_analysis/windTurbineAvoidance")
          # Rcpp::sourceCpp("Functions/discretisePathInSpace2.cpp")
          print(count)
          #Select individual of interest
          dataTransitory <-  dataVulture_filtered.df_utm[ dataVulture_filtered.df_utm$individual.local.identifier == indiv.v[count],]
          dataTransitory$Datedecimal <- decimal_date(ymd_hms(dataTransitory$timestamp))
          
          # Filtrate points for which time interval is too long
          timeDifferenceWithPrevious <- difftime(dataTransitory$timestamp, dplyr::lag(dataTransitory$timestamp), units="min")
          timeDifferenceWithNext <- difftime(dataTransitory$timestamp, dplyr::lead(dataTransitory$timestamp), units="min") 
          
          whichPointToFilterBecauseTooLongSampling <- which(
            timeDifferenceWithPrevious > timeThresholdIndependentMinutes &
              abs(timeDifferenceWithNext) > timeThresholdIndependentMinutes 
          )
          
          #Filtrate
          if(length(whichPointToFilterBecauseTooLongSampling)>0){
            whichPointToFilterBecauseTooLongSampling <- c(whichPointToFilterBecauseTooLongSampling[[1]] - 1, 
                                                          whichPointToFilterBecauseTooLongSampling,
                                                          whichPointToFilterBecauseTooLongSampling[[length(whichPointToFilterBecauseTooLongSampling)]] + 1)
            
            dataTransitory <- dataTransitory[-whichPointToFilterBecauseTooLongSampling,]
          }
          
          #Rediscretise
          dataTransitoryRediscretised.df <-discretisePathInSpace2(
            longitudesVector=dataTransitory$longitude,
            latitudesVector=dataTransitory$latitude,
            datesVector=dataTransitory$Datedecimal,
            distanceSampling=distanceSamplingInMeter,
            timeIntervalToConsiderPoints=timeThresholdIndependentMinutes*60,
            displayProgress=TRUE
          )
          
          if(nrow(dataTransitory)>3 && nrow(dataTransitoryRediscretised.df)>3){
            
            dataTransitoryRediscretised.df <- as.data.frame(dataTransitoryRediscretised.df)
            colnames(dataTransitoryRediscretised.df) <- c("x", "y", "datedecimal")
            
            dataTransitoryRediscretised.df$timestamp <- date_decimal(dataTransitoryRediscretised.df$datedecimal, tz = "UTC")
            dataTransitory$timestamp <- ymd_hms(dataTransitory$timestamp, tz = "UTC")
            
            # Find the match with the nearest point in time when not possible to perfectly match the time
            SampleTime <- 
              structure(dataTransitoryRediscretised.df$timestamp, 
                        class = c("POSIXct", "POSIXt"), tzone = "UTC")
            rTime <- 
              structure(c(dataTransitory$timestamp), 
                        class = c("POSIXct", "POSIXt"), tzone = "UTC")
            
            sDT <- data.table(SampleTime)
            rDT <- data.table(rTime)
            
            # rolling join to nearest & add the height above ground value to dataTransitoryRediscretised.df
            nearestPtIndices <- rDT[sDT, on = .(rTime = SampleTime), roll = "nearest", which = TRUE]
            dataTransitoryRediscretised.df$height.above.ground <- dataTransitory[nearestPtIndices,34]
            dataTransitoryRediscretised.df$distToClosestTurbine <- dataTransitory[nearestPtIndices,35]
            
            
            #There is an issue with rediscretisation: since we filter points out of the area before rediscretising, then, a linear interpolation can occur between two distant locations, despite the time threshold because they might range below it
            #Yet, the value of this threshold is chosen to account for time interval between locations when outside the area too.
            #Thus, here, we will remove the interpolation that exists between known pairs of exit/entrance of the area buffer.
            
            entranceExitBufferArea.df_transitory <- entranceExitBufferArea.df[entranceExitBufferArea.df$individual == indiv.v[count], ]
            entranceExitBufferArea.df_transitory$timestamp_entrance <- ymd_hms(entranceExitBufferArea.df_transitory$timestamp_entrance)
            entranceExitBufferArea.df_transitory$timestamp_exit <- ymd_hms(entranceExitBufferArea.df_transitory$timestamp_exit)
            
            entranceExitBufferArea.df_transitory$interval <- entranceExitBufferArea.df_transitory$timestamp_entrance %--% entranceExitBufferArea.df_transitory$timestamp_exit
            
            whichPointToKeep <- sapply(
              entranceExitBufferArea.df_transitory$interval,
              function(x){which(dataTransitoryRediscretised.df$timestamp %within% x)}
            )
            whichPointToKeep <- unlist(whichPointToKeep)
            
            if(length(whichPointToKeep)>0){
              
              dataTransitoryRediscretised.df <- dataTransitoryRediscretised.df[whichPointToKeep,]
              
              # Initialise aboveThresholdAltitude column full of NA
              dataTransitoryRediscretised.df$aboveThresholdAltitude <- NA
              
              # If height >= altitudeThresholdInMeter the point is considered above the threshold
              dataTransitoryRediscretised.df$aboveThresholdAltitude[which(dataTransitoryRediscretised.df$height.above.ground >= altitudeThresholdInMeter)] <- 1
              dataTransitoryRediscretised.df$aboveThresholdAltitude[which(dataTransitoryRediscretised.df$height.above.ground < altitudeThresholdInMeter)] <- 0
              
              # Add individual identity
              dataTransitoryRediscretised.df$ID <- indiv.v[count]
              
              # Reshape data before extraction
              dataTransitoryRediscretised.df <- as.matrix(dataTransitoryRediscretised.df)
              dataTransitoryRediscretised.df <- c(t(dataTransitoryRediscretised.df))
              
              #Export
              return(dataTransitoryRediscretised.df)
              
              #Relax memory
              # rm(list=ls())
              # gc()
            }
          }
        }
        # Stop the cluster
        stopCluster(cl)
        
        # If dataVulture.df_utm_rediscretised is not empty
        if (length(dataVulture.df_utm_rediscretised) != 0){
          
          # Reshape data
          dataVulture.df_utm_rediscretised <- matrix(unlist(dataVulture.df_utm_rediscretised), ncol=8, byrow=TRUE)
          dataVulture.df_utm_rediscretised <- as.data.frame(dataVulture.df_utm_rediscretised)
          
          dataVulture.df_utm_rediscretised[,1] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,1]))
          dataVulture.df_utm_rediscretised[,2] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,2]))
          dataVulture.df_utm_rediscretised[,3] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,3]))
          dataVulture.df_utm_rediscretised[,5] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,5]))
          dataVulture.df_utm_rediscretised[,6] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,6]))
          dataVulture.df_utm_rediscretised[,7] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,7]))
          
          colnames(dataVulture.df_utm_rediscretised) <- c("x", "y", "datedecimal", "timestamp", "height.above.ground" ,"distToClosestTurbine", "aboveThresholdAltitude", "ID")
          
          # Save rediscretised path
          filename <- substr(yearsRawData[[y]]$rawFilesName[p], 
                             as.integer(underscoreindices[[1]][1,1])+1, 
                             as.integer(ptindice[[1]][1,1])-1)
          
          save(dataVulture.df_utm_rediscretised,
               file = paste("./Data/Rediscretised_Path/Rediscretised_Path_5m/RediscretisedPath_5M_",filename,"_",allWindFarms[[f]]$name,".Rdata", sep = ""))
        }
      } else {
        summaryRawData[c,4:7] <- c("NA","NA","NA","NA","NA")
      }
    }
  }
}



#______________________________________________________________________
##
#### Combine into a single Rdata file         ####
##
#______________________________________________________________________


for (f in selectedWindFarms){
  for (y in 1:3){
    allFiles <- list.files("./Data/Rediscretised_Path/Rediscretised_Path_5m", paste(year[y],"_",allWindFarms[[f]]$name,sep="") , all.files = F, full.names = T)
    
    if (length(allFiles) != 0){
      for (n in 1:length(allFiles)){
        load(allFiles[n])
        if(n==1){
          dataVulture.df_utm_rediscretised_tot <- dataVulture.df_utm_rediscretised
        } else {
          dataVulture.df_utm_rediscretised_tot <- rbind(dataVulture.df_utm_rediscretised_tot,dataVulture.df_utm_rediscretised)
        }
      }
      
      # Order by ID then by time - Necessary to reconstruct paths bouts afterwards
      dataVulture.df_utm_rediscretised <- dataVulture.df_utm_rediscretised_tot[order(dataVulture.df_utm_rediscretised_tot$ID,
                                                                                     dataVulture.df_utm_rediscretised_tot$timestamp),]
      
      # To be sure to have the right variable type - should stay the same even without this
      dataVulture.df_utm_rediscretised[,1] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,1]))
      dataVulture.df_utm_rediscretised[,2] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,2]))
      dataVulture.df_utm_rediscretised[,3] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,3]))
      dataVulture.df_utm_rediscretised[,5] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,5]))
      dataVulture.df_utm_rediscretised[,6] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,6]))
      dataVulture.df_utm_rediscretised[,7] <- as.numeric(as.character(dataVulture.df_utm_rediscretised[,7]))
      
      # Give name and save full dataset
      filename <- paste("./Data/Rediscretised_Path/Rediscretised_Path_5m/RediscretisedPath_5M_",year[y],"_",allWindFarms[[f]]$name,".Rdata",sep="")
      save(dataVulture.df_utm_rediscretised, file = filename)
    }
  }
}



#______________________________________________________________________
##
#### Extract the observed paths to estimate sinuosity         ####
##
#______________________________________________________________________


for (y in 1:3){ # Loop on years
  
  # Loop on all wind farms 
  for (f in selectedWindFarms){
    # Parameters to control the rotation angle (here 0° to have the observed value) & the buffer around turbines (not used here - see ys_04_RotationAnalysis)
    j=1 # control for rotation angle
    i=1 # control for buffer around turbines
    
    # Parameters - primarily used in RotationAnalysis, not here, but need to be parametrised because this script is derived from the RotationAnalysis
    valueSpacingRotation <- 10 # Step for rotation values
    numberPointsRemovePath <- 10 # Minimum number of locations required to consider a path
    rotationAngle.v <- seq(from=0, to=350, by=valueSpacingRotation) #Angle of wind farm rotation (in degrees)
    bufferSize.v <- seq(from=50, to=1000, by=50) # Size of buffer that will be used around wind turbine (in meters)
    
    # Spatial initial parameters
    dataCoordinatesTurbines.df.sp_rotated <- maptools::elide(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)
    proj4string(dataCoordinatesTurbines.df.sp_rotated) <- allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp@proj4string
    dataCoordinatesTurbines.df.sp_rotated_buffered <- gBuffer(dataCoordinatesTurbines.df.sp_rotated,  width=bufferSize.v[i], quadsegs = 100)
    farm.spatialPolygons_rotated <- maptools::elide(allWindFarms[[f]]$farm.sP, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)
    proj4string(farm.spatialPolygons_rotated) <- allWindFarms[[f]]$farm.sP@proj4string
    
    # Load data
    load(list.files("./Data/Rediscretised_Path/Rediscretised_Path_5m", paste("RediscretisedPath_5M_",year[y],"_",allWindFarms[[f]]$name,sep="") , all.files = F, full.names = T))
    pointsIndiv.sp <- dataVulture.df_utm_rediscretised
    
    # Give coordinates and projection
    coordinates(pointsIndiv.sp) <- ~ x + y
    proj4string(pointsIndiv.sp) <- dataCoordinatesTurbines.df.sp_rotated_buffered@proj4string
    
    # What intervals between points are >30s
    time_1 <- pointsIndiv.sp[1:nrow(pointsIndiv.sp)-1,]$timestamp # remove the last
    time_2 <- pointsIndiv.sp[2:nrow(pointsIndiv.sp),]$timestamp # remove the first
    TooLarge_int <- which(abs(difftime(time_1,time_2,units = "secs")) > 30) # Math both time to estimate interval
    startBoutIn.v  <- c(1, TooLarge_int + 1) # Give the number fo the interval so real time = interval + 1
    
    #Create independent bout as soon as time interval between two consecutive points is > 30s (bout name = position of the starting points in the list)
    pointsIndiv.sp@data$Bout <- NA
    pointsIndiv.sp@data$Bout[startBoutIn.v] <- startBoutIn.v 
    pointsIndiv.sp@data$Bout <- zoo::na.locf(pointsIndiv.sp$Bout)
    
    #What points are in area & remove those outside
    inArea.v <- sp::over(pointsIndiv.sp, farm.spatialPolygons_rotated)
    pointsInFarm <- pointsIndiv.sp[!is.na(inArea.v),]
    
    #First determine whether it's a path "fully below", "fully above", or a mixture of both
    summaryBout <- pointsInFarm@data %>% group_by(Bout) %>% summarise(
      CountAboveThreshold = sum(as.numeric(as.character(aboveThresholdAltitude))),
      CountTot = n()
    )
    
    ##Remove if too few number of points
    whichToRemoveBout <- c(summaryBout$Bout[summaryBout$CountTot < numberPointsRemovePath])
    
    # Update pointsInFarm and summaryBout
    if(length(whichToRemoveBout)>0){
      pointsInFarm <- pointsInFarm[!(pointsInFarm@data$Bout %in% whichToRemoveBout),]
      summaryBout <- summaryBout[!(summaryBout$Bout %in% whichToRemoveBout),]
    }
    
    save(pointsInFarm, file = paste("./Data/ObsPaths/ObsPaths_5m_",allWindFarms[[f]]$name,"_Causses",year[y],".Rdata", sep = ""))
  }
}





#__________________________________________________________________________________________________
##
#####  Estimation of the persistence based on observed data  - rediscretised at 5m ####
##
#__________________________________________________________________________________________________



## Initialize a data frame to save results
kappaEstimates.df <- matrix(nrow = length(selectedWindFarms), ncol=5)
kappaEstimates.df <- as.data.frame(kappaEstimates.df) # A data frame that will contain summary data of the filtration
colnames(kappaEstimates.df) <- c("farm","2019","2020","2021","mean")
year <- c("2019","2020","2021")
r = 0

for (f in selectedWindFarms){
  r = r + 1
  
  kappaEstimates.df[r,1] <- allWindFarms[[f]]$name
  
  for (y in 1:3){
    
    # Load data
    load(list.files("./Data/ObsPaths", paste("ObsPaths_5m_",allWindFarms[[f]]$name,"_Causses",year[y],sep="") , all.files = F, full.names = T))
    
    pointsInFarmBis <- spTransform(pointsInFarm, CRS("+proj=longlat +datum=WGS84"))
    
    # Initialize a empty vector
    turningAngles.v <- vector()
    
    # Loop on all paths
    for (l in 1:length(unique(pointsInFarmBis@data$Bout))){
      # Extract a paths
      testPath <- pointsInFarmBis[which(pointsInFarmBis$Bout == unique(pointsInFarmBis$Bout)[l]),]
      
      # Need to convert into a move object
      testPath.m <- as(testPath,"Move")
      
      # Calculate the turning angles of the considered path
      turningAngles.v <- c(turningAngles.v, turnAngleGc(testPath.m))
    }
    
    # Convert turning angles from degrees to rad
    radturningAngles.v <-  turningAngles.v*(pi/180) 
    
    # Estimation of Kappa for the VonMises distribution - persistence in simulations
    # k = 32.21 for S1 in 2019 (with rediscretisation at 5m)
    kappaEstimates.df[r,y+1] <- est.kappa(radturningAngles.v)
  }
  
  kappaEstimates.df[r,5] <- mean(c(kappaEstimates.df[r,2],
                                   kappaEstimates.df[r,3],
                                   kappaEstimates.df[r,4]))
  
  # Add variables types
  kappaEstimates.df[,2] <- as.numeric(kappaEstimates.df[,2])
  kappaEstimates.df[,3] <- as.numeric(kappaEstimates.df[,3])
  kappaEstimates.df[,4] <- as.numeric(kappaEstimates.df[,4])
  kappaEstimates.df[,5] <- as.numeric(kappaEstimates.df[,5])
 
}

#save(kappaEstimates.df, file = "./Outputs/Simulations/kappaEstimates_5m_OWF_Causses.Rdata")

