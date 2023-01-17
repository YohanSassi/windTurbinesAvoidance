#_______________________________________________________________________________________________________________
##
##      PROJECT: Avoidance of wind turbines by griffon vultures
##
##      GOAL OF THE SCRIPT: For a considered buffer size around a wind turbine, the script will rotate the wind
##                          farm around its barycenter from 0° to 350° and each time estimate the proportion of 
##                          fixes that are in the buffer compared to what is out. Those 36 values will be ranked 
##                          in increasing order and a p-value is estimated as the position of the observed 
##                          proportion. This will be done for each buffer size, each
##                          individuals and in each wind farm considered.
##
##
##      CREDIT: Yohan Sassi (yoh.sassi22@gmail.com) & Benjamin Robira (benjamin.robira@normalesup.org)
##
#_______________________________________________________________________________________________________________


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

#Datahandling
library(dplyr)
library(zoo)
library(data.table)
library(zoom)
library(scales)

#Parallelisation
library(parallel)
library(doParallel)

#Plot
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(lattice)
library(cowplot)

#Homemade functions
source("Functions/df_to_spatial_df_utm.R")#To convert long/lat to UTM

# Load wind farms and associated wind turbine lists
load("./Data/RawData/AllWindFarms_Causses.Rdata")
load("./Data/RawData/AllWindTurbines_Causses.Rdata")
load("./Data/RawData/Causses_WindFarmNames_Dict.Rdata")



#________________________________________________________________
##
#### Is there an avoidance ? Rotation analysis all farms       ####
##
##   - based on points -
##   - All individuals pooled -
#________________________________________________________________


# Initial parameters
valueSpacingRotation <- 10 # Step for rotation values
numberPointsRemovePath <- 10 # Minimum number of locations required to consider a path
rotationAngle.v <- seq(from=0, to=350, by=valueSpacingRotation) #Angle of wind farm rotation (in degrees)
bufferSize.v <- seq(from=50, to=1000, by=50) # Size of buffer that will be used around wind turbine (in meters)
nbCoresToSubstract = 2
timeThresholdIndependentMinutes = 0.5 # Time between two location to consider them independent
distanceSamplingInMeter = 50 # Spatial step between two rediscretized location
turblenceBufferInMeter = 15 # Turbulence buffer around turbine
wantWindFarms <- c(1,4,10,11) #Correspond to the selected wind farms
year <- c("2019", "2020", "2021")

combine = T
y = 1 # Should be changed depending on year you want to work on

# Loop the analysis on each selected farms
for(f in wantWindFarms){
  
  allFiles <- list.files("./Data/ObsPaths", paste("ObsPaths2_",allWindFarms[[f]]$name,"_Causses",sep="") , all.files = F, full.names = T)
  
  # Load the data sets corresponding to this wind farm
  if (combine == T){
    
    # Load sequentially the observed paths
    load(allFiles[1])
    allPointsInFarm <- pointsInFarm
    
    for (d in 2:length(allFiles)){
      load(allFiles[d])
      allPointsInFarm  <- rbind(allPointsInFarm, pointsInFarm)
    }
    
    pointsInFarm <- allPointsInFarm
    
  } else {
    # Load the observed paths- specific to the year y
    load(allFiles[which(grepl(year[y], allFiles, fixed = TRUE))])
  }
  
  # Initialize a matrix that will be filled by row thanks to the counter r
  results <- matrix(NA, nrow=720, ncol=7)
  r = 0
  
  ## Determining avoidance: Rotation procedure
  for(j in 1:length(rotationAngle.v)){
    
    ##Rotate the farm and the turbines
    #Farm
    farm.spatialPolygons_rotated <- maptools::elide(allWindFarms[[f]]$farm.sP, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)
    proj4string(farm.spatialPolygons_rotated) <- allWindFarms[[f]]$farm.sP@proj4string
    
    #Turbines
    dataCoordinatesTurbines.df.sp_rotated <- maptools::elide(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)
    proj4string(dataCoordinatesTurbines.df.sp_rotated) <- allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp@proj4string
    
    for(i in 1:length(bufferSize.v)){ 
      
      r = r + 1
      
      #Buffer turbines
      dataCoordinatesTurbines.df.sp_rotated_buffered <- gBuffer(dataCoordinatesTurbines.df.sp_rotated,  width=bufferSize.v[i], quadsegs = 100)
      
      
      # If still some points in farm
      if(nrow(pointsInFarm)>0){ 
        results[r,1:3] <- c(paste("allIndPooled_",year[1],"_",allWindFarms[[f]]$name,sep=""), rotationAngle.v[j], bufferSize.v[i])
        
        #What coordinates are in buffer & below height threshold
        whichPBelow <- pointsInFarm[which(pointsInFarm$aboveThresholdAltitude == 0),]
        
        #Points below
        pointsInBuffer <-  whichPBelow[!is.na(sp::over(whichPBelow, dataCoordinatesTurbines.df.sp_rotated_buffered)),]
        if(nrow(pointsInBuffer)>0){
          results[r,4:5] <- c(nrow(whichPBelow), nrow(pointsInBuffer)/nrow(whichPBelow)*100) #nb point in farm below threshold + %in turbines buffer
        }else{
          results[r,4:5] <- c(nrow(whichPBelow), 0)
        }
        
        #Visual check - to better vizualise set buffer size at a specific value (i)
        # plot(allWindFarms[[f]]$farmBuffer.sP, lty = "dotted") #Initial background buffer
        # plot(whichPBelow[,1:2], pch = 20, col = alpha('red',0.3), add = T)
        # plot(pointsInBuffer[,1:2], pch = 20, col = "green", add = T)
        # plot(allWindFarms[[f]]$farmBarycentre.sp, cex = 1, add = T)
        # plot(farm.spatialPolygons_rotated, add = TRUE)
        # plot(dataCoordinatesTurbines.df.sp_rotated, pch = 13, cex = 1.3 , add=TRUE)
        # plot(dataCoordinatesTurbines.df.sp_rotated_buffered, add = TRUE)
        # legend("top", paste(rotationAngle.v[j], "-", bufferSize.v[i], "-", as.character(round(nrow(pointsInBuffer)/nrow(whichPBelow)*100,digits = 3))))
        # Sys.sleep(0.5)
        
        #Points above
        whichPAbove <- pointsInFarm[which(pointsInFarm$aboveThresholdAltitude == 1),]
        
        pointsInBuffer <-  whichPAbove[!is.na(sp::over(whichPAbove, dataCoordinatesTurbines.df.sp_rotated_buffered)),]
        if(nrow(pointsInBuffer)>0){
          results[r,6:7] <- c(nrow(whichPAbove), nrow(pointsInBuffer)/nrow(whichPAbove)*100) #nb point in farm above threshold + %in turbines buffer
        }else{
          results[r,6:7] <- c(nrow(whichPAbove), 0)
        }
        
      }else{
        results[r,] <- c(paste("allIndPooled_",year[1],"_",allWindFarms[[f]]$name,sep=""), rotationAngle.v[j], bufferSize.v[i], NA, NA, NA, NA)
      }
    }
  }
  
  # Organise output data
  resultsRotation.df <- as.data.frame(results)
  colnames(resultsRotation.df) <- c("ID", 
                                    "rotationAngleDegree", 
                                    "bufferAroundTurbineMeter", 
                                    "numberOfPointsBelow", "percentPointsInBufferBelow",
                                    "numberOfPointsAbove", "percentPointsInBufferAbove")
  
  
  # Add variables type
  resultsRotation.df[,2] <- as.numeric(resultsRotation.df[,2])
  resultsRotation.df[,3] <- as.numeric(resultsRotation.df[,3])
  resultsRotation.df[,4] <- as.numeric(resultsRotation.df[,4])
  resultsRotation.df[,5] <- as.numeric(resultsRotation.df[,5])
  resultsRotation.df[,6] <- as.numeric(resultsRotation.df[,6])
  resultsRotation.df[,7] <- as.numeric(resultsRotation.df[,7])
  
  # Create new dataframe to store results of rotation analysis
  resultsRotationPvalueBelow.df <- data.frame(ID = character(), 
                                              bufferAroundTurbineMeter = integer(),
                                              posObsValue = integer(),
                                              pValue = numeric(),
                                              significance = character(),
                                              nbValues = integer(),
                                              nbValuesWithMoreThanZero = integer(),
                                              nbValuesPercentDifferentFromZero = integer(),
                                              AboveThreshold = integer())
  
  resultsRotationPvalueAbove.df <- data.frame(ID = character(), 
                                              bufferAroundTurbineMeter = integer(),
                                              posObsValue = integer(),
                                              pValue = numeric(),
                                              significance = character(),
                                              nbValues = integer(),
                                              nbValuesWithMoreThanZero = integer(),
                                              nbValuesPercentDifferentFromZero = integer(),
                                              AboveThreshold = integer())
  
  
  # Loop to create the p-values for below and above paths
  for(b in 1:length(bufferSize.v)){
    
    # Get observed points above & below altitude threshold
    numberPointsTrulyObservedInArea_below <- unique(resultsRotation.df$numberOfPointsBelow[which(resultsRotation.df$rotationAngleDegree == 0)])
    numberPointsTrulyObservedInArea_above <- unique(resultsRotation.df$numberOfPointsAbove[which(resultsRotation.df$rotationAngleDegree == 0)])
    
    #Filter out simulations with insufficient sampling (= less than 50% of points than truly observed)
    resultsRotation.df_rdc_below <- resultsRotation.df[which(as.numeric(resultsRotation.df$numberOfPointsBelow) > round(0.5 * numberPointsTrulyObservedInArea_below)),]
    resultsRotation.df_rdc_above <- resultsRotation.df[which(as.numeric(resultsRotation.df$numberOfPointsAbove) > round(0.5 * numberPointsTrulyObservedInArea_above)),]
    
    #Order within individual by percent of overlap for a given buffer
    orderedResultsPercentpts_below <- dplyr::arrange(resultsRotation.df_rdc_below[which(resultsRotation.df_rdc_below$bufferAroundTurbineMeter == bufferSize.v[b]),], percentPointsInBufferBelow)
    orderedResultsPercentpts_above <- dplyr::arrange(resultsRotation.df_rdc_above[which(resultsRotation.df_rdc_above$bufferAroundTurbineMeter == bufferSize.v[b]),], percentPointsInBufferAbove)
    
    
    if(0 %in% orderedResultsPercentpts_below$rotationAngleDegree){
      respValue <- which(orderedResultsPercentpts_below$rotationAngleDegree == 0)/nrow(orderedResultsPercentpts_below)
      resultsRotationPvalueBelow.df[nrow(resultsRotationPvalueBelow.df)+1,] <- c(paste("allIndPooled_",year[1],"_",allWindFarms[[f]]$name,sep=""),
                                                                                 bufferSize.v[b],
                                                                                 which(orderedResultsPercentpts_below$rotationAngleDegree == 0),
                                                                                 respValue,
                                                                                 ifelse(respValue < 0.05,"*","NS"),
                                                                                 nrow(orderedResultsPercentpts_below),
                                                                                 nrow(orderedResultsPercentpts_below[
                                                                                   orderedResultsPercentpts_below$percentPointsInBufferBelow > 0,
                                                                                 ]),
                                                                                 length(unique(orderedResultsPercentpts_below$percentPointsInBufferBelow)), 
                                                                                 0)
      
    } else {
      resultsRotationPvalueBelow.df[nrow(resultsRotationPvalueBelow.df)+1,] <- c(paste("allIndPooled_",year[1],"_",allWindFarms[[f]]$name,sep=""),
                                                                                 bufferSize.v[b],
                                                                                 NA,
                                                                                 NA,
                                                                                 NA,
                                                                                 nrow(orderedResultsPercentpts_below),
                                                                                 nrow(orderedResultsPercentpts_below[
                                                                                   orderedResultsPercentpts_below$percentPointsInBufferBelow > 0,
                                                                                 ]),
                                                                                 length(unique(orderedResultsPercentpts_below$percentPointsInBufferBelow)), 
                                                                                 0)
    }
    
    if(0 %in% orderedResultsPercentpts_above$rotationAngleDegree){
      respValue <- which(orderedResultsPercentpts_above$rotationAngleDegree == 0)/nrow(orderedResultsPercentpts_above)
      resultsRotationPvalueAbove.df[nrow(resultsRotationPvalueAbove.df)+1,] <- c(paste("allIndPooled_",year[1],"_",allWindFarms[[f]]$name,sep=""),
                                                                                 bufferSize.v[b],
                                                                                 which(orderedResultsPercentpts_above$rotationAngleDegree == 0),
                                                                                 respValue,
                                                                                 ifelse(respValue < 0.05,"*","NS"),
                                                                                 nrow(orderedResultsPercentpts_above),
                                                                                 nrow(orderedResultsPercentpts_above[
                                                                                   orderedResultsPercentpts_above$percentPointsInBufferAbove > 0,]),
                                                                                 length(unique(orderedResultsPercentpts_above$percentPointsInBufferAbove)),
                                                                                 1)
    } else {
      resultsRotationPvalueAbove.df[nrow(resultsRotationPvalueAbove.df)+1,] <- c(paste("allIndPooled_",year[1],"_",allWindFarms[[f]]$name,sep=""),
                                                                                 bufferSize.v[b],
                                                                                 NA,
                                                                                 NA,
                                                                                 NA,
                                                                                 nrow(orderedResultsPercentpts_above),
                                                                                 nrow(orderedResultsPercentpts_above[
                                                                                   orderedResultsPercentpts_above$percentPointsInBufferAbove > 0,]),
                                                                                 length(unique(orderedResultsPercentpts_above$percentPointsInBufferAbove)),
                                                                                 1)
    }
  }
  
  
  # Add variables type
  resultsRotationPvalueBelow.df[,2] <- as.numeric(resultsRotationPvalueBelow.df[,2])
  resultsRotationPvalueBelow.df[,3] <- as.numeric(resultsRotationPvalueBelow.df[,3])
  resultsRotationPvalueBelow.df[,4] <- as.numeric(resultsRotationPvalueBelow.df[,4])
  resultsRotationPvalueBelow.df[,6] <- as.numeric(resultsRotationPvalueBelow.df[,6])
  resultsRotationPvalueBelow.df[,7] <- as.numeric(resultsRotationPvalueBelow.df[,7])
  resultsRotationPvalueBelow.df[,8] <- as.numeric(resultsRotationPvalueBelow.df[,8])
  resultsRotationPvalueBelow.df[,9] <- as.numeric(resultsRotationPvalueBelow.df[,9])
  
  resultsRotationPvalueAbove.df[,2] <- as.numeric(resultsRotationPvalueAbove.df[,2])
  resultsRotationPvalueAbove.df[,3] <- as.numeric(resultsRotationPvalueAbove.df[,3])
  resultsRotationPvalueAbove.df[,4] <- as.numeric(resultsRotationPvalueAbove.df[,4])
  resultsRotationPvalueAbove.df[,6] <- as.numeric(resultsRotationPvalueAbove.df[,6])
  resultsRotationPvalueAbove.df[,7] <- as.numeric(resultsRotationPvalueAbove.df[,7])
  resultsRotationPvalueAbove.df[,8] <- as.numeric(resultsRotationPvalueAbove.df[,8])
  resultsRotationPvalueAbove.df[,9] <- as.numeric(resultsRotationPvalueAbove.df[,9])
  
  # Save files
  if (combine == T){
    save(timeThresholdIndependentMinutes,
         distanceSamplingInMeter,
         turblenceBufferInMeter,
         resultsRotation.df,resultsRotationPvalueBelow.df,
         resultsRotationPvalueAbove.df,
         file = paste("./Outputs/AnalysisResults/RotationAnalysisResult2_allyears_",allWindFarms[[f]]$name,".Rdata", sep=""))

  } else {
    save(timeThresholdIndependentMinutes,
         distanceSamplingInMeter,
         turblenceBufferInMeter,
         resultsRotation.df,resultsRotationPvalueBelow.df,
         resultsRotationPvalueAbove.df,
         file = paste("./Outputs/AnalysisResults/RotationAnalysisResult2_", year[y],"_",allWindFarms[[f]]$name,".Rdata", sep=""))
    }
}



