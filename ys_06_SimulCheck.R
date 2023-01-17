#________________________________________________________________________________________________________________
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
#________________________________________________________________________________________________________________

rm(list = ls())

#Spatial
library(raster)
library(rgeos)
library(CircularDDM)
library(geosphere)
library(sp)
library(rgdal)
library(move)
library(CircStats)

#plot
library(zoom)
library(ggplot2)
library(ggforce)

# Progress bar
library(svMisc)
library(tictoc)


# Load needed files
load("./Data/RawData/AllWindFarms_Causses.Rdata")
load("./Data/RawData/AllWindTurbines_Causses.Rdata")
source("Functions/SimulateAvoidanceFunction.R") #Functon to simulate paths




#______________________________________________________________________
##
#### Agent centered function to simulate avoidance      ####
##
#______________________________________________________________________

#Summary:
# We give a range of perception to the agent (i.e. distance at which avoidance will occur: "distanceAvoidance") and a deviation angle ("directionStepAvoidance").
# Once a turbine is in the range of perception, the agent move with such a deviation angle, orientated such as to avoid the turbine 
# (i.e. if the turbine is on the right respectively to the heading of the agent, it will move left). Then, after deviation, the agent keeps moving in the new 
# direction, unless it comes closer to a new turbine and tries avoiding it too. This for "progressiveAvoidance=FALSE" otherwise it keeps turning with same angle 
# until the distance to the closest turbine is no longer decreasing. The ballistic movement (when no turbine perceived or avoidance performed) is obtained by using a von mises distribution.
# We can adjust the distance between two successive simulated points in meters ("stepLength") as well as the linearity of movement ("persistence").
# The simulation stops when the agent is out of the map.



simulateAvoidance <- function(
  mapSizexMax,
  mapSizexMin,
  mapSizeyMax,
  mapSizeyMin,
  distanceAvoidance,
  stepLength,
  persistence,
  directionStepAvoidance,
  Nturbines,
  locationsTurbines.df,
  progressiveAvoidance=FALSE
){
  #Start on the side of the map
  startPoint <- c(ifelse(rbinom(1,1,0.5) == 0, mapSizexMin, mapSizexMax), ifelse(rbinom(1,1,0.5) == 0, mapSizeyMin, mapSizeyMax))#One of the corner
  if((startPoint[1] == mapSizexMax & startPoint[2] == mapSizeyMax) | (startPoint[1] == mapSizexMin & startPoint[2] == mapSizeyMin) ){
    startPoint[1] = abs(startPoint[1] - runif(1, mapSizexMin, mapSizexMax)) + mapSizexMin
  } else{
    startPoint[2] = abs(startPoint[2] - runif(1, mapSizeyMin, mapSizeyMax)) + mapSizeyMin
  }
  
  distanceToTurbineSquared.v <- apply(locationsTurbines.df, 1, function(x){
    sum((x - startPoint)**2)
  }
  )
  if(min(distanceToTurbineSquared.v) <= distanceAvoidance*distanceAvoidance){
    return(NA)
  }
  #Choose direction
  #Changed not to have the exact opposite now since it caused issues:
  targetPoint <- if(startPoint[1] == mapSizexMin | startPoint[1] == mapSizexMax){
    c(mapSizexMax - startPoint[1] + mapSizexMin, runif(1, mapSizeyMin, mapSizeyMax))
  }else{
    c(runif(1, mapSizexMin, mapSizexMax), mapSizeyMax - startPoint[2] + mapSizeyMin)
  }
  
  refDirection = atan2(targetPoint[2] - startPoint[2], targetPoint[1] - startPoint[1])
  refCurrentDirection <- refDirection

  location = startPoint#Fake point to start
  
  track.df <- matrix(NA, ncol=2, nrow=max(c(mapSizexMax-mapSizexMin, mapSizeyMax - mapSizeyMin)/stepLength*5))
  track.df[1,] <- startPoint
  
  directionAvoidanceChosen = NA
  minDistanceToTurbines = 100000000000000000000000000000000000000000000000000000000000
  
  turbineToAvoid <- NA
  turbineToAvoidPrevious <- NA
  
  while(location[1] >= mapSizexMin & location[1] <= mapSizexMax &
        location[2] >= mapSizeyMin & location[2] <= mapSizeyMax){#Stop if went out of map
    
    #Distance to closest wind turbine
    minDistanceToTurbinesPrevious <- minDistanceToTurbines
    distanceToTurbineSquared.v <- apply(locationsTurbines.df, 1, function(x){
      sum((x - location)**2)
    }
    )
    minDistanceToTurbines <- min(distanceToTurbineSquared.v)
    ##ID turbine to avoid
    
    turbineToAvoidPrevious <- turbineToAvoid
    turbineToAvoid <- which(distanceToTurbineSquared.v == min(distanceToTurbineSquared.v))
    #print(turbineToAvoid)
    
    if(!is.na(turbineToAvoidPrevious) & turbineToAvoidPrevious != turbineToAvoid#If new turbine the closest
    ){
      directionAvoidanceChosen = NA
    }
    
    #If distance >= to distance avoidance, move rather "straight"
    library(CircularDDM)
    if(min(distanceToTurbineSquared.v) >= distanceAvoidance*distanceAvoidance){
      directionStep = CircularDDM::rvm(1, k = persistence, mu = refDirection)
      location <- c(
        location[1] + stepLength*cos(directionStep),
        location[2] + stepLength*sin(directionStep)
      )
    }else{#Otherwise, start avoiding wind turbine, more or less sharply
      if(is.na(directionAvoidanceChosen)){
        directionAvoidanceChosen = "YES"
        whichTurbineToAvoid <- which(distanceToTurbineSquared.v == min(distanceToTurbineSquared.v))
        
        if(which(is.na(track.df[,1]))[1] > 2){#Calculate movement current heading
          previousLoc <- as.numeric(as.character(track.df[which(is.na(track.df[,1]))[1]-2,1:2]))
          refCurrentDirection <- atan2(location[2] - previousLoc[2], location[1] - previousLoc[1])
        }else{
          refCurrentDirection <- refDirection #If did not move enough, keep initial direction
        }
        
        #Rotation centered on previous location
        #Create previous as if the initial perfect heading was truly maintaine
        fakePreviousLoc <-
          c(
            location[1] - stepLength*cos(refDirection),
            location[2] - stepLength*sin(refDirection)
          )
        rotateCenterFunction <- function(centerRotation, refLocRotation, coordinatesToTransform){
          refLocRotation.centered <- refLocRotation - centerRotation
          angleRotation <- atan2(refLocRotation.centered[2], 
                                 refLocRotation.centered[1]) %% (2*pi)
          coordinatesToTransform.centered <- t(apply(coordinatesToTransform, 1, function(x) x - centerRotation))
          coordinatesToTransform.centered.rotated <-
            cbind(
              coordinatesToTransform.centered[,1]*cos(angleRotation) + coordinatesToTransform.centered[,2]*sin(angleRotation),
              -coordinatesToTransform.centered[,1]*sin(angleRotation) + coordinatesToTransform.centered[,2]*cos(angleRotation)
            )
          return(as.data.frame(coordinatesToTransform.centered.rotated))
        }
        #For location
        location.rotated <- as.vector(rotateCenterFunction(fakePreviousLoc, location, as.data.frame(t(location)))) 
        
        #For turbines
        newCoordinatesTurbineToAvoid <- rotateCenterFunction(fakePreviousLoc, location, as.data.frame(locationsTurbines.df))

        side <- ifelse(
          #If turbine is spotted ahead of movement
          (newCoordinatesTurbineToAvoid[whichTurbineToAvoid, 2] < 0),
          1,#if turbine on the right, turn left // Accounting for movement direction
          -1#if turbine on the left, turn right
        )
        #Change the ref direction
        refDirection <- refDirection %% (2*pi) + side*directionStepAvoidance*pi/180
        
        directionStep <- refDirection %% (2*pi)
        
        #Avoidance
        location <- c(
          location[1] + stepLength*cos(directionStep),
          location[2] + stepLength*sin(directionStep)
        )
      }else{
        
        if(progressiveAvoidance){
          if(minDistanceToTurbinesPrevious > minDistanceToTurbines#still getting closer to the turbine
          ){
            #Change the ref direction
            refDirection <- refDirection + side*directionStepAvoidance*pi/180
            
            directionStep <- refDirection %% (2*pi)
            
            #Avoidance
            location <- c(
              location[1] + stepLength*cos(directionStep),
              location[2] + stepLength*sin(directionStep)
            )
          }else{
            directionStep = CircularDDM::rvm(1, k = persistence, mu = refDirection)#Otherwise, move straight
            location <- c(
              location[1] + stepLength*cos(directionStep),
              location[2] + stepLength*sin(directionStep)
            )
          }
        }else{
          directionStep = CircularDDM::rvm(1, k = persistence, mu = refDirection)#Otherwise, move straight
          location <- c(
            location[1] + stepLength*cos(directionStep),
            location[2] + stepLength*sin(directionStep)
          )
        }
      }
    }
    #Record track
    addRow <- which(is.na(track.df[,1]))[1]
    if(is.na(addRow)){
      break
    }
    track.df[addRow,] <- location
  }
  track.df <- track.df[!is.na(track.df[,1]),]
  return(track.df)
}




#______________________________________________________________________
##
#### Simulate paths with custom parameters      ####
##
#______________________________________________________________________

# Load the kappa estimates on the selected wind farms
load("./Outputs/Simulations/kappaEstimates_5m_OWF_Causses.Rdata") 

#Parameters
distanceAvoidance = seq(from=50, to=1000, by=50) # Distance minimal at which it should avoid a turbine (in meters)
stepLength = 5 #Length between step in path simulations (in meters), should be small to simulate continuous behaviour
persistence = mean(kappaEstimates.df$mean) #Variance of a von mises distribution - control for linearity of movement
directionStepAvoidance = seq(from=0, to=14, by=1) #Turning angle in degree when the avoidance area is reached - adapted to stepLength
nbPaths <- 1000 # Number of paths simulated
selectedWindFarms = 4 #c(1,4,10,11) #Selected wind farms
valueSpacingRotation <- 10 # Step for rotation values
rotationAngle.v <- seq(from=0, to=350, by=valueSpacingRotation) #Angle of wind farm rotation (in degrees)
bufferSize.v <- seq(from=50, to=1000, by=50) # Size of buffer that will be used around wind turbine (in meters)

# For hand simulations
f = 4 # Select the farm you want to work on
a = 2 # Control for avoidance degree
b = 6 # Control the distance at which it should start avoiding the wind turbine


# Simulations
dataSimulated <-
  lapply(1:nbPaths, function(x){
    simulateAvoidance(
      mapSizexMax = extent(allWindFarms[[f]]$farm.sP)@xmax,
      mapSizexMin = extent(allWindFarms[[f]]$farm.sP)@xmin,
      mapSizeyMax = extent(allWindFarms[[f]]$farm.sP)@ymax,
      mapSizeyMin = extent(allWindFarms[[f]]$farm.sP)@ymin,
      distanceAvoidance = distanceAvoidance[b],
      stepLength = stepLength,
      persistence = persistence,
      directionStepAvoidance = directionStepAvoidance[a],
      Nturbines = Nturbines,
      locationsTurbines.df = allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp@coords,
      progressiveAvoidance=TRUE
    )
  }
  )

#______________________________________________________________________
##
#### Rotation analysis on simulated tracks     ####
##
#______________________________________________________________________

# Initialize a matrix that will be filled by row thanks to the counter r
results <- matrix(NA, nrow=720, ncol=5)
r = 0

## Prepare the simulated data to the needed format
for (i in 1:length(dataSimulated)){
  dataSimulated[[i]] <- cbind(dataSimulated[[i]], rep(0,nrow(dataSimulated[[i]])))
  dataSimulated[[i]] <- cbind(dataSimulated[[i]], rep(i,nrow(dataSimulated[[i]])))
}

pointsInFarmCheck <- as.data.frame(do.call(rbind,lapply(dataSimulated,function(x) x[,1:4])))
colnames(pointsInFarmCheck) <- c("x","y","aboveThresholdAltitude","bout")
pointsInFarmCheck <- SpatialPointsDataFrame(pointsInFarmCheck[,1:2], data = pointsInFarmCheck, proj4string = allWindFarms[[f]]$farm.sP@proj4string)
#plot(pointsInFarmCheck[,1:2], pch = 20, col = alpha('red',0.3), add = T)


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
    if(nrow(pointsInFarmCheck)>0){ #Change for pointsInFarm is updated !!!!!
      results[r,1:3] <- c(paste("allIndPooled_simulated_",allWindFarms[[f]]$name,sep=""), rotationAngle.v[j], bufferSize.v[i])
      
      #What coordinates are in buffer & below height threshold
      whichPBelow <- pointsInFarmCheck[which(pointsInFarmCheck$aboveThresholdAltitude == 0),] #Change for pointsInFarm is updated !!!!!
      
      #Points below
      pointsInBuffer <-  whichPBelow[!is.na(sp::over(whichPBelow, dataCoordinatesTurbines.df.sp_rotated_buffered)),]
      if(nrow(pointsInBuffer)>0){
        results[r,4:5] <- c(nrow(whichPBelow), nrow(pointsInBuffer)/nrow(whichPBelow)*100) #nb point in farm below threshold + %in turbines buffer
      }else{
        results[r,4:5] <- c(nrow(whichPBelow), 0)
      }
       
      
      #Plot the situation
      #  plot(allWindFarms[[f]]$farmBuffer.sP, lty = "dotted") #Initial background buffer
      #  plot(pointsInFarmCheck[,1:2], pch = 20, col = alpha('red',0.3), add = T)
      #  plot(pointsInBuffer[,1:2], pch = 20, col = alpha('green',0.3), add = T)
      #  plot(farm.spatialPolygons_rotated, add = TRUE)
      #  plot(dataCoordinatesTurbines.df.sp_rotated, pch = 13, cex = 1.3 , add=TRUE)
      #  plot(dataCoordinatesTurbines.df.sp_rotated_buffered, add = TRUE)
      #  plot(allWindFarms[[f]]$farmBarycentre.sp, pch = 3, add = TRUE)
      #  legend("top", paste(rotationAngle.v[j], "-", bufferSize.v[i], "-", as.character(round(nrow(pointsInBuffer)/nrow(whichPBelow)*100,digits = 3))))
      # 
      # # plot(allWindFarms[[f]]$farmBuffer.sP, lty = "dotted") #Initial background buffer
      # plot(allWindFarms[[f]]$farm.sP, add = T) #Initial background buffer
      # plot(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, pch = 13, cex = 2 , add=TRUE)
      # 
       # resfactor = 3
       # dev.copy(png, "./Outputs/Figures/OptiSimulS2.png" ,res = 72*resfactor, height=640*resfactor, width=640*resfactor)
       # dev.off()

      #  Sys.sleep(1)
      #  
      # # Save plot
      #  resfactor = 3
      #  dev.copy(png, paste("./Outputs/Simulations/VisualCheck/RotaVisu_dir",
      #                      directionStepAvoidance[a],
      #                      "_dist",
      #                      distanceAvoidance[b],
      #                      "_buff",
      #                      bufferSize.v[i],
      #                      "_rot",
      #                      rotationAngle.v[j],
      #                      ".png", sep=""),
      #           res = 72*resfactor, height=640*resfactor, width=640*resfactor)
      #  dev.off()
      
    }else{
      results[r,] <- c(paste("allIndPooled_simulated_",allWindFarms[[f]]$name,sep=""), rotationAngle.v[j], bufferSize.v[i], NA, NA)
    }
  }
}

# Organize output data
resultsRotation.df <- as.data.frame(results)
colnames(resultsRotation.df) <- c("ID", 
                                  "rotationAngleDegree", 
                                  "bufferAroundTurbineMeter", 
                                  "numberOfPointsBelow", "percentPointsInBufferBelow")

# Add variables type
resultsRotation.df[,2] <- as.numeric(resultsRotation.df[,2])
resultsRotation.df[,3] <- as.numeric(resultsRotation.df[,3])
resultsRotation.df[,4] <- as.numeric(resultsRotation.df[,4])
resultsRotation.df[,5] <- as.numeric(resultsRotation.df[,5])


resultsRotationPvalueBelow.df <- data.frame(ID = character(), 
                                            bufferAroundTurbineMeter = integer(),
                                            nbPointsObs = integer(),
                                            posObsValue = integer(),
                                            pValue = numeric(),
                                            significance = character(),
                                            nbValues = integer(),
                                            nbValuesWithMoreThanZero = integer(),
                                            nbValuesPercentDifferentFromZero = integer(),
                                            distanceAvoidance = integer()
)


# Loop to create the p-values for below and above paths
for(k in 1:length(bufferSize.v)){
  
  # Get observed points below altitude threshold
  numberPointsTrulyObservedInArea_below <- unique(resultsRotation.df$numberOfPointsBelow[which(resultsRotation.df$rotationAngleDegree == 0)])
  
  #Filter out simulations with insufficient sampling (= less than 50% of points than truly observed)
  resultsRotation.df_rdc_below <- resultsRotation.df[which(resultsRotation.df$numberOfPointsBelow > round(0.5 * numberPointsTrulyObservedInArea_below)),]
  
  #Order within individual by percent of overlap for a given buffer
  orderedResultsPercentpts_below <- dplyr::arrange(resultsRotation.df_rdc_below[which(resultsRotation.df_rdc_below$bufferAroundTurbineMeter == bufferSize.v[k]),], percentPointsInBufferBelow)
  
  
  if(0 %in% orderedResultsPercentpts_below$rotationAngleDegree){
    respValue <- which(orderedResultsPercentpts_below$rotationAngleDegree == 0)/nrow(orderedResultsPercentpts_below)
    resultsRotationPvalueBelow.df[nrow(resultsRotationPvalueBelow.df)+1,] <- c(paste("Simul_dir",directionStepAvoidance[a],"_dist",distanceAvoidance[b],sep=""), 
                                                                               bufferSize.v[k],
                                                                               numberPointsTrulyObservedInArea_below,
                                                                               which(orderedResultsPercentpts_below$rotationAngleDegree == 0),
                                                                               respValue,
                                                                               ifelse(respValue < 0.05,"*","NS"),
                                                                               nrow(orderedResultsPercentpts_below),
                                                                               nrow(orderedResultsPercentpts_below[
                                                                                 orderedResultsPercentpts_below$percentPointsInBufferBelow > 0,
                                                                               ]),
                                                                               length(unique(orderedResultsPercentpts_below$percentPointsInBufferBelow)),
                                                                               distanceAvoidance[b]
    )
    
  } else {
    resultsRotationPvalueBelow.df[nrow(resultsRotationPvalueBelow.df)+1,] <- c(paste("Simul_dir",directionStepAvoidance[a],"_dist",distanceAvoidance[b],sep=""),
                                                                               bufferSize.v[k],
                                                                               NA,
                                                                               NA,
                                                                               NA,
                                                                               NA,
                                                                               nrow(orderedResultsPercentpts_below),
                                                                               nrow(orderedResultsPercentpts_below[
                                                                                 orderedResultsPercentpts_below$percentPointsInBufferBelow > 0,
                                                                               ]),
                                                                               length(unique(orderedResultsPercentpts_below$percentPointsInBufferBelow)),
                                                                               distanceAvoidance[b]
    )
  }
}

#Add variable types
resultsRotationPvalueBelow.df[,2] <- as.numeric(resultsRotationPvalueBelow.df[,2])
resultsRotationPvalueBelow.df[,3] <- as.numeric(resultsRotationPvalueBelow.df[,3])
resultsRotationPvalueBelow.df[,4] <- as.numeric(resultsRotationPvalueBelow.df[,4])
resultsRotationPvalueBelow.df[,5] <- as.numeric(resultsRotationPvalueBelow.df[,5])
resultsRotationPvalueBelow.df[,7] <- as.numeric(resultsRotationPvalueBelow.df[,7])
resultsRotationPvalueBelow.df[,8] <- as.numeric(resultsRotationPvalueBelow.df[,8])
resultsRotationPvalueBelow.df[,9] <- as.numeric(resultsRotationPvalueBelow.df[,9])
resultsRotationPvalueBelow.df[,10] <- as.numeric(resultsRotationPvalueBelow.df[,10])

#Save file
# save(pointsInFarmCheck,
#      resultsRotation.df,
#      resultsRotationPvalueBelow.df,
#      file = paste("./Outputs/Simulations/RotaRes_S2/ABM_RotaRes_dir",directionStepAvoidance[a],"_dist",distanceAvoidance[b],".Rdata", sep=""))



# Plot of the results for simulations
resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

ggplot(resBelow.df, aes(x = as.integer(posObsValue), y = as.integer(bufferAroundTurbineMeter))) + 
  coord_cartesian(xlim = c(1, 36), ylim = c(0, 1010)) +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_y_continuous("Buffer size", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle(paste(allWindFarms[[f]]$name,"-","Simulations","
Alpha = ",directionStepAvoidance[a],", dist = ",distanceAvoidance[b])) +
  theme_light() +
  geom_rect(aes(xmin = 0 + 0.5, xmax = 2 - 0.5, ymin = 0 - 0.5, ymax = 1000 + 5, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")










#__________________________________________________________________________________________________
##
##### Automated rotation analysis with simulated tracks on different wind farms     ####
##
#__________________________________________________________________________________________________


# Comments:
# Maximum angle a vulture can adopt when flying (emergency situations excluded) - at the beginning of the thermal they are under the most of constrains
# In Williams et al. 2018 they found that the minimum circle radius a vulture can describe is 20m -> in 1m the max is 2.864873 °
# I.e. 14.32437 °/5m


tic()

# Load the kappa estimates on the selected wind farms
load("./Outputs/Simulations/kappaEstimates_5m_OWF_Causses.Rdata") 

#Parameters
distanceAvoidance = seq(from=50, to=1000, by=50) # Distance minimal at which it should avoid a turbine (in meters)
stepLength = 5 #Length between step in path simulations (in meters), should be small to simulate continuous behaviour
persistence = mean(kappaEstimates.df$mean) #Variance of a von mises distribution - control for linearity of movement
directionStepAvoidance = seq(from=0, to=14, by=1) #Turning angle in degree when the avoidance area is reached - adapted to stepLength
nbPaths <- 1000 # Number of paths simulated
selectedWindFarms = c(1,4,10,11) #Selected wind farms
valueSpacingRotation <- 10 # Step for rotation values
rotationAngle.v <- seq(from=0, to=350, by=valueSpacingRotation) #Angle of wind farm rotation (in degrees)
bufferSize.v <- seq(from=50, to=1000, by=50) # Size of buffer that will be used around wind turbine (in meters)
selectedAngles <- c(1,2,6)


for (f in selectedWindFarms ){
  
  # Create a directory corresponding to the wind farm to store simulations' results
  dirPath <- paste("./Outputs/Simulations/RotaRes_",allWindFarms[[f]]$name, sep ="")
  # dir.create(dirPath)
  
  
  for (a in 6){   #selectedAngles #1:length(directionStepAvoidance)
    for (b in 1){ #9 #1:length(distanceAvoidance)
      
      # Add a seed to generate always the same random starting points for the simulations
      #set.seed(12345)
      
      # Simulations of paths
      dataSimulated <-
        lapply(1:nbPaths, function(x){
          simulateAvoidance(
            mapSizexMax = extent(allWindFarms[[f]]$farm.sP)@xmax,
            mapSizexMin = extent(allWindFarms[[f]]$farm.sP)@xmin,
            mapSizeyMax = extent(allWindFarms[[f]]$farm.sP)@ymax,
            mapSizeyMin = extent(allWindFarms[[f]]$farm.sP)@ymin,
            distanceAvoidance = distanceAvoidance[b],
            stepLength = stepLength,
            persistence = persistence,
            directionStepAvoidance = directionStepAvoidance[a],
            Nturbines = Nturbines,
            locationsTurbines.df = allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp@coords,
            progressiveAvoidance=TRUE
          )
        }
        )
      
      
      ## Prepare the simulated data to the needed format
      for (c in 1:length(dataSimulated)){
        dataSimulated[[c]] <- cbind(dataSimulated[[c]], rep(0,nrow(dataSimulated[[c]])))
        dataSimulated[[c]] <- cbind(dataSimulated[[c]], rep(c,nrow(dataSimulated[[c]])))
      }
      
      pointsInFarmCheck <- as.data.frame(do.call(rbind,lapply(dataSimulated,function(x) x[,1:4])))
      colnames(pointsInFarmCheck) <- c("x","y","aboveThresholdAltitude","bout")
      pointsInFarmCheck <- SpatialPointsDataFrame(pointsInFarmCheck[,1:2], data = pointsInFarmCheck, proj4string = allWindFarms[[f]]$farm.sP@proj4string)
      
      
      # Initialize a matrix that will be filled by row thanks to the counter r
      results <- matrix(NA, nrow=720, ncol=5)
      r = 0
      
      # Annoncement to know where the code is
      print(paste("RotaSimul_dir",directionStepAvoidance[a],"_dist",distanceAvoidance[b],sep=""))
      
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
          if(nrow(pointsInFarmCheck)>0){ 
            results[r,1:3] <- c(paste("allIndPooled_simulated_",allWindFarms[[f]]$name,sep=""), rotationAngle.v[j], bufferSize.v[i])
            
            #What coordinates are in buffer & below height threshold
            whichPBelow <- pointsInFarmCheck[which(pointsInFarmCheck$aboveThresholdAltitude == 0),]
            
            #Points below
            pointsInBuffer <-  whichPBelow[!is.na(sp::over(whichPBelow, dataCoordinatesTurbines.df.sp_rotated_buffered)),]
            if(nrow(pointsInBuffer)>0){
              results[r,4:5] <- c(nrow(whichPBelow), nrow(pointsInBuffer)/nrow(whichPBelow)*100) #nb point in farm below threshold + %in turbines buffer
            }else{
              results[r,4:5] <- c(nrow(whichPBelow), 0)
            }
            
          }else{
            results[r,] <- c(paste("allIndPooled_simulated_",allWindFarms[[f]]$name,sep=""), rotationAngle.v[j], bufferSize.v[i], NA, NA)
          }
        }
        
        progress(j*2.8)
      }
      
      # Organize output data
      resultsRotation.df <- as.data.frame(results)
      colnames(resultsRotation.df) <- c("ID", 
                                        "rotationAngleDegree", 
                                        "bufferAroundTurbineMeter", 
                                        "numberOfPointsBelow", "percentPointsInBufferBelow")
      
      # Add variables type
      resultsRotation.df[,2] <- as.numeric(resultsRotation.df[,2])
      resultsRotation.df[,3] <- as.numeric(resultsRotation.df[,3])
      resultsRotation.df[,4] <- as.numeric(resultsRotation.df[,4])
      resultsRotation.df[,5] <- as.numeric(resultsRotation.df[,5])

      # Create new dataframe to store results of rotation analysis
      resultsRotationPvalueBelow.df <- data.frame(ID = character(), 
                                                  bufferAroundTurbineMeter = integer(),
                                                  nbPointsObs = integer(),
                                                  posObsValue = integer(),
                                                  pValue = numeric(),
                                                  significance = character(),
                                                  nbValues = integer(),
                                                  nbValuesWithMoreThanZero = integer(),
                                                  nbValuesPercentDifferentFromZero = integer(),
                                                  distanceAvoidance = integer()
      )
      
      
      # Loop to create the p-values for below and above paths
      for(k in 1:length(bufferSize.v)){
        
        # Get observed points below altitude threshold
        numberPointsTrulyObservedInArea_below <- unique(resultsRotation.df$numberOfPointsBelow[which(resultsRotation.df$rotationAngleDegree == 0)])
        
        #Filter out simulations with insufficient sampling (= less than 50% of points than truly observed)
        resultsRotation.df_rdc_below <- resultsRotation.df[which(as.numeric(resultsRotation.df$numberOfPointsBelow) > round(0.5 * numberPointsTrulyObservedInArea_below)),]
        
        #Order within individual by percent of overlap for a given buffer
        orderedResultsPercentpts_below <- dplyr::arrange(resultsRotation.df_rdc_below[which(resultsRotation.df_rdc_below$bufferAroundTurbineMeter == bufferSize.v[k]),], percentPointsInBufferBelow)
        
        
        if(0 %in% orderedResultsPercentpts_below$rotationAngleDegree){
          respValue <- which(orderedResultsPercentpts_below$rotationAngleDegree == 0)/nrow(orderedResultsPercentpts_below)
          resultsRotationPvalueBelow.df[nrow(resultsRotationPvalueBelow.df)+1,] <- c(paste("Simul_dir",directionStepAvoidance[a],"_dist",distanceAvoidance[b],sep=""), 
                                                                                     bufferSize.v[k],
                                                                                     numberPointsTrulyObservedInArea_below,
                                                                                     which(orderedResultsPercentpts_below$rotationAngleDegree == 0),
                                                                                     respValue,
                                                                                     ifelse(respValue < 0.05,"*","NS"),
                                                                                     nrow(orderedResultsPercentpts_below),
                                                                                     nrow(orderedResultsPercentpts_below[
                                                                                       orderedResultsPercentpts_below$percentPointsInBufferBelow > 0,
                                                                                     ]),
                                                                                     length(unique(orderedResultsPercentpts_below$percentPointsInBufferBelow)),
                                                                                     distanceAvoidance[b]
          )
          
        } else {
          resultsRotationPvalueBelow.df[nrow(resultsRotationPvalueBelow.df)+1,] <- c(paste("Simul_dir",directionStepAvoidance[a],"_dist",distanceAvoidance[b],sep=""),
                                                                                     bufferSize.v[k],
                                                                                     NA,
                                                                                     NA,
                                                                                     NA,
                                                                                     NA,
                                                                                     nrow(orderedResultsPercentpts_below),
                                                                                     nrow(orderedResultsPercentpts_below[
                                                                                       orderedResultsPercentpts_below$percentPointsInBufferBelow > 0,
                                                                                     ]),
                                                                                     length(unique(orderedResultsPercentpts_below$percentPointsInBufferBelow)),
                                                                                     distanceAvoidance[b]
          )
        }
      }
      
      #Add variable types
      resultsRotationPvalueBelow.df[,2] <- as.numeric(resultsRotationPvalueBelow.df[,2])
      resultsRotationPvalueBelow.df[,3] <- as.numeric(resultsRotationPvalueBelow.df[,3])
      resultsRotationPvalueBelow.df[,4] <- as.numeric(resultsRotationPvalueBelow.df[,4])
      resultsRotationPvalueBelow.df[,5] <- as.numeric(resultsRotationPvalueBelow.df[,5])
      resultsRotationPvalueBelow.df[,7] <- as.numeric(resultsRotationPvalueBelow.df[,7])
      resultsRotationPvalueBelow.df[,8] <- as.numeric(resultsRotationPvalueBelow.df[,8])
      resultsRotationPvalueBelow.df[,9] <- as.numeric(resultsRotationPvalueBelow.df[,9])
      resultsRotationPvalueBelow.df[,10] <- as.numeric(resultsRotationPvalueBelow.df[,10])
      
      #Save file
      save(pointsInFarmCheck,
           resultsRotation.df,
           resultsRotationPvalueBelow.df,
           file = paste(dirPath,"/ABM_RotaRes_dir",directionStepAvoidance[a],"_dist",distanceAvoidance[b],".Rdata", sep=""))
    }
  }
}
toc()

