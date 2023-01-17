#________________________________________________________________________________________________________________
##
##      PROJECT: Avoidance of wind turbines by griffon vultures
##
##      GOAL OF THE SCRIPT:  This script aims to fit the best agent-based model to the data
##
##
##      CREDIT: Yohan Sassi (yoh.sassi22@gmail.com) & Benjamin Robira (benjamin.robira@normalesup.org)
##
#________________________________________________________________________________________________________________


# Load needed files, functions & package
library(ggplot2)
library(viridis)
library(tidyr)
library(rgeos)
library(sp)
library(raster)
library(tidyverse)
library(tictoc)
library(parallel)
library(doParallel)

load("./Data/RawData/AllWindFarms_Causses.Rdata")
load("./Data/RawData/AllWindTurbines_Causses.Rdata")
load("./Outputs/Simulations/kappaEstimates_5m_OWF_Causses.Rdata") 


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
#### Function to optimize parameters on site      ####
##
#______________________________________________________________________

LLsite <- function(par){
  avoidanceAngle = par[1]
  distanceStartAvoidance = par[2]
  
  #Run agent-based model
  dataSimulated <- lapply(1:nbPaths, function(x){
    simulateAvoidance(
      mapSizexMax = listOfDfOfMapSize[[i]][1,2],
      mapSizexMin = listOfDfOfMapSize[[i]][1,1],
      mapSizeyMax = listOfDfOfMapSize[[i]][2,2],
      mapSizeyMin = listOfDfOfMapSize[[i]][2,1],
      distanceAvoidance = distanceStartAvoidance,
      stepLength = 5,
      persistence = persistenceInBallisticMovement,
      directionStepAvoidance = avoidanceAngle,
      Nturbines = nrow(distributionTurbinesToUse),
      locationsTurbines.df = distributionTurbinesToUse,
      progressiveAvoidance=TRUE
    )
  }
  )
  
  # Prepare data to the right format sp
  for (j in 1:length(dataSimulated)){
    dataSimulated[[j]] <- cbind(dataSimulated[[j]], rep(0,nrow(dataSimulated[[j]])))
    dataSimulated[[j]] <- cbind(dataSimulated[[j]], rep(j,nrow(dataSimulated[[j]])))
  }
  
  pointsInFarmCheck <- as.data.frame(do.call(rbind,lapply(dataSimulated,function(x) x[,1:4])))
  colnames(pointsInFarmCheck) <- c("x","y","aboveThresholdAltitude","bout")
  
  # BUFFER = THE LARGEST OBSERVED DISTANCE OF AVOIDANCE
  # Create buffer around turbines
  dataCoordinatesTurbines.df.sp_rotated_buffered <- gBuffer(SpatialPoints(listOfDfOfTurbinesLoc[[i]], proj4string = CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")),
                                                            width=minBufferDetectedSignificant[i], quadsegs = 100)
  
  # Find points in buffer & calculate the associated percentage
  pointsInBuffer <-  pointsInFarmCheck[!is.na(sp::over(SpatialPoints(pointsInFarmCheck[,1:2], proj4string = CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")), 
                                                       dataCoordinatesTurbines.df.sp_rotated_buffered)),]
  
  observedPercentInBuffer <- nrow(pointsInBuffer)/nrow(pointsInFarmCheck)*100
  
  # BUFFER = SIMULATED DISTANCE OF AVOIDANCE
  if (par[2] != 0) {
    
    # Create buffer around turbines
    dataCoordinatesTurbines.df.sp_rotated_buffered <- gBuffer(SpatialPoints(listOfDfOfTurbinesLoc[[i]], proj4string = CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")),
                                                              width=par[2], quadsegs = 100)
    
    # Find points in buffer & calculate the associated percentage
    pointsInBuffer <-  pointsInFarmCheck[!is.na(sp::over(SpatialPoints(pointsInFarmCheck[,1:2], proj4string = CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")), 
                                                         dataCoordinatesTurbines.df.sp_rotated_buffered)),]
    
    observedPercentInBuffer_2 <- nrow(pointsInBuffer)/nrow(pointsInFarmCheck)*100
    
  } else {
    observedPercentInBuffer_2 <- 0
    
  }
  
  distanceVec <- seq(from=50,to=450,by=50)
  observedPercentInBuffer_vec <- rep(NA, times = length(distanceVec))
  for(distValue in 1:length(distanceVec)){
    # Create buffer around turbines
    dataCoordinatesTurbines.df.sp_rotated_buffered <- gBuffer(SpatialPoints(listOfDfOfTurbinesLoc[[i]], proj4string = CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")),
                                                              width=distanceVec[distValue], quadsegs = 100)
    
    # Find points in buffer & calculate the associated percentage
    pointsInBuffer <-  pointsInFarmCheck[!is.na(sp::over(SpatialPoints(pointsInFarmCheck[,1:2], proj4string = CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")), 
                                                         dataCoordinatesTurbines.df.sp_rotated_buffered)),]
    
    observedPercentInBuffer_vec[distValue] <- nrow(pointsInBuffer)/nrow(pointsInFarmCheck)*100
  }
  
  return(c(observedPercentInBuffer, observedPercentInBuffer_2, observedPercentInBuffer_vec))
}






#________________________________________________________________
#
#### Optimization of parameters to match observed results ####
#
#________________________________________________________________

## All parameters (note: by commodity the code is made as if multiple wind farms could be tested, hence the counter i)
numberOfSites <- 1 
wantWindFarms <- 4 
bufferSize.v <- seq(from=50, to=500, by=50) # Size of buffer that will be used around wind turbine (in meters)
persistenceInBallisticMovement <-  mean(kappaEstimates.df$mean) #Variance of a von mises distribution - control for linearity of movement
minAngleToExplore <- 1
maxAngleToExplore <- 20 # Maximum angle vulture reach in thermals
maxDistanceToExplore <- 1000 #if in m for instance, adding 1000m. The larger you put, the longer in time to compute
nbPaths <- 10000 # Number of paths to simulate



listOfDfOfTurbinesLoc <- vector(mode = "list", length = numberOfSites) #Here you need to insert in a list of df the coordinates of the turbines
for (i in 1:numberOfSites){
  listOfDfOfTurbinesLoc[[i]] <- allWindTurbines[[wantWindFarms[i]]]$dataCoordinatesTurbines.df.sp@coords
}

listOfDfOfMapSize <- vector(mode = "list", length = numberOfSites) # Each time, a df with in row x, and y, in col, min and max
for (i in 1:numberOfSites){
  listOfDfOfMapSize[[i]] <- allWindFarms[[wantWindFarms[i]]]$farm.sP@bbox
}



# Result on Montfrech wind farm:
f = 4
#load("./Outputs/AnalysisResults/RotationAnalysisResult_allyears_S2.Rdata")
load("./Outputs/AnalysisResults/RotationAnalysisResult2_allyears_S2.Rdata")

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

ggplot(resBelow.df, aes(x = as.integer(posObsValue), y = as.integer(bufferAroundTurbineMeter))) + 
  scale_x_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_y_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle(paste(allWindFarms[[f]]$name,"- all year")) +
  theme_light() +
  geom_rect(aes(xmin = 0 + 0.5, xmax = 2 - 0.5, ymin = 0 - 0.5, ymax = 1000 + 5, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")


# From visual inspection - in Montfrech wind farm the avoidance stop after 450m
minBufferDetectedSignificant <- bufferSize.v[9] #The distance at which significance of "lower presence" than expected





### Optimization procedure
# Initialize result table
valueMapAvoidanceAngle <- seq(from=minAngleToExplore , to=maxAngleToExplore, by = 1)
valueMapAvoidanceDistance <- seq(from=0, to=maxDistanceToExplore, by = 50)
toTestParameters <- crossing(valueMapAvoidanceAngle, valueMapAvoidanceDistance)

# Initialize expected percent in buffer observed + wind turbine positions
i=1
expectedPercentInBuffer <- resultsRotation.df$percentPointsInBufferBelow[which(resultsRotation.df$rotationAngleDegree == 0 & 
                                                                                resultsRotation.df$bufferAroundTurbineMeter == minBufferDetectedSignificant[i])] #The value of the percentage of point in buffer for each site (pct in buffer for each site at a specific buffer size)
distributionTurbinesToUse <- listOfDfOfTurbinesLoc[[i]]

# Optimization procedure (paralleled)
cores = detectCores()
nbCoresToSubstract = 2


tic()

resSimul <- mclapply(1:nrow(toTestParameters), 
mc.cores = cores[1] - nbCoresToSubstract, FUN = function(r){ 
  x <- as.numeric(toTestParameters[r,])
  LLsite(x)})

toc()

# Fill the full result dataframe
toTestParameters <- cbind(toTestParameters, do.call("rbind", resSimul))

#Results of observed percent in function of distance to turbines
vectorObservedPercent <- resultsRotation.df$percentPointsInBufferBelow[which(resultsRotation.df$rotationAngleDegree == 0 & resultsRotation.df$bufferAroundTurbineMeter < 500)]

# Reorganise data and match with observed pct
colnames(toTestParameters)[3:13] <- c("fitValue1", "fitValue2", paste0("fitValuev", 1:9))
toTestParameters <- toTestParameters %>% 
  left_join(resultsRotation.df %>% 
              select(rotationAngleDegree, bufferAroundTurbineMeter, numberOfPointsBelow, percentPointsInBufferBelow) %>% 
              filter(rotationAngleDegree == 0), by = c("valueMapAvoidanceDistance"="bufferAroundTurbineMeter")) %>% 
  mutate(fitValue1 = (fitValue1 - expectedPercentInBuffer)**2, 
         fitValue2 = (fitValue2 - percentPointsInBufferBelow)**2,
         fitValuev1 = (fitValuev1 - vectorObservedPercent[1])**2,
         fitValuev2 = (fitValuev2 - vectorObservedPercent[2])**2,
         fitValuev3 = (fitValuev3 - vectorObservedPercent[3])**2,
         fitValuev4 = (fitValuev4 - vectorObservedPercent[4])**2,
         fitValuev5 = (fitValuev5 - vectorObservedPercent[5])**2,
         fitValuev6 = (fitValuev6 - vectorObservedPercent[6])**2,
         fitValuev7 = (fitValuev7 - vectorObservedPercent[7])**2,
         fitValuev8 = (fitValuev8 - vectorObservedPercent[8])**2,
         fitValuev9 = (fitValuev9 - vectorObservedPercent[9])**2
  ) 


# Save optimization procedure results
save(toTestParameters,
     file="./Outputs/Simulations/OptimABM_S2.Rdata")





