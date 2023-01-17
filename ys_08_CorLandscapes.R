#____________________________________________________________________________________________________________________
##
##      PROJECT: Avoidance of wind turbines by griffon vultures
##
##      GOAL OF THE SCRIPT: Estimate the overlap between topography and use by vultures in flight
##                          
##
##      CREDIT: Yohan Sassi (yoh.sassi22@gmail.com) & Benjamin Robira (benjamin.robira@normalesup.org)
##
#____________________________________________________________________________________________________________________


rm(list = ls())


# Packages needed
library(raster)
library("move")
library(sp)
library(zoom)
library(adehabitatHR)
library(stringr)
library(lubridate)
library(dplyr)
library(scales)
library(sf)
library(amt)
library(purrr)
library(MASS)
library(glmmTMB)


#Homemade functions & needed files
source("Functions/df_to_spatial_df_utm.R") #To convert long/lat to UTM

IndInformation <- read.csv("./Data/RawData/ind_Data_GVCausses.csv", sep = ";", h = T)
IndInformation <- IndInformation[-nrow(IndInformation),]



#___________________________________________________________
##
#### Extract topography of the wind farms         ####
##
#___________________________________________________________

# Load wind farms and associated wind turbine lists
load("./Data/RawData/AllWindFarms_Causses.Rdata")
load("./Data/RawData/AllWindTurbines_Causses.Rdata")

# Load elevation raster from MNT
elevationCaussesRaster <- raster::raster('./Data/RawData/Elevation_Causses.tif')

# Extract topography from wind farms geofences extent iteratively
for (f in 1:length(allWindFarms)){
  
  # Extract topography from wind farms geofences SpatialPolygon extent
  windFarmTopo <-  crop(elevationCaussesRaster, extent(allWindFarms[[f]]$farm.sP))
  
  # Visual check
  # plot(windFarmTopo)
  # plot(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, pch = 13, cex = 1, col = "red", add = T)
  # 
  # Save geofence topography as raster layer
  # filename <- paste("windFarmTopo_",allWindFarms[[f]]$name,sep="")
  # writeRaster(windFarmTopo, file=paste("./Outputs/Figures/",filename,".tif",sep=""))
  
  allWindFarms[[f]]$windFarmTopo <- windFarmTopo
}

# save(allWindFarms, 
#      file = "./Data/RawData/AllWindFarms_Causses.Rdata")







#______________________________________________________________________
##
#### Create UD per wind farm geofences for each individuals   ####
##
#______________________________________________________________________

# Parameters
farmsnb <- c(1,4,10,11) # Farms under study
c = 0 # Counter
summaryIndBout <- data.frame(id = character(), nbBout = numeric())




# -- LOOP ON EACH WIND FARM 
for (f in farmsnb){
  
  # load the observed tracks for this farm
  allFiles <- list.files("./Data/ObsPaths", paste("ObsPaths2_",allWindFarms[[f]]$name,"_Causses",sep="") , all.files = F, full.names = T)
  
  
  # Load sequentially the observed paths
  load(allFiles[1])
  allPointsInFarm <- pointsInFarm
  
  for (d in 2:length(allFiles)){
    load(allFiles[d])
    allPointsInFarm  <- rbind(allPointsInFarm, pointsInFarm)
  }
  
  # Paths of this farm over the three year combined in same data set
  pointsInFarm <- allPointsInFarm
  
  # unique list of individual present in this wind farm
  allind <- unique(pointsInFarm$ID)
  
  #Estimate the grid
  biggridforUD <- raster::extend(allWindFarms[[f]]$windFarmTopo, c(50,50), values = NA)
  gridforUD <- as(biggridforUD, "SpatialPixelsDataFrame")
  
  
  for (i in 1:length(allind)){ # If the considered individual was detected -> UD can be estimated
    
    c = c+1 # Counter
    
    # Individual name
    hyfenindices <- str_locate_all(pattern ='-', allind[i]) # locate the - in the name
    individualName <- substr(allind[i], 
                             as.integer(hyfenindices[[1]][3,1])+1, 
                             as.integer(hyfenindices[[1]][4,1])-1)
    
    
    # Extract the rediscretised paths for considered individuals
    indDatainGeo.sp <- pointsInFarm[which(pointsInFarm$ID == allind[i]),]
    
    # Remove duplicated times
    indDatainGeo.sp <- indDatainGeo.sp[!duplicated(indDatainGeo.sp@data$timestamp) == T,]
    
    ## -- Give unique bout numbers
    
    boutstart <- indDatainGeo.sp@data[1:nrow(indDatainGeo.sp@data)-1,]$Bout # remove the last
    boutend <-  indDatainGeo.sp@data[2:nrow(indDatainGeo.sp@data),]$Bout# remove the first
    notMatch <- which(ifelse(boutstart == boutend,1,0) == 0) # Match both time to estimate interval
    startBoutIn.v  <- c(1, notMatch + 1) # Give the number fo the interval so real time = interval + 1
    
    #Create independent bout as soon as time interval between two consecutive points is > 30s (bout name = position of the starting points in the list)
    indDatainGeo.sp@data$uniqueBout <- NA
    indDatainGeo.sp@data$uniqueBout[startBoutIn.v] <- startBoutIn.v 
    indDatainGeo.sp@data$uniqueBout <- zoo::na.locf(indDatainGeo.sp@data$uniqueBout)
    
    ## --
    
    # Number of bout
    summaryBout <- indDatainGeo.sp@data %>% group_by(uniqueBout) %>% summarise(
      CountTot = n()
    )
    
    
    #Transform to ltraj
    data1.rdc_ltraj <- as.ltraj(xy = indDatainGeo.sp@coords, 
                                date = ymd_hms(indDatainGeo.sp@data[,2]),
                                id = indDatainGeo.sp@data$ID,
                                burst = indDatainGeo.sp@data$ID,
                                typeII = TRUE,
                                proj4string = CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" ")))
    
    
    
    # Compute the home ranges -------------------------------------------------
    HR <- BRB(data1.rdc_ltraj,
              D = 0, # Coefficient of diffusion (because high resolution GPS data)
              Tmax = 1*30, # Maximum time between to point, above that it's ignored - 30s
              Lmin = 20, # Minimum distance between successive location to be considered
              type = "UD", # Utilisation distribution
              hmin = 150, # Minimum smoothing parameter
              filtershort = FALSE, # Indeed for a vulture in a thermal step can be lower than 20m but they are still active
              grid = gridforUD)
    
    # Visual check
    # plot(HR)
    # plot(allWindFarms[[f]]$farm.sP, col="red", add=T)
    # plot(indDatainGeo.sp, col = "green", pch = 16, cex = 1, alpha = 0.5, add=TRUE)
    # plot(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, pch = 13, cex = 1, add = T)
    
    # To weight the individuals differently we will multiply values by nb of path / by the total number of paths in the geofence (this will be done in a next step)
    HR$dens <- HR$dens * nrow(summaryBout)
    
    # Convert & save the HR as Raster
    indHR.rs <- raster(HR)
    
    writeRaster(indHR.rs, 
                file=paste("./Outputs/Figures/IndHRinFarm/hr2_",individualName, "_", allWindFarms[[f]]$name,".tif",sep=""),
                overwrite = T)
    
    # add number of bout for later
    summaryIndBout[c,] <- c(paste(individualName, "_", allWindFarms[[f]]$name, sep=""),
                            nrow(summaryBout))
    
  }
}


# Save the individual file
save(summaryIndBout,
     file = "./Outputs/FilteringSummary/summaryBout2.Rdata") # Save as R.Data









#___________________________________________________________
##
#### Create mean UD per wind farm    ####
##
#___________________________________________________________

load("./Outputs/FilteringSummary/summaryBout2.Rdata")

farmsnb <- c(1,4,10,11) # Farms under study
allFiles <- list.files("./Outputs/Figures/IndHRinFarm","hr2_", all.files = F, full.names = T)

for(f in farmsnb){
  
  # extract UD for specific farm
  needIso <- allFiles [which(str_detect(allFiles ,allWindFarms[[f]]$name))]
  summaryInd <- summaryIndBout[which(summaryIndBout$id %in% str_match(needIso,"./Outputs/Figures/IndHRinFarm/hr2_\\s*(.*?)\\s*.tif")[,2]),]
  trajtot <- sum(as.numeric(summaryInd$nbBout))
  
  
  # Stack all HR in the geofence and add them to each other
  allUD <- stack(needIso)
  meanUD <- calc(allUD, sum)
  
  # divide each cell by the total number of paths in the geofence
  meanUD <- meanUD / trajtot
  
  #save the overlap map into raster
  writeRaster(meanUD, file=paste("./Outputs/Figures/overlapUD2_",allWindFarms[[f]]$name,".tif",sep=""))
  
  # Visual check
  # plot(meanUD)
  # plot(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, add = T)
  
}








#________________________________________________________________________
##
#### Resource selection function   ####
##
#________________________________________________________________________

load("./Data/RawData/AllWindFarms_Causses.Rdata")
load("./Data/RawData/AllWindTurbines_Causses.Rdata")


combine = T
selectedWindFarms <- c(1,4,10,11)


for (f in selectedWindFarms){
  
  allFiles <- list.files("./Data/ObsPaths", paste("ObsPaths2_",allWindFarms[[f]]$name,"_Causses",sep="") , all.files = F, full.names = T)
  
  # Load the data sets corresponding to this wind farm and combine them into a full one
  if (combine == T){
    
    # Load sequentially the observed paths
    load(allFiles[1])
    allPointsInFarm <- pointsInFarm
    
    for (d in 2:length(allFiles)){
      load(allFiles[d])
      allPointsInFarm  <- rbind(allPointsInFarm, pointsInFarm)
    }
    
    pointsInFarm <- allPointsInFarm
  } 
  
  ## -- Give unique bout numbers
  
  boutstart <- pointsInFarm@data[1:nrow(pointsInFarm@data)-1,]$Bout # remove the last
  boutend <-  pointsInFarm@data[2:nrow(pointsInFarm@data),]$Bout# remove the first
  notMatch <- which(ifelse(boutstart == boutend,1,0) == 0) # Match both time to estimate interval
  startBoutIn.v  <- c(1, notMatch + 1) # Give the number fo the interval so real time = interval + 1
  
  #Create independent bout as soon as time interval between two consecutive points is > 30s (bout name = position of the starting points in the list)
  pointsInFarm@data$uniqueBout <- NA
  pointsInFarm@data$uniqueBout[startBoutIn.v] <- startBoutIn.v 
  pointsInFarm@data$uniqueBout <- zoo::na.locf(pointsInFarm@data$uniqueBout)
  
  ## --
  
  # Check data
  # head(pointsInFarm@coords,50)
  # head(pointsInFarm@data,50)
  
  # Convert into a dataframe to then make a track 
  forTrack <- as.data.frame(cbind(pointsInFarm@coords,
                                  pointsInFarm@data$timestamp,
                                  pointsInFarm@data$ID,
                                  pointsInFarm$uniqueBout))
  
  colnames(forTrack) <- c("x","y","t","ID","bout")
  
  
  # Sample randomly 30% of point per bout
  forTracksampled <- forTrack %>%
    group_by(bout) %>%
    nest() %>%
    mutate(n = round(0.3*nrow(data.frame(data)))) %>%
    mutate(samp = map2(data,n,sample_n)) %>%
    select(-data) %>%
    unnest(samp) %>%
    ungroup()

  
  
  # Convert in track for RSF
  pointsInFarm.xyt <- mk_track(forTracksampled,
                               x,
                               y,
                               all_cols = TRUE)
  pointsInFarm.xyt$x_ <- as.numeric(pointsInFarm.xyt$x_)
  pointsInFarm.xyt$y_ <- as.numeric(pointsInFarm.xyt$y_)
  
  
  
  #-- Extract canyon's ridges --#
  
  # Extract wind farm topo
  windFarmTopo <- allWindFarms[[f]]$windFarmTopo
  
  # Extract middle altitude (what we visually detect as threshold)
  windFarmTopo[windFarmTopo@data@values > ((max(windFarmTopo@data@values) + min(windFarmTopo@data@values)) / 2)] <- NA
  
  # Extract the ridges
  canyonRidge <-  boundaries(windFarmTopo, type = "inner", classes = F)
  canyonRidge[canyonRidge@data@values != 1] <- NA
  
  # Take a buffer around the ridges (300m)
  canyonRidgeBuff <- raster::buffer(canyonRidge, width=300, doEdge=FALSE, dissolve = TRUE)
  canyonRidgeBuff[canyonRidgeBuff@data@values != 1] <- 0
  
  # Proportion of 1
  propCanyon <- (freq(canyonRidgeBuff, value = 1) / ncell(canyonRidgeBuff))
  
  # Visual check
  # plot(allWindFarms[[f]]$windFarmTopo)
  # plot(canyonRidge, col = "red", add = T)
  # plot(canyonRidgeBuff,col = alpha("red",0.2), add = T)
  
  poly <- st_as_sf(allWindFarms[[f]]$farm.sP)
  
  #--
  
  
  # Extract random points associated with each individuals ID
  for (i in 1:length(unique(pointsInFarm.xyt$ID))){
    wantInd <- pointsInFarm.xyt[which(pointsInFarm.xyt$ID == unique(pointsInFarm.xyt$ID)[i]),]
    wantIndRdm <- random_points(poly, n=10*nrow(wantInd), presence = wantInd) %>% 
      mutate(ID = rep(unique(wantInd$ID)))
    
    #plot(test)
    
    # Paste data below each other
    if (i==1) {
      allIndForRSF <- wantIndRdm
    } else {
      allIndForRSF <- rbind(allIndForRSF, wantIndRdm)
    }
  }
  
  # Associate covaritates to points (observed + available)
  rsf1 <- allIndForRSF %>% 
    extract_covariates(canyonRidgeBuff) %>%
    mutate(layer = coalesce(layer, 0)) %>%
    mutate(canyon = layer == 1) %>%
    mutate(w = ifelse(allIndForRSF$case_ == TRUE, 1, 5000))
  
  # Visual check
  # plot(rsf1)
  # plot(canyonRidge, col = "black", legend = FALSE, add = T)
  # plot(canyonRidgeBuff, col=alpha("green",0.4), legend=FALSE, add=T)
  
  #Fit the RSF model controlling for ind ID
  rsfRes <- glmmTMB(case_ ~ canyon + (1|ID), 
                    family=binomial(), 
                    data = rsf1,
                    weights = w)
  
  # show results
  print(paste("------------------- Result of RSF model for wind farm", allWindFarms[[f]]$name,": ------------------------"))
  print(summary(rsfRes))
  print(confint(rsfRes, level = 0.95))

}


