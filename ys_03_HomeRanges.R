#_______________________________________________________________________________________________________________
##
##      PROJECT: Avoidance of wind turbines by griffon vultures
##
##      GOAL OF THE SCRIPT: This script will compute the home ranges for each vulture individual. Potentially
##                          all individuals will be pooled to have a global home range and plotted on a map
##                      
##
##
##
##      CREDIT: Yohan Sassi (yoh.sassi22@gmail.com) & Benjamin Robira (benjamin.robira@normalesup.org)
##
#_______________________________________________________________________________________________________________


rm(list = ls())

# Package needed
library(dplyr)
library(lubridate)
library(adehabitatLT)
library(adehabitatHR)
library(amt)
library(tictoc)
library(stringr)
library(zoom)
library("move")
library(sf)
library(sp)
library(rgeos)
library(raster)
library(nominatimlite)
library(glmmTMB)
library(swfscMisc)
library(librarian)


#Homemade functions & needed files
source("Functions/df_to_spatial_df_utm.R") #To convert long/lat to UTM

IndInformation <- read.csv("./Data/RawData/ind_Data_GVCausses.csv", sep = ";", h = T)
IndInformation <- IndInformation[-nrow(IndInformation),]

load("./Data/RawData/AllWindTurbines_Causses.Rdata")


#Select point in data the closest to the indicated time. Output is the dataframe with subselection at desired times. Input form is indicated below.
selectOnePointCloseTime <- function(data, # x, y, t, with t in lubridate format
                                    vectorTimesToSelect # as hh:mm:ss
){
  shelf(dplyr)
  shelf(lubridate)
  dataOutput <- data %>% 
    mutate(
      t = ymd_hms(as.character(t)),
      Date = lubridate::date(t),
      rowNb = 1:nrow(.)
    ) %>%
    group_by(Date) %>% 
    summarise(
      whichSelect = lapply(vectorTimesToSelect, function(x)
        min(rowNb[which(abs(t - ymd_hms(paste(Date, x, sep=" "))) == 
                          min(abs(t - ymd_hms(paste(Date, x, sep=" ")))))
        ])
      )
    )
  whichToSelect <- sapply(dataOutput$whichSelect, function(x) unlist(x))
  return(data[unique(whichToSelect),])
}


#Estimate 95% CI with bootstrap
mean_95CI <- function(v1,nbboot=1000){
  d <- numeric(nbboot)
  for (i in 1:nbboot){
    v1bis <- sample(v1,replace=T)
    d[i] <- mean(v1bis)
  }
  return(c(mean(v1),quantile(d,c(0.025,0.975)))) #Give mean value + upper and lower limits for the 95% CI
}



#________________________________________________________________
#
#### Data preparation, cleaning / filtering      ####
#
#________________________________________________________________


## !!! THINK ABOUT A DATA REPOSITORY ON MOVEBANK PEOPLE CAN DOWNLOAD DIRECTLY !!!

# Parameters
indNames <- unique(IndInformation$Ind_ID) # Individual names
loginStored <- movebankLogin(username="#####", password="#####") # Store login requirement for MOVEBANK
studyName <- "Eurasian griffon vulture in France (Grands Causses 2018) ID_PROG 961" # Study name
year <- c("2019", "2020", "2021")


# -- LOOP ON EACH INDIVIDUAL
tic()
for (i in 1:length(indNames)) {
  
  # Download data for an individual for a specific period - from movebank
  indData <- getMovebankLocationData(study= studyName,
                                     sensorID = "GPS",
                                     login=loginStored,
                                     animalName = indNames[i],
                                     timestamp_start="20190101000000000",
                                     timestamp_end="20211231235959000")
  
  # Remove outlier & extract only interesting columns (reduce the weight of the dataset)
  indData <- indData[which(indData$visible == "true"),c(1,3:5,11,14:17,21,24,26)]
  
  # Filter by speed to have only location in flight -> avoid resting time on cliffs & gps.hdop < 4 for reduced GPS location errors
  # Nathan et al. 2012, D’Eon & Delparte 2005, Martin-Diaz et al. 2020
  indData <- indData[which(indData$ground.speed > 4 & indData$gps.hdop < 4),]
  
  #Transform coordinates to UTM
  df_to_spatial_df_utm(dataframe=indData, 
                       column_longitude="location.long", 
                       column_latitude="location.lat", 
                       utm_zone=31, hemisphere="N")
  
  # Add the UTM coordinates to the initial df
  indData <- cbind(indData, dataframe.sp_utm@coords)
  
  # Free memory
  rm(dataframe.sp,dataframe.sp_utm)
  gc()
  
  # Add column of date & take the list
  indData$timestamp <- strptime(indData$timestamp, format = "%Y-%m-%d %H:%M:%S")
  indData$date <- strptime(indData$timestamp, format = "%Y-%m-%d")
  allDates <- unique(indData$date)
  
  # -- LOOP ON EACH DAY FOR THIS INDIVIDUAL
  for (d in 1:length(allDates)){ 
    
    # Data for a specific day
    wantindData <- indData[which(indData$date == allDates[d]),]
    
    # Convert into track_xyt for amt
    wantindData_Track <- make_track(wantindData,
                                    longitude,
                                    latitude,
                                    timestamp)
    
    ## Subsample to have a point every hour or the closest
    # if no points found in 10mn then it take the following point and start again from this point
    wantindData_TrackResampled <- track_resample(wantindData_Track, 
                                                 rate = minutes(10), 
                                                 tolerance = minutes(0), 
                                                 start = 1)
    # Concatenate the re sampled fixes
    if (d == 1){
      allSubsampled <- wantindData_TrackResampled 
    } else {
      allSubsampled <- rbind(allSubsampled, wantindData_TrackResampled)
    }
  }
  
  # Keep only the points corresponding to the resampled paths
  indData <- indData[which(indData$longitude %in% allSubsampled$x_ &
                             indData$latitude %in% allSubsampled$y_ &
                             indData$timestamp %in% allSubsampled$t_),]
  
  # Estimate the distance between Cassagne (the feeding station from where all individuals have been equipped) and the considered location
  indData$distToC <- pointDistance(cbind(3.249370, 44.205059),
                                   cbind(indData$location.long,indData$location.lat),
                                   lonlat = TRUE)
  
  # Save the individual file
  hyfenindices <- str_locate_all(pattern ='-', indNames[i]) # locate the - in the name
  cutIndName <- substr(indNames[i], 
                       as.integer(hyfenindices[[1]][3,1])+1, 
                       as.integer(hyfenindices[[1]][4,1])-1) # cut between 3rd and 4th -
  save(indData,
       file = paste("./Data/FilteredIndData/indDataCausses_allyear_",cutIndName,".Rdata", sep="")) # Save as R.Data
  
  # Free memory
  rm(allSubsampled)
  gc()
}
toc()



#__________________________________________________________________________________________________________
#
#### Check and remove if needed time spend in other area than the Grands Causses     ####
#
#__________________________________________________________________________________________________________

## See distribution of distance to Cassagne per ind

allFiles <- list.files("./Data/FilteredIndData",all.files = FALSE, full.names = TRUE)

for (f in 1:length(allFiles)){ # Loop on each file (i.e. each individual)
  
  # Load the data of a specific individual
  load(allFiles[f])
  
  #Extract dates to loop on each
  indAllDates <- unique(indData.sp@data$date)
  
  # To store data
  maxDist.v <- vector(mode = "numeric", length = length(indAllDates))
  sndMaxInd <- data.frame(indAllDates, maxDist.v)
  
  for (d in 1:length(indAllDates)){ # Loop on each date for this individual
    sndMaxInd[d,2] <- max(indData$distToC[which(indData$date == indAllDates[d])])
    
  }
  
  # Extract the name 
  hyfenindices <- str_locate_all(pattern ='_', allFiles[f]) # locate the _ in the name
  pointindices <- str_locate_all(pattern ='.Rdata', allFiles[f]) # locate the _ in the name
  cutIndName <- substr(allFiles[f], 
                       as.integer(hyfenindices[[1]][2,1])+1, 
                       as.integer(pointindices[[1]][1,1])-1) # before .RData
  
  # Add name into the df
  sndMaxInd$indName <- rep(cutIndName, length(indAllDates)) 
  
  # If it's first file it become the beginning of the full df, otherwise you bind it at the end of the full df
  if (f == 1){ 
    allIndMaxDist <- sndMaxInd
  } else {
    allIndMaxDist <- rbind(allIndMaxDist, sndMaxInd)
  }
  
  # Save the plot as png - by individual
  plot <- ggplot(sndMaxInd, aes(indAllDates,maxDist.v)) +
    geom_line() +
    xlab("Dates") +
    ylab("Distance max / day from Cassagne (m)") +
    theme_light()
  
  print(plot)
  
  ggsave(paste("./Outputs/Figures/indMaxSNDCausses",cutIndName,".png",sep=""), plot,
          dpi = 300)
}

# Save the full dataset with every individual
#save(allIndMaxDist,file = paste("./Outputs/FilteringSummary/allIndMaxNSDCausses.Rdata", sep="")) # Save as R.Data


# Plot and save the SDN max per day for all individuals
plotAll <- ggplot(allIndMaxDist, aes(indAllDates,maxDist.v/1000, color = indName)) +
  geom_line() +
  guides(col= guide_legend(title= "Individuals' name:")) +
  xlab("Dates") +
  scale_y_continuous("Max distance per day from colony center [m]" , breaks = seq(0, 1000, by = 100)) +
  geom_hline(yintercept=55, linetype="dashed", color = "black", size=0.5) +
  theme_light()

print(plotAll)

# ggsave(paste("./Outputs/Figures/AllindMaxSNDCausses.png",sep=""), 
#        plotAll,
#        dpi = 300, 
#        width = 10)




#__________________________________________________________________________________________________________
#
#### Estimation of the individual home range through UD & combination to population level   ####
#
#__________________________________________________________________________________________________________


allFiles <- list.files("./Data/FilteredIndData",all.files = FALSE, full.names = TRUE)

# Remove Beaufixe and Courage, as they didn't crossed any wind farm we remove them from DV estimates for simplicity in sample size
allFiles <- allFiles[-c(2,7)]

for (f in 1:length(allFiles)){
  
  # Load the data of a specific individual
  load(allFiles[f])
  
  # Filtrate to keep only the resident time - filter out all points where distance to Cassagne > 55km
  indData <- indData[which(indData$distToC <= 55000),]
  
  # Create a SpatialPointDataframe required to ltraj
  indData.sp <- SpatialPointsDataFrame(coords = cbind(indData$longitude,indData$latitude),
                                       proj4string = CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" ")),
                                       data = indData)
  
  #Transform to ltraj
  data1.rdc_ltraj <- as.ltraj(xy = indData.sp@coords, date = ymd_hms(indData.sp@data[,2]),
                              id = indData.sp@data$individual.local.identifier,
                              burst = indData.sp@data$individual.local.identifier,
                              typeII = TRUE,
                              proj4string = CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" ")))

  # Compute the coefficient of diffusion ------------------------------------
  diffusionCoeff <- BRB.D(data1.rdc_ltraj,
                          Tmax=1*60*60,
                          Lmin=20) # Tmax = 1h

  # Compute the home ranges -------------------------------------------------
  HR <- BRB(data1.rdc_ltraj,
            D = diffusionCoeff, # Coefficient of diffusion
            Tmax = 1*60*60, # Maximum time between to point, above that it's ignored - 1h
            Lmin = 20, # Minimum distance between successive location to be considered
            type = "UD", # Utilisation distribution
            hmin = 500, # Minimum smoothing parameter
            filtershort = TRUE,
            same4all = TRUE,
            grid = 100,
            extent=0.4)
  # plot(HR)
  # zm()

  # Find individual name
  hyfenindices <- str_locate_all(pattern ='_', allFiles[f]) # locate the _ in the name
  pointindices <- str_locate_all(pattern ='.Rdata', allFiles[f]) # locate the _ in the name
  cutIndName <- substr(allFiles[f], 
                       as.integer(hyfenindices[[1]][2,1])+1, 
                       as.integer(pointindices[[1]][1,1])-1) # before .RData
  
  # Extract the individual 95% Isopleth
  border95 <- getverticeshr(HR, percent = 95)
  border95$id <- cutIndName

  #plot(border95)

  # Add them iteratively within a bigger object
  if (f == 1){
    allBorder95 <- border95
  } else {
    allBorder95 <- rbind(allBorder95, border95)
  }
}

# Add the projection to the SpatialPolygonDataFrame
allBorder95@proj4string <- CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))

# Plot the superposition of the HR
#spplot(allBorder95)

# Create a grid below corresponding the what the HR was calculated on
grid4Map <- GridTopology(HR@grid@cellcentre.offset, HR@grid@cellsize, HR@grid@cells.dim)
grid4Map.s <- SpatialGrid(grid4Map)
proj4string(grid4Map.s) <- CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))

# Estimate the number of overlaping HR per cell
overlap <- over(grid4Map.s, allBorder95, returnList=TRUE)
ct <- sapply(overlap, nrow)
overlapGrid <- SpatialGridDataFrame(grid4Map.s, data=data.frame(ct=ct))
overlapGrid$ct[overlapGrid$ct<1] <- NA # Remove not used cells

# Plot the final map where colour correspond to the number of HR superimposed
spplot(overlapGrid, "ct", col.regions=bpy.colors(20))

# Save as a raster the population home range
Raster_HomeRange <- raster(overlapGrid)
#writeRaster(Raster_HomeRange, file=paste('./Outputs/Figures/AllIndHR_2.tif',sep=""))









#__________________________________________________________________________________________________________
#
#### RSF to consider macro-avoidance of wind farms   ####
#
#__________________________________________________________________________________________________________


allFiles <- list.files("./Data/FilteredIndData",all.files = FALSE, full.names = TRUE)

# Remove Beaufixe and Courage, as they didn't crossed any wind farm we remove them from DV estimates for simplicity in sample size
allFiles <- allFiles[-c(2,7)]


# # Create the bounding box to sample random point
# centerCol <- SpatialPoints(coords = cbind(3.249370, 44.205059), CRS("+proj=longlat")) %>% 
#   st_as_sf(coords = c("x", "y")) %>% 
#   st_transform(CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" ")))
# 
# poly <- st_buffer(centerCol, dist = 55000)

          
# Create an object with all the operating turbine to estimate the distance to it after
allturbines <- rbind(allWindTurbines$S1$dataCoordinatesTurbines.df.sp,
                     allWindTurbines$S2$dataCoordinatesTurbines.df.sp,
                     allWindTurbines$S3$dataCoordinatesTurbines.df.sp,
                     allWindTurbines$S4$dataCoordinatesTurbines.df.sp,
                     allWindTurbines$S5$dataCoordinatesTurbines.df.sp,
                     allWindTurbines$S6$dataCoordinatesTurbines.df.sp,
                     allWindTurbines$S8$dataCoordinatesTurbines.df.sp) # Merge all operating turbines

allOperationalTurbines.sf <- allturbines %>% 
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))) # Convert to spatial




meanDistanceColony <- 26000

Colcenter <- data.frame(x = 3.249370, y = 44.205059)
df_to_spatial_df_utm(dataframe=Colcenter, 
                     column_longitude="x", 
                     column_latitude="y", 
                     utm_zone=31, hemisphere="N")
Colcenter_utm <- dataframe.sp_utm




for (f in 1:length(allFiles)){ # Loop on all individuals
  
  # Load the data of a specific individual
  load(allFiles[f])
  
  # Filtrate to keep only the resident time - filter out all points where distance to Cassagne > 55km
  indData <- indData[which(indData$distToC <= 55000),]
  
  # Remove data before 9:00 and after 15pm
  indData <- indData %>% 
    mutate(hour_t = hour(timestamp)) %>% 
    filter(hour_t > 9 & hour_t < 15)
  
  
  # Subsample location closest in time from 10:00, 12:00 and 14:00
  forTrack <- as.data.frame(cbind(indData$longitude, indData$latitude, as.character(indData$timestamp)))
  colnames(forTrack) <- c("x","y","t") # Format needed
  
  subindData <- selectOnePointCloseTime(forTrack, c("10:00:00","12:00:00","14:00:00")) # Find closest in time

  
  # Convert in track for RSF
  subindData.xyt <- mk_track(subindData,
                               x,
                               y,
                               all_cols = TRUE)
  subindData.xyt$x_ <- as.numeric(subindData.xyt$x_)
  subindData.xyt$y_ <- as.numeric(subindData.xyt$y_)
  
   
  # Sample random point in the bbox and estimate the distance to the wind turbine
  #wantIndRdm <- random_points(poly, n=10*nrow(subindData.xyt), presence = subindData.xyt)
  
  # Sample random point corrected for central place forager -> Benhamou & Courbin 2023
  nPointsNull <- 10*nrow(subindData.xyt)
  nRemaining <- nPointsNull
  
  c = 0 # Counter
  while(nRemaining != 0){
    
    c = c+1 # counter
    nPointsNull <- nRemaining
    
    # Sample within a reconstructed bivariate exponential distribution - 10time more random points
    nullCoordinates <- data.frame(
      dist = rexp(nPointsNull, 1/meanDistanceColony),
      angle = runif(nPointsNull, 0, 2*pi)
    ) %>% 
      mutate(
        x = Colcenter_utm@coords[[1]] + cos(angle)*dist,
        y = Colcenter_utm@coords[[2]] + sin(angle)*dist
      ) %>% 
      filter(dist < 55000)
    
    if (c == 1){
      nullCoordinates_full <- nullCoordinates
      nRemaining <- 10*nrow(subindData.xyt) - nrow(nullCoordinates_full)
      
    } else {
      nullCoordinates_full <- rbind(nullCoordinates_full, nullCoordinates)
      nRemaining <- 10*nrow(subindData.xyt) - nrow(nullCoordinates_full)
    }
  }
  
  # Reorganised sampled random points
  wantIndRdm <- nullCoordinates_full %>% 
    mutate(case_ = FALSE) %>% 
    rename("x_" = "x", "y_" = "y") %>% 
    dplyr::select(c("x_", "y_", "case_")) %>% 
    as_tibble()
  
  # Add real data to the sampled points
  wantIndRdm <- rbind(wantIndRdm,
                      subindData.xyt[,1:2] %>% 
                        mutate(case_ = TRUE))
  
  # Estimate convert to spatial each location
  wantIndRdm.sf <- wantIndRdm %>% 
    st_as_sf(coords = c("x_", "y_")) %>%
    st_set_crs(CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))) # Convert to spatial
  
  
  # Estimate the distance with all operating turbines
  distanceMatrix <- st_distance(
    wantIndRdm.sf,
    allOperationalTurbines.sf
  ) 
  
  # Keep for each location the distance to the closest turbine
  wantIndRdm$distanceClosestWindTurbine <- apply(distanceMatrix, 1, min) 
  
  
  # Associate weight to points (observed + available)
  wantIndRdm <- wantIndRdm %>% 
    mutate(w = ifelse(wantIndRdm $case_ == TRUE, 1, 5000)) #%>%  mutate(ID = unique(indData$individual.local.identifier)) 
   
  
  # Visual check
  # plot(wantIndRdm.sf)
  # plot(allOperationalTurbines.sf$geometry, col = "black", legend = FALSE, add = T)
  
  #Fit the RSF model controlling for ind ID
  rsfRes <- glmmTMB(case_ ~ distanceClosestWindTurbine,
                    family=binomial(),
                    data = wantIndRdm,
                    weights = w)

  if (f==1) {
    wantIndRdm.all <- wantIndRdm
  } else {
    wantIndRdm.all <- rbind(wantIndRdm.all,wantIndRdm)
  }
  
  # show results
  # print(paste("------------------- Result of RSF model : ------------------------"))
  # print(summary(rsfRes))
  # print(confint(rsfRes, level = 0.95))
  
  if (f==1) {
    rsfRes.vec <- exp(rsfRes$sdr$par.fixed[2])[[1]]
  } else {
    rsfRes.vec <- c(rsfRes.vec, exp(rsfRes$sdr$par.fixed[2])[[1]])
  }
}


mean(rsfRes.vec)
sd(rsfRes.vec)
