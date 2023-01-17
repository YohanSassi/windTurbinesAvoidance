#_______________________________________________________________________________________________________________
##      PROJECT: Avoidance of wind turbines by griffon vultures
##
##      GOAL OF THE SCRIPT: create spatial polygons of the wind farms and the spatial points corresponding to
##                          the wind turbines associated to each farms. This will be used to see if there are
##                          farm's dependent patterns of avoidance.
##
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

#Datahandling
library(dplyr)
library(zoo)
library(data.table)
library(zoom)
library(scales)
library(quanteda)

#Homemade functions
source("Functions/df_to_spatial_df_utm.R")#To convert long/lat to UTM




#___________________________________________________________
#
#### Create the spatial polygon of the zone            ####
#
#___________________________________________________________



## Import csv file with the zones' coordinates
dataCoordinatesAllFarmsArea.df <- read.csv("./Data/RawData/Coord_Zones_Causses_WGS.csv",h=T)

## Create a list in which all the farms will be inserted
allWindFarms <- vector("list")

for (i in 1:length(unique(dataCoordinatesAllFarmsArea.df$Name))){
  
  ## Extract the coordinates into a dataframe
  dataCoordinatesFarmArea.df <- dataCoordinatesAllFarmsArea.df[which(dataCoordinatesAllFarmsArea.df$Name == unique(dataCoordinatesAllFarmsArea.df$Name)[i]),]
  farmAreacoords.df <- cbind(dataCoordinatesFarmArea.df$x, dataCoordinatesFarmArea.df$y)
  farmAreacoords.df <- as.data.frame(farmAreacoords.df)
  colnames(farmAreacoords.df) <- c("x", "y")
  head(farmAreacoords.df)
  
  ## Use of Ben's function to transform WGS84 coordinates into UTM Zone 31
  df_to_spatial_df_utm(dataframe=farmAreacoords.df, 
                       column_longitude="x", 
                       column_latitude="y", 
                       utm_zone=31, hemisphere="N")
  
  ## Create spatial points from UTM coordinates
  farmAreacoords.df.sp <- dataframe.sp_utm
  
  ## Calculate distance between farm's corners to have the longest side (unit = m if UTM coordinates)
  ## and calculate the diagonal length --> for zone buffer
  farmHorizontalength <- sp::spDists(farmAreacoords.df.sp[1:2,] , longlat = FALSE)[1,2]
  farmVerticalength <- sp::spDists(farmAreacoords.df.sp[c(1,4),] , longlat = FALSE)[1,2]
  farmDiagonalength <- sqrt(farmHorizontalength^2 + farmVerticalength^2) # Add to refine the buffer size around the wind farm
  
  ## Extract the UTM farm's coordinates to use them for spatial Polygons
  x_coords <- farmAreacoords.df.sp@coords[,1]
  y_coords <- farmAreacoords.df.sp@coords[,2]
  
  # Transform spatialPoints to spatialPolygons (+ farm's buffer)
  farm.polygon <- sp::Polygon(cbind(x_coords,y_coords))
  farm.spatialPolygons <- sp::SpatialPolygons(list(sp::Polygons(list(farm.polygon),ID = "A")))
  
  farmBarycentre <- apply(farmAreacoords.df.sp@coords, 2, mean) # Extract coordinated of the wind farm barycenter
  farmBarycentre.sp <- sp::SpatialPoints(cbind(farmBarycentre[[1]],farmBarycentre[[2]])) # Convert wind farm barycenter
  
  #farmBuffer.spatialPolygons <- gBuffer(farm.spatialPolygons, width=max(farmHorizontalength,farmVerticalength))
  farmBuffer.spatialPolygons <- gBuffer(farmBarycentre.sp, width=((farmDiagonalength/2)+50), quadsegs = 100) # Restricted buffer zone around farm area (+50m to avoid direct contact)
  
  # Set polygons' projection into UTM Zone 31
  proj4string(farm.spatialPolygons) <- CRS(paste("+proj=utm","+zone=31", "+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
  proj4string(farmBuffer.spatialPolygons) <- CRS(paste("+proj=utm","+zone=31", "+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
  proj4string(farmBarycentre.sp) <- CRS(paste("+proj=utm","+zone=31", "+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
  
  farmList <- list(name = unique(dataCoordinatesAllFarmsArea.df$Name)[i], 
                   farm.sP = farm.spatialPolygons, 
                   farmBuffer.sP = farmBuffer.spatialPolygons,
                   farmBarycentre = farmBarycentre,
                   farmBarycentre.sp = farmBarycentre.sp)
  
  allWindFarms[[unique(dataCoordinatesAllFarmsArea.df$Name)[i]]] <- farmList
}



#___________________________________________________________
#
##### Create the spatial points for wind farm turbines  ####
#
#___________________________________________________________

dataCoordinatesAllTurbines.df <- read.csv("./Data/RawData/Mat_inzones_Causses_filteredWGS.csv",h=T)

## Create a list in which all the farms will be inserted
allWindTurbines <- vector("list")

for (i in 1:length(unique(dataCoordinatesAllFarmsArea.df$Name))){
  
  dataCoordinatesTurbines.df <- dataCoordinatesAllTurbines.df[which(dataCoordinatesAllTurbines.df$Farm_Name == unique(dataCoordinatesAllFarmsArea.df$Name)[i]),]
  
  # Transform long/lat to UTM
  df_to_spatial_df_utm(dataframe=dataCoordinatesTurbines.df, 
                       column_longitude="x", 
                       column_latitude="y", 
                       utm_zone=31, hemisphere="N")
  
  dataCoordinatesTurbines.df.sp <- dataframe.sp_utm
  
  turbineList <- list(dataCoordinatesTurbines.df.sp = dataCoordinatesTurbines.df.sp)
  allWindTurbines[[unique(dataCoordinatesAllFarmsArea.df$Name)[i]]] <- turbineList
  
}


plot(dataCoordinatesTurbines.df.sp, pch = 13, add=TRUE)




# Save both lists in Rdata
# save(allWindFarms, file = "AllWindFarms_Causses.Rdata")
# save(allWindTurbines, file = "AllWindTurbines_Causses.Rdata")




#___________________________________________________________
#
##### Visual check  ####
#
#___________________________________________________________

plot(allWindFarms$S2$farmBuffer.sP)
plot(allWindFarms$S2$farm.sP, add = TRUE)
plot(allWindFarms$S2$farmBarycentre.sp, add = TRUE)
plot(allWindTurbines$S2$dataCoordinatesTurbines.df.sp, pch = 13, add = TRUE)





#___________________________________________________________
#
##### Create wind farm names dictionary  ####
#
#___________________________________________________________

# Load correspondance between farm key and names
df_windFarmNames <- read.csv("./Data/RawData/WindFarmNames.csv",h=T)

# Extract both keys and name
windFarmNames <- as.list(c(as.character(df_windFarmNames[,2])))
windFarmCodes <- c(as.character(df_windFarmNames[,1]) )
names(windFarmNames) <- windFarmCodes

# Create and save dictionary of the match between key and farm names
windFarmNames.dict <- dictionary(windFarmNames)
#save(windFarmNames.dict, file = "./Data/RawData/Causses_WindFarmNames_Dict.Rdata")

  
