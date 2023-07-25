#________________________________________________________________________________________________________________
##
##      PROJECT: Avoidance of wind turbines by griffon vultures
##
##      GOAL OF THE SCRIPT: Present the results of the avoidance analysis
##                          
##
##      CREDIT: Yohan Sassi (yoh.sassi22@gmail.com) & Benjamin Robira (benjamin.robira@normalesup.org)
##
#________________________________________________________________________________________________________________


rm(list = ls())

#Spatial
options("rgdal_show_exportToProj4_warnings"="none")

library(raster)
library(rgeos)
library(sp)
library(rgdal)
library(sf)
library(maptools)
library(raster)
library(shadow)
library(geosphere)
library(nngeo)

#Datetime
library(lubridate)

#Datahandling
library(dplyr)
library(zoo)
library(data.table)
library(zoom)
library(scales)
library(stringr)

#Plotting
library(ggplot2)
library(ggridges)
library(ggmap)
library(RColorBrewer)
library(lattice)
library(cowplot)
library(tmap)
library(viridis)
library(ggspatial)


## Load necessary documents
load("./Data/RawData/AllWindFarms_Causses.Rdata")
load("./Data/RawData/AllWindTurbines_Causses.Rdata")





#_________________________________________________________________
#
#### Tables and figures for main text -- ####
#
#_________________________________________________________________

#### Figure 1 - Panel topography and UD for wind farms  ####

elevationCaussesRaster <- raster::raster('./Data/RawData/Elevation_Causses.tif')

overlapUD_S1 <- raster::raster('./Outputs/Figures/overlapUD2_S1.tif')
overlapUD_S1@crs <- CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
overlapUD_S1 <- crop(overlapUD_S1, allWindFarms[[1]]$farm.sP)

overlapUD_S2 <- raster::raster('./Outputs/Figures/overlapUD2_S2.tif') 
overlapUD_S2@crs <- CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
overlapUD_S2 <- crop(overlapUD_S2, allWindFarms[[4]]$farm.sP)

overlapUD_S6 <- raster::raster('./Outputs/Figures/overlapUD2_S6.tif') 
overlapUD_S6@crs <- CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
overlapUD_S6 <- crop(overlapUD_S6, allWindFarms[[10]]$farm.sP)

overlapUD_S7 <- raster::raster('./Outputs/Figures/overlapUD2_S7.tif')
overlapUD_S7@crs <- CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
overlapUD_S7 <- crop(overlapUD_S7, allWindFarms[[11]]$farm.sP)


# Topography plot


# for S1 -- 

# Convert into df to be able to plot
f=1
topo_pts <- rasterToPoints(allWindFarms[[f]]$windFarmTopo, spatial = TRUE)
topo_df  <- data.frame(topo_pts)
turbines <- data.frame(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp)


# Topo plot
TopoS1 <- ggplot() +
  geom_raster(data = topo_df, 
              aes(x = x, y = y, fill = Elevation_Causses)) +
  scale_fill_gradientn(colours = terrain.colors(7)) +
  guides(fill = guide_colourbar(title = "Elevation")) + 
  geom_point(data = turbines,
             aes(x = longitude, y = latitude),
             pch = 13,
             size = 2)  +
          scale_x_continuous("", breaks = NULL, expand = c(0, 0)) +
          scale_y_continuous("", breaks = NULL, expand = c(0, 0)) +
          theme_light() +
          #ggtitle(paste("La Beaume - ",allWindFarms[[f]]$name,sep = ""))+ 
  theme(legend.position = "none") +
  annotation_scale()

# overlap UD
over_pts <- rasterToPoints(overlapUD_S1, spatial = TRUE)
over_df  <- data.frame(over_pts)
over_df <- over_df  %>% rename("pctsuperimposed_UD" = "layer")

overUDS1 <-  ggplot() +
  geom_raster(data = over_df, 
              aes(x = x, y = y, fill = pctsuperimposed_UD)) +
  scale_fill_viridis_c() +
  guides(fill = guide_colourbar(title = "% of superimposed UD")) +
  geom_point(data = turbines,
             aes(x = longitude, y = latitude),
             pch = 13,
             size = 2,
             colour = "white") +
  scale_x_continuous("", breaks = NULL,expand = c(0, 0)) +
  scale_y_continuous("", breaks = NULL,expand = c(0, 0)) +
  theme_light() +
  #ggtitle(paste("La Beaume - ",allWindFarms[[f]]$name,sep = ""))+ 
  theme(legend.position = "none")


forS1 <- plot_grid(TopoS1,overUDS1,
                   nrow = 1, ncol = 2)

# for S2 -- 

# Convert into df to be able to plot
f=4
topo_pts <- rasterToPoints(allWindFarms[[f]]$windFarmTopo, spatial = TRUE)
topo_df  <- data.frame(topo_pts)
turbines <- data.frame(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp)


# Topo plot
TopoS2 <- ggplot() +
  geom_raster(data = topo_df, 
              aes(x = x, y = y, fill = Elevation_Causses)) +
  scale_fill_gradientn(colours = terrain.colors(7)) +
  guides(fill = guide_colourbar(title = "Elevation")) + 
  geom_point(data = turbines,
             aes(x = longitude, y = latitude),
             pch = 13,
             size = 2)+
  scale_x_continuous("", breaks = NULL,expand = c(0, 0)) +
  scale_y_continuous("", breaks = NULL,expand = c(0, 0)) +
  theme_light() +
  #ggtitle(paste("Montfrech - ",allWindFarms[[f]]$name,sep = "")) + 
  theme(legend.position = "none")+
  annotation_scale()

# overlap UD
over_pts <- rasterToPoints(overlapUD_S2, spatial = TRUE)
over_df  <- data.frame(over_pts)
over_df <- over_df  %>% rename("pctsuperimposed_UD" = "layer")

overUDS2 <-  ggplot() +
  geom_raster(data = over_df, 
              aes(x = x, y = y, fill = pctsuperimposed_UD)) +
  scale_fill_viridis_c() +
  guides(fill = guide_colourbar(title = "% of superimposed UD")) +
  geom_point(data = turbines,
             aes(x = longitude, y = latitude),
             pch = 13,
             size = 2,
             colour = "white") +
  scale_x_continuous("", breaks = NULL,expand = c(0, 0)) +
  scale_y_continuous("", breaks = NULL,expand = c(0, 0)) +
  theme_light() +
  #ggtitle(paste("Montfrech - ",allWindFarms[[f]]$name,sep = "")) + 
  theme(legend.position = "none")


forS2 <- plot_grid(TopoS2,overUDS2,
                   nrow = 1, ncol = 2)

# for S6 -- 

# Convert into df to be able to plot
f=10
topo_pts <- rasterToPoints(allWindFarms[[f]]$windFarmTopo, spatial = TRUE)
topo_df  <- data.frame(topo_pts)
turbines <- data.frame(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp)


# Topo plot
TopoS6 <- ggplot() +
  geom_raster(data = topo_df, 
              aes(x = x, y = y, fill = Elevation_Causses)) +
  scale_fill_gradientn(colours = terrain.colors(7)) +
  guides(fill = guide_colourbar(title = "Elevation")) + 
  geom_point(data = turbines,
             aes(x = longitude, y = latitude),
             pch = 13,
             size = 2) +
  scale_x_continuous("",breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous("",breaks = NULL, expand = c(0, 0)) +
  theme_light() +
  #ggtitle(paste("Mas de Naï - ",allWindFarms[[f]]$name,sep = "")) + 
  theme(legend.position = "none")+
  annotation_scale()

# overlap UD
over_pts <- rasterToPoints(overlapUD_S6, spatial = TRUE)
over_df  <- data.frame(over_pts)
over_df <- over_df  %>% rename("pctsuperimposed_UD" = "layer")

overUDS6 <-  ggplot() +
  geom_raster(data = over_df, 
              aes(x = x, y = y, fill = pctsuperimposed_UD)) +
  scale_fill_viridis_c() +
  guides(fill = guide_colourbar(title = "% of superimposed UD")) +
  geom_point(data = turbines,
             aes(x = longitude, y = latitude),
             pch = 13,
             size = 2,
             colour = "white") +
  scale_x_continuous("", breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous("", breaks = NULL,expand = c(0, 0)) +
  theme_light() +
  #ggtitle(paste("Mas de Naï - ",allWindFarms[[f]]$name,sep = "")) + 
  theme(legend.position = "none")


forS6 <- plot_grid(TopoS6,overUDS6,
                   nrow = 1, ncol = 2)

# for S7 -- 

# Convert into df to be able to plot
f=11
topo_pts <- rasterToPoints(allWindFarms[[f]]$windFarmTopo, spatial = TRUE)
topo_df  <- data.frame(topo_pts)
turbines <- data.frame(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp)


# Topo plot
TopoS7 <- ggplot() +
  geom_raster(data = topo_df, 
              aes(x = x, y = y, fill = Elevation_Causses)) +
  scale_fill_gradientn(colours = terrain.colors(7)) +
  guides(fill = guide_colourbar(title = "Elevation")) + 
  geom_point(data = turbines,
             aes(x = longitude, y = latitude),
             pch = 13,
             size = 2) +
  scale_x_continuous("",breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous("",breaks = NULL, expand = c(0, 0)) +
  theme_light() +
  #ggtitle(paste("Saint Affrique - ",allWindFarms[[f]]$name,sep = "")) + 
  theme(legend.position = "none")+
  annotation_scale()

# overlap UD
over_pts <- rasterToPoints(overlapUD_S7, spatial = TRUE)
over_df  <- data.frame(over_pts)
over_df <- over_df  %>% rename("pctsuperimposed_UD" = "layer")

overUDS7 <-  ggplot() +
  geom_raster(data = over_df, 
              aes(x = x, y = y, fill = pctsuperimposed_UD)) +
  scale_fill_viridis_c() +
  guides(fill = guide_colourbar(title = "% of superimposed UD")) +
  geom_point(data = turbines,
             aes(x = longitude, y = latitude),
             pch = 13,
             size = 2,
             colour = "white") +
  scale_x_continuous("",breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous("",breaks = NULL, expand = c(0, 0)) +
  theme_light() +
  #ggtitle(paste("Saint Affrique - ",allWindFarms[[f]]$name,sep = "")) + 
  theme(legend.position = "none")

forS7 <- plot_grid(TopoS7,overUDS7,
                   nrow = 1, ncol = 2)


# Global pannel

plot_grid(NULL,
          forS1,
          forS2,
          forS6,
          forS7,
          nrow = 5, ncol = 1,
          rel_heights = c(0.12,1,1,1,1),
          labels = c("",
                     "La Beaume - S1",
                     "Montfrech - S2",
                     "Mas de Naï - S6",
                     "Saint Affrique - S7"),
          label_x = 0.34,
          label_y = 1.08
          #vjust = 1.5
          )




#### Figure 2 - Explanation of rotation method  ####

# Visualization of the rotation analysis
# Load specific dataset of the example wind farm: e.g. La Beaume - S1
load("./Data/ObsPaths/ObsPaths2_S1_Causses2019.Rdata")

f = 1
rotationAngle.v <- seq(from=0, to=350, by=10) #Angle of wind farm rotation (in degrees)
bufferSize.v <- seq(from=50, to=1000, by=50) # Size of buffer that will be used around wind turbine (in meters)


## PLOT OBS
#Initial background
plot(allWindFarms[[f]]$farmBuffer.sP, lty = "dotted")
plot(allWindFarms[[f]]$farmBarycentre.sp, cex = 1, add = T)


#Rotation of farm zone
j= 1 # Rotation 0°
farm.spatialPolygons_rotated <- maptools::elide(allWindFarms[[f]]$farm.sP, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)
plot(farm.spatialPolygons_rotated, cex = 10, add = TRUE) #col = alpha('orange',0.4)

#Rotation of wind turbines
dataCoordinatesTurbines.df.sp_rotated <- maptools::elide(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)

#Rotation of buffered wind turbine !!
i = 6 # 300m buffer
dataCoordinatesTurbines.df.sp_rotated_buffered <- gBuffer(dataCoordinatesTurbines.df.sp_rotated,  width=bufferSize.v[i], quadsegs = 100)
dataCoordinatesTurbines.df.sp_rotated_buffered@proj4string <- CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
plot(dataCoordinatesTurbines.df.sp_rotated_buffered, add=TRUE)

## Add example paths
bout_nb <- sample(unique(pointsInFarm@data$Bout),2,replace = F)
test_path.sp <- pointsInFarm[pointsInFarm@data$Bout %in% bout_nb,]

# Color inside and outside wind turbine buffer
test_pathIn.sp <- test_path.sp[which(over(test_path.sp, dataCoordinatesTurbines.df.sp_rotated_buffered)==1),]
plot(test_path.sp, pch = 20, col = alpha("green", 0.4), add = T)
plot(test_pathIn.sp, pch = 20, col = alpha("red", 0.4), add = T)
plot(dataCoordinatesTurbines.df.sp_rotated, pch = 13, cex = 1.3 , add=TRUE)



## PLOT OF ROTATION
#Initial background
plot(allWindFarms[[f]]$farmBuffer.sP, lty = "dotted")
plot(allWindFarms[[f]]$farmBarycentre.sp, cex = 1, add = T)

# Rotation of polygon
j= 1 # Rotation 10°
farm.spatialPolygons_rotated <- maptools::elide(allWindFarms[[f]]$farm.sP, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)
plot(farm.spatialPolygons_rotated, cex = 10, add = TRUE) 

j= 2 # Rotation 10°
farm.spatialPolygons_rotated <- maptools::elide(allWindFarms[[f]]$farm.sP, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)
plot(farm.spatialPolygons_rotated, cex = 10, add = TRUE) 

j= 3 # Rotation 20°
farm.spatialPolygons_rotated <- maptools::elide(allWindFarms[[f]]$farm.sP, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)
plot(farm.spatialPolygons_rotated, cex = 10, add = TRUE) 

j= 4 # Rotation 20°
farm.spatialPolygons_rotated <- maptools::elide(allWindFarms[[f]]$farm.sP, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)
plot(farm.spatialPolygons_rotated, cex = 10, add = TRUE) 

# Turbines
dataCoordinatesTurbines.df.sp_rotated <- maptools::elide(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp, rotate=rotationAngle.v[j], center=allWindFarms[[f]]$farmBarycentre)

#Rotation of buffered wind turbine !!
i = 6 # 300m buffer
dataCoordinatesTurbines.df.sp_rotated_buffered <- gBuffer(dataCoordinatesTurbines.df.sp_rotated,  width=bufferSize.v[i], quadsegs = 100)
dataCoordinatesTurbines.df.sp_rotated_buffered@proj4string <- CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
plot(dataCoordinatesTurbines.df.sp_rotated_buffered, add=TRUE)

## Add example paths
test_path.sp <- pointsInFarm[pointsInFarm@data$Bout %in% bout_nb,]

# Color inside and outside wind turbine buffer
test_pathIn.sp <- test_path.sp[which(over(test_path.sp, dataCoordinatesTurbines.df.sp_rotated_buffered)==1),]
plot(test_path.sp, pch = 20, col = alpha("green", 0.4), add = T)
plot(test_pathIn.sp, pch = 20, col = alpha("red", 0.4), add = T)
plot(dataCoordinatesTurbines.df.sp_rotated, pch = 13, cex = 1.3 , add=TRUE)



## PLOT OF % VALUES DISTRIBUTION (SIMULATED DATA)
# Prepare a simulated data set for illustration
bufferSize.v <- seq(from=50, to=1000, by=50) # Size of buffer that will be used around wind turbine (in meters)
simuData.df <- data.frame(buffer = numeric(0), pctFix = numeric(0))
c = 15

for (i in 1:length(bufferSize.v)){
  
  # Simulation of percentage of points in buffer
  testData <- abs(rnorm(36, c, 5))
  
  # subData
  sub <- cbind(rep(bufferSize.v[i],36),
               testData)
  
  # Add to the following
  simuData.df <- rbind(simuData.df,
                       sub)
  # Counter
  c = c + 3
}

colnames(simuData.df) <- c("buffer","pctFixes")


# Visual plot
ggplot(simuData.df, 
       aes(x = pctFixes, y = as.factor(buffer), 
           fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    quantiles = 0.05,
    scale = 1.2
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0"),
    labels = c("(0, 0.05]", "(0.05, 1]")
  ) +
  scale_x_continuous("Percentage of locations in buffer") +
  scale_y_discrete("Buffer size [m]", breaks = seq(from=100, to=1000, by=100)) +
  theme_light() +
  theme(legend.position = c(.90, .1),
        legend.text = element_text(size=17),
        legend.title = element_text(size=20),
        axis.text=element_text(size=17),
        axis.title=element_text(size=20)) + 
  coord_flip(expand = T) 




#### Figure 3 - Plot the rotation result for Montfrech wind farm  ####
load("./Outputs/AnalysisResults/RotationAnalysisResult2_allyears_S2.Rdata")

f = 4 # Wind farm number

# Load img for plot illustration
img <- readPNG("./Outputs/Figures/inTurbine.png")

# Plot of the results for below paths (B)
resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

below <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  coord_cartesian(xlim = c(0, 1010), ylim = c(1, 36)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous("Position of the observed 
proportion of locations", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("Buffer size [m]", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle("Within rotor swept zone") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  annotation_raster(img, xmin = 85, xmax = 217, ymin = 24.5, ymax = 32)


# Plot of the results for above paths (A)
# Load img for plot illustration
img1 <- readPNG("./Outputs/Figures/aboveTurbine.png")

resAbove.df <- resultsRotationPvalueAbove.df[,2:4]

above <- ggplot(resAbove.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  coord_cartesian(xlim = c(0, 1010), ylim = c(1, 36)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous("Position of the observed 
proportion of locations", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle("Above rotor swept zone") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  annotation_raster(img1, xmin = 85, xmax = 217, ymin = 24.7, ymax = 38.4)


# Panel plot
# resPlotS2 <- plot_grid(above, below, ncol = 1, nrow = 2, labels = c("(A)","(B)"))
# 
# # Save plot
# resfactor = 3
# dev.copy(png, paste("./Outputs/AnalysisResults/MontfrechRotaRes.png", sep=""),res = 72*resfactor, height=1200*resfactor, width=500*resfactor)
# dev.off()



## Plot of the raster with buffered wind farm / topo and UD (C)
f=4

overlapUD_S2 <- raster::raster('./Outputs/Figures/overlapUD_S2.tif') 
overlapUD_S2@crs <- CRS(paste("+proj=utm","+zone=31","+ellps=WGS84", "+datum=WGS84", "+units=m", "+towgs84:0,0,0", sep=" "))
overlapUD_S2 <- crop(overlapUD_S2, allWindFarms[[4]]$farm.sP)

# Extract middle altitude (what we visually detect as threshold)
windFarmTopo <- allWindFarms[[f]]$windFarmTopo
windFarmTopo[windFarmTopo@data@values > ((max(windFarmTopo@data@values) + min(windFarmTopo@data@values)) / 2)] <- NA

# Extract the ridges
canyonRidge <-  boundaries(windFarmTopo, type = "inner", classes = F)
canyonRidge[canyonRidge@data@values != 1] <- NA

# Take a buffer around the ridges (200m) & resize to match the farm geofence
canyonRidgeBuff <- raster::buffer(canyonRidge, width=300, doEdge=FALSE, dissolve = TRUE)
canyonRidgeBuff[canyonRidgeBuff@data@values != 1] <- 0

canyonRidgeBuff_pts <- rasterToPoints(canyonRidgeBuff , spatial = TRUE)
canyonRidgeBuff_df  <- data.frame(canyonRidgeBuff_pts )

canyonRidge_pts <- rasterToPoints(canyonRidge , spatial = TRUE)
canyonRidge_df  <- data.frame(canyonRidge_pts)



topo_pts <- rasterToPoints(allWindFarms[[f]]$windFarmTopo, spatial = TRUE)
topo_df  <- data.frame(topo_pts)
turbines <- data.frame(allWindTurbines[[f]]$dataCoordinatesTurbines.df.sp)

over_pts <- rasterToPoints(overlapUD_S2, spatial = TRUE)
over_df  <- data.frame(over_pts)
over_df <- over_df  %>% rename("pctsuperimposed_UD" = "layer")

topo_df$Elevation_Causses_norm <- (topo_df$Elevation_Causses-min(topo_df$Elevation_Causses))/diff(range(topo_df$Elevation_Causses))
over_df$pctsuperimposed_UD_norm <- (over_df$pctsuperimposed_UD-min(over_df$pctsuperimposed_UD))/diff(range(over_df$pctsuperimposed_UD))

turbineBuff <- gBuffer(allWindTurbines$S2$dataCoordinatesTurbines.df.sp, width = 450, quadsegs = 100)

## Plot the overlap
topover <-  ggplot() +
  geom_raster(data = topo_df, aes(x = x, y = y, fill = Elevation_Causses_norm)) +
  scale_fill_gradientn(colours = terrain.colors(7)) +
  geom_tile(data = canyonRidge_df, aes(x = x, y = y, alpha=0.01)) +
  #geom_tile(data = canyonRidgeBuff_df, aes(x = x, y = y, alpha=0.01)) +
  annotate(geom="raster", x=over_df$x, y=over_df$y, alpha=0.6,
           fill = scales::colour_ramp(c("transparent","transparent","blue","red"))(over_df$pctsuperimposed_UD_norm)) +
  geom_point(data = turbines,
             aes(x = longitude, y = latitude),
             pch = 13,
             size = 2,
             colour = "black") +
  geom_point(data = turbineBuff,
             aes(x = turbineBuff@polygons[[1]]@Polygons[[1]]@coords[,1],
                 y = turbineBuff@polygons[[1]]@Polygons[[1]]@coords[,2]),
             size = 0.05) +
  scale_x_continuous("", breaks = NULL,expand = c(0.1, 0.1)) +
  scale_y_continuous("", breaks = NULL,expand = c(0.1, 0.1)) +
  theme_void() +
  theme(legend.position = "none")



# Plot of the optimization procedure (D)

# Optimization map
load("./Outputs/Simulations/OptimABM_S2_10_3.Rdata")
minAngleToExplore <- 1
maxAngleToExplore <- 14 # Maximum angle vulture reach in thermals
minDistanceToExplore <- 0
maxDistanceToExplore <- 1000 #if in m for instance, adding 1000m. The larger you put, the longer in time to compute

# FitQuality1 -> look for the fit of each simulations to the percentage obtained at 450m on observed data
toTestParameters$fitQuality1 <- toTestParameters$fitValue1/
  quantile(toTestParameters$fitValue1[toTestParameters$valueMapAvoidanceDistance < 0.01], 0.95, na.rm = T)  # <0.01 because == 0 can lead to mis-subsetting (double issue)
# division by the percentage of points obtained by simulation of a ballistic movement (d = 0 & alpha doesn't matter)

toTestParameters$fitQuality1[toTestParameters$fitQuality1>1] <- NA # Filter out all conditions that give worst result than no avoidance (ballistic movement)
toTestParameters$fitQuality1 <- 1-toTestParameters$fitQuality1 # Reverse scale so best fit = 1

# FitQuality2 -> look for the fit of each simulation with the same buffer in the observed data
load("./Outputs/Simulations/nullSimulTracks.Rdata")

##Calculate % of points for each buffer for null simulations
distanceVec <- seq(from = 50, to = maxDistanceToExplore, by = 50)
percentInBufferNoAvoidance <- lapply(distanceVec, function(x){
  nrow(pointsInFarmCheck[!is.na(sp::over(SpatialPoints(pointsInFarmCheck[,1:2], 
                                                       proj4string = CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")), 
                                         gBuffer(SpatialPoints(allWindTurbines[[4]]$dataCoordinatesTurbines.df.sp, 
                                                               proj4string = CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")),
                                                 width=x, quadsegs = 100))),]) / 
    nrow(pointsInFarmCheck)})
percentInBufferNoAvoidance <- cbind(distanceVec, unlist(percentInBufferNoAvoidance)) %>% as.data.frame()
colnames(percentInBufferNoAvoidance) <- c("distance", "percentageNoAvoidance")


toTestParameters <- toTestParameters %>% 
  left_join(
    percentInBufferNoAvoidance,
    by = c("valueMapAvoidanceDistance" = "distance")
  ) %>% 
  mutate(fitQuality2 = fitValue2/(percentPointsInBufferBelow - percentageNoAvoidance)**2) %>% 
  ungroup()

toTestParameters$fitQuality2[toTestParameters$fitQuality2>1] <- NA # Filter out all conditions that give worst result than no avoidance (ballistic movement)
toTestParameters$fitQuality2 <- 1-toTestParameters$fitQuality2 # Reverse scale so best fit = 1

#Other fit qualities
toTestParameters <- toTestParameters %>% 
  mutate(
    fitQualityv1 = fitValuev1/(percentInBufferNoAvoidance[1,2] - vectorObservedPercent[1])**2,
    fitQualityv2 = fitValuev2/(percentInBufferNoAvoidance[2,2] - vectorObservedPercent[2])**2,
    fitQualityv3 = fitValuev3/(percentInBufferNoAvoidance[3,2] - vectorObservedPercent[3])**2,
    fitQualityv4 = fitValuev4/(percentInBufferNoAvoidance[4,2] - vectorObservedPercent[4])**2,
    fitQualityv5 = fitValuev5/(percentInBufferNoAvoidance[5,2] - vectorObservedPercent[5])**2,
    fitQualityv6 = fitValuev6/(percentInBufferNoAvoidance[6,2] - vectorObservedPercent[6])**2,
    fitQualityv7 = fitValuev7/(percentInBufferNoAvoidance[7,2] - vectorObservedPercent[7])**2,
    fitQualityv8 = fitValuev8/(percentInBufferNoAvoidance[8,2] - vectorObservedPercent[8])**2,
    fitQualityv9 = fitValuev9/(percentInBufferNoAvoidance[9,2] - vectorObservedPercent[9])**2
  ) %>% 
  mutate(
    fitQualityv1 = ifelse(fitQualityv1>1,NA,1-fitQualityv1),
    fitQualityv2 = ifelse(fitQualityv2>1,NA,1-fitQualityv2),
    fitQualityv3 = ifelse(fitQualityv3>1,NA,1-fitQualityv3),
    fitQualityv4 = ifelse(fitQualityv4>1,NA,1-fitQualityv4),
    fitQualityv5 = ifelse(fitQualityv5>1,NA,1-fitQualityv5),
    fitQualityv6 = ifelse(fitQualityv6>1,NA,1-fitQualityv6),
    fitQualityv7 = ifelse(fitQualityv7>1,NA,1-fitQualityv7),
    fitQualityv8 = ifelse(fitQualityv8>1,NA,1-fitQualityv8),
    fitQualityv9 = ifelse(fitQualityv9>1,NA,1-fitQualityv9)
  )


#Extract best fit value for all
valueFit <- c()
parameters <- c()
distanceInterest <- c()
for(value in 1:9){
  valueOfInterest <- toTestParameters[,19+value]
  valueOfInterest[valueOfInterest>1] <- NA
  valueOfInterest <- 1-valueOfInterest
  toReturn <- which(valueOfInterest > 0.95)
  
  #Save results
  valueFit <- c(valueFit, valueOfInterest[toReturn])
  parameters <- rbind(parameters, toTestParameters[toReturn,1:2])
  distanceInterest <- c(distanceInterest, rep(percentInBufferNoAvoidance$distance[value], times = length(toReturn)))
  
}

dfForPlotBestFit <- cbind(valueFit, parameters, distanceInterest)
colnames(dfForPlotBestFit) <- c("Fit_quality", "Angle", "Perception_range", "Distance_to_turbine")

# save(dfForPlotBestFit,
#      file="./Outputs/Simulations/dfForPlotBestFit.Rdata")


# Plot
load("./Outputs/Simulations/dfForPlotBestFit.Rdata")

resSimul <- ggplot(dfForPlotBestFit %>% filter(Angle<=14), aes(Angle, Perception_range, color = as.factor(Distance_to_turbine), label = as.factor(Distance_to_turbine))) + #shape = as.factor(Fit_class)
  theme_light() +
  ggtitle("") +
  scale_color_viridis(discrete = TRUE, name = "Buffer size [m]") +
  #scale_color_manual(values = cbbPalette, name = "Buffer size [m]") + 
  #geom_text(hjust=-0.5, vjust=-0.5) +
  geom_label_repel(aes(label = as.factor(Distance_to_turbine), hjust=0.5, vjust=0.5),
                   nudge_x = .15,
                   nudge_y = 1,
                   min.segment.length = 0,
                   box.padding = 0.35, 
                   point.padding = 0.5,
                   label.size = NA,
                   segment.color = 'grey50',
                   show.legend = FALSE) + 
  geom_point(size = 2.5) +
  #scale_shape_discrete(name = "Fit quality", labels =c("]0.950,0.975]", "]0.0.975,1]")) + #"]0.900,0.933]", "]0.933,0.966]","]0.966,1]"
  scale_x_continuous(name = "Turning angle [°]", limits = c(1,14), breaks = seq(from=1, to=14, by=2)) + #scales::pretty_breaks(n = 8)
  scale_y_continuous(name = "Perceptual range [m]", limits = c(0,800), breaks = scales::pretty_breaks(n = 8)) +
  theme(plot.margin = unit(c(0.1,1.4,0.15,0.15), "cm"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=14),
        legend.direction = "horizontal",
        legend.position = c(0.65,0.85),
        legend.box.background = element_rect(color = NA,fill="white"),
        legend.spacing.y = unit(-0.2, "cm"))


resSimul

# Save the plot
resfactor = 3
dev.copy(png, paste("./Outputs/Simulations/testoptimMap_S2_10_3.png", sep=""),res = 72*resfactor, height=450*resfactor, width=600*resfactor)
dev.off()

# plot_grid(resPlotS2, resSimul,
#           nrow = 1, ncol = 2,
#           labels = c("","(C)"))
# 
# # Save the plot
# resfactor = 3
# dev.copy(png, paste("./Outputs/Simulations/optimMap_S2.png", sep=""),res = 72*resfactor, height=800*resfactor, width=500*resfactor)
# dev.off()


# Plot for the 4 graphs
plot_grid(above,topover, 
          below, resSimul,
          nrow = 2, ncol = 2,
          labels = c("(A)","(C)","(B)","(D)"))


# Save the plot
resfactor = 3
dev.copy(png, paste("./Fig3_V4.png", sep=""),res = 72*resfactor, height=800*resfactor, width=500*resfactor)
dev.off()




#_________________________________________________________________
#
#### Tables and figures for supplementary materials  ####
#
#_________________________________________________________________


#### Figure S1 - Distance to center of the colony ####

## see ys_03_HomeRanges script for dataset construction
load("./Outputs/FilteringSummary/allIndMaxNSDCausses.Rdata")

#Remove Beaufixe and Courage, as they didn't crossed any wind farm we remove them from DV estimates for simplicity in sample size
allIndMaxDist <- allIndMaxDist %>% 
  filter(indName != "Beaufixe",
         indName != "Courage")

# Plot and save the SDN max per day for all individuals
plotAll <- ggplot(allIndMaxDist, aes(indAllDates,maxDist.v/1000, color = indName)) +
  geom_line() +
  guides(col= guide_legend(title= "Individuals' name:")) +
  xlab("Dates") +
  scale_y_continuous("Max distance per day from colony center [m]" , breaks = seq(0, 1000, by = 100)) +
  geom_hline(yintercept=55, linetype="dashed", color = "black", linewidth=0.5) +
  theme_light()

print(plotAll) # Show graph

# ggsave(paste("./Outputs/Figures/FigS1_AllindMaxSNDCausses.png",sep=""),
#        plotAll,
#        dpi = 300,
#        width = 10)


# Mean max distnace reached (Clarence and Rostand removed from the calculation as they are settle far from this colony)
want <- allIndMaxDist %>% 
  filter(indName != "Clarence",
         indName != "Rostand")  %>%
  group_by(indName) %>% 
  summarise_at(vars(maxDist.v), list(mean = mean)) 

mean(want$mean)
sd(want$mean)




#### Figure S2 - Panel plot of rotation results on simulated tracks for all operating farms  ####

directionStepAvoidance = seq(from=0, to=14, by=1) #Turning angle in degree when the avoidance area is reached - adapted to stepLength
distanceAvoidance = seq(from=50, to=1000, by=50) # Distance minimal at which it should avoid a turbine (in meters)

a = c(1,2,6)  # Control the avoidance angle
b = c(9,1) # Control the distance of avoidance ## !!!! SHOULD BE CHANGED FOR 9 !!!!!


# Results for S1
f = 1

load(list.files("./Outputs/Simulations/RotaRes_S1", paste("ABM_RotaRes_dir",directionStepAvoidance[a[1]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

# Plot of the results for simulations
resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS1_1 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("Position of the observed
proportion of locations", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #ggtitle(paste("La Beaume - Simulations Alpha = ",directionStepAvoidance[a[1]],", dist = ",distanceAvoidance[b])) +
  ggtitle(expression(paste("", alpha == 0, ", d = 450"))) +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S1", paste("ABM_RotaRes_dir",directionStepAvoidance[a[2]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS1_2 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("
  # Alpha = ",directionStepAvoidance[a[2]],", dist = ",distanceAvoidance[b])) +
  ggtitle(expression(paste(alpha == 1, ", d = 450"))) +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S1", paste("ABM_RotaRes_dir",directionStepAvoidance[a[3]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS1_3 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("
  # Alpha = ",directionStepAvoidance[a[3]],", dist = ",distanceAvoidance[b])) +
  ggtitle(expression(paste(alpha == 5, ", d = 450"))) +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S1", paste("ABM_RotaRes_dir",directionStepAvoidance[a[3]],"_dist",distanceAvoidance[b[2]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS1_4 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("
  # Alpha = ",directionStepAvoidance[a[3]],", dist = ",distanceAvoidance[b])) +
  ggtitle(expression(paste(alpha == 5, ", d = 50"))) +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
#---


# Results for S2
f = 4

load(list.files("./Outputs/Simulations/RotaRes_S2", paste("ABM_RotaRes_dir",directionStepAvoidance[a[1]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

# Plot of the results for simulations
resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS2_1 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("Position of the observed
proportion of locations", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("Montfrech - Simulations","
  # Alpha = ",directionStepAvoidance[a[1]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")


# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S2", paste("ABM_RotaRes_dir",directionStepAvoidance[a[2]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS2_2 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("
  # Alpha = ",directionStepAvoidance[a[2]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") + 
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")

# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S2", paste("ABM_RotaRes_dir",directionStepAvoidance[a[3]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS2_3 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("
  # Alpha = ",directionStepAvoidance[a[3]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")


# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S2", paste("ABM_RotaRes_dir",directionStepAvoidance[a[3]],"_dist",distanceAvoidance[b[2]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS2_4 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("
  # Alpha = ",directionStepAvoidance[a[3]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")
#---



# Results for S6
f = 10

load(list.files("./Outputs/Simulations/RotaRes_S6", paste("ABM_RotaRes_dir",directionStepAvoidance[a[1]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

# Plot of the results for simulations
resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS6_1 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("Position of the observed
proportion of locations", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200))+
  #   ggtitle(paste("Mas de Naï - Simulations","
  # Alpha = ",directionStepAvoidance[a[1]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")


# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S6", paste("ABM_RotaRes_dir",directionStepAvoidance[a[2]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS6_2 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("
  # Alpha = ",directionStepAvoidance[a[2]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")

# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S6", paste("ABM_RotaRes_dir",directionStepAvoidance[a[3]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS6_3 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("
  # Alpha = ",directionStepAvoidance[a[3]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")

# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S6", paste("ABM_RotaRes_dir",directionStepAvoidance[a[3]],"_dist",distanceAvoidance[b[2]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS6_4 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  #   ggtitle(paste("
  # Alpha = ",directionStepAvoidance[a[3]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")
#---



# Results for S7
f = 11

load(list.files("./Outputs/Simulations/RotaRes_S7", paste("ABM_RotaRes_dir",directionStepAvoidance[a[1]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

# Plot of the results for simulations
resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS7_1 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("Position of the observed
proportion of locations", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("Buffer size [m]", breaks = seq(from=0, to=1010, by=200)) +
  #ggtitle(paste("Saint Affrique - Simulations","Alpha = ",directionStepAvoidance[a[1]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")


# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S7", paste("ABM_RotaRes_dir",directionStepAvoidance[a[2]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS7_2 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("Buffer size [m]", breaks = seq(from=0, to=1010, by=200)) +
  #ggtitle(paste("Alpha = ",directionStepAvoidance[a[2]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")

# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S7", paste("ABM_RotaRes_dir",directionStepAvoidance[a[3]],"_dist",distanceAvoidance[b[1]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS7_3 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("Buffer size [m]", breaks = seq(from=0, to=1010, by=200)) +
  #ggtitle(paste(expression(~ alpha = 1), directionStepAvoidance[a[3]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")


# Plot of the results for simulation

load(list.files("./Outputs/Simulations/RotaRes_S7", paste("ABM_RotaRes_dir",directionStepAvoidance[a[3]],"_dist",distanceAvoidance[b[2]],".Rdata",sep="") , all.files = F, full.names = T))

resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

plotS7_4 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("Buffer size [m]", breaks = seq(from=0, to=1010, by=200)) +
  #ggtitle(paste(expression(~ alpha = 1), directionStepAvoidance[a[3]],", dist = ",distanceAvoidance[b])) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none")
#---



### Panel plot for below and above paths for operating wind farms
plot_grid(plotS1_1, plotS1_2, plotS1_3, plotS1_4,
          plotS2_1, plotS2_2, plotS2_3, plotS2_4,
          plotS6_1, plotS6_2, plotS6_3, plotS6_4,
          plotS7_1, plotS7_2, plotS7_3, plotS7_4,
          ncol = 4, nrow = 4,
          labels = c("(A)","","","",
                     "(B)","","","",
                     "(C)","","","",
                     "(D)","","","")) 


# resfactor = 3
# dev.copy(png, paste("./Outputs/AnalysisResults/DistriPanelPlot_SimuOWF_2.png", sep=""),res = 72*resfactor, height=950*resfactor, width=770*resfactor)
# dev.off()






#### Figure S3 - Panel plot of rotation results on all operating wind farms ####
year <- c("2019", "2020", "2021", "allyears") #Combine the years we have data on 
y = 4 # Choose the year you want the graphs on


# Results for S1
f = 1
load(paste("./Outputs/AnalysisResults/RotationAnalysisResult2_", year[y],"_",allWindFarms[[f]]$name,".Rdata", sep = ""))

# Plot of the results for below paths
resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

below_S1 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  coord_cartesian(xlim = c(0, 1010), ylim = c(1, 36)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle("Within rotor swept zone") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5))


# Plot of the results for above paths
resAbove.df <- resultsRotationPvalueAbove.df[,2:4]

above_S1 <- ggplot(resAbove.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  coord_cartesian(xlim = c(0, 1010), ylim = c(1, 36)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous("Position of the observed 
proportion of locations", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle("Above rotor swept zone") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5))
#---


# Results for S6
f = 10
load(paste("./Outputs/AnalysisResults/RotationAnalysisResult2_", year[y],"_",allWindFarms[[f]]$name,".Rdata", sep = ""))


# Plot of the results for below paths
resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

below_S6 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  coord_cartesian(xlim = c(0, 1010), ylim = c(1, 36)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=14))


# Plot of the results for above paths
resAbove.df <- resultsRotationPvalueAbove.df[,2:4]

above_S6 <- ggplot(resAbove.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  coord_cartesian(xlim = c(0, 1010), ylim = c(1, 36)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous("Position of the observed 
proportion of locations", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=14))
#---



# Results for S7
f = 11
load(paste("./Outputs/AnalysisResults/RotationAnalysisResult2_", year[y],"_",allWindFarms[[f]]$name,".Rdata", sep = ""))


# Plot of the results for below paths
resBelow.df <- resultsRotationPvalueBelow.df[,2:4]

below_S7 <- ggplot(resBelow.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  coord_cartesian(xlim = c(0, 1010), ylim = c(1, 36)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous("", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("Buffer size [m]", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=14))


# Plot of the results for above paths
resAbove.df <- resultsRotationPvalueAbove.df[,2:4]

above_S7 <- ggplot(resAbove.df, aes(x = as.integer(bufferAroundTurbineMeter), y = as.integer(posObsValue))) + 
  coord_cartesian(xlim = c(0, 1010), ylim = c(1, 36)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous("Position of the observed 
proportion of locations", limits = c(0,36), breaks = seq(from=0, to=36, by=5)) +
  scale_x_continuous("Buffer size [m]", breaks = seq(from=0, to=1010, by=200)) +
  ggtitle("") +
  theme_light() +
  geom_rect(aes(xmin = 0 - 0.5, xmax = 1000 + 5, ymin = 0 + 0.4, ymax = 2 - 0.4, fill = "red"), color = "red",
            linetype=0 , alpha = 0.01, ) +
  geom_point() + 
  theme(legend.position = "none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=14))
#---



### Panel plot for below and above paths for operating wind farms
plot_grid(above_S1, below_S1, 
          above_S6, below_S6, 
          above_S7, below_S7,
          ncol = 2, nrow = 3,
          labels = c("(A)","",
                     "(B)","",
                     "(C)",""))


# resfactor = 3
# dev.copy(png, paste("./Outputs/AnalysisResults/AllFarmsRotaRes.png", sep=""),res = 72*resfactor, height=800*resfactor, width=550*resfactor)
# dev.off()







#### Table S1 - Summary vultures' data  ####

# Load information data for these individuals
vultData.df <- read.csv("./Data/RawData/Ind_Data_GVCausses.csv",h=T, sep = ";")
vultData.df <- vultData.df[-nrow(vultData.df),] # Remove last row because of import problem

# Estimate the day they have been followed on
vultData.df$date_diff <- as.Date(as.character(vultData.df$LastLocDay), format="%Y-%m-%d")-
  as.Date(as.character(vultData.df$FirstLocDay), format="%Y-%m-%d")

# Remove Beaufixe and Courage
vultData.df <- vultData.df[-c(2,7),]

# Extract Individuals name
individualName <- c()

for (i in 1:nrow(vultData.df)){
  
  # Name
  hyfenindices <- str_locate_all(pattern ='-', vultData.df$Ind_ID[i]) # locate the - in the name
  name <- substr(vultData.df$Ind_ID[i], 
                 as.integer(hyfenindices[[1]][3,1])+1, 
                 as.integer(hyfenindices[[1]][4,1])-1)
  individualName[i] <- name
}

# Combine both to start the summary df
SummaryVultData.df <- as.data.frame(cbind(individualName, vultData.df$date_diff))
SummaryVultData_atRisk <- as.data.frame(cbind(individualName, vultData.df$date_diff))

# Loop on farms
farmnb <- c(1,4,10,11) # Wind farm of interest
c=2

for (f in farmnb){
  c = c+1 #+1 to new column
  
  # Look for appropriate files
  allFiles <- list.files("./Data/ObsPaths", paste("ObsPaths2_",allWindFarms[[f]]$name,"_Causses",sep="") , all.files = F, full.names = T)
  
  # Combine them into a full file
  combine = T
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
  
  # Count number of track per ind
  summaryBout <- pointsInFarm@data %>% 
    group_by(ID) %>% 
    summarise(CountTot = length(unique(uniqueBout)))
  
  # Count number of track per ind that crossed the threshold
  summaryBoutAtRisk <- pointsInFarm@data %>% 
    group_by(ID) %>% 
    filter(.,aboveThresholdAltitude == 0) %>%
    summarise(CountTot = length(unique(uniqueBout)))
  
  # Fill the df
  for (n in 1:nrow(SummaryVultData.df)){
    name <- SummaryVultData.df$individualName[n]
    
    allname <- c()
    for (i in 1:nrow(summaryBout)){
      hyfenindices <- str_locate_all(pattern ='-', summaryBout$ID[i]) # locate the - in the name
      name2 <- substr(summaryBout$ID[i], 
                      as.integer(hyfenindices[[1]][3,1])+1, 
                      as.integer(hyfenindices[[1]][4,1])-1)
      allname[i] <- name2
    }
    
    if (name %in% allname) {
      SummaryVultData.df[n,c] <- as.integer(summaryBout[which(str_detect(pattern = name, summaryBout$ID) == TRUE),2])
      SummaryVultData_atRisk[n,c] <- as.integer(summaryBoutAtRisk[which(str_detect(pattern = name, summaryBoutAtRisk$ID) == TRUE),2])
    } else {
      SummaryVultData.df[n,c] <- "NA"
      SummaryVultData_atRisk[n,c] <- "NA"
    }
  }
}




# Reshape table
SummaryVultData.df$V3 <- str_replace_all(SummaryVultData.df$V3, "NA", "-")
SummaryVultData.df$V4 <- str_replace_all(SummaryVultData.df$V4, "NA", "-")
SummaryVultData.df$V5 <- str_replace_all(SummaryVultData.df$V5, "NA", "-")
SummaryVultData.df$V6 <- str_replace_all(SummaryVultData.df$V6, "NA", "-")

SummaryVultData_atRisk$V3 <- str_replace_all(SummaryVultData_atRisk$V3, "NA", "-")
SummaryVultData_atRisk$V4 <- str_replace_all(SummaryVultData_atRisk$V4, "NA", "-")
SummaryVultData_atRisk$V5 <- str_replace_all(SummaryVultData_atRisk$V5, "NA", "-")
SummaryVultData_atRisk$V6 <- str_replace_all(SummaryVultData_atRisk$V6, "NA", "-")

colnames(SummaryVultData.df) <- c("Vulture names",
                                  "Nb days of tracking",
                                  "nb paths in La Beaume",
                                  "nb paths in Montfrech",
                                  "nb paths in Mas de Naï",
                                  "nb paths in Saint Affrique")

colnames(SummaryVultData_atRisk) <- c("Vulture names",
                                      "Nb days of tracking",
                                      "nb paths in La Beaume",
                                      "nb paths in Montfrech",
                                      "nb paths in Mas de Naï",
                                      "nb paths in Saint Affrique")

# Save the summary table
write.csv(SummaryVultData.df,
          file = "./Outputs/Figures/SummaryVultData.csv")

write.csv(SummaryVultData_atRisk,
          file = "./Outputs/Figures/SummaryVultData_atRisk.csv")



#### Table S2 - Summary wind farms data  ####
load("./Data/RawData/AllWindFarms_Causses.Rdata")
load("./Data/RawData/AllWindTurbines_Causses.Rdata")

farmnb <- c(1,4,10,11) # Wind farm of interest
c=0 # Counter
r=0 # counter
pctatRisk <- c(0,0,0,0)
InfoPaths <- data.frame("farm_name" = character(0), 
                        "totPaths" = integer(0), 
                        "mixedBout" = integer(0), 
                        "totFix" = integer(0), 
                        "fixAbove" = integer(0)) # Df to save info

# loop to work on filtered data
for (f in farmnb){
  r = r+1
  
  # Look for appropriate files
  allFiles <- list.files("./Data/ObsPaths", paste("ObsPaths2_",allWindFarms[[f]]$name,"_Causses",sep="") , all.files = F, full.names = T)
  
  combine = T
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
  
  
  summaryBout <- pointsInFarm@data %>% 
    group_by(Bout) %>% 
    summarise(
      CountAboveThreshold = sum(as.numeric(as.character(aboveThresholdAltitude))),
      CountTot = n()
    )
  
  summaryBoutAtRisk <- pointsInFarm@data %>% 
    group_by(ID) %>% 
    filter(.,aboveThresholdAltitude == 0) %>%
    summarise(CountTot = length(unique(Bout)))
  
  pctatRisk[r] <- sum(summaryBoutAtRisk$CountTot)/length(unique(pointsInFarm$Bout))*100
  
  
  # Loop on the 3 years
  for (j in 1:length(allFiles)){
    c = c+1 # Counter
    
    # Load iteratively the data
    load(allFiles[j])
    
    # Take info about bouts
    summaryBout <- pointsInFarm@data %>%
      group_by(Bout) %>%
      summarise(
        CountAboveThreshold = sum(as.numeric(as.character(aboveThresholdAltitude))),
        CountTot = n()
      )
    
    mixedBout <- summaryBout$Bout[summaryBout$CountAboveThreshold != 0 &
                                    summaryBout$CountAboveThreshold != summaryBout$CountTot] # number of mixed path (i.e. that cross the altitude threshold)
    
    
    # Extract needed data
    InfoPaths[c,] <- c(allWindFarms[[f]]$name, 
                       length(unique(pointsInFarm$Bout)),
                       length(mixedBout),
                       nrow(pointsInFarm),
                       nrow(pointsInFarm[which(pointsInFarm$aboveThresholdAltitude == 1),]))
  }
}


## Data on farms
farmName <- c("La Beaume - S1", "Montfrech - S2", "Mas de Naï - S6", "Saint Affrique - S7")

nbTurbines <- c(length(allWindTurbines$S1$dataCoordinatesTurbines.df.sp),
                length(allWindTurbines$S2$dataCoordinatesTurbines.df.sp),
                length(allWindTurbines$S6$dataCoordinatesTurbines.df.sp),
                length(allWindTurbines$S7$dataCoordinatesTurbines.df.sp))

maxRotor <- c(max(unique(allWindTurbines$S1$dataCoordinatesTurbines.df.sp$ht_max)),
              max(unique(allWindTurbines$S2$dataCoordinatesTurbines.df.sp$ht_max)),
              max(unique(allWindTurbines$S6$dataCoordinatesTurbines.df.sp$ht_max)),
              max(unique(allWindTurbines$S7$dataCoordinatesTurbines.df.sp$ht_max)))

minRotor <- c(max(unique(allWindTurbines$S1$dataCoordinatesTurbines.df.sp$ht_max)) - unique(allWindTurbines$S1$dataCoordinatesTurbines.df.sp$diam_rotor),
              max(unique(allWindTurbines$S2$dataCoordinatesTurbines.df.sp$ht_max)) - unique(allWindTurbines$S2$dataCoordinatesTurbines.df.sp$diam_rotor),
              max(unique(allWindTurbines$S6$dataCoordinatesTurbines.df.sp$ht_max)) - unique(allWindTurbines$S6$dataCoordinatesTurbines.df.sp$diam_rotor),
              max(unique(allWindTurbines$S7$dataCoordinatesTurbines.df.sp$ht_max)) - unique(allWindTurbines$S7$dataCoordinatesTurbines.df.sp$diam_rotor))

totPaths <- c(sum(as.numeric(InfoPaths$totPaths[which(InfoPaths$farm_name == "S1")])),
              sum(as.numeric(InfoPaths$totPaths[which(InfoPaths$farm_name == "S2")])),
              sum(as.numeric(InfoPaths$totPaths[which(InfoPaths$farm_name == "S6")])),
              sum(as.numeric(InfoPaths$totPaths[which(InfoPaths$farm_name == "S7")])))

mixedBout <- c(pctatRisk[1],pctatRisk[2],pctatRisk[3],pctatRisk[4])

pctAbove <- c(sum(as.numeric(InfoPaths$fixAbove[which(InfoPaths$farm_name == "S1")])) / sum(as.numeric(InfoPaths$totFix[which(InfoPaths$farm_name == "S1")])) * 100,
              sum(as.numeric(InfoPaths$fixAbove[which(InfoPaths$farm_name == "S2")])) / sum(as.numeric(InfoPaths$totFix[which(InfoPaths$farm_name == "S2")])) * 100,
              sum(as.numeric(InfoPaths$fixAbove[which(InfoPaths$farm_name == "S6")])) / sum(as.numeric(InfoPaths$totFix[which(InfoPaths$farm_name == "S6")])) * 100,
              sum(as.numeric(InfoPaths$fixAbove[which(InfoPaths$farm_name == "S7")])) / sum(as.numeric(InfoPaths$totFix[which(InfoPaths$farm_name == "S7")])) * 100)

# Organize the final summary dataframe
# & arrange appearance
SummaryFarmData.df <- as.data.frame(cbind(farmName, nbTurbines, minRotor, maxRotor, totPaths, mixedBout, round(pctAbove, digits = 2)))

colnames(SummaryFarmData.df) <- c("Wind farm names",
                                  "Nb turbines",
                                  "Min rotor height",
                                  "Max rotor height",
                                  "nb tot paths",
                                  "nb paths crossing alt threshold",
                                  "Percentage fixes above alt threshold")

# Save the summary table
write.csv(SummaryFarmData.df,
          file = "./Outputs/Figures/SummaryFarmData.csv")

