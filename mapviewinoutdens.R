library(sp)
library(raster)
library(usdm)
library(mapview)
library(rgbif)
library(scrubr)

setwd("C:/Users/Russell/Desktop/fire")

#pull out the dataframe
dat <- MODIS_C6_Global_7d


#Change name of df
sp = dat

sp$lon <-sp$longitude
sp$lat <-sp$latitude

sp$species <- "fire"

sp <- sp[,c( 'lon', 'lat', 'species')]

#Remove all NA values
#sp<-sp[-which(is.na(sp)),]

#correct dataframe from tibble
sp<-as.data.frame(sp)
#turn into a spatial dataframe
coordinates(sp) <- ~lon + lat
#give it CRS
proj4string(sp) <- projection(raster())
#check the class
class(sp)

mapview(sp, map.types = "Esri.NatGeoWorldMap", layer.name = "Species",
        alpha = 1, fill="red", size = 0.1)

#######################################
dat1 <- someagree


#Change name of df
sp1 = dat1

sp1$lon <-sp1$longitude
sp1$lat <-sp1$latitude

sp1$species <- sp1$scientific_name

sp1 <- sp1[,c( 'lon', 'lat', 'species')]

#Remove all NA values
sp1<-sp1[-which(is.na(sp1)),]

#correct dataframe from tibble
sp1<-as.data.frame(sp1)
#turn into a spatial dataframe
coordinates(sp1) <- ~lon + lat
#give it CRS
proj4string(sp1) <- projection(raster())
#check the class
class(sp1)

mapview(sp1, map.types = "Esri.NatGeoWorldMap", layer.name = "Species",
        alpha = 1)


