library(ggplot2)
library(rgeos)
library(ggspatial)
library(sp)
library(adehabitatHR)
library(raster)
library(rgdal)
library(mapview)
library(maptools)


setwd("C:/Users/Russell/Desktop/EvData/Sakaerat")
list.files()

#import spatial data (I changed the format to , instead of ;)
#do whatever works for you to import it
dat <- read.csv("HR_13mo.csv")
#clean NA
dat <- na.omit(dat)

#make a spatialpoints object 
sp <- SpatialPoints(coords = cbind(dat$X, dat$Y),
                    proj4string = CRS("+init=epsg:32647"))

#Transform the UTMs into Lat/Long 
sp.latlon <- spTransform(sp,
                         CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# calculates an MCP based on 95% of the points
MCP100 <- mcp(sp.latlon, percent = 100, unin = "m", unout = "ha")
MCPlatlong <- spTransform(MCP100,
                          CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))


#import the shapefiles
list.files()
Sakaerat <- sf::st_read("Sakaerat_simple.shp")

#make the entire polygonset a single polygon
Sakaerat2 <- Sakaerat[2]
Sakaerat.poly <- as(Sakaerat2, "Spatial")
Sakaerat.poly <- spTransform(Sakaerat.poly,
                      CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

#isolate single part of forest to clip MCP
poly2 <- Sakaerat2[2,]
poly2 <- as(poly2, "Spatial")
poly2  <- spTransform(poly2,
                       CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

#poly5 the station layer
poly5 <- Sakaerat2[6,]
poly5 <- as(poly5, "Spatial")
poly5  <- spTransform(poly5,
                      CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))


#covert MCP to polygondataframe too
MCPlatlong <- as(MCPlatlong, "SpatialPolygonsDataFrame")
MCPlatlong <- spTransform(MCPlatlong,
                      CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

#create a buffer to make the polygons work
#without crashing R (I still can't explain this)
#also, ignore the warnings....They don't matter
MCPbuff <- gBuffer(MCPlatlong, width=0, byid = T)
polybuff2 <- gBuffer(poly2, width=0, byid = T)
polybuff5 <- gBuffer(poly5, width=0, byid = T)
sakaeratbuff <- gBuffer(Sakaerat.poly, width=0, byid = T)

#remove the forest part
MCPclip <- gDifference(MCPbuff, polybuff2)
#remove the forest part
MCPclip <- gDifference(MCPclip, polybuff5)
#remove the highway part
MCPclip <- intersect(MCPclip, sakaeratbuff)
#see what it looks like
plot(MCPclip, col="blue")

#change back to a simple spatial polygon
MCPclip <- as(MCPclip, "SpatialPolygons")


#change back to a simple spatial polygon dataframe to write to shapefile
MCPclip <- as(MCPclip, "SpatialPolygonsDataFrame")

#aggregate the polygon
MCPclip.agg <- aggregate(MCPclip,dissolve=T)

MCPclip.agg <- as(MCPclip.agg, "SpatialPolygonsDataFrame")

#check it in mapview
mapview(MCPclip.agg)+mapview(sakaeratbuff)

#write the clipped MCP into a shapefile
writeOGR(MCPclip.agg, ".", "MCPclip_agg1", driver="ESRI Shapefile")

