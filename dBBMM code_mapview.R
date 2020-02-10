
# Load Libraries
library("ggplot2")
library("scales")
library("dplyr")
library("move")
library("adehabitatHR")
library("ggspatial")
library("rgeos")
library(lubridate)

# set wokring directory make sure you only have your data .csv file in 
# this folder
setwd("C:/Users/Russell/Desktop/Inat Data/R Ecology Training/dBBMM")

# skip line here
#list.files(paste0(getwd(), "/Data/"))

# read in animal data
animal.data <-  read.csv("GW_RG.csv")
# review names of data
#names(animal.data)
# and preview that it has read in correctly
#head(animal.data)

# we want to work with animal ID, easting, northing and timestamp
animal.data <- animal.data %>%
  dplyr::select(individual.local.identifier,
                location.long, location.lat,
                study.local.timestamp)

# rename the columns to easier names
names(animal.data) <- c("animal", "x", "y", "datetime")


# Change date to a readable format
#animal.data$datetime <- parse_date_time(animal.data$datetime, orders="%m/%d/%Y H:M:S")
 

# make the time stamp a date format for R
animal.data$datetime <- as.POSIXct(animal.data$datetime, format="%m/%d/%Y %H:%M")

# check how the consistent the tracking was
ggplot(animal.data) +
  geom_point(aes(x = datetime, y = animal))

# preview locations of animals
ggplot(animal.data) +
  geom_point(aes(x = x, y = y, colour = animal)) +
  coord_equal()

# and the distribution of time lags between tracks
animal.data %>%
  group_by(animal) %>%
  mutate(time.lag = as.numeric(difftime(datetime, lag(datetime), units = "hours"))) %>%
  ggplot() +
  geom_density(aes(x = time.lag, group = animal), fill = "black", alpha = 0.25) +
  scale_x_log10() # show with and without to highlight pattern

# we'll start with one individual
indi.data <- animal.data[animal.data$animal == "1",]

### GPS accuracy for each animal
set_loc.error <- 5

# Remove duplicates
indi.data$datetime<-unique(indi.data$datetime, .keep_all= FALSE)

#convert latlong to utm system for dbbmm functionality
sp <- SpatialPoints(coords=cbind(animal.data$x,animal.data$y), proj4string = CRS("+init=epsg:4326"))
sp@coords
spUTM<-spTransform(sp, CRSobj = CRS("+init=epsg:32647"))
spUTMdf<-as.data.frame(spUTM@coords)
indi.data$x<-spUTMdf$coords.x1
indi.data$y<-spUTMdf$coords.x2


# convert data into a move object ready for dBBMM calculation
# make note of the CRS object
# Calculating trajectory:
move.obj <- move(x = indi.data$x, y = indi.data$y, proj = CRS("+init=epsg:32647"), 
             time = as.POSIXct(indi.data$datetime))

# review move object - can hold more than one individual but we'll stick with
# one for ease
move.obj

# window and margin size
ws <- 15
mrg <- 5

# this sets the grid that the utilisation distribution will be calculated accross 
set_grid.ext <- .45
set_dimsize <- 100

# store the start time
ind.start <- Sys.time()

# Calculating dBBMM:
dbbmm <- brownian.bridge.dyn(object = move.obj, # move object
                             location.error = set_loc.error, # locations error
                             margin = mrg, window.size = ws,  # window and margin
                             ext = set_grid.ext, dimSize = set_dimsize,  # grid
                             verbose = F)

# print the time it took
print(paste("------ dBBMM computation time:", 
            round(difftime(Sys.time(), ind.start, units = "min"), 3), "mins"))

# store the movement variances with the original data
indi.data$var <- getMotionVariance(dbbmm)

# review the movement variance
var.plot <- ggplot(indi.data, aes(datetime, var)) + 
  geom_line() +
  theme_bw() +
  scale_size_manual(values = 0.6) +
  scale_x_datetime(labels = date_format("%m-%Y"), 
                   breaks = date_breaks("month")) + 
  labs(x="Date (month-year)", y=(expression(paste(sigma^2,"m")))) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 14),
        axis.title = element_text(size = 16, face = 2))

var.plot

# save the plot as the png
png(file = paste0(indi.data$animal[1], "_MoveVarplot.png"), width = 800, height = 800)
var.plot
dev.off()

# these lines convert the raster produced by the dBBMM into a true utilisation distribution
dbbmm.sp <- as(dbbmm, "SpatialPixelsDataFrame")
dbbmm.sp.ud <- new("estUD", dbbmm.sp)
dbbmm.sp.ud@vol = FALSE
dbbmm.sp.ud@h$meth = "dBBMM"
dbbmm.ud <- getvolumeUD(dbbmm.sp.ud, standardize = TRUE) # convert UD values to volume

########### DO NOT WORRY ABOUT EXPLAINING FOR EXAMPLE ONLY #
dbbmm.ud.eg <- as(dbbmm.ud, "SpatialPixelsDataFrame")
preview.ud <- as.data.frame(dbbmm.ud.eg) %>% 
  dplyr::rename(value = n)

ggplot(preview.ud, aes(x = Var2, y = Var1)) +
  geom_tile(aes(fill = value)) +
  coord_equal(xlim = c(817000, 822000), ylim = c(1608500, 1615000))
########### end of example raster bit


# we can then pull out different contours
poly.050 <- getverticeshr(dbbmm.ud, percent = 50)
poly.095 <- getverticeshr(dbbmm.ud, percent = 95)
poly.099 <- getverticeshr(dbbmm.ud, percent = 99)

# writeOGR(obj = poly.050, dsn = gsub(".{1}$", "", loc.output), overwrite_layer = TRUE,
#          layer = paste0(indi, "_dBBMM050_", settings, "_polygon"), driver = "ESRI Shapefile")
# writeOGR(obj = poly.095, dsn = gsub(".{1}$", "", loc.output), overwrite_layer = TRUE,
#          layer = paste0(indi, "_dBBMM095_", settings, "_polygon"), driver = "ESRI Shapefile")

dBBMM.plot <- ggplot() +
  geom_spatial_polygon(data = poly.099, aes(x = long, y = lat, group = group),
                       alpha = 0.15, crs = 32647) +
  geom_spatial_polygon(data = poly.095, aes(x = long, y = lat, group = group),
                       alpha = 0.15, crs = 32647) +
  geom_spatial_polygon(data = poly.050, aes(x = long, y = lat, group = group),
                       alpha = 0.15, crs = 32647) +
  geom_point(data = indi.data, aes(x = x, y = y), alpha = 0.25, size = 0.5) +
  labs(title = "Elephant1", x = "Longitude", y = "Latitude") +
  coord_sf(crs = 32647) +
  theme_bw() +
  theme(legend.position = "bottom")

dBBMM.plot

png(file = paste0(indi.data$animal[1], "_dBBMMplot.png"), width = 450, height = 1000)
dBBMM.plot
dev.off()


# get area of polygon and convert to ha, from square m
gArea(poly.095) / 10000




##########################################
##########################################

#Make a satellite map instead
library(ggmap)

p99<-spTransform(poly.099, 
                 CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
p95<-spTransform(poly.095,
                 CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
p50<-spTransform(poly.050,
                 CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

sp <- SpatialPoints(coords = cbind(indi.data$x, indi.data$y),
                    proj4string = CRS("+init=epsg:32647"))

#Transform the UTMs into Lat/Long 
sp.latlon <- spTransform(sp,
                         CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

coords.latlon <- data.frame(sp.latlon@coords)

#To find the extent of the largest polygon, so as to centre the isopleth right in the middle
loc <- c(mean(extent(p99)[1:2]), mean(extent(p99)[3:4]))

#to use ggmap you have to create your own API key and replace the "xxx"
#get it here:
#https://cloud.google.com/maps-platform/#get-started
register_google(key="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
satmap=get_map(location=loc, source="google",zoom=11,maptype="satellite")


dBBMM.plot <- 
  # ggplot() +
  ggmap(satmap) +
  geom_spatial_polygon(data = p99, aes(x = long, y = lat, group = group),
                       alpha = 0.2, linetype = 3, colour = "black", fill = "white"
  ) +
  geom_spatial_polygon(data = p95, aes(x = long, y = lat, group = group),
                       alpha = 0.2, linetype = 2, colour = "black", fill = "white"
  ) +
  geom_spatial_polygon(data = p50, aes(x = long, y = lat, group = group),
                       alpha = 0.2, linetype = 1, colour = "black", fill = "white"
  ) +
  geom_point(data = coords.latlon, aes(x = coords.x1, y = coords.x2),
             alpha = 0.75, size = 1, pch = 21) +
  labs(title = "Elephant 1",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = extent(p99)[1:2]) +
  scale_y_continuous(limits = extent(p99)[3:4]) +
  # coord_sf(crs = 32647) +
  theme_bw() +
  theme(legend.position = "bottom")

dBBMM.plot


########################################################
########################################################
#explore your data on an interactive map

library(mapview)

sp = animal.data

sp$lon <-sp$x
sp$lat <-sp$y

sp$species <- "elephant"

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

#on the mapview change the layers for the map background with the 
#layer icon on the top left under the +/- zoom button
mapview(sp, layer.name = "Species",
        alpha = 0.5, color="black", cex = 0.5)+
  mapview(poly.050, alpha.regions=0.2, col.regions = "grey")+
  mapview(poly.095, alpha.regions=0.2, col.regions = "grey")+
  mapview(poly.099, alpha.regions=0.2, col.regions = "grey")
  



