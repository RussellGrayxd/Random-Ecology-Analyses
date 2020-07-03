#install.packages("remotes")
#remotes::install_github("nmcdev/nmcMetIO")

setwd("C:/Users/Russe/Desktop/Helping with R/Image analyses/varanus")

library(nmcMetIO)
library(ggmap)
library(sp)
library(ggplot2)
library(rgbif)
library(SuperpixelImageSegmentation)
library(mapview)
library(dplyr)
library(raster)
library(OpenImageR)
library(spatialEco)


fam <- name_suggest(q = "Varanus salvator", rank = "species")
# Show total number of records for your taxa in the database
occ_search(taxonKey = fam$key, return = "meta")$count
# Pulls your data from GBIF, limit to 200 records as an example dataset
spdat <- occ_search(taxonKey = fam$key,return = "data", limit = 200)

dat <- spdat$`2470685`

# pull out coords and species dat
dat <- dplyr::select(dat, decimalLongitude, decimalLatitude, species)
dat <- na.omit(dat)

# convert to a spatial object
sp <- SpatialPoints(cbind(dat$decimalLongitude, dat$decimalLatitude),
                    proj4string = CRS("+init=epsg:4326"))


# check the points
mapview(sp)

# get bounding box
# coords.x1 is longitude coords.x2 is latitude
# but in this case we will use the mapview coordinates
# at the top left and bottom right corners of the window
bbox <- list(
  p1 = list(long = 100.53656, lat = 13.73496),
  p2 = list(long = 100.54641, lat = 13.72751)
)

# create an extent object for reprojection later on
# using min/max long, then min/max lat
ext <- extent(c(100.53656, 100.54641, 13.72751, 13.73496))

# lets restrict the points to only show locations within our bounding box
sp <- crop(sp, ext)

# set this as the new spatial point object
sp <- SpatialPoints(cbind(dat59$decimalLongitude, dat59$decimalLatitude),
                    proj4string = CRS("+init=epsg:4326"))
# check the point
mapview(sp)


# define the image size
image_size <- define_image_size(bbox, major_dim = 600)

# get the image
overlay_file <- "C:/Users/Russe/Desktop/Helping with R/Image analyses/varanus/map.png"
get_arcgis_map_image(bbox, map_type = "World_Street_Map", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)


# read in the image
im_km = OpenImageR::readImage("map.png")

# open the image
OpenImageR::imageShow(im_km)


#--------------
# "slic" method
#--------------

res_slic = superpixels(input_image = im_km,
                       method = "slic",
                       superpixel = 200, 
                       compactness = 20,
                       return_slic_data = TRUE,
                       return_labels = TRUE, 
                       write_slic = "", 
                       verbose = TRUE)




#---------------
# "slico" method       [ the "slico" method does not require the "compactness" parameter ]
#---------------

res_slico = superpixels(input_image = im_km,
                        method = "slico",
                        superpixel = 200, 
                        return_slic_data = TRUE,
                        return_labels = TRUE, 
                        write_slic = "", 
                        verbose = TRUE)




par(mfrow=c(1,1), mar = c(0.2, 0.2, 0.2, 0.2))

plot_slic = OpenImageR::NormalizeObject(res_slic$slic_data)
plot_slic = grDevices::as.raster(plot_slic)
plot(plot_slic)

plot_slico = OpenImageR::NormalizeObject(res_slico$slic_data)
plot_slico = grDevices::as.raster(plot_slico)
plot(plot_slico)




# Call an empty object container
init = Image_Segmentation$new()

# run the model, try to get clusters of simiklar colors 
# to represent a single animal
spx_km = init$spixel_segmentation(input_image = im_km, 
                                  superpixel = 900, 
                                  AP_data = TRUE,
                                  use_median = TRUE, 
                                  sim_wL = 3, 
                                  sim_wA = 10, 
                                  sim_wB = 10,
                                  sim_color_radius = 10, 
                                  kmeans_method = "kmeans",
                                  kmeans_initializer = "kmeans++",
                                  kmeans_num_init = 3, 
                                  kmeans_max_iters = 100,
                                  verbose = TRUE)



plot.new()
# open the image post-model
OpenImageR::imageShow(spx_km$AP_image_data)

#----------------------------------------------
# convert data to raster
im.r <- raster(spx_km$AP_image_data[,,1])
plot(im.r)

# add the projection CRS
crs(im.r) <- CRS('+init=epsg:4326')
# set the extent
extent(im.r) <- ext

#set a mapview pallete
library(RColorBrewer)
pal <- mapviewPalette("mapviewTopoColors")

mapview(im.r, alpha.regions = 0.3, col.regions = pal(100))

# since this is pretty much a reverse vegetation scale,
# lets call this raster "human_disturbance
rastpoly <- rasterToPolygons(im.r, dissolve = TRUE)

# now hover over the water values so we can find the number
# and isolate it from the object, click to find "Feature ID"
mapview(rastpoly) #feature ID is 1



#-----------GET AREA OF WATERBODIES----------------------

library(rgeos)

# pull out feature ID 1
water<- rastpoly[1,]
# convert to a spatial object
water.poly <- as(water, "SpatialPolygons")
# add a buffer to prevent orphaned holes
water.buff <- gBuffer(water.poly, width=0, byid = T)
# check the map overlay
mapview(water.buff)

# convert to UTM using the UTM epsg
# reproject to UTM for metric area analysis
water.UTM<- spTransform(water.buff,
              CRS("+init=epsg:32647"))
# get the area in sq.meters
gArea(water.UTM)



#-----------GET distance to water for all animals----------------------
# convert animal points to UTM
sp.UTM <- spTransform(sp,
                      CRS("+init=epsg:32647"))

# get euclidean distance (miimum distance) from points
# to water bodies and add to the spatial points object
sp.UTM$distance_to_water <- apply(gDistance(sp.UTM, water.UTM ,byid=TRUE),2,min)

# view on map
mapview(sp.UTM) + mapview(water.UTM)

# Convert the spatialpoints object to dataframe and save
sp.df <- as.data.frame(sp.UTM)

# save as .csv file
write.csv(sp.df, "varanus_DtoW")
