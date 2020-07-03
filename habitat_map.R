#install.packages("remotes")
#remotes::install_github("nmcdev/nmcMetIO")

setwd("C:/Users/Russe/Desktop/Helping with R/Image analyses/elephants")

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


fam <- name_suggest(q = "Elephas maximus", rank = "species")
# Show total number of records for your taxa in the database
occ_search(taxonKey = fam$key, return = "meta")$count
# Pulls your data from GBIF, limit to 200 records as an example dataset
spdat <- occ_search(taxonKey = fam$key,return = "data", limit = 200)

# pull out coords and species dat
dat <- dplyr::select(spdat, decimalLongitude, decimalLatitude, species)
dat <- na.omit(dat)

# convert to a spatial object
sp <- SpatialPoints(cbind(dat$decimalLongitude, dat$decimalLatitude),
                    proj4string = CRS("+init=epsg:4326"))


# check the points
mapview(sp)


# lets focus on point 59, since it has several 
# nearby habitat types
dat59 <- dat[59,]
# set this as the new spatial point object
sp <- SpatialPoints(cbind(dat59$decimalLongitude, dat59$decimalLatitude),
                    proj4string = CRS("+init=epsg:4326"))
# check the point
mapview(sp)

# define the bounding box, check the values 
# and input them into the sequence

# get bounding box
# coords.x1 is longitude coords.x2 is latitude
# but in this case we will use the mapview coordinates
# at the top left and bottom right corners of the window
bbox <- list(
  p1 = list(long = 101.33019, lat = 14.59251),
  p2 = list(long = 101.34688, lat = 14.57781)
)

# create an extent object for reprojection later on
# using min/max long, then min/max lat
ext <- extent(c(101.33019, 101.34688, 14.57781, 14.59251))

# define the image size
image_size <- define_image_size(bbox, major_dim = 600)

# get the image
overlay_file <- "C:/Users/Russe/Desktop/Helping with R/Image analyses/elephants/map.png"
get_arcgis_map_image(bbox, map_type = "World_Imagery", file = overlay_file,
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




par(mfrow=c(1,2), mar = c(0.2, 0.2, 0.2, 0.2))

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
                                  superpixel = 600, 
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



par(mfrow=c(1,1))
# open the image post-model
OpenImageR::imageShow(spx_km$AP_image_data)

#----------------------------------------------
# convert data to raster
im.r <- raster(spx_km$AP_image_data[,,1])
plot(im.r)

# add the projection CRS
crs(im.r) <- CRS('+init=epsg:4326')
# set the extent
extent(im.r) <- c(101.33019,101.34688, 14.57781, 14.59251)

#set a mapview pallete
library(RColorBrewer)
pal <- mapviewPalette("mapviewTopoColors")

mapview(im.r, alpha.regions = 0.3, col.regions = pal(100))

# since this is pretty much a reverse vegetation scale,
# lets call this raster "human_disturbance
human_disturbance <- im.r

# now lets invert the values, and make it vegetation cover
vegetation <- raster.invert(im.r)
plot(vegetation)

# plot with labels
plot(vegetation, col = c("yellow", "tan","lightgreen", "green", "darkgreen"),
     main = "Land Cover Types", legend = FALSE)
legend("bottomright", legend = c("Fallow field", "Disturbed","Low vegetation", "Forest","Dense Forest"),
fill = c("yellow", "tan", "lightgreen", "green","darkgreen"))





