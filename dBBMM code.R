
# Load Libraries
library("ggplot2")
library("scales")
library("dplyr")
library("move")
library("adehabitatHR")
library("ggspatial")
library("rgeos")

# set wokring directory make sure you only have your data .csv file in 
# this folder
setwd("copy and paste your file folder here, then change all \ to /")

# skip line here
list.files(paste0(getwd(), "/Data/"))

# read in animal data
animal.data <- Wilson_Gray_sample1
# review names of data
names(animal.data)
# and preview that it has read in correctly
head(animal.data)

# we want to work with animal ID, easting, northing and timestamp
animal.data <- animal.data %>%
  dplyr::select(individual.local.identifier,
                location.long, location.lat,
                study.local.timestamp)

# rename the columns to easier names
names(animal.data) <- c("animal", "x", "y", "datetime")


# Change date to a readable format
animal.data$datetime <- parse_date_time(animal.data$datetime, orders="%m/%d/%Y H:M:S")
 

# make the time stamp a date format for R
#animal.data$datetime <- as.POSIXct(animal.data$datetime)

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

# convert data into a move object ready for dBBMM calculation
# make note of the CRS object
# Calculating trajectory:
move.obj <- move(x = indi.data$x, y = indi.data$y, proj = CRS("+init=epsg:23845"), 
             time = as.POSIXct(indi.data$datetime))

# review move object - can hold more than one individual but we'll stick with
# one for ease
move.obj

# window and margin size
ws <- 25
mrg <- 5

# this sets the grid that the utilisation distribution will be calculated accross 
set_grid.ext <- 10
set_dimsize <- 1000

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
  labs(title = "OPHA2", x = "Longitude", y = "Latitude") +
  coord_sf(crs = 32647) +
  theme_bw() +
  theme(legend.position = "bottom")

dBBMM.plot

png(file = paste0(indi.data$animal[1], "_dBBMMplot.png"), width = 450, height = 1000)
dBBMM.plot
dev.off()


# get area of polygon and convert to ha, from square m
gArea(poly.095) / 10000

