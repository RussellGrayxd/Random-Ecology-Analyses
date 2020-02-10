library(ggplot2)
library(ggmap)
library(RColorBrewer) # for color selection
library(lubridate)

## Read the input spatiotemporal data
animal.data <- monarchs

## Break down by month
month <- vector()
for(i in 1:nrow(animal.data)) {
  dateStr <- toString(animal.data$Tracking_month[i])
  dateStrSplit <- strsplit(dateStr, "/")[[1]]
  month[i] <- as.Date(dateStrSplit[3])
}

## Create a month attribute
animal.data$month <- animal.data$Tracking_month

## Convert month to factor
animal.data$month <- factor(animal.data$Tracking_month, levels=month.name, ordered=TRUE)

## Specify a map with center at the center of all the coordinates
mean.longitude <- mean(animal.data$longitude)
mean.latitude <- mean(animal.data$latitude)
register_google(key = "AIzaSyCq_aqS9pfXOqSFtCit4PvwC0wQL5ZL_eg")
animal.map <- get_map(location = c(lon=-99, lat=27), maptype = "toner", color = "bw", zoom = 4, scale = 2)

## Convert into ggmap object
animal.map <- ggmap(animal.map, extent="device", legend="none")


## Plot a heat map layer: Polygons with fill colors based on
## relative frequency of events
animal.map <- animal.map + stat_density2d(data=animal.data,
                                          aes(x=longitude, y=latitude, fill=..level.., alpha=..level..), geom="polygon")+
                            scale_fill_gradientn(colours="orange")


## Remove any legends
animal.map <- animal.map + guides(size=FALSE, alpha = FALSE)

## Give the map a title
animal.map <- animal.map + ggtitle("Inaturalist Monthly Monarch Butterfly Observations 2007-2019")

## Plot animals by each month
animal.map <- animal.map + facet_wrap(~month)
print(animal.map) # this is necessary to display the plot