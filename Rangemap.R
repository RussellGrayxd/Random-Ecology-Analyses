library(sp)
library(ggplot2)
library(ggmap)
library(raster)
library(rgdal)
library(mapview)
library(ggspatial)

setwd("C:/Users/Russell/Desktop/EvData/shapefiles")
list.files()

Shapefile <- readOGR(dsn="C:/Users/Russell/Desktop/EvData/shapefiles",
        layer="data_0")

#explore shapefile because I don't know what or where it is
mapview(Shapefile,  map.types = "Esri.NatGeoWorldMap", 
        layer.name = "Macaca leonina")

library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)
library("sf")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) + 
  geom_sf(fill= "antiquewhite") + 
  geom_text(data= world_points,aes(x=X, y=Y, label=name), size = 2,
            color = "black", fontface = "bold", check_overlap = FALSE) +
  geom_spatial_polygon(data = Shapefile, aes(x = long, y = lat, group = group), size = 0.5,
               alpha = 0.2, linetype = 2, color = "black", fill="#49ace5") +
  annotate(geom = "text", x = -90, y = 26, 
           label = "SE Asia", fontface = "italic", 
           color = "grey22", size = 6) + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(89.55, 109.77), ylim = c(29.30, 9.62), 
           expand = FALSE) + xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Macaca Leonina Range") + 
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))

