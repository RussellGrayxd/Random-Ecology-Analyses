
############## ANIMATED OPHA MOVEMENTS FOR AM-RE DATA ################
install.packages("rgeos")
install.packages("sf")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("beepr")
install.packages("animation")
install.packages("magick")
install.packages("reshape")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("sp")
install.packages("ggmap")
install.packages("adehabitatHR")
install.packages("scales")
install.packages("plotly")
install.packages("ggspatial")
install.packages("ggsn")


library(rgeos)
library(sf)
library(rgdal)
library(ggplot2)
library(beepr)
library(animation)
library(magick)
library(reshape2)
library(ggplot2)
library(dplyr)
library(sp)
library(ggmap)
library(adehabitatHR)
library(scales)
library(cowplot)
library(raster)
library(plotly)
library(ggspatial)
library(ggsn)

######## PREPARING OPHA DATA ##########

setwd("C:/Users/Russell/Desktop/Inat Data/Animated Ecology")
list.files()

snakedata <- TuMe1942

names(snakedata)

merged_data <- snakedata[,c(1,2,3,4,5,6)]

names(merged_data) <- c("ID", "Snake.ID.", "Tracking.date", "Tracking.time", "longitude", "latitude")

head(merged_data)

merged_data$datetime <- as.POSIXct(strptime(paste0(merged_data$Tracking.date, merged_data$time),
                                            "%Y-%m-%d %H:%M:%S", tz = ""))

merged_data_all <- merged_data

merged_data <- merged_data[merged_data$Snake.ID. == "fw154",]
merged_data$Snake.ID. <- "GS007"


h007 <- 324
h010 <- 176
h013 <- 387
h014 <- 86
h015 <- 120

h.value <- h007


merged_data <- merged_data[order(merged_data$datetime , decreasing = FALSE),]

head(merged_data, 3)

## had to ditch top abort point becaseu of NA in utms
# merged_data <- merged_data[-1,]

### infilling the utms for no moves, beware of starting with NA
for(u in 1:length(merged_data$longitude)){
  if(is.na(merged_data[u,]$longitude)){
    merged_data[u,]$longitude <- merged_data[u-1,]$longitude
  }
}

for(u in 1:length(merged_data$latitude)){
  if(is.na(merged_data[u,]$latitude)){
    merged_data[u,]$latitude <- merged_data[u-1,]$latitude
  }
}

merged_data_forplot <- merged_data
merged_data_forplot[is.na(merged_data_forplot$datetime),]


#### SHAPEFILES #####

# core <- readOGR("/Users/benmarshall/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Sakaerat Biosphere Zones/SBR_Core.shp")
# bufferz <- readOGR("/Users/benmarshall/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Sakaerat Biosphere Zones/Buffer zone.shp")
# transit <- readOGR("/Users/benmarshall/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Sakaerat Biosphere Zones/Transitional.shp")
# 
# HW304 <- readOGR("/Users/benmarshall/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Roads and Trails/Hg304.shp")

# core <- readOGR("D:/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Sakaerat Biosphere Zones/SBR_Core.shp")
# bufferz <- readOGR("D:/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Sakaerat Biosphere Zones/Buffer zone.shp")
# transit <- readOGR("D:/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Sakaerat Biosphere Zones/Transitional.shp")
# 
# HW304 <- readOGR("D:/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Roads and Trails/Hg304.shp")

# bufferz <- st_read(dsn = "D:/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Sakaerat Biosphere Zones/Buffer zone.shp")
# transit <- st_read(dsn = "D:/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Sakaerat Biosphere Zones/Transitional.shp")
# HW304 <- st_read(dsn = "D:/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Roads and Trails/Hg304.shp")
# core <- st_read(dsn = "D:/Documents/Work Files/Sakaerat Conservation and Snake Education Team/GIS and Mapping Resources/Shapfiles/Sakaerat Biosphere Zones/SBR_Core.shp")
# core <- st_transform(core, 4326)


latlong <- CRS("+init=epsg:4326")

##### TEST PLOT

coords <- cbind(merged_data_forplot$longitude, merged_data_forplot$latitude)
SP <- SpatialPoints(coords,proj4string = utm)
MCP <- mcp(SP, percent = 95, unin = "m", unout = "ha")
MCParea <- MCP$area

K <- kernelUD(SP, h = h.value, grid = 800, extent = 5)
# K_href_95 <- getverticeshr(K, 95)
# kernelarea <- kernel.area(K, percent = 95, unin = "m", unout = "ha")
# ylimforHRgraph <- max(cbind(MCParea, kernelarea))
# ylimforHRgraph

# for(contr in c(seq(20, 90, 10), 95)){
#   contr_name <- paste0("K_UTM_", contr)
#   K_contr <- getverticeshr(K, contr)
#   assign(contr_name, K_contr)
# }

K_UTM_95 <- getverticeshr(K, 95)
K_UTM_50 <- getverticeshr(K, 50)

K95latlong <- spTransform(K_UTM_95, latlong)
K50latlong <- spTransform(K_UTM_50, latlong)

areasall <- NULL

MCParea <- MCP$area
kernelareas <- kernel.area(K, percent = c(50, 95), unin = "m", unout = "ha")
areas <- data.frame(MCParea, kernelareas[1], kernelareas[2],
                       merged_data_forplot$datetime[length(merged_data_forplot$ID)])
names(areas) <- c("MCP", "K50", "K95", "Date")
row.names(areas) <- "Area (ha)"
areasall <- rbind(areasall, areas)
SPlatlong <- spTransform(SP, latlong)
MCPlatlong <- spTransform(MCP, latlong)


# This is the corresponding loop to convert all the contours generated into lat long format.
# for(kutm in ls(pattern = "K_UTM_*")){
#   K_Number <- sub("K_UTM_", "", kutm)
#   K_To_Convert <- get(kutm)
#   K_LatLong <- spTransform(K_To_Convert, latlong)
#   assign(paste0("K_LatLong_", K_Number), K_LatLong)
# }

location <- c(mean(range(SPlatlong@coords[,1])), mean(range(SPlatlong@coords[,2])))

map <- get_map(location = location, crop = F,
               maptype = "satellite",
               source = "google",
               zoom = 13)

map <- map007

# map007 <- map
# map010 <- map
# map013 <- map
# map014 <- map
# map015 <- map


# core <- st_transform(core, 4326)
# bufferz <- st_transform(bufferz, 4326)
# transit <- st_transform(transit, 4326)
# HW304 <- st_transform(HW304, 4326)


# K95latlong <- st_as_sf(x = K95latlong)
# K95latlong <- st_transform(K95latlong, 3857)
# K50latlong <- st_as_sf(x = K50latlong)
# K50latlong <- st_transform(K50latlong, 3857)
# MCPlatlong <- st_as_sf(x = MCPlatlong)
# MCPlatlong <- st_transform(MCPlatlong, 3857)

K95latlong <- st_as_sf(x = K95latlong)
K95latlong <- st_transform(K95latlong, 4326)
K50latlong <- st_as_sf(x = K50latlong)
K50latlong <- st_transform(K50latlong, 4326)
MCPlatlong <- st_as_sf(x = MCPlatlong)
MCPlatlong <- st_transform(MCPlatlong, 4326)

#started using data from here****
xlim <- c(st_bbox(K95latlong)[1] - 0.005, 
          st_bbox(K95latlong)[3] + 0.005)

ylim <- c(st_bbox(K95latlong)[2] - 0.005, 
          st_bbox(K95latlong)[4] + 0.005)


ggmap(map) +
  geom_sf(data = K95latlong, inherit.aes = FALSE,
               fill = "white",  alpha = 0.05, linetype = 1, size = 0.5, colour = "black") +
  coord_sf(xlim = xlim, ylim = ylim) +
  geom_point(data = as.data.frame(SPlatlong),
             aes(x = coords.x1, y = coords.x2),
             colour = "black", pch = 3, size = 3) +
  ggsn::scalebar(x.min = xlim[1], x.max = xlim[2], y.min = ylim[1], y.max = ylim[2],
                 dist = 1, dd2km = TRUE, model = 'WGS84', location = "bottomright",
                 height = 0.01, st.size = 5, st.bottom = FALSE) +
  north(x.min = xlim[1], x.max = xlim[2], y.min = ylim[1], y.max = ylim[2],
        symbol = 10)


# northSymbols()

# as.data.frame(K95latlong)

# ylim[2] - ((ylim[2] - ylim[1]) / 20)
# xlim[1] - ((Xlim[2] - Xlim[1]) / 20)

i = 6
CurrSet <- merged_data_forplot[1:i,]

fullmap <- ggmap(map, padding = 0) +
  ggsn::scalebar(x.min = xlim[1], x.max = xlim[2], y.min = ylim[1], y.max = ylim[2],
                 dist = 1, dd2km = TRUE, model = 'WGS84', location = "bottomright",
                 st.size = 15, st.bottom = FALSE) +
  north(x.min = xlim[1], x.max = xlim[2], y.min = ylim[1], y.max = ylim[2],
        symbol = 10) +
  # geom_sf(data = core, inherit.aes = FALSE, 
  #         size = 2, alpha = 0.5, fill = "#3a80f2", linetype = 1, colour = "#3a80f2") +
  # geom_sf(data = bufferz, inherit.aes = FALSE, 
  #         size = 2, alpha = 0.5, fill = "#49ace5", linetype = 2, colour = "#49ace5") +
  geom_sf(data = K95latlong, inherit.aes = FALSE, 
               fill = "white",  alpha = 0.1, linetype = 3, size = 1, colour = "black") +
  geom_sf(data = K50latlong, inherit.aes = FALSE, 
               fill = "white",  alpha = 0.1, linetype = 2, size = 1, colour = "black") +
  geom_sf(data = MCPlatlong, inherit.aes = FALSE, 
               colour = "black",  alpha = 0, linetype = 1, size = 2.5) +
  geom_segment(data = as.data.frame(SPlatlong), 
               aes(x = coords.x1[i-1], xend = coords.x1[i],
                   y = coords.x2[i-1], yend = coords.x2[i]), size = 2.5, colour = "red4") +
  geom_segment(data = as.data.frame(SPlatlong), 
               aes(x = coords.x1[i-2], xend = coords.x1[i-1],
                   y = coords.x2[i-2], yend = coords.x2[i-1]), size = 2, alpha = 0.5, colour = "red4") +
  geom_segment(data = as.data.frame(SPlatlong), 
               aes(x = coords.x1[i-3], xend = coords.x1[i-2],
                   y = coords.x2[i-3], yend = coords.x2[i-2]), size = 1.5, alpha = 0.25, colour = "red4") +
  geom_point(data = as.data.frame(SPlatlong),
             aes(x = coords.x1, y = coords.x2),
             colour = "black", pch = 3, size = 5) +
  geom_point(data = as.data.frame(SPlatlong), 
             aes(x = coords.x1[i], y = coords.x2[i]),
             colour = "red", pch = 16, size = 8) +
  labs(colour = "white", title = CurrSet$Snake.ID.[1], 
       subtitle = CurrSet$datetime,
       x = "Longitude", y = "Latitude") +
  annotate('text', x = xlim[1] + ((xlim[2] - xlim[1]) / 45),
           y = ylim[2] - ((ylim[2] - ylim[1]) / 15),
           label = CurrSet$Snake.ID.[1], fontface = 2,
           size = 40, colour = "white", alpha = 0.8, hjust = 0, vjust = 0)+
  annotate('text', x = xlim[1] + ((xlim[2] - xlim[1]) / 45),
           y = ylim[2] - ((ylim[2] - ylim[1]) / 8),
           label = CurrSet$datetime[i], fontface = 3,
           size = 30, colour = "white", alpha = 0.8, hjust = 0, vjust = 0)+
  annotate('text', x = xlim[1] + ((xlim[2] - xlim[1]) / 45),
           y = ylim[1] + ((ylim[2] - ylim[1]) / 45),
           label = paste0("Solid line is the 95% MCP = ", round(tail(areasall, 1)$MCP, digits = 0), " ha",
                          "\Outer dashed line is 95% kernel = ", round(tail(areasall, 1)$K95, digits = 0), " ha"),
           fontface = 3, size = 15, colour = "white", alpha = 0.8, hjust = 0, vjust = 0) +
  coord_sf(xlim = xlim, ylim = ylim) +
  theme_bw()+ 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 8),
        text = element_text(size = 30, colour = "black"),
        title = element_text(face = 1, size = 40), 
        plot.title = element_text(size = 0, face = 2,
                                  margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        plot.subtitle = element_text(face = 3, size = 0,
                                     margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black",
                                   margin = margin(t = 15, r = 0, b = 25, l = 0)),
        axis.text.y = element_text(angle = 45, hjust = 1, colour = "black",
                                   margin = margin(t = 0, r = 15, b = 0, l = 25)),
        plot.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"),
        axis.ticks.length = unit(0.5, "cm")) +
NULL

png("test_map2.png", width = 1920, height = 1920, units = "px")
fullmap
dev.off()

getwd()


######### ANIMATION ########

### Alternative map only plot ----------------
length(animaldata$ID) / 60
length(animaldata$ID) / 60 / 18

interval.for.2min <- 2 / (length(animaldata$ID) / 60)

interval.for.2min

ani.options(interval = 0.2, ani.width = 1920, ani.height = 1920, autoplay = FALSE,
            ffmpeg = Sys.which("ffmpeg"))

areasall <- NULL

length(animaldata$ID)

dev.off()

start <- Sys.time()


saveVideo({
  for (i in 5:length(animaldata$ID)) {
    
    CurrSet <- merged_data_forplot[1:i,]
    coords <- cbind(CurrSet$longitude, CurrSet$latitude)
    SP <- SpatialPoints(coords,proj4string = utm)
    MCP <- mcp(SP, percent = 95, unin = "m", unout = "ha")
    K <- kernelUD(SP, h = h.value, grid = 1000, extent = 31)

    # here's an alternative to simply having a 50 and 95% contour, this will make one every 10%
    # BEWARE - this will make the entire process much much longer
    # for(contr in c(seq(20, 90, 10), 95)){
    #   contr_name <- paste0("K_UTM_", contr)
    #   K_contr <- getverticeshr(K, contr)
    #   assign(contr_name, K_contr)
    # }

    for(contr in c(50, 95)){
      contr_name <- paste0("K_UTM_", contr)
      K_contr <- getverticeshr(K, contr)
      assign(contr_name, K_contr)
    }

    MCParea <- MCP$area
    kernelareas <- kernel.area(K, percent = c(50, 95), unin = "m", unout = "ha")
    areas <- cbind(MCParea, kernelareas[1], kernelareas[2])
    areas <- as.data.frame(areas)
    areas <- cbind(areas, CurrSet$datetime[length(CurrSet$ID)])
    names(areas) <- c("MCP", "K50", "K95", "Date")
    # row.names(areas) <- "Area (ha)"
    areasall <- rbind(areasall, areas)
    # names(areasall) <- c("MCP", "K50", "K95", "Date")
    SPlatlong <- spTransform(SP, latlong)
    MCPlatlong <- spTransform(MCP, latlong)

    # This is the corresponding loop to convert all the contours generated into lat long format.
    for(kutm in ls(pattern = "K_UTM_*")){
      K_Number <- sub("K_UTM_", "", kutm)
      K_To_Convert <- get(kutm)
      K_LatLong <- spTransform(K_To_Convert, latlong)
      assign(paste0("K_LatLong_", K_Number), K_LatLong)
    }

    K95latlong <- st_as_sf(x = K95latlong)
    K95latlong <- st_transform(K95latlong, 4326)
    K50latlong <- st_as_sf(x = K50latlong)
    K50latlong <- st_transform(K50latlong, 4326)
    MCPlatlong <- st_as_sf(x = MCPlatlong)
    MCPlatlong <- st_transform(MCPlatlong, 4326)
    
    fullmap <- ggmap(map, padding = 0) +
      ggsn::scalebar(x.min = xlim[1], x.max = xlim[2], y.min = ylim[1], y.max = ylim[2],
                     dist = 1, dd2km = TRUE, model = 'WGS84', location = "bottomright",
                     st.size = 15, st.bottom = FALSE) +
      north(x.min = xlim[1], x.max = xlim[2], y.min = ylim[1], y.max = ylim[2],
            symbol = 10) +
      # geom_sf(data = core, inherit.aes = FALSE,
      #         size = 2, alpha = 0.5, fill = "#3a80f2", linetype = 1, colour = "#3a80f2") +
      # geom_sf(data = bufferz, inherit.aes = FALSE,
      #         size = 2, alpha = 0.5, fill = "#49ace5", linetype = 2, colour = "#49ace5") +
      geom_sf(data = K95latlong, inherit.aes = FALSE,
              fill = "white",  alpha = 0.1, linetype = 3, size = 1, colour = "black") +
      geom_sf(data = K50latlong, inherit.aes = FALSE,
              fill = "white",  alpha = 0.1, linetype = 2, size = 1, colour = "black") +
      geom_sf(data = MCPlatlong, inherit.aes = FALSE,
              colour = "black",  alpha = 0, linetype = 1, size = 2.5) +
      geom_segment(data = as.data.frame(SPlatlong), 
                   aes(x = coords.x1[i-1], xend = coords.x1[i],
                       y = coords.x2[i-1], yend = coords.x2[i]), size = 2.5, colour = "red4") +
      geom_segment(data = as.data.frame(SPlatlong), 
                   aes(x = coords.x1[i-2], xend = coords.x1[i-1],
                       y = coords.x2[i-2], yend = coords.x2[i-1]), size = 2, alpha = 0.5, colour = "red4") +
      geom_segment(data = as.data.frame(SPlatlong), 
                   aes(x = coords.x1[i-3], xend = coords.x1[i-2],
                       y = coords.x2[i-3], yend = coords.x2[i-2]), size = 1.5, alpha = 0.25, colour = "red4") +
      geom_point(data = as.data.frame(SPlatlong),
                 aes(x = coords.x1, y = coords.x2),
                 colour = "black", pch = 3, size = 5) +
      geom_point(data = as.data.frame(SPlatlong), 
                 aes(x = coords.x1[i], y = coords.x2[i]),
                 colour = "red", pch = 16, size = 8) +
      labs(colour = "white", title = CurrSet$Snake.ID.[1], 
           subtitle = tail(CurrSet, 1)$datetime,
           x = "Longitude", y = "Latitude") +
      annotate('text', x = xlim[1] + ((xlim[2] - xlim[1]) / 45),
               y = ylim[2] - ((ylim[2] - ylim[1]) / 15),
               label = CurrSet$Snake.ID.[1], fontface = 2,
               size = 40, colour = "white", alpha = 0.8, hjust = 0, vjust = 0)+
      annotate('text', x = xlim[1] + ((xlim[2] - xlim[1]) / 45),
               y = ylim[2] - ((ylim[2] - ylim[1]) / 8),
               label = CurrSet$datetime[i], fontface = 3,
               size = 30, colour = "white", alpha = 0.8, hjust = 0, vjust = 0)+
      annotate('text', x = xlim[1] + ((xlim[2] - xlim[1]) / 45),
               y = ylim[1] + ((ylim[2] - ylim[1]) / 45),
               label = paste0("Solid line is the 95% MCP = ", round(tail(areasall, 1)$MCP, digits = 0), " ha",
                              "\nOuter dashed line is 95% kernel = ", round(tail(areasall, 1)$K95, digits = 0), " ha"),
               fontface = 3, size = 15, colour = "white", alpha = 0.8, hjust = 0, vjust = 0) +
      coord_sf(xlim = xlim, ylim = ylim) +
      theme_bw()+ 
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 8),
            text = element_text(size = 30, colour = "black"),
            title = element_text(face = 1, size = 40), 
            plot.title = element_text(size = 0, face = 2,
                                      margin = margin(t = 20, r = 0, b = 0, l = 0)), 
            plot.subtitle = element_text(face = 3, size = 0,
                                         margin = margin(t = 20, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "black",
                                       margin = margin(t = 15, r = 0, b = 25, l = 0)),
            axis.text.y = element_text(angle = 45, hjust = 1, colour = "black",
                                       margin = margin(t = 0, r = 15, b = 0, l = 25)),
            plot.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "black", 
                                     size = 0.5, linetype = "solid"),
            axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"),
            axis.ticks.length = unit(0.5, "cm")) +
      NULL
    
    print(paste("Datapoint", i,"/",length(merged_data_forplot$ID), "Complete"))
    print(paste(CurrSet$Snake.ID.[1], 
                CurrSet$datetime[length(CurrSet$ID)]))
    print(fullmap)
    Sys.sleep(0)
  }
  beep(2)
}, video.name = paste0(merged_data_forplot$Snake.ID.[1], "_MAPONLYanimated_NEW.mp4"))


## started at 21:45
end <- Sys.time()
end
end - start
## ended at 06:20 the following morning
## this was 2811 frames for OPHA015 starting at frame 55

?system

system("shutdown -t 560 -s")