library(raster)
library(ggplot2)
library(ggalt)
library(mapview)
library(sp)


# get a polygon of the country you want by filling in
# the country = "" argument with the ISO-3 code of a country
# here I chose 
Thailand <- getData("GADM", country = "THA", level = 1)

# reduce the size of the polygons by converting it from
# a spatialpolygonsdataframe, to a spatialpolygons object
Thailand <- as(Thailand, "SpatialPolygons")

# plot the polygon with mapview
mapview(Thailand)

# now lets generate 10,000 random spatialpoints for the example
sp <- spsample(x=Thailand, n=1000, "random")

# plot them over out Thailand polygon to check them out
mapview(Thailand) + mapview(sp, col.regions="red", cex=2)

# make sure the projections of each spatial object match!
crs(sp) <- crs(Thailand)

# now lets convert the Thailand polygon to a dataframe for analysis
mymap <- fortify(Thailand)

# count the points in each polygon division of the polygon
count <- poly.counts(pts = sp, polys = Thailand)
count <- stack(count)

# Plot the analysis as a chloropleth map
ggplot() +
  geom_cartogram(data = mymap, aes(x = long, y = lat, map_id = id), 
                 map = mymap) +
  geom_cartogram(data = count, aes(fill = values, map_id = ind),
                 map = mymap, color = "black", size = 0.3) +
  scale_fill_gradientn(colours = rev(brewer.pal(10, "Spectral"))) +
  coord_map() +
  theme_map()+  
  labs(fill = "")+
  theme(legend.position = "left", 
        plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 0.8, barheight = 10),
         size = guide_legend(title.position="top", title.hjust = 0.5))+
  ggtitle("Point in polygon analysis")
