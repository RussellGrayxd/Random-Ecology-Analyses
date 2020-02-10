
install.packages("beepr")
install.packages("animation")
install.packages("magick")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("sp")
install.packages("ggmap")
install.packages("adehabitatHR")
install.packages("lubridate")
install.packages("scales")
install.packages("cowplot")
Sys.setenv(PATH = paste("C:/Program Files/ImageMagick/bin",
                        Sys.getenv("PATH"), sep = ";"))

#This generates the permissions from a google API key for the map
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
ggmap(get_googlemap())
register_google(key = "AIzaSyCq_aqS9pfXOqSFtCit4PvwC0wQL5ZL_eg")
geocode("florida")


# first step is to load in all the necessary R packages
# beepr has functions that make a noise
library(beepr)
# animation is the key, it holds all the functions that create and convert stuff to video of gifs
library(animation)
# this loads in the previously installed ImageMagick software
library(magick)
# reshape2 has the melt function that is useful when plotting with ggplot
library(reshape2)
# ggplot is the rather powerful and versatile plotting package for R
library(ggplot2)
# dplyr has a selection of data manipulation functions
library(dplyr)
# sp contains functions pertaining to all sorts of spatial data manipulations and projection
library(sp)
# ggmap is the package that can pull maps from online sources like google etc
library(ggmap)
# adehabitatHR has all the functions that we need for home range estimations
library(adehabitatHR)
# lubridate has more advance functions when dealing with dates in R
library(lubridate)
# scales expands the functionality when setting scales in ggplot, espeically when using dates
library(scales)
# cowplot is what we will use to plot multiple graphs in a single frame
library(cowplot)

Sys.which('ffmpeg')



## As with all R projects and efforts we first need to set our wordking directory
# set your working directory using the line below
# alternative access it via rstudios interface, or hotkey ctrl + shift + h
setwd("~/C:/Users/Russell/Desktop/UF Invasive work/Animation/Animated Ecology")

list.files()

## Next read in your data file. Make sure it is a .csv file
animaldata = X154_animateUTM

## The next couple of parts are to get the distances travelled between UTMs
# These are straight line distances, nothing sophisticated.
# create a function that gets the difference between UTM and next UTMs for Eastings
attach(animaldata)
diffE = function(animaldata) {Easting[1:(length(Easting)-1)]-
    Easting[2:(length(Easting))]} 
diffE_list=sapply(animaldata, diffE)[,1] # repeat for all rows
diffE_list

# create a function that gets the difference between UTM and next UTMs for Northing
diffN = function(animaldata) {Northing[1:(length(Northing)-1)]-
    Northing[2:(length(Northing))]} # func to find length of b
diffN_list=sapply(animaldata, diffN)[,1] # repeat for all rows
diffN_list

# pythagoras to calc the actual differnce between utm locations
# sqrt(a^2+b^2), finding c for all
UTMdist=sqrt((diffE_list^2)+(diffN_list^2))

# Next step is create a few new columns that we will use in the plotting later on
# now we create a new column in the animaldata to stoe the results
# make note we add an additional 0 to the data because there is no result for the first row of the data
animaldata$Moves = c(0, UTMdist)
# but actually to plot the progression of distance we need the cumulative distance
# so another new column and the use of cumsum() to calculate it
animaldata$CsumMoves=cumsum(animaldata$Moves)
# convert the dates to a form that R will actually read as dates
# make sure that the format matches your date format
animaldata$Tracking_date = as.Date(animaldata$Tracking_date, format= "%m/%d/%Y")
# to save people having to change ever instance of the code referring to an animals ID
# we can copy the data to consistently named columns, and repeat also for the times
animaldata$ID=animaldata$Snake_ID
animaldata$Tracking_time = animaldata$Tracking_time

# we detach data here because we don't want to accidental reference something within animaldata
# if we forgot to specify the object to find whatever it is in
detach(animaldata)


# define the correct CRS for your data
# you will need to find the correct epsg for your data's utm zone
utm=CRS('+init=epsg:26917')
# second one is a basic lat and long epsg, corresponds to WGS84
# we need for ggmap's sake
latlong=CRS("+init=epsg:4326")

## The next section we run the spatial analysis using the whole dataset
# this provides us with maximum values to be used in define our graphs,
# otherwise ggplot would constantly recalculate them, frame by frame

# pull out the two columns that contain your utm coordidates
coords = cbind(animaldata$Easting, animaldata$Northing)
# convert the coords object to a spatial points object that can be plotted and used in spatial calculations
SP=SpatialPoints(coords,proj4string = utm)
# calculate a 95% MCP using the newly create SP object
MCP=mcp(SP, percent=95, unin = "m", unout = "ha")
# next few lines calculate a fixed kernel density utilisation esimtation, using href as the smoothing factor
# pay close attention to this line, this will require customisation per species and potentially per individual
# there are all sort of horrible problems assocaited with kernel smoothing factors and spatial data
# do not treat the place holder values here as the gold standard
K=kernelUD(SP, h='href', grid=800, extent=2.2)
G=getvolumeUD(K)
R=as.image.SpatialGridDataFrame(G)
# get the area of the MCP calculated
MCParea=MCP$area
# get the area of the kernel that corresponds to 50 and 95% probabilty
# these values can easily be changed if your study species warrants a more resticted or expanded 'core' and 'activity' areas
kernelareas=kernel.area(K, percent=c(50,95), unin="m", unout="ha")
# this saves the maximum values of these ranges for use later
ylimforHRgraph = max(cbind(MCParea, kernelareas))

# go use ggmap we need to convert the SP into a latitude longitude format, not UTM
# remember we saved the WGS84 espg as latlong
SPlatlong = spTransform(SP, latlong)

# location is the object we will pass to ggmap to centre the map on the points
# so here we get the mean of both x and y axis and keeps them in a numeric vector
# that give ggmap a centre to focus on
location = c(mean(SPlatlong@coords[,1]), mean(SPlatlong@coords[,2]))

# here we just pull out a few more ranges of coordinates that makes the plotting run much smoother,
# and avoids ggplot's repeated scale recalculations
location = c(mean(SPlatlong@coords[,1]), mean(SPlatlong@coords[,2]))
Xlocation=c(min(SPlatlong@coords[,1])-0.05, max(SPlatlong@coords[,1]+0.05))
Ylocation=c(min(SPlatlong@coords[,2])-0.025, max(SPlatlong@coords[,2]+0.025))



# this creates a background layer to print the shapefiles upon, using google maps
# use ?get_map to see all the options available
map <- get_map(location=location, crop = F,
               maptype="satellite",
               source="google",
               zoom=5)

        # preview the map area and make sure it looks to cover all the locations in the dataset being used
# zoom levels may need to be changed. Again run ?get_map to get the details on what each zoom level is
ggmap(map) + geom_point(data = as.data.frame(SPlatlong),
                        aes(x = coords.x1, y = coords.x2),
                        colour = "black", pch=3, size=0.2)


# ani.options is the function that deals with all our settings for building the video
# set delay between frames when replaying, resolution and aspect ratio
# we must also tell it where the ffmpeg compression software is located
# Take care because without locating correctly ffmpeg the compression will fail and there will be no video
# you will only be warned of this at the end of the process
# to locate it you can use 
Sys.which('ffmpeg')
# I can only confirm the line above works Mac OSX
ani.options(interval=.3, ani.width = 800, ani.height = 800, ani.res=1200, autoplay=FALSE,
            ffmpeg="C:\\PROGRA~1\\IMAGEM~1.7-Q\\ffmpeg.exe")
# further options for any of these functions can be found by running a line that is the function preceeded by a ?
# eg ?ani.options

# now we create a function that will create a MCP in a form that is compatible wth ggplot
# the shapefiles we will make showing the 95% MCP are only usable with ggmap
# this will display a 100% MCP
chullget = function(CurrSet) {CurrSet[chull(CurrSet$Easting, CurrSet$Northing), ]}

# length(animaldata$ID)
dev.off()

# before we begin we need to create a location to store all our areas as they are being produced
areasall=NULL
# saveVideo is a slightly more unusual function. It requires an expression to be passed to it
# so in the case we are giving it a for loop that produces, and prints, graphs for repeated subsets of data
# you can also replace this with saveGIF and the things runs effectively the same - do change the file extension before running it
saveGIF({
  # the first step of the loop is defining how long and the maximum amount of data to use
  # it would be wise to replace length(animal$ID) with a lower number just to make sure the code works
  # before commiting to running a full dataset
  # it starts at five because we need at least five locations to calculate the kernels
  for (i in 5:length(animaldata$ID)) {
    # first step is to subset the data by i
    # i is the changing variable that will increase by one every time the loop is repeated
    CurrSet = animaldata[1:i,]
    
    # now the code essentially follows what has been described above but for only the subset defined by the above line
    coords = cbind(CurrSet$Easting, CurrSet$Northing)
    
    # next five lines should be familiar
    SP=SpatialPoints(coords,proj4string = utm)
    MCP=mcp(SP, percent=95, unin = "m", unout = "ha")
    K=kernelUD(SP, h='href', grid=800, extent=20)
    G=getvolumeUD(K)
    R=as.image.SpatialGridDataFrame(G)
    # the next two generate contours based on 50 and 95% probabilties pulled from the kernel utilisation distribution
    K50=getverticeshr(K, 50)
    K95=getverticeshr(K, 95)
    # next two lines pull the areas of the MCP and the 50 and 95% contours
    MCParea=MCP$area
    kernelareas=kernel.area(K, percent=c(50,95), unin="m", unout="ha")
    # next few lines creates a new object to store them in and renames them so more can be added
    areas=cbind(MCParea, kernelareas[1], kernelareas[2])
    areas=as.data.frame(areas)
    areas=cbind(areas, CurrSet$Tracking_date[length(CurrSet$ID)])
    names(areas) = c("MCP", "K50", "K95", "Date")
    row.names(areas)="Area (ha)"
    # after we have given them a workable-with name we can attatch it to the empty object created before the saveVideo was run
    # every run of the loop, with every i value, adds one more row to the areasall object
    # we need all of them as it grows to plot on the graphs
    areasall=rbind(areasall,areas)
    
    # same as before, code that converts the UTM based shapes to WGS84 to help with plotting in ggmap
    SPlatlong = spTransform(SP, latlong)
    MCPlatlong = spTransform(MCP, latlong)
    K50latlong = spTransform(K50, latlong)
    K95latlong = spTransform(K95, latlong)
    
    ## the next chunk is purely defining how the ggmap plot should look
    # I reconmmend people looking up ggplot cheatsheets and the like to investigate the various options that ggplot can provide
    # ggplot is pretty great and there are lots of aesthetic options available, experiment...
    # I would recommend experimentation outside of the saveVideo function to save time and to quick re-run changes made
    
    # first line is tellng r to store it in fullmap and to base it on the previously stored map object
    # this produces the satellite backed map found in the top left
    # map was defined earlier using get_map()
    fullmap=ggmap(map)+
      geom_polygon(data = fortify(K95latlong), # these three geom_polygon()s plot all our defined shapefiles
                   aes(long, lat, group = group),
                   fill = "white",  alpha = 0.3)+
      geom_polygon(data = fortify(K50latlong), # fortify is just a function that coerces shapefiles into a more approachable format for ggmap
                   aes(long, lat, group = group),
                   fill = "white",  alpha = 0.3)+
      geom_polygon(data = fortify(MCPlatlong),
                   aes(long, lat, group = group),
                   colour="black",  alpha = 0)+
      geom_point(data = as.data.frame(SPlatlong),
                 aes(x = coords.x1, y = coords.x2),
                 colour = "black", pch=3, size=0.2)+
      geom_point(data = as.data.frame(SPlatlong), # plots the points of the SP object we defined, but we need to coerce it to a dataframe to get at the real coords
                 aes(x = coords.x1[i], y = coords.x2[i]),
                 colour = "red", pch=16, size=1)+
      geom_segment(data = as.data.frame(SPlatlong), # geom_segment() draws a line between two points, the first segment is between the last and penultimate locations
                   aes(x = coords.x1[i-1], xend = coords.x1[i],
                       y = coords.x2[i-1], yend = coords.x2[i]), size=1)+
      geom_segment(data = as.data.frame(SPlatlong), # this is between the penultimate and pen-penultimate locations
                   aes(x = coords.x1[i-2], xend = coords.x1[i-1],
                       y = coords.x2[i-2], yend = coords.x2[i-1]), size=0.75, alpha=0.5)+
      geom_segment(data = as.data.frame(SPlatlong), # and again
                   aes(x = coords.x1[i-3], xend = coords.x1[i-2],
                       y = coords.x2[i-3], yend = coords.x2[i-2]), size=0.5, alpha=0.25)+
      labs(title=CurrSet$ID[1], subtitle = paste(CurrSet$Tracking_date[length(CurrSet$ID)], # a line that prints a title with the snakes name and the most recent datapoint
                                                 CurrSet$Tracking_time[length(CurrSet$ID)]),
           x="Longitude", y="Latitude")+
      theme_bw()+ # one of the many themes in ggplot
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+ # a line that neatens the borders
      theme(axis.text.x = element_text(angle = 45, hjust = 1), # changes the scales
            text=element_text(size=12))+
      theme(axis.text.y = element_text(angle = 45, hjust = 1), # changes the scales
            text=element_text(size=12))
    
    # uses the preivously defined chullget function to produce a convex hull based on the subset of data for this run
    hull = chullget(CurrSet)
    
    # this section creates the bottom left graph that shows the 100% MCP being built as the locations are added
    g=ggplot(CurrSet) + geom_point(aes(x=CurrSet$Easting, y=CurrSet$Northing), alpha=0.2, pch=16) +
      geom_point(aes(x=CurrSet$Easting[i], y=CurrSet$Northing[i]), alpha=1, pch=21, colour="red") + # this is the bit that highlights only the most recent of points
      geom_segment(aes(x = CurrSet$Easting[i-1], xend = CurrSet$Easting[i],
                       y = CurrSet$Northing[i-1], yend = CurrSet$Northing[i]), size=1) +
      geom_segment(data = as.data.frame(SPlatlong),
                   aes(x = coords.x1[i-2], xend = coords.x1[i-1],
                       y = coords.x2[i-2], yend = coords.x2[i-1]), size=0.75, alpha=0.5)+
      geom_segment(data = as.data.frame(SPlatlong),
                   aes(x = coords.x1[i-3], xend = coords.x1[i-2],
                       y = coords.x2[i-3], yend = coords.x2[i-2]), size=0.5, alpha=0.25)+
      geom_polygon(data = hull, aes(x=hull$Easting, y=hull$Northing), alpha = 0.2) + # this adds the conex hull we made before
      theme_bw() +
      coord_cartesian(xlim = c(min(animaldata$Easting), max(animaldata$Easting)), # make sure that the scales remain constant between frames
                      ylim = c(min(animaldata$Northing), max(animaldata$Northing)))+ # so we make sure they are limted to the max and min values of the full dataset
      labs(title = "", x = "Easting", y= "Northing")+
      theme(axis.text.y = element_text(angle = 45, hjust = 1),
            text=element_text(size=12))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text=element_text(size=12))
    
    # this line just makes sure that Tracking_date is being read as a date, if it wasn't the x axis would not work
    # it would be labelled with numbers and we couldn't define breaks as a peroid of time
    CurrSet$Tracking_date = as.Date(CurrSet$Tracking_date, format= "%Y-%m-%d")
    
    distanceVtime = ggplot(CurrSet) + geom_point(aes(x=Tracking_date, y=CsumMoves)) +
      geom_point(aes(x=Tracking_date[length(CurrSet$ID)], y=CsumMoves[length(CurrSet$ID)]), pch=21, colour="red") +
      geom_line(aes(x=Tracking_date, y=CsumMoves), alpha=0.8) +
      theme_bw() +
      coord_cartesian(xlim = c(min(animaldata$Tracking_date), max(animaldata$Tracking_date)),
                      ylim = c(min(animaldata$CsumMoves), max(animaldata$CsumMoves)))+
      scale_x_date(breaks=date_breaks("4 days"), # making sure the x axis is something sensible using the above mentions scales package
                   labels=date_format("%d %m %Y"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            text=element_text(size=12))+
      annotate("text", x=as.Date(animaldata$Tracking_date[length(animaldata$ID)]), y=max(animaldata$CsumMoves)/10, size=4,
               label=paste("Distance =", round(CurrSet$CsumMoves[length(CurrSet$ID)]/1000, digits = 2), "km" ),
               hjust = 1)+ # this little chunk simply places the total distance moved on the graph and will update per frame
      scale_y_continuous(breaks = seq(0, max(animaldata$CsumMoves),1000))+ # y and x axis breaks will need to be changed dependent on your study species
      labs(title = "", 
           x = "Date", y= "Distance Moved (m)")+
      theme(axis.text.y = element_text(angle = 45, hjust = 1),
            text=element_text(size=12))
    
    # once again making sure r is reading this variable as a date in the correct format
    areasall$Date=as.Date(areasall$Date, format="%Y-%m-%d")
    
    # melt is an odd function that sort of flattens a dataframe
    # we need to do this so we can use can split the colour by a facotrised variable in a second
    meltedareas=melt(areasall)
    # Now I had some difficutlies here getting the date to read properly as you can see from the melt results
    # it has been returned as a numeric value
    # this next line is to get it into the correct data format, you can use this to test for the correct origin and date format
    meltedareas[meltedareas$variable=="Date",][,2]= as.Date(meltedareas[meltedareas$variable=="Date",][,2],
                                                            origin = as.Date("1900-01-01", format="%Y-%m-%d"))
    
    # this creates a new column filled with the date ready for ggplot, 
    ## DOUBLE CHECK this stage and make sure that the origin is correct for your data - THIS WILL BE THE ONE DISPLAYED
    # I know there are some irritating differences between the origin used by Mac and Windows Excel versions,
    # something like a 4 year difference
    meltedareas$Date=as.Date(meltedareas[meltedareas$variable=="Date",][,2],
                             origin = as.Date("1900-01-01", format="%Y-%m-%d"))
    # we can now get rid of that bottom row so we only have values in 'variable' that we want to use as a factor in the ggplot
    meltedareas=meltedareas[!meltedareas$variable == "Date",]
    
    # A quick line to generate an x location for the labels
    xloclabel = min(as.Date(animaldata$Tracking_date)) + ( max(as.Date(animaldata$Tracking_date))-min(as.Date(animaldata$Tracking_date)) )*0.75
    
    # similar situation as above, a whole series of lines generating the ggplot for the areas

    
    Grapharrange=ggdraw() +
      draw_plot(fullmap, x = 0, y = 0.5, width = 0.5, height = 0.5) +
      draw_plot(distanceVtime, x = 0.50, y = 0, width = 0.5, height = 0.5) +
      draw_plot(g, x = 0.0, y = 0, width = 0.5, height = 0.5)
    
    # next few lines will print a few lines to the console as the code runs keeping you updated on progress
    # each frame generated will have its own print out
    # it starts at five because the kernel estimations require at least 5 points to be calculated
    print(paste("Datapoint", i,"/",length(animaldata$ID), "Complete"))
    print(paste(CurrSet$ID[1], 
                CurrSet$Tracking_date[length(CurrSet$ID)], 
                CurrSet$Tracking_time[length(CurrSet$ID)]))
    print(Grapharrange)
  }
  beep(2)
}, video.name = paste0(animaldata$ID[1], "animated.mp4"))


dev.off()