library("ConR")
library("rgbif")
library("wdpar")
library("dplyr")
library("ggmap")
library("scrubr")
library("bdvis")

#Use the name_suggest function to download records for many species based on higher taxonomic ranks
fam <- name_suggest(q = "Tylototriton uyenoi", rank = "species")
occ_search(taxonKey = fam$key, return = "meta")$count
#this pulls your data from GBIF; change the data limit to whatever you think is appropriate
spdat <- occ_search(taxonKey = fam$key,return = "data", limit = 10000) 

###############################################################
###############################################################
###############################################################
#############Current sp. of conservation concern ##############
###############################################################
###############################################################
###############################################################


#clean your occurence records (search scrubr package for more options)
spdat<-dframe(spdat) %>% coord_impossible()
spdat<-dframe(spdat) %>% coord_unlikely()
spdatNNA<-spdat[-which(is.na(spdat)),]

#Set up a dataframe with lat, lon, and taxa
reddat<-data.frame(spdat$species, spdat$latitude, spdat$longitude)

#format columns
reddat$ddlat<-reddat$spdat.latitude
reddat$ddlon<-reddat$spdat.longitude
reddat$tax<-reddat$spdat.species
reddat<-select(reddat, -spdat.latitude)
reddat<-select(reddat, -spdat.longitude)
reddat<-select(reddat, -spdat.species)
reddat$Date_collected<-spdat$eventDate

#remove NA values
reddat<-reddat[-which(is.na(reddat)),]


#make inate dataframe
EuMa$ddlat<-EuMa$latitude
EuMa$ddlon<-EuMa$longitude
EuMa$tax<-EuMa$scientific_name
EuMa<-select(EuMa, -latitude)
EuMa<-select(EuMa, -longitude)
EuMa<-select(EuMa, -scientific_name)
EuMa$Date_collected<-EuMa$observed_on
EuMa<-select(EuMa, -observed_on)

#merge inat and gbif data
alldat <-rbind(EuMa,reddat)
alldat<-alldat[-which(is.na(alldat)),]

write.csv(alldat, file = "alldat.csv")


################################################################
################################################################

#Evaluate your species conservation status
#note that this function also provides detailed maps that are saved as PNG
#files under the folder "IUCN restults map" in your wd
Results<- IUCN.eval(alldat)


#Create .csv table of your results
write.csv(Results)


##############################################################
#Form plot data
spdatNNA$coly<-spdatNNA$year
spdatNNA$Date_collected<-spdatNNA$eventDate

###############################################################
###############################################################
###############################################################
#############Focusing in on sp. of conservation concern #######
###############################################################
###############################################################
###############################################################

#plot map with species data
world = map_data("world")
X11(width=13, height=7.8)
ggplot(world, aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "gray90", 
               color = "gray40", size = .2) +
  geom_point(data = alldat,
             aes(ddlon, ddlat, color = "Red"), alpha=0.3, 
             size = 2)+
  scale_color_manual(name = "",labels = "Eunectes murinus",
                     values = "red")+
  theme_classic()+
  theme(legend.position = "bottom",legend.box = "horizontal", 
        legend.text=element_text(size=rel(1.5), face = "bold"))


###################################################################

alldat$Date_collected<-as.Date(alldat$Date_collected ,"%m/%d/%y")


# Create temporal polar plot for modern data observations 
# (better representation at species level)
X11(width=13, height=7.8)
tempolar(alldat, color="blue", plottype = "p", )

# Create calander heatmap for observations 
# (better representation at species level)
X11(width=13, height=7.8)
bdcalendarheat(indf = alldat, title = "Calendar Heatmap of E. murinus observations")