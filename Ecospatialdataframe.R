library(ecospat)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer) 
library(raster)
library(rgbif)
library(mapview)


#Pull species data from GBIF
fam <- name_suggest(q = "Coniophanes imperialis", rank = "species")
occ_search(taxonKey = fam$key, return = "meta")$count
dat <- occ_search(taxonKey = fam$key,return = "data", limit = 500) #we limit the number of records to 10000, to restrict download time, remember to change this, if you want to download your own big dataset

#pull species data to compare
fam <- name_suggest(q = "Rhadinaea decorata", rank = "species")
occ_search(taxonKey = fam$key, return = "meta")$count
dat1 <- occ_search(taxonKey = fam$key,return = "data", limit = 500) #we limit the number of records to 10000, to restrict download time, remember to change this, if you want to download your own big dataset

dat <- dat$`9166116`
######################
sp = dat
sp1= dat1

sp$lon <-sp$decimalLongitude
sp$lat <-sp$decimalLatitude
sp1$lon <-sp1$decimalLongitude
sp1$lat <-sp1$decimalLatitude

sp$species <- as.factor(dat$species)
sp1$species <- as.factor(dat1$species)

sp <- sp[,c( 'lon', 'lat', 'species')]
sp1 <- sp1[,c( 'lon', 'lat', 'species')]

spec1<-sp
spec2<-sp1

sp$species <- dat$species
sp1$species <- dat1$species


#Remove all NA values
sp<-sp[-which(is.na(sp)),]
sp1<-sp1[-which(is.na(sp1)),]
spec1<-spec1[-which(is.na(spec1)),]
spec2<-spec2[-which(is.na(spec2)),]


#################Get raster data#########################

sp<-rbind(sp, sp1)

coordinates(sp) <- ~lon + lat
class(sp)


#------------------------------

bio <-raster::getData('worldclim', var='bio', res=10)
bio

library(usdm)
v1 <- vifstep(bio)
v2 <- vifcor(bio, th=0.7)

v1

v2

#Raster file 
biom <- exclude(bio, v2)



plot(biom[[1]])
points(sp, cex=0.5, pch=16)


proj4string(sp) <- projection(raster())


mapview(sp)
head(sp)
#--------------------------------

#########################
#make ecospat data frame

#Combine species data that will be tested
spec.list <- as.data.frame(rbind(spec1, spec2))
sapply(spec.list, class)


#Combine data in a ecospat dataframe
plot(biom[[1]])

points(spec.list, cex=0.5, pch=16)


#Remove NA to fit spec.list to raster file
spec.list <- as.data.frame(spec.list)
df <- extract(biom, spec.list[,1:2])
spec.list.red <- spec.list[complete.cases(df),]

#Make your organized ecospat dataframe, change your n value to an appropraite 
# number for your data. 
df <- ecospat.makeDataFrame(spec.list.red, expl.var=biom, n=5000)
head(df)



pca_Ph_Pl <-dudi.pca(na.omit(df, center = T, scale = T, scannf = F, nf = 2))  
2 

# Now, we can use directly this result with the niceOverPlot function to represent a   
# 2D environmental space in a central plot, and each environmental gradient represented   
# by each axis at top and right. We must provide to the function with the number of presences  
# of each species in the same order as in the input data (nº of presences  
# for Sp1 and nº of presences for Sp2)  
x11(height = 10, width = 15)
niceOverPlot(pca_Ph_Pl, n1=357 , n2= 365)  



