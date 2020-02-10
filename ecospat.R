library(ecospat)

ecospat.makeDataFrame (dat, use.gbif=TRUE, precision=NULL,
                       year=NULL, remdups=TRUE, mindist=NULL, n=1000)

data(ecospat.testData)
names(ecospat.testData)


data(ecospat.testNiche.inv)
names(ecospat.testNiche.inv)
  

data(ecospat.testNiche.nat)
names(ecospat.testNiche.nat)

x <- ecospat.testData[c(2,3,4:8)]
proj<- x[1:90,] #A projection dataset.
cal<- x[91:300,] #A calibration dataset
mess.object<-ecospat.mess (proj, cal, w="default")

ecospat.plot.mess (mess.object, cex=1, pch=15)

fpath <- system.file("extdata", "ecospat.testTree.tre", package="ecospat")
tree <- read.tree(fpath)
data <- ecospat.testData[9:52]

pd<- ecospat.calculate.pd(tree, data, method = "spanning", type = "species", root = TRUE, average =T)

pd

plot(pd)


inv <- ecospat.testNiche.inv

nat <- ecospat.testNiche.nat

pca.env <- dudi.pca(rbind(nat,inv)[,3:10],scannf=F,nf=2)

ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

# PCA scores for the whole study area
scores.globclim <- pca.env$li
# PCA scores for the species native distribution
scores.sp.nat <- suprow(pca.env,nat[which(nat[,11]==1),3:10])$li
# PCA scores for the species invasive distribution
scores.sp.inv <- suprow(pca.env,inv[which(inv[,11]==1),3:10])$li
# PCA scores for the whole native study area
scores.clim.nat <- suprow(pca.env,nat[,3:10])$li
# PCA scores for the whole invaded study area
scores.clim.inv <- suprow(pca.env,inv[,3:10])$li


# gridding the native niche
grid.clim.nat <- ecospat.grid.clim.dyn(glob=scores.globclim,
                                       glob1=scores.clim.nat,
                                       sp=scores.sp.nat, R=100,
                                       th.sp=0)
# gridding the invasive niche
grid.clim.inv <- ecospat.grid.clim.dyn(glob=scores.globclim,
                                       glob1=scores.clim.inv,
                                       sp=scores.sp.inv, R=100,
                                       th.sp=0)
# Compute Schoener's D, index of niche overlap
D.overlap <- ecospat.niche.overlap (grid.clim.nat, grid.clim.inv, cor=T)$D
D.overlap


eq.test <- ecospat.niche.equivalency.test(grid.clim.nat, grid.clim.inv,
                                          rep=10, alternative = "greater")


sim.test <- ecospat.niche.similarity.test(grid.clim.nat, grid.clim.inv,
                                          rep=10, alternative = "greater",
                                          rand.type=2)

ecospat.plot.overlap.test(eq.test, "D", "Equivalency")


ecospat.plot.overlap.test(sim.test, "D", "Similarity")


niche.dyn <- ecospat.niche.dyn.index (grid.clim.nat, grid.clim.inv, intersection = 0)




x11(width = 15, height = 7)
par(mfrow=c(1,2))

#Plot niche overlap map
ecospat.plot.niche.dyn(grid.clim.nat, grid.clim.inv, quant=0.25, interest=2,
                       title= "Niche Overlap", name.axis1="PC1",
                       name.axis2="PC2")
ecospat.shift.centroids(scores.sp.nat, scores.sp.inv, scores.clim.nat, scores.clim.inv)


# gridding the native niche
grid.clim.t.nat <- ecospat.grid.clim.dyn(glob=as.data.frame(rbind(nat,inv)[,10]),
                                         glob1=as.data.frame(nat[,10]),
                                         sp=as.data.frame(nat[which(nat[,11]==1),10]),
                                         R=1000, th.sp=0)




# gridding the invaded niche
grid.clim.t.inv <- ecospat.grid.clim.dyn(glob=as.data.frame(rbind(nat,inv)[,10]),
                                         glob1=as.data.frame(inv[,10]),
                                         sp=as.data.frame(inv[which(inv[,11]==1),10]),
                                         R=1000, th.sp=0)
t.dyn<-ecospat.niche.dyn.index (grid.clim.t.nat, grid.clim.t.inv,
                                intersection=0.1)
ecospat.plot.niche.dyn(grid.clim.t.nat, grid.clim.t.inv, quant=0,
                       interest=2, title= "Niche Overlap",
                       name.axis1="Average temperature")



############################################################################

#Biotic interactions
data <- ecospat.testData[c(9:16,54:57)]

ecospat.co_occurrences (data)


#Pairwise occurence analysis 
data<- ecospat.testData[c(53,62,58,70,61,66,65,71,69,43,63,56,68,57,55,60,54,67,59,64)]
nperm <- 100
outpath <- getwd()
ecospat.Cscore(data, nperm, outpath)


#Data preparation for correlation plot of variables
data <- ecospat.testData[,4:8]
ecospat.cor.plot(data)


#Callibration and evaluation of dataset
data <- ecospat.testData
caleval <- ecospat.caleval (data = ecospat.testData[53], xy = data[2:3],
                            row.num = 1:nrow(data), nrep = 2, ratio = 0.7,
                            disaggregate = 0.2, pseudoabs = 100, npres = 10,
                            replace = FALSE)

#Core niche modelling

fit <- ecospat.testData$glm_Saxifraga_oppositifolia

obs<-ecospat.testData$glm_Saxifraga_oppositifolia[which(ecospat.testData$Saxifraga_oppositifolia==1)]


#Calculate Boyce index
ecospat.boyce (fit, obs, nclass = 0, window.w = "default", res = 100,
               PEplot = TRUE)$Spearman.cor


                                                  



