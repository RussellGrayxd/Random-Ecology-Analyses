rmarkdown::render("image_point_analysis.R")

library(imager)
library(spatstat)
library(ggplot2)
library(ggpubr)
library(ecespa)

#-----------------------------------------
setwd("C:/Users/Russe/Desktop/Helping with R/Image analyses")
list.files()

#-----------------------------------------

# Import the image file
im <- load.image("wildlifetrusts_40661540865.jpg")
plot(im)

# greyscale
im.g <- grayscale(im)
# since the birds are white, lets turn it negative
im.g <- -im.g
plot(im.g)

# determine the best threshold to identify animals
layout(t(1:3))
threshold(im.g,"25%") %>% plot
threshold(im.g,"15%") %>% plot 
threshold(im.g,"10%") %>% plot

# create df and summarize
df <- as.data.frame(im.g)
head(df,5)


m <- lm(value ~ x + y,data=df) #linear trend
summary(m)


# extract and remove the luminance based on linear trend
layout(t(1:2))
im.f <- im.g-fitted(m)
plot(im.g,main="Before")
plot(im.f,main="After trend removal")



# morphological operations
im.t <- threshold(im.f,"3%")
px <- as.pixset(1-im.t) #Convert to pixset
plot(px)


# enlarge the pixset by 3 pixels
grow(px,2) %>% plot(main="Growing by 3 pixels") # this one is better


# shrink the pixet by 3 pixels
shrink(px,2) %>% plot(main="Shrinking by 3 pixels")

# check side by side
layout(t(1:2))
plot(px,main="Original")
shrink(px,3) %>% grow(3) %>% plot(main="Shrink, then grow")


# clean fills holes
layout(t(1:2))
plot(px,main="Original")
grow(px,2)%>% plot(main="Grow, then shrink")


par(mfrow=c(1,2))
# get the outline of points
plot(im)
fill(px,3) %>% clean(1) %>% highlight

## Split into connected components (individual coins)
pxs <- split_connected(px)
## Compute their respective area
area <- sapply(pxs,sum)
## count the birds
length(pxs)


# convert  to points by changing to df and aggregating the data mean values
# therefore plotting a point in the center of each polygon
pxs_df <- as.data.frame(pxs)
pxs_df <- pxs_df[,c(1,2,3)]
pxs_agg = aggregate(pxs_df,
                by = list(pxs_df$im),
                FUN = mean)

# check the data
plot(im)
points(pxs_agg$x, pxs_agg$y, col ="red", pch = 16)

# there are a few duplicate counts, let see if we can
# run a nearest neighbor cluster to remove them

# create a new df
xy <- as.data.frame(cbind(pxs_agg$x, pxs_agg$y))
colnames(xy) <- c("x","y")
head(xy)

# use the nndist() fucntion by spatstat to aggregate nearest neighbors
# the k argument indicates whihc nearest neighbor to aggregate
# nearest neighbours
xy$d <- nndist(xy)

# second nearest neighbours
xy$d2 <- nndist(xy, k=2)

# check the summary
summary(xy$d)
quantile(xy$d, 0.25)

# remove the 1st Qu.nearest neighbor values
xy_remnn <- subset(xy, d > quantile(xy$d, 0.25))

# check the data side by side
par(mfrow=c(1,2))
plot(im, main = "Before NN removal")
points(pxs_agg$x, pxs_agg$y, col ="red", pch = 16)
# Add a legend
legend("top", 
       legend = c("presences"), 
       col = c("red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = T, 
       inset = c(0.1, 0.1))
plot(im, main = "After NN removal")
points(pxs_agg$x, pxs_agg$y, col ="blue", pch = 16);
points(xy_remnn$x,xy_remnn$y, col ="red", pch = 16)

# Add a legend
legend("top", 
       legend = c("presences", "removed"), 
       col = c("red", "blue"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = T, 
       inset = c(0.1, 0.1))


#---------------------------------------------------
#####################################################
############ Count and density statistics############
#####################################################

# make a new dataframe with only x,y columns
df <- xy_remnn[,c(1,2)]

# convert to a ppp object for patstat analyses
df_ss    <- haz.ppp(df)

# plot to ensure match with new ppp object
par(mfrow=c(1,2))
plot(im);
plot(df_ss, main=NULL, cols="red", pch=20, add =TRUE)


# make quadrats for counts
# nx=num. rows, ny= num. columns
Q <- quadratcount(df_ss, nx= 6, ny=3)
plot(im)
plot(df_ss, pch=20, cols="red", main=NULL, add=TRUE)  # Plot points
plot(Q, add=TRUE, col = "white", size=13, lwd=3, cex = 2)  # Add quadrat grid


par(mfrow=c(2,2))
# Compute the density for each quadrat
Q.d <- intensity(Q)
plot(im)
# Plot the densities as a heatmap
plot(intensity(Q, image=TRUE),las=1, col=rev(heat.colors(25, alpha=0.3)), add=TRUE)  # Plot density raster
plot(df_ss, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE, alpha = 0.3)  # Add points



library(viridis)



# density with default function
K1 <- density(df_ss) # Using the default bandwidth
plot(im)
plot(K1, main=NULL, las=1, col=inferno(25, alpha=0.5), add=TRUE)
contour(K1, add=TRUE, col = "white")

# set the bandwidth sigma
K2 <- density(df_ss, sigma=50) # Using a 50km bandwidth
plot(im)
plot(K2, main=NULL, las=1, col=inferno(25, alpha=0.5), add=TRUE)
contour(K1, add=TRUE, col = "white")

# change and test other density functions
K3 <- density(df_ss, kernel = "disc", sigma=50) # Using a 50km bandwidth
plot(im)
plot(K1, main=NULL, las=1, col=inferno(25, alpha=0.5), add=TRUE)
contour(K1, add=TRUE, col = "white")

#####################################################
#####################################################
#                DISTANCE ANALYSES 


# ANN vs neighbor order plot
ANN <- apply(nndist(df_ss, k=1:df_ss$n),2,FUN=mean)
plot(ANN ~ eval(1:df_ss$n), type="b", main=NULL, las=1)

# K functions
K <- Kest(df_ss)
plot(K, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))

# L functions
L <- Lest(df_ss, main=NULL)
plot(L, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))

# plot L function and the Lexpected line set
plot(L, . -r ~ r, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))


# pair the correlation g
g  <- pcf(df_ss)
plot(g, main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))




#####################################################
#####################################################
#                HYPOTHESIS TESTING
par(mfrow=c(1,1))
#run an (Average nearest neighbor analysis) ANN analysis for birds locations 
# assuming a uniform point density
ann.p <- mean(nndist(df_ss, k=1))
ann.p

# Next, we will generate the distribution of 
# expected ANN values given a homogeneous (CSR/IRP) 
# point process using Monte Carlo methods. 
# This is our null model.
n     <- 599L               # Number of simulations
ann.r <- vector(length = n) # Create an empty object to be used to store simulated ANN values
for (i in 1:n){
        rand.p   <- rpoint(n=df_ss$n, win=df_ss$window)  # Generate random point locations
        ann.r[i] <- mean(nndist(rand.p, k=1))  # Tally the ANN values
}

# You can plot the last realization of the 
# homogeneous point process to see what a 
# completely random placement of birds could look like.
plot(im)
points(xy_remnn$x,xy_remnn$y, col ="blue", pch = 16)
plot(rand.p, pch=16, main=NULL, cols="red", add=TRUE)
# Add a legend
legend("top", 
       legend = c("Observed", "Random"), 
       col = c("red", "blue"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = T, 
       inset = c(0.1, 0.1))

# plot the histogram of expected values under the 
# null and add a blue vertical line showing where 
# our observed ANN value lies relative to this 
# distribution.
Corner_text <- function(text, location="topright"){
        legend(location,legend=text, bty ="n", pch=NA) 
}

# plot the histogram
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")


# the observed ANN value is larger than the 
# expected ANN values one could expect under the null 
# hypothesis. A larger observed value indicates that 
# the stores are far less clustered than expected 
# under the null.

# A pseudo p-value can be extracted from 
# Monte Carlo simulations. First, we need to find the 
# number of simulated ANN values greater than our 
# observed ANN value.
N.greater <- sum(ann.r > ann.p)


# To compute the p-value, find the end of the 
# distribution closest to the observed ANN value, 
# then divide that count by the total count. Note 
# that this is a so-called one-sided P-value.
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
p

# In our working example, you’ll note that or 
# simulated ANN value was nowhere near the range 
# of ANN values computed under the null yet we 
# don’t have a p-value of zero. This is by design 
# since the strength of our estimated p will be 
# proportional to the number of simulations–this 
# reflects the chance that given an infinite 
# number of simulations at least one realization 
# of a point pattern could produce an ANN value 
# more extreme than ours.

par(mfrow=c(1,2))

plot(im)
points(xy_remnn$x,xy_remnn$y, col ="blue", pch = 16)
plot(rand.p, pch=16, main=NULL, cols="red", add=TRUE)
# Add a legend
legend("top", 
       legend = c("Observed", "Random"), 
       col = c("red", "blue"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = T, 
       inset = c(0.1, 0.1))
# plot the histogram
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue");
Corner_text(text="p = 0.00167")



