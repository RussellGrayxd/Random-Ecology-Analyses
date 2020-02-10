if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")

library(BiocManager)
library(EBImage)
library(keras)

#Read images
setwd("C:/Users/Russell/Desktop/Inat Data/R Ecology Training/Image recognition/image recognition/Camera Traps/Training Test")

#make a string of file names in your camera trap folder 
#make sure you only have camera trap images with >=10 labelled for training
pics<-list.files()

#create an empty list to be populated with the followig function
mypic<-list()


for (i in 1:length(list.files())) {mypic[[i]] <-readImage(pics[i])}

#explore
#print(mypic[[1]])
#display(mypic[[8]])
#summary(mypic[[1]])
#hist(mypic[[1]])
#str(mypic)


#Resize images so they will all be a standard size
for (i in 1:length(list.files())) {mypic[[i]]<-resize(mypic[[i]], 28, 28)}


# Reshape the data according to standard image size
for (i in 1:length(list.files())) {mypic[[i]] <- array_reshape(mypic[[i]], c(28,28,3))}


#combine all presence and absence data into one for training 
#choose 10 images that only show your background, 
#the next 10 photos only photos with animals
# first, identify the photos to be used in the folder as "presence" and "absence"
list.files() #the numbers will be next to the left of the photos called "training"

trainx <- NULL
for(i in 1:10) {trainx <- rbind(trainx, mypic[[i]])}
for(i in 11:20) {trainx <- rbind(trainx, mypic[[i]])}
str(trainx)

#Create a test sample with one of each image
#this will include a single presence and single absence image
#you can add any that are within your trianing set for each
setwd("C:/Users/Russell/Desktop/Inat Data/R Ecology Training/Image recognition/image recognition/Camera Traps/Test2")
#call in photos to test
picstest<-list.files()
#create an empty list to be populated with the followig function
mypictest<-list()
for (i in 1:length(list.files())) {mypictest[[i]] <-readImage(picstest[i])}
#Resize images so they will all be a standard size
for (i in 1:length(list.files())) {mypictest[[i]]<-resize(mypictest[[i]], 28, 28)}
# Reshape the data according to standard image size
for (i in 1:length(list.files())) {mypictest[[i]] <- array_reshape(mypictest[[i]], c(28,28,3))}

################################
library(dplyr)
testx <- do.call("rbind", mypictest)
trainy<-c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1) #these indicate presence/absence of training photos
testy<- c(0,0,0,1,1,1,1,1,1,1,1,1)


#One hot encoding
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)
trainLabels


#Create the model
model <- keras_model_sequential()
model %>%
          layer_dense(units = 256, activation = "relu", input_shape = c(2352)) %>%
          layer_dense(units = 128, activation = 'relu') %>%
          layer_dense(units = 2, activation = 'softmax')

summary(model)


#Compile your model
model %>%
  compile(loss = 'binary_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy'))



# fit model
history <- model %>%
  fit(trainx,
      trainLabels,
      epochs = 30,
      batch_size = 32,
      validation_split = 0.2)


plot(history)


#evaluate the model
model %>% evaluate(trainx, trainLabels)

pred <- model %>% predict_classes(trainx)
prob <- model %>% predict_proba(trainx)
cbind(prob, Predited = pred, Actual = trainy)



#evaluate and predict test data
model %>% evaluate(testx, testLabels)
pred <- model %>% predict_classes(testx)




images.vec <- list.files(pattern = ".JPG", full.names = FALSE)
pres.img <- images.vec[pred == 1]
abs.img <- images.vec[pred == 0]
pres.img.loc <- picstest
presfold <- "./Presence/"
dir.create(presfold)
pres.img.loc
file.copy(from = pres.img.loc,
          to = paste0(presfold, "pres", seq(1, length(pres.img.loc), 1), ".JPG"))

###################################################################
