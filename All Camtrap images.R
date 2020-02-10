######################################################################################################
################ Image recognition and categorization of misfired camtrap data #######################
######################################################################################################


# first you have to dowbnload Biocmanager to install EBI images
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")

# then install the other packages if you don't already have them
install.packages("keras")
install.packages("dplR")


# open the necessary packages in library
library(BiocManager)
library(EBImage)
library(keras)
library(dplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~ IMPORT AND RESHAPE IMAGES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# You will likely be promted to download and install miniconda if you have no python installed on your
# system, just press "Y" and proceed when prompted and it will auto-install

# First, set your working directory to the "Training Test" folder with
# 10 presence (animal in image), and 10 absence (no animal) images
# NoteL when training your own model, you should try to classify more of these images, like 
# use 100 presence and 100 absence, that will ensure the model is at or near 100% accuracy
setwd("C:/Users/Russe/Desktop/Inat Data/R Ecology Training/Image recognition/image recognition/Camera Traps/Training")

# make a string of file names in your camera trap folder 
# make sure you only have camera trap images labelled for training ("presence" & "absence")
# and no other files
# "pic" will store the string of file names
pic <- list.files()

# this function iterates through the images, importing them, resizing/standardize them, and turning them
# into a readable matrix that can be used by keras. It is important that this step is iterative,
# otherwise you would be importing massive image files and crash R. 
mypic <- lapply(pic, function(x) {
  x %>%
    readImage() %>%
    resize(28, 28) %>%
    array_reshape(c(28,28,3))
})


# combine all presence and absence data into one object for training 
# choose 10 images that only show your background, 
# the next 10 photos only photos with animals
# first, identify (name) the photos in the folder as either "presence" and "absence"
list.files() 

trainx <- NULL # create an empty object to fill with data
for(i in 1:15) {trainx <- rbind(trainx, mypic[[i]])} # the first photos listed will be absence (alphabetic order)
for(i in 16:30) {trainx <- rbind(trainx, mypic[[i]])} # the next ten are presence


# Read inyour test set with a few presence and absence images
# if you label them, you will be able to see the accuracy of the 
# model when they are categorized without opening every file to check.
# set your working directory to the folder "Test2"
setwd("C:/Users/Russe/Desktop/Inat Data/R Ecology Training/Image recognition/image recognition/Camera Traps/Test")

# call in photos to test
picstest <- list.files()

# resize and reshape all the files so R isn't carrying a massive load
mypictest<-lapply(picstest, function(x) {
  x %>%
    readImage() %>%
    resize(28, 28) %>%
    array_reshape(c(28,28,3))
})


################################
# we will now combine the test set into a matrix
testx <- do.call("rbind", mypictest)
trainy<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #these indicate presence/absence of training photos
testy<- c(0,0,0,0,0,1,1,1,1,1,1,1) # these indicate the presence absence of the test photos
# make sure you classify them correctly with your own data, remember absence files will be read first
# because R reads files in alphabetic order


# One hot encoding 
# learn more about that here: 
# https://hackernoon.com/what-is-one-hot-encoding-why-and-when-do-you-have-to-use-it-e3c6186d008f
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)
trainLabels # make sure we have our presence and absence in seperate columns


#Create the model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(2352)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')

summary(model) # check our model summary, make sure all parameters are trainable


#Compile the model
model %>%
  compile(loss = 'binary_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy'))


# fit the model
# Note: in machine learning, we want our model to converge at a "global minimum"
# so with each iteration of gradient descent should be decreasing and the line 
# should flatline at 0
history <- model %>%
  fit(trainx,
      trainLabels,
      epochs = 30,
      batch_size = 32,
      validation_split = 0.20)


plot(history) # check our plot accuracy and loss


# evaluate the model
model %>% evaluate(trainx, trainLabels) # accuracy should be at or near 100%

# predict the model and classify training images
pred <- model %>% predict_classes(trainx)
# What probability do we have of correct classification?
prob <- model %>% predict_proba(trainx)
cbind(prob, Predited = pred, Actual = trainy) #Check our accuracy if 100% predicted and actual will match



# evaluate and predict test data
model %>% evaluate(testx, testLabels)
pred <- model %>% predict_classes(testx)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ DIVIDE TEST DATA INTO PRESENCE ABSENCE FOLDERS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Make sure working directory is still in "Test2" folder
setwd("C:/Users/Russe/Desktop/Inat Data/R Ecology Training/Image recognition/image recognition/Camera Traps/Test")

# set images that were classified as 1 in a folder named "presence"
# set images that were classified as 0 in a folder named "absence
images.vec <- list.files(pattern = ".JPG", full.names = FALSE, recursive = TRUE)
pres.img <- images.vec[pred == 1]
abs.img <- images.vec[pred == 0]
pres.img.loc <- list.files(path = ".", 
                           pattern = paste(pres.img, collapse = "|"), 
                           full.names = TRUE)
presfold <- "./Presence/"
dir.create(presfold)
pres.img.loc

file.copy(from = pres.img.loc,
          to = paste0(presfold, "pres", seq(1, length(pres.img.loc), 1), ".JPG"))

abs.img.loc <- list.files(path = ".", 
                          pattern = paste(abs.img, collapse = "|"), 
                          full.names = TRUE)

absfold <- "./absence/"
dir.create(absfold)

file.copy(from = abs.img.loc,
          to = paste0(absfold, "abs", seq(1, length(abs.img), 1), ".JPG"))



# ~~~~~~~~~~~~~~~~~~~~~ TEST MODEL ON LARGER DATASET ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set working directory to all camera trap images to see how the model works with them
# make sure there is no other files but camera trap images in this folder
setwd("C:/Users/Russe/Desktop/Inat Data/R Ecology Training/Image recognition/image recognition/Camera Traps/Camtraps_all")

# call in photos to test
camtraps <- list.files()

# resize and reshape all the files so R isn't carrying a massive load
mycamtraps<-lapply(camtraps, function(x) {
  x %>%
    readImage() %>%
    resize(28, 28) %>%
    array_reshape(c(28,28,3))
})

# combine the data into a matrix
testx2 <- do.call("rbind", mycamtraps)
# predict them using the model you created
pred2 <- model %>% predict_classes(testx2)

########## divide into folders

images.vec <- list.files(pattern = ".JPG", full.names = FALSE, recursive = TRUE)
pres.img <- images.vec[pred2 == 1]
abs.img <- images.vec[pred2 == 0]
pres.img.loc <- list.files(path = ".", 
                           pattern = paste(pres.img, collapse = "|"), 
                           full.names = TRUE)
presfold <- "./Presence/"
dir.create(presfold)
pres.img.loc

file.copy(from = pres.img.loc,
          to = paste0(presfold, "pres", seq(1, length(pres.img.loc), 1), ".JPG"))

abs.img.loc <- list.files(path = ".", 
                          pattern = paste(abs.img, collapse = "|"), 
                          full.names = TRUE)

absfold <- "./absence/"
dir.create(absfold)

file.copy(from = abs.img.loc,
          to = paste0(absfold, "abs", seq(1, length(abs.img), 1), ".JPG"))

