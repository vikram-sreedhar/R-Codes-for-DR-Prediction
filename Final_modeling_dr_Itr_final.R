library(keras)
library(EBImage)
library(tensorflow)
library(tfruns)
library(e1071)
library(kernlab)
library(readr)
library(caret)
library(caTools)
library(Boruta)
library(mlbench)
library(psych)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

### DATA PREPERATION & EXPLORATION ###############################################################################################

##### T R A I N    D A T A ######

#Read Images No DR


Folder <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_0"
files <- list.files(path = Folder, pattern = "*.jpg", full.names = TRUE)


images_0<-list()


for (i in c(901:950,1801:1850,2701:2750,3601:3650,4501:4550,5401:5450,6301:6350,7201:7250,8101:8150,9001:9050,10951:11000,11851:11900,12751:12800,13651:13700,14551:14600,15551:15600,16451:16500,17351:17400,18251:18300,19151:19200,20051:20100,21901:21950,22801:22850,23701:23750,24651:24700,25975:26025)) {images_0[[i]] <- readImage(files[i])}

str(images_0)


images_0

# Explore,
print(images_0[[26012]])
summary(images_0[[26012]])
EBImage::display(images_0[[26012]])
plot(images_0[[26025]])
graphics::hist(images_0[[26012]])

par("mar")
#[1] 5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))


par(mfrow = c(10, 10))
for (i in 25976:26025) plot(as.raster(images_0[[i]]))

par(mfrow = c(10, 10))
for (i in 25976:26025) plot(images_0[[i]])


# Resize 
for (i in c(901:950,1801:1850,2701:2750,3601:3650,4501:4550,5401:5450,6301:6350,7201:7250,8101:8150,9001:9050,10951:11000,11851:11900,12751:12800,13651:13700,14551:14600,15551:15600,16451:16500,17351:17400,18251:18300,19151:19200,20051:20100,21901:21950,22801:22850,23701:23750,24651:24700,25975:26025))  {images_0[[i]] <- EBImage::resize(images_0[[i]], 28, 28)}

# Reshape
for (i in c(901:950,1801:1850,2701:2750,3601:3650,4501:4550,5401:5450,6301:6350,7201:7250,8101:8150,9001:9050,10951:11000,11851:11900,12751:12800,13651:13700,14551:14600,15551:15600,16451:16500,17351:17400,18251:18300,19151:19200,20051:20100,21901:21950,22801:22850,23701:23750,24651:24700,25975:26025)) {images_0[[i]] <- array_reshape(images_0[[i]], c(28,28,3))}

#Row Bind
data0_x <- NULL
for (i in c(901:950,1801:1850,2701:2750,3601:3650,4501:4550,5401:5450,6301:6350,7201:7250,8101:8150,9001:9050,10951:11000,11851:11900,12751:12800,13651:13700,14551:14600,15551:15600,16451:16500,17351:17400,18251:18300,19151:19200,20051:20100,21901:21950,22801:22850,23701:23750,24651:24700,25975:26025)) {data0_x <- rbind(data0_x, images_0[[i]])}
str(data0_x)

# Dependent categorical variable for No DR
data0_y<-rep(c(0),times=1301)


#Read Images Mild DR


Folder1 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_1"
files1 <- list.files(path = Folder1, pattern = "*.jpg", full.names = TRUE)
images_1<-list()

for (i in c(501:550,851:900,1235:1284,1915:1964,2051:2100,2975:3024,3445:3494,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8565:8614,9375:9424,9775:9824,10155:10204,10665:10714,11155:11204)) {images_1[[i]] <- readImage(files1[i])}

str(images_1)


images_1

# Explore,
print(images_1[[11192]])
summary(images_1[[11192]])
EBImage::display(images_1[[11192]])
plot(images_1[[11912]])
graphics::hist(images_1[[11912]])

par("mar")
#[1] 5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))


par(mfrow = c(10, 10))
for (i in 10445:10544) plot(as.raster(images_0[[i]]))

par(mfrow = c(10, 10))
for (i in 10945:11044) plot(images_1[[i]])


# Resize 
for (i in c(501:550,851:900,1235:1284,1915:1964,2051:2100,2975:3024,3445:3494,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8565:8614,9375:9424,9775:9824,10155:10204,10665:10714,11155:11204)) {images_1[[i]] <- EBImage::resize(images_1[[i]], 28, 28)}

# Reshape
for (i in c(501:550,851:900,1235:1284,1915:1964,2051:2100,2975:3024,3445:3494,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8565:8614,9375:9424,9775:9824,10155:10204,10665:10714,11155:11204)) {images_1[[i]] <- array_reshape(images_1[[i]], c(28,28,3))}

#Row Bind
data1_x <- NULL
for (i in c(501:550,851:900,1235:1284,1915:1964,2051:2100,2975:3024,3445:3494,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8565:8614,9375:9424,9775:9824,10155:10204,10665:10714,11155:11204)) {data1_x <- rbind(data1_x, images_1[[i]])}
str(data1_x)
summary(data1_x)
# Dependent categorical variable for mild DR
data1_y<-rep(c(1),times=1050)



#Read Images moderate DR 


Folder2 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_2"
files2 <- list.files(path = Folder2, pattern = "*.jpg", full.names = TRUE)
images_2<-list()

for (i in c(501:550,851:900,1235:1284,1915:1964,2051:2100,2975:3024,3445:3494,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8565:8614,9375:9424,9775:9824,10155:10204,10665:10714,11155:11204)) {images_2[[i]] <- readImage(files2[i])}

str(images_2)


images_2

# Explore,
print(images_2[[7612]])
summary(images_2[[7612]])
EBImage::display(images_2[[7612]])
plot(images_2[[7612]])
graphics::hist(images_2[[7612]])

par("mar")
#[1] 5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))


par(mfrow = c(10, 10))
for (i in 7445:7544) plot(as.raster(images_2[[i]]))

par(mfrow = c(10, 10))
for (i in 7245:7344) plot(images_2[[i]])


# Resize 
for (i in c(501:550,851:900,1235:1284,1915:1964,2051:2100,2975:3024,3445:3494,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8565:8614,9375:9424,9775:9824,10155:10204,10665:10714,11155:11204)) {images_2[[i]] <- EBImage::resize(images_2[[i]], 28, 28)}

# Reshape
for (i in c(501:550,851:900,1235:1284,1915:1964,2051:2100,2975:3024,3445:3494,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8565:8614,9375:9424,9775:9824,10155:10204,10665:10714,11155:11204)) {images_2[[i]] <- array_reshape(images_2[[i]], c(28,28,3))}

#Row Bind
data2_x <- NULL
for (i in c(501:550,851:900,1235:1284,1915:1964,2051:2100,2975:3024,3445:3494,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8565:8614,9375:9424,9775:9824,10155:10204,10665:10714,11155:11204)) {data2_x <- rbind(data2_x, images_2[[i]])}
str(data2_x)
summary(data2_x)
# Dependent categorical variable for moderate DR
data2_y<-rep(c(2),times=1050)

#Read Images severe DR


Folder3 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_3"
files3 <- list.files(path = Folder3, pattern = "*.jpg", full.names = TRUE)
images_3<-list()

for (i in c(501:550,851:900,1235:1334,1915:1964,2051:2150,2975:3024,3445:3544,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8465:8514)) {images_3[[i]] <- readImage(files3[i])}

str(images_3)


images_3

# Explore,
print(images_3[[5812]])
summary(images_3[[5812]])
EBImage::display(images_3[[5812]])
plot(images_3[[5812]])
graphics::hist(images_3[[5812]])

par("mar")
#[1] 5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))


par(mfrow = c(10, 10))
for (i in 5445:5544) plot(as.raster(images_3[[i]]))

par(mfrow = c(10, 10))
for (i in 5245:5344) plot(images_3[[i]])


# Resize 
for (i in c(501:550,851:900,1235:1334,1915:1964,2051:2150,2975:3024,3445:3544,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8465:8514)) {images_3[[i]] <- EBImage::resize(images_3[[i]], 28, 28)}

# Reshape
for (i in c(501:550,851:900,1235:1334,1915:1964,2051:2150,2975:3024,3445:3544,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8465:8514)) {images_3[[i]] <- array_reshape(images_3[[i]], c(28,28,3))}

#Row Bind
data3_x <- NULL
for (i in c(501:550,851:900,1235:1334,1915:1964,2051:2150,2975:3024,3445:3544,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8050,8465:8514)) {data3_x <- rbind(data3_x, images_3[[i]])}
str(data3_x)
summary(data3_x)
# Dependent categorical variable for severe DR
data3_y<-rep(c(3),times=950)

#Read Images Proliferative DR


Folder4 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_4"
files4 <- list.files(path = Folder4, pattern = "*.jpg", full.names = TRUE)
images_4<-list()

for (i in c(501:625,851:900,1235:1284,1915:1964,2051:2150,2975:3024,3445:3544,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8025)) {images_4[[i]] <- readImage(files4[i])}

str(images_4)


images_4

# Explore,
print(images_4[[6912]])
summary(images_4[[6912]])
EBImage::display(images_4[[6912]])
plot(images_4[[6912]])
graphics::hist(images_4[[6912]])

par("mar")
#[1] 5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))


par(mfrow = c(10, 10))
for (i in 6445:6544) plot(as.raster(images_4[[i]]))

par(mfrow = c(10, 10))
for (i in 6245:6344) plot(images_4[[i]])


# Resize 
for (i in c(501:625,851:900,1235:1284,1915:1964,2051:2150,2975:3024,3445:3544,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8025)) {images_4[[i]] <- EBImage::resize(images_4[[i]], 28, 28)}

# Reshape
for (i in c(501:625,851:900,1235:1284,1915:1964,2051:2150,2975:3024,3445:3544,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8025)) {images_4[[i]] <- array_reshape(images_4[[i]], c(28,28,3))}

#Row Bind
data4_x <- NULL
for (i in c(501:625,851:900,1235:1284,1915:1964,2051:2150,2975:3024,3445:3544,4225:4274,4995:5044,5265:5314,5885:5934,6025:6074,6985:7034,7601:7650,8001:8025)) {data4_x <- rbind(data4_x, images_4[[i]])}
str(data4_x)
summary(data4_x)
# Dependent categorical variable for Proliferative DR
data4_y<-rep(c(4),times=900)


## Combining all data array

Trainx <- rbind(data0_x,data1_x,data2_x,data3_x,data4_x)
Trainy <- abind::abind(data0_y,data1_y,data2_y,data3_y,data4_y)

# Reshaping Train data for CNN modelling

Train_fin_x<-array_reshape(Trainx,c(-1,28,28,3))

str(Train_fin_x)

summary(Train_fin_x)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.4807  0.5289  0.5387  0.6087  1.0000  

#### Test data ################################################


#Selecting 500 images in random for test data



Folder_Test_0 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_0"
files_test_0 <- list.files(path = Folder_Test_0, pattern = "*.jpg", full.names = TRUE)
images_test_0<-list()

for (i in c(4501:4550,5401:5450)) {images_test_0[[i]] <- readImage(files_test_0[i])}


Folder_Test_1 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_1"
files_test_1 <- list.files(path = Folder_Test_1, pattern = "*.jpg", full.names = TRUE)
images_test_1<-list()

for (i in c(10155:10204,10665:10714)) {images_test_1[[i]] <- readImage(files_test_1[i])}


Folder_Test_2 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_2"
files_test_2 <- list.files(path = Folder_Test_2, pattern = "*.jpg", full.names = TRUE)
images_test_2<-list()

for (i in c(4225:4274,4995:5044)) {images_test_2[[i]] <- readImage(files_test_2[i])}


Folder_Test_3 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_3"
files_test_3 <- list.files(path = Folder_Test_3, pattern = "*.jpg", full.names = TRUE)
images_test_3<-list()

for (i in c(6985:7034,7601:7650)) {images_test_3[[i]] <- readImage(files_test_3[i])}


Folder_Test_4 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_4"
files_test_4 <- list.files(path = Folder_Test_4, pattern = "*.jpg", full.names = TRUE)
images_test_4<-list()

for (i in c(4995:5044,5265:5314)) {images_test_4[[i]] <- readImage(files_test_4[i])}

str(images_test_0)
str(images_test_1)
str(images_test_2)
str(images_test_3)
str(images_test_4)


# Resize 
for (i in c(4501:4550,5401:5450)) {images_test_0[[i]] <- EBImage::resize(images_test_0[[i]], 28, 28)}
for (i in c(10155:10204,10665:10714)) {images_test_1[[i]] <- EBImage::resize(images_test_1[[i]], 28, 28)}
for (i in c(4225:4274,4995:5044)) {images_test_2[[i]] <- EBImage::resize(images_test_2[[i]], 28, 28)}
for (i in c(6985:7034,7601:7650)) {images_test_3[[i]] <- EBImage::resize(images_test_3[[i]], 28, 28)}
for (i in c(4995:5044,5265:5314)){images_test_4[[i]] <- EBImage::resize(images_test_4[[i]], 28, 28)}
# Reshape
for (i in c(4501:4550,5401:5450)) {images_test_0[[i]] <- array_reshape(images_test_0[[i]], c(28,28,3))}
for (i in c(10155:10204,10665:10714)) {images_test_1[[i]] <- array_reshape(images_test_1[[i]], c(28,28,3))}
for (i in c(4225:4274,4995:5044)) {images_test_2[[i]] <- array_reshape(images_test_2[[i]], c(28,28,3))}
for (i in c(6985:7034,7601:7650)) {images_test_3[[i]] <- array_reshape(images_test_3[[i]], c(28,28,3))}
for (i in c(4995:5044,5265:5314)) {images_test_4[[i]] <- array_reshape(images_test_4[[i]], c(28,28,3))}

#Row Bind
test0_x <- NULL
for (i in c(4501:4550,5401:5450)) {test0_x <- rbind(test0_x, images_test_0[[i]])}

test1_x <- NULL
for (i in c(10155:10204,10665:10714)) {test1_x <- rbind(test1_x, images_test_1[[i]])}

test2_x <- NULL
for (i in c(4225:4274,4995:5044)) {test2_x <- rbind(test2_x, images_test_2[[i]])}

test3_x <- NULL
for (i in c(6985:7034,7601:7650)) {test3_x <- rbind(test3_x, images_test_3[[i]])}

test4_x <- NULL
for (i in c(4995:5044,5265:5314))  {test4_x <- rbind(test4_x, images_test_4[[i]])}

str(test0_x)
str(test1_x)
str(test2_x)
str(test3_x)
str(test4_x)


# Dependent categorical variable for test data
test0_y<-rep(c(0),times=100)
test1_y<-rep(c(1),times=100)
test2_y<-rep(c(2),times=100)
test3_y<-rep(c(3),times=100)
test4_y<-rep(c(4),times=100)


## Combining all data array

Testx <- rbind(test0_x,test1_x,test2_x,test3_x,test4_x)
Testy <- abind::abind(test0_y,test1_y,test2_y,test3_y,test4_y)

# Reshaping Train data for CNN modelling

Test_fin_x<-array_reshape(Testx,c(-1,28,28,3))

str(Test_fin_x)

summary(Test_fin_x)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.4784  0.5258  0.5368  0.6042  1.0000   


# One hot encoding
TrainLabels <- to_categorical(Trainy)
TestLabels <- to_categorical(Testy)


########################  Hyper Parameter Tuning for Conventional Neural network model ####################


FLAGS_1<-flags(flag_integer('dense_units',32),
               flag_integer('dense_units2',64),
               flag_numeric('dropout1',0.25),
               flag_numeric('dropout2',0.25),
               flag_integer('dense_units3',256),
               flag_integer('batch_size',32))


Experiment_model <- keras_model_sequential()

Experiment_model %>%
  layer_conv_2d(filters = FLAGS_1$dense_units, 
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(28,28,3)) %>%
  layer_conv_2d(filters = FLAGS_1$dense_units,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_conv_2d(filters = FLAGS_1$dense_units2,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_conv_2d(filters = FLAGS_1$dense_units2,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = FLAGS_1$dropout1) %>%
  layer_flatten() %>%
  layer_dense(units = FLAGS_1$dense_units3, activation = 'relu') %>%
  layer_dropout(rate=FLAGS_1$dropout2) %>%
  layer_dense(units = 5, activation = 'softmax') %>%
  
  compile(loss = 'categorical_crossentropy',
          optimizer = optimizer_sgd(lr = 0.01,
                                    decay = 1e-6,
                                    momentum = 0.90,
                                    nesterov = T),
          metrics = c('accuracy'))

#Fit the mode
history <- Experiment_model %>%
  fit(Train_fin_x,
      TrainLabels,
      epochs = 50,
      batch_size = 32,
      validation_data = list(Test_fin_x, TestLabels))


setwd("C:/Users/Vikram/Desktop")

runs <- tuning_run("exp1.R",
                   flags = list(dense_units = c(32, 64),
                   dense_units2 = c(32,64),
                   dropout1 = c(0.2,0.25),
                   dropout2 = c(0.2,0.25),
                   batch_size = c(32,64)))


##################### CONVENTIONAL NEURAL NETWORK MODELLING ################################################
# Model Architecture

model <- keras_model_sequential()

model %>%
  layer_conv_2d(filters = 32, 
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(28,28,3)) %>%
  layer_conv_2d(filters = 32,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate=0.25) %>%
  layer_dense(units = 5, activation = 'softmax') %>%
  
  compile(loss = 'categorical_crossentropy',
          optimizer = 'adam',
          metrics = c('accuracy'))
summary(model)


#Model: "sequential"
#_______________________________________________________________________________________________________________________________________
#Layer (type)                                                Output Shape                                          Param #              
#=======================================================================================================================================
#  conv2d (Conv2D)                                             (None, 26, 26, 32)                                    896                  
#_______________________________________________________________________________________________________________________________________
#conv2d_1 (Conv2D)                                           (None, 24, 24, 32)                                    9248                 
#_______________________________________________________________________________________________________________________________________
#max_pooling2d (MaxPooling2D)                                (None, 12, 12, 32)                                    0                    
#_______________________________________________________________________________________________________________________________________
#dropout (Dropout)                                           (None, 12, 12, 32)                                    0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_2 (Conv2D)                                           (None, 10, 10, 64)                                    18496                
#_______________________________________________________________________________________________________________________________________
#conv2d_3 (Conv2D)                                           (None, 8, 8, 64)                                      36928                
#_______________________________________________________________________________________________________________________________________
#max_pooling2d_1 (MaxPooling2D)                              (None, 4, 4, 64)                                      0                    
#_______________________________________________________________________________________________________________________________________
#dropout_1 (Dropout)                                         (None, 4, 4, 64)                                      0                    
#_______________________________________________________________________________________________________________________________________
#flatten (Flatten)                                           (None, 1024)                                          0                    
#_______________________________________________________________________________________________________________________________________
#dense (Dense)                                               (None, 256)                                           262400               
#_______________________________________________________________________________________________________________________________________
#dropout_2 (Dropout)                                         (None, 256)                                           0                    
#_______________________________________________________________________________________________________________________________________
#dense_1 (Dense)                                             (None, 5)                                             1285                 
#=======================================================================================================================================
#  Total params: 329,253
#Trainable params: 329,253
#Non-trainable params: 0
#model

# Fit model

setwd("D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/Final Codings")
history <- model %>%
  fit(Train_fin_x,
      TrainLabels,
      epochs = 75,
      batch_size = 32,
      validation_data = list(Test_fin_x, TestLabels),
      callbacks = callback_tensorboard('ctg_final/one'))
      
plot(history)
tensorboard('ctg_final/one')

# Evaluation & Prediction - train data
model %>% evaluate(Train_fin_x, TrainLabels)

#=] - 1s 178us/sample - loss: 0.0452 - accuracy: 0.9893
#$loss
#[1] 0.07921534

#$accuracy
#[1] 0.9893354


pred <- model %>% predict_classes(Train_fin_x)
table(Predicted = pred, Actual = Trainy)

#         Actual
#Predicted    0    1    2    3    4
#        0 1295    8   21    0    0
#        1    1 1032    0    0    1
#        2    5    5 1023    1    0
#        3    0    3    4  946    0
#        4    0    2    2    3  899


prob <- model %>% predict_proba(Train_fin_x)
d1<-cbind(prob, Predicted_class = pred, Actual = Trainy)
write.csv(d1,"d1.csv")

# Evaluation & Prediction - test data
model %>% evaluate(Test_fin_x, TestLabels)


#=] - 0s 186us/sample - loss: 0.0478 - accuracy: 0.9880
#$loss
#[1] 0.08198654

#$accuracy
#[1] 0.988

pred1 <- model %>% predict_classes(Test_fin_x)
table(Predicted = pred1, Actual = Testy)
#        Actual
#Predicted   0   1   2   3   4
#        0  98   0   1   0   0
#        1   0  97   0   0   0
#        2   2   3  99   0   0
#        3   0   0   0 100   0
#        4   0   0   0   0 100

prob1 <- model %>% predict_proba(Test_fin_x)
d2<-cbind(prob1, Predicted_class = pred1, Actual = Testy)
write.csv(d2,"d2.csv")

#### Testing of model on independent data set ########################

Folder_ind_0 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_0"
files_ind_0 <- list.files(path = Folder_ind_0, pattern = "*.jpg", full.names = TRUE)
images_ind_0<-list()

for (i in 19501:19550) {images_ind_0[[i]] <- readImage(files_ind_0[i])}

Folder_ind_1 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_1"
files_ind_1 <- list.files(path = Folder_ind_1, pattern = "*.jpg", full.names = TRUE)
images_ind_1<-list()

for (i in 5701:5750) {images_ind_1[[i]] <- readImage(files_ind_1[i])}


Folder_ind_2 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_2"
files_ind_2 <- list.files(path = Folder_ind_2, pattern = "*.jpg", full.names = TRUE)
images_ind_2<-list()

for (i in 6601:6650) {images_ind_2[[i]] <- readImage(files_ind_2[i])}


Folder_ind_3 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_3"
files_ind_3 <- list.files(path = Folder_ind_3, pattern = "*.jpg", full.names = TRUE)
images_ind_3<-list()

for (i in 7201:7250) {images_ind_3[[i]] <- readImage(files_ind_3[i])}


Folder_ind_4 <- "D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/label_4"
files_ind_4 <- list.files(path = Folder_ind_4, pattern = "*.jpg", full.names = TRUE)
images_ind_4<-list()

for (i in 4401:4450) {images_ind_4[[i]] <- readImage(files_ind_4[i])}

str(images_ind_0)
str(images_ind_1)
str(images_ind_2)
str(images_ind_3)
str(images_ind_4)


# Resize 
for (i in 19501:19550) {images_ind_0[[i]] <- EBImage::resize(images_ind_0[[i]], 28, 28)}
for (i in 5701:5750) {images_ind_1[[i]] <- EBImage::resize(images_ind_1[[i]], 28, 28)}
for (i in 6601:6650) {images_ind_2[[i]] <- EBImage::resize(images_ind_2[[i]], 28, 28)}
for (i in 7201:7250) {images_ind_3[[i]] <- EBImage::resize(images_ind_3[[i]], 28, 28)}
for (i in 4401:4450) {images_ind_4[[i]] <- EBImage::resize(images_ind_4[[i]], 28, 28)}
# Reshape
for (i in 19501:19550) {images_ind_0[[i]] <- array_reshape(images_ind_0[[i]], c(28,28,3))}
for (i in 5701:5750) {images_ind_1[[i]] <- array_reshape(images_ind_1[[i]], c(28,28,3))}
for (i in 6601:6650) {images_ind_2[[i]] <- array_reshape(images_ind_2[[i]], c(28,28,3))}
for (i in 7201:7250) {images_ind_3[[i]] <- array_reshape(images_ind_3[[i]], c(28,28,3))}
for (i in 4401:4450){images_ind_4[[i]] <- array_reshape(images_ind_4[[i]], c(28,28,3))}

#Row Bind
ind0_x <- NULL
for (i in 19501:19550) {ind0_x <- rbind(ind0_x, images_ind_0[[i]])}

ind1_x <- NULL
for (i in 5701:5750) {ind1_x <- rbind(ind1_x, images_ind_1[[i]])}

ind2_x <- NULL
for (i in 6601:6650) {ind2_x <- rbind(ind2_x, images_ind_2[[i]])}

ind3_x <- NULL
for (i in 7201:7250) {ind3_x <- rbind(ind3_x, images_ind_3[[i]])}

ind4_x <- NULL
for (i in 4401:4450) {ind4_x <- rbind(ind4_x, images_ind_4[[i]])}

str(ind0_x)
str(ind1_x)
str(ind2_x)
str(ind3_x)
str(ind4_x)


# Dependent categorical variable for test data
ind0_y<-rep(c(0),times=50)
ind1_y<-rep(c(1),times=50)
ind2_y<-rep(c(2),times=50)
ind3_y<-rep(c(3),times=50)
ind4_y<-rep(c(4),times=50)


## Combining all data array

Indx <- rbind(ind0_x,ind1_x,ind2_x,ind3_x,ind4_x)
Indy <- abind::abind(ind0_y,ind1_y,ind2_y,ind3_y,ind4_y)

# Reshaping Train data for CNN modelling

Ind_fin_x<-array_reshape(Indx,c(-1,28,28,3))

str(Ind_fin_x)

summary(Ind_fin_x)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.4843  0.5202  0.5324  0.5872  1.0000  


# One hot encoding
IndLabels <- to_categorical(Indy)



# Evaluation & Prediction - Independent data
model %>% evaluate(Ind_fin_x, IndLabels)
#=] - 0s 197us/sample - loss: 2.8364 - accuracy: 0.3880
#$loss
#[1] 2.50051

#$accuracy
#[1] 0.388

# Model is not good fit for independent data.  This is because the data selection is either biased towards more of left eye or right eye images.  We can use flop function and check the accuracy of the model.  

for (i in 19501:19550) {images_ind_0[[i]] <- flop(images_ind_0[[i]])}
for (i in 5701:5750) {images_ind_1[[i]] <- flop(images_ind_1[[i]])}
for (i in 6601:6650) {images_ind_2[[i]] <- flop(images_ind_2[[i]])}
for (i in 7201:7250) {images_ind_3[[i]] <- flop(images_ind_3[[i]])}
for (i in 4401:4450) {images_ind_4[[i]] <- flop(images_ind_4[[i]])}

# Resize 
for (i in 19501:19550) {images_ind_0[[i]] <- EBImage::resize(images_ind_0[[i]], 28, 28)}
for (i in 5701:5750) {images_ind_1[[i]] <- EBImage::resize(images_ind_1[[i]], 28, 28)}
for (i in 6601:6650) {images_ind_2[[i]] <- EBImage::resize(images_ind_2[[i]], 28, 28)}
for (i in 7201:7250) {images_ind_3[[i]] <- EBImage::resize(images_ind_3[[i]], 28, 28)}
for (i in 4401:4450) {images_ind_4[[i]] <- EBImage::resize(images_ind_4[[i]], 28, 28)}
# Reshape
for (i in 19501:19550) {images_ind_0[[i]] <- array_reshape(images_ind_0[[i]], c(28,28,3))}
for (i in 5701:5750) {images_ind_1[[i]] <- array_reshape(images_ind_1[[i]], c(28,28,3))}
for (i in 6601:6650) {images_ind_2[[i]] <- array_reshape(images_ind_2[[i]], c(28,28,3))}
for (i in 7201:7250) {images_ind_3[[i]] <- array_reshape(images_ind_3[[i]], c(28,28,3))}
for (i in 4401:4450){images_ind_4[[i]] <- array_reshape(images_ind_4[[i]], c(28,28,3))}

#Row Bind
ind0_x <- NULL
for (i in 19501:19550) {ind0_x <- rbind(ind0_x, images_ind_0[[i]])}

ind1_x <- NULL
for (i in 5701:5750) {ind1_x <- rbind(ind1_x, images_ind_1[[i]])}

ind2_x <- NULL
for (i in 6601:6650) {ind2_x <- rbind(ind2_x, images_ind_2[[i]])}

ind3_x <- NULL
for (i in 7201:7250) {ind3_x <- rbind(ind3_x, images_ind_3[[i]])}

ind4_x <- NULL
for (i in 4401:4450) {ind4_x <- rbind(ind4_x, images_ind_4[[i]])}

str(ind0_x)
str(ind1_x)
str(ind2_x)
str(ind3_x)
str(ind4_x)

## Combining all data array

Indx <- rbind(ind0_x,ind1_x,ind2_x,ind3_x,ind4_x)

# Reshaping Train data for CNN modelling

Ind_fin_x<-array_reshape(Indx,c(-1,28,28,3))

str(Ind_fin_x)

summary(Ind_fin_x)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.4843  0.5202  0.5324  0.5872  1.0000 

# Evaluation & Prediction - test data
model %>% evaluate(Ind_fin_x, IndLabels)
#=] - 0s 200us/sample - loss: 3.1559 - accuracy: 0.2680
#$loss
#[1] 3.156368

#$accuracy
#[1] 0.268


pred2 <- model %>% predict_classes(Ind_fin_x)
table(Predicted = pred2, Actual = Indy)
#         Actual
# Predicted  0  1  2  3  4
#         0  5  6 13  2  6
#         1  9 21  8 13  3
#         2 23  3 19  2 18
#         3  6  6  5 13 14
#         4  7 14  5 20  9


prob2 <- model %>% predict_proba(Ind_fin_x)
d3<-cbind(prob2, Predicted_class = pred2, Actual = Indy)



################ SUPPOR VECTOR MACHINES MODELLING ####################################

#Data conversion for SVM modelling from array to dataframe

options(expressions = 500000)
Train_df<-as.data.frame(Train_fin_x)
Train_df$DR_class<-paste(Trainy)
Train_df$DR_class<-as.factor(Train_df$DR_class)
Test_df<-as.data.frame(Test_fin_x)
Test_df$DR_class<-paste(Testy)
Test_df$DR_class<-as.factor(Test_df$DR_class)

str(Train_df)
str(Test_df)

# Dimension reduction
CorlMat<-cor(Train_df[,1:2352])
highlyCorrelated<- caret::findCorrelation(CorlMat, cutoff=0.5)
highlyCorrelated<-as.data.frame(highlyCorrelated)
write.csv(CorlMat,"CorlMat.csv")
write.csv(highlyCorrelated,"highlyCorrelated.csv")

# RBF kernel 
model_rbf <- e1071::svm(DR_class~ ., data = Train_df,type = "C-classification", kernel = "radial")
summary(model_rbf)

#Call:
#  svm(formula = DR_class ~ ., data = Train_df, type = "C-classification", kernel = "radial")


#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  radial 
cost:  1 

#Number of Support Vectors:  5211

#( 1297 1049 1050 934 881 )


#Number of Classes:  5 

#Levels: 
#  0 1 2 3 4



Eval_RBF<- predict(model_rbf, Train_df)

#confusion matrix - RBF Kernel
caret::confusionMatrix(Eval_RBF,Train_df$DR_class)

#Confusion Matrix and Statistics

#         Reference
#Prediction    0    1    2    3    4
#         0 1273   62   74   16   10
#         1    3  935   11    4   26
#         2    1   20  934    1    5
#         3    6   11    8  916   11
#         4   18   22   23   13  848

#Overall Statistics

#Accuracy : 0.9343          
#95% CI : (0.9273, 0.9409)
#No Information Rate : 0.2478          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.9174          

#Mcnemar's Test P-Value : < 2.2e-16       

#Statistics by Class:

#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
#Sensitivity            0.9785   0.8905   0.8895   0.9642   0.9422
#Specificity            0.9590   0.9895   0.9936   0.9916   0.9825
#Pos Pred Value         0.8871   0.9551   0.9719   0.9622   0.9177
#Neg Pred Value         0.9927   0.9731   0.9730   0.9921   0.9880
#Prevalence             0.2478   0.2000   0.2000   0.1809   0.1714
#Detection Rate         0.2424   0.1781   0.1779   0.1744   0.1615
#Detection Prevalence   0.2733   0.1864   0.1830   0.1813   0.1760
#Balanced Accuracy      0.9687   0.9400   0.9415   0.9779   0.9624

# Predicting the model results  on Test data
Eval_RBF_test<- predict(model_rbf, Test_df)


#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF_test,Test_df$DR_class)


#Confusion Matrix and Statistics

#         Reference
#Prediction  0  1  2  3  4
#         0 97  3 10  3  0
#         1  1 86  2  0  8
#         2  0  3 82  0  1
#         3  0  0  4 95  0
#         4  2  8  2  2 91

#Overall Statistics

#Accuracy : 0.902           
#95% CI : (0.8725, 0.9266)
#No Information Rate : 0.2             
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.8775          

#Mcnemar's Test P-Value : NA              

#Statistics by Class:

#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
#Sensitivity            0.9700   0.8600   0.8200   0.9500   0.9100
#Specificity            0.9600   0.9725   0.9900   0.9900   0.9650
#Pos Pred Value         0.8584   0.8866   0.9535   0.9596   0.8667
#Neg Pred Value         0.9922   0.9653   0.9565   0.9875   0.9772
#Prevalence             0.2000   0.2000   0.2000   0.2000   0.2000
#Detection Rate         0.1940   0.1720   0.1640   0.1900   0.1820
#Detection Prevalence   0.2260   0.1940   0.1720   0.1980   0.2100
#Balanced Accuracy      0.9650   0.9163   0.9050   0.9700   0.9375

#SVM Fitting on Independent dataset

Ind_df<-as.data.frame(Ind_fin_x)
Ind_df$DR_class<-paste(Indy)
Ind_df$DR_class<-as.factor(Ind_df$DR_class)

# Predicting the model results on Independent data
Eval_RBF_Ind<- predict(model_rbf, Ind_df)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF_Ind,Ind_df$DR_class)

#Confusion Matrix and Statistics

#          Reference
#Prediction  0  1  2  3  4
#         0 34  8 20  7  2
#         1  5 27  5 18  0
#         2  3  4 14  5  4
#         3  4  2  3 10  6
#         4  4  9  8 10 38

#Overall Statistics
                                          
#               Accuracy : 0.492           
#                 95% CI : (0.4284, 0.5557)
#    No Information Rate : 0.2             
#    P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                  Kappa : 0.365           
                                          
# Mcnemar's Test P-Value : 2.086e-05       

#Statistics by Class:
  
#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
#Sensitivity            0.6800   0.5400   0.2800   0.2000   0.7600
#Specificity            0.8150   0.8600   0.9200   0.9250   0.8450
#Pos Pred Value         0.4789   0.4909   0.4667   0.4000   0.5507
#Neg Pred Value         0.9106   0.8821   0.8364   0.8222   0.9337
#Prevalence             0.2000   0.2000   0.2000   0.2000   0.2000
#Detection Rate         0.1360   0.1080   0.0560   0.0400   0.1520
#Detection Prevalence   0.2840   0.2200   0.1200   0.1000   0.2760
#Balanced Accuracy      0.7475   0.7000   0.6000   0.5625   0.8025


#####################################################################
#Hyperparameter tuning and Cross Validation - Non-Linear - SVM 
######################################################################

# We will use the train function from caret package to perform crossvalidation

# Making grid of "sigma" and C values. 
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))

# Performing 5-fold cross validation
radsvmtune <- caret::train(DR_class~., data=Ind_df, method="svmRadial", metric="Accuracy", 
                        tuneGrid=grid, trControl=trainControl())

# Printing cross validation result
print(radsvmtune)
# Best tune at sigma = 0.01 & C=2, Accuracy - 0.935

# Plotting model results
plot(radsvmtune)

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test & Independent data
evaluate_non_linear<- predict(radsvmtune, Ind_df)
confusionMatrix(evaluate_non_linear, Ind_df$DR_class)

# Accuracy    - 0.9333
# Sensitivity - 0.9569
# Specificity - 0.9044

plot(model_rbf,formula = V1~V2,data = Ind_df)


setwd("D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/Final Codings")




##########################################################################


#      G E N R A T I V E    A D V E R S A R I A L    N E T W O R K

##########################################################################


l <- 28
h <- 28
w <- 28
c <- 3

gi <- layer_input(shape = l)

go <- gi %>% layer_dense(units = 100 * 14 * 14) %>%
  layer_activation_leaky_relu() %>% 
  layer_reshape(target_shape = c(14, 14, 100)) %>% 
  layer_conv_2d(filters = 256, 
                kernel_size = 5,
                padding = "same") %>% 
  layer_activation_leaky_relu() %>% 
  layer_conv_2d_transpose(filters = 256, kernel_size = 4,strides = 2, padding = "same") %>% 
  layer_activation_leaky_relu() %>% 
  layer_conv_2d(filters = 256, 
                kernel_size = 5,
                padding = "same")%>%
  layer_activation_leaky_relu() %>%
  layer_conv_2d_transpose(filters = 256, kernel_size = 5,padding = "same") %>% 
  layer_activation_leaky_relu() %>% 
  layer_conv_2d(filters = c, 
                kernel_size = 7,
                activation = "tanh", 
                padding = "same")
g <- keras_model(gi, go)

summary(g)


Model: "model"
#_______________________________________________________________________________________________________________________________________
#Layer (type)                                                Output Shape                                          Param #              
#=======================================================================================================================================
#input_1 (InputLayer)                                        [(None, 28)]                                          0                    
#_______________________________________________________________________________________________________________________________________
#dense_14 (Dense)                                            (None, 19600)                                         568400               
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu (LeakyReLU)                                     (None, 19600)                                         0                    
#_______________________________________________________________________________________________________________________________________
#reshape (Reshape)                                           (None, 14, 14, 100)                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_28 (Conv2D)                                          (None, 14, 14, 256)                                   640256               
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_1 (LeakyReLU)                                   (None, 14, 14, 256)                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_transpose (Conv2DTranspose)                          (None, 28, 28, 256)                                   1048832              
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_2 (LeakyReLU)                                   (None, 28, 28, 256)                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_29 (Conv2D)                                          (None, 28, 28, 256)                                   1638656              
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_3 (LeakyReLU)                                   (None, 28, 28, 256)                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_transpose_1 (Conv2DTranspose)                        (None, 28, 28, 256)                                   1638656              
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_4 (LeakyReLU)                                   (None, 28, 28, 256)                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_30 (Conv2D)                                          (None, 28, 28, 3)                                     37635                
#=======================================================================================================================================
#Total params: 5,572,435
#Trainable params: 5,572,435
#Non-trainable params: 0
#_______________________________________________________________________________________________________________________________________


di <- layer_input(shape = c(h, w, c))
do <- di %>% 
  layer_conv_2d(filters = 100, kernel_size = 3) %>% 
  layer_activation_leaky_relu() %>% 
  layer_conv_2d(filters = 100, kernel_size = 4,strides = 2) %>%
  layer_activation_leaky_relu() %>% 
  layer_conv_2d(filters = 100, kernel_size = 4,strides = 2) %>%
  layer_activation_leaky_relu() %>% 
  layer_conv_2d(filters = 100, kernel_size = 4,strides = 2) %>%
  layer_activation_leaky_relu() %>% 
  layer_flatten() %>%
  layer_dropout(rate = 0.4) %>%  
  layer_dense(units = 1, activation = "sigmoid")
d <- keras_model(di, do)


d %>% compile(loss = "binary_crossentropy",
              optimizer = optimizer_rmsprop(lr = 0.0008,clipvalue = 1.0,decay = 1e-8))

summary(d)


#Model: "model_1"
#_______________________________________________________________________________________________________________________________________
#Layer (type)                                                Output Shape                                          Param #              
#=======================================================================================================================================
#input_2 (InputLayer)                                        [(None, 28, 28, 3)]                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_31 (Conv2D)                                          (None, 26, 26, 100)                                   2800                 
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_5 (LeakyReLU)                                   (None, 26, 26, 100)                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_32 (Conv2D)                                          (None, 12, 12, 100)                                   160100               
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_6 (LeakyReLU)                                   (None, 12, 12, 100)                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_33 (Conv2D)                                          (None, 5, 5, 100)                                     160100               
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_7 (LeakyReLU)                                   (None, 5, 5, 100)                                     0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_34 (Conv2D)                                          (None, 1, 1, 100)                                     160100               
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_8 (LeakyReLU)                                   (None, 1, 1, 100)                                     0                    
#_______________________________________________________________________________________________________________________________________
#flatten_7 (Flatten)                                         (None, 100)                                           0                    
#_______________________________________________________________________________________________________________________________________
#dropout_21 (Dropout)                                        (None, 100)                                           0                    
#_______________________________________________________________________________________________________________________________________
dense_15 (Dense)                                            (None, 1)                                             101                  
#=======================================================================================================================================
#Total params: 483,201
#Trainable params: 483,201
#Non-trainable params: 0
#_______________________________________________________________________________________________________________________________________


# adversarial network
freeze_weights(d) 
gani <- layer_input(shape = l)
gano <- d(g(gani))
gan <- keras_model(gani, gano)
gan %>% compile(loss = "binary_crossentropy",
                optimizer = optimizer_rmsprop(lr = 0.0004,clipvalue = 1.0,decay = 1e-8))


iterations<-60
b <- 32  
setwd("D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/Final Codings")
dir <- "FakeImages1"
dir.create(dir)
start <- 1; 

# fake images
for (i in 1:5251) {
random_latent_vectors<-matrix(rnorm(b*l),nrow = b,ncol = l)
gi<-g%>% predict(random_latent_vectors)
stop<-start+b-1
real_images<-Train_fin_x[start:stop,,,]
rows<-nrow(real_images)
combined_images<-array(0,dim = c(rows*2,dim(real_images)[-1]))
combined_images[1:rows,,,]<-gi
combined_images[(rows+1):(rows*2),,,]<-real_images
labels<-rbind(matrix(1,nrow=b,ncol=1),matrix(0,nrow=b,ncol=1))
labels<-labels+(0.5*array(runif(prod(dim(labels))),dim=dim(labels)))
d_loss<-d%>% train_on_batch(combined_images,labels)
random_latent_vectors<-matrix(rnorm(b*l),nrow=b,ncol=l)
misleading_targets<-array(0,dim=c(b,1))
a_loss<-gan %>% train_on_batch(random_latent_vectors,misleading_targets)
start<-start+b
if(start>(nrow(Train_fin_x)-b))
  start<-1
if(start %% 5251 ==0){save_model_hdf5(gan,"gan.h5")
  cat("discriminator loss:",d_loss,"\n")
  cat("adversarial loss:",a_loss,"\n")
  
  image_array_save(gi[1,,,],path = file.path(dir,paste0("generated_cornea",step,".jpg")))
  image_array_save(real_images[1,,,],path = file.path(dir,paste0("real_cornea",step,".jpg")))
  
  }

}

a_loss
#[1] 10.95594

d_loss
-0.8120005


#A stable GAN will have a discriminator loss around 0.5, typically between 0.5 and maybe as high as 0.7 or 0.8. 
#The generator loss is typically higher and may hover around 1.0, 1.5, 2.0, or even higher.


# we see that in above model adversarial loss begin to increase considerably, while the discriminative less than 
# zero-the discriminator is ending up dominating the generator. If that's the case, lets iterate by reduce the
# discriminator learning rate, and increase the dropout rate of the discriminator


di <- layer_input(shape = c(h, w, c))
do <- di %>% 
  layer_conv_2d(filters = 100, kernel_size = 3) %>% 
  layer_activation_leaky_relu() %>% 
  layer_conv_2d(filters = 100, kernel_size = 4,strides = 2) %>%
  layer_activation_leaky_relu() %>% 
  layer_conv_2d(filters = 100, kernel_size = 4,strides = 2) %>%
  layer_activation_leaky_relu() %>% 
  layer_flatten() %>%
  layer_dropout(rate = 0.25) %>%  
  layer_dense(units = 1, activation = "sigmoid")
d <- keras_model(di, do)


d %>% compile(loss = "binary_crossentropy",
              optimizer = optimizer_rmsprop(lr = 0.0008,clipvalue = 1.0,decay = 1e-8))

summary(d)

#Model: "model_4"
#_______________________________________________________________________________________________________________________________________
#Layer (type)                                                Output Shape                                          Param #              
#=======================================================================================================================================
#  input_5 (InputLayer)                                        [(None, 28, 28, 3)]                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_38 (Conv2D)                                          (None, 26, 26, 100)                                   2800                 
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_12 (LeakyReLU)                                  (None, 26, 26, 100)                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_39 (Conv2D)                                          (None, 12, 12, 100)                                   160100               
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_13 (LeakyReLU)                                  (None, 12, 12, 100)                                   0                    
#_______________________________________________________________________________________________________________________________________
#conv2d_40 (Conv2D)                                          (None, 5, 5, 100)                                     160100               
#_______________________________________________________________________________________________________________________________________
#leaky_re_lu_14 (LeakyReLU)                                  (None, 5, 5, 100)                                     0                    
#_______________________________________________________________________________________________________________________________________
#flatten_9 (Flatten)                                         (None, 2500)                                          0                    
#_______________________________________________________________________________________________________________________________________
#dropout_23 (Dropout)                                        (None, 2500)                                          0                    
#_______________________________________________________________________________________________________________________________________
#dense_17 (Dense)                                            (None, 1)                                             2501                 
#=======================================================================================================================================
#Total params: 325,501
#Trainable params: 325,501
#Non-trainable params: 0
#_______________________________________________________________________________________________________________________________________

# adversarial network
# Train

freeze_weights(d) 
gani <- layer_input(shape = l)
gano <- d(g(gani))
gan <- keras_model(gani, gano)
gan %>% compile(loss = "binary_crossentropy",
                optimizer = optimizer_rmsprop(lr = 0.0004,clipvalue = 1.0,decay = 1e-8))


iterations<-60
b <- 32  
setwd("D:/Masters in Data Analytics - LJMU/Data set/DR Dataset/Final Codings")
dir <- "FakeImages2"
dir.create(dir)
start <- 1; 

# fake images
for (i in 1:5251) {
  random_latent_vectors<-matrix(rnorm(b*l),nrow = b,ncol = l)
  gi<-g%>% predict(random_latent_vectors)
  stop<-start+b-1
  real_images<-Train_fin_x[start:stop,,,]
  rows<-nrow(real_images)
  combined_images<-array(0,dim = c(rows*2,dim(real_images)[-1]))
  combined_images[1:rows,,,]<-gi
  combined_images[(rows+1):(rows*2),,,]<-real_images
  labels<-rbind(matrix(1,nrow=b,ncol=1),matrix(0,nrow=b,ncol=1))
  labels<-labels+(0.5*array(runif(prod(dim(labels))),dim=dim(labels)))
  d_loss<-d%>% train_on_batch(combined_images,labels)
  random_latent_vectors<-matrix(rnorm(b*l),nrow=b,ncol=l)
  misleading_targets<-array(0,dim=c(b,1))
  a_loss<-gan %>% train_on_batch(random_latent_vectors,misleading_targets)
  start<-start+b
  if(start>(nrow(Train_fin_x)-b))
    start<-1
  if(start %% 5251 ==0){save_model_hdf5(gan,"gan.h5")
    cat("discriminator loss:",d_loss,"\n")
    cat("adversarial loss:",a_loss,"\n")
    
    image_array_save(gi[1,,,],path = file.path(dir,paste0("generated_cornea",step,".jpg")))
    image_array_save(real_images[1,,,],path = file.path(dir,paste0("real_cornea",step,".jpg")))
    
  }
  
}


a_loss
#[1] 1.339273

d_loss
#[1] 0.5742356


######################################################################################################################################