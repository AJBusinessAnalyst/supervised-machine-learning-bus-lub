### Bus Lab 147
### Bus Lab II###

##  We are interested in data reported from buses that operate along the MBTA 1 & 47 routes.
#   Each row represents a data report from a single bus on either one of those two routes
#  at a single point in time and 
#  includes the following information: 
#      Bus ID, 
#      Route number 
#      Geographic location of a bus - longitude & latitude - to be fed into the  
#        supervised learning algorithms for the purposes of this lab.

## perform supervised learning on the bus data from the two routes 
## to attempt to identify the bus route associated with each record. 
## This type of model could be used to predict the next arrival time 
## of the next bus along a specific bus route.

###  1, Import and prepare data

setwd ("alejandro")
setwd("Desktop")
setwd("My Studies/Labs")
setwd("..")
getwd()
library(readxl)
bus147 <- read_excel ("bus1_47.xlsx")
str(bus147)
## a)   Which attribute describes the "class" or the predicted outcome label?.
##       Route Tag
## unique function 
## b)   Remove any attributes that will not contribute to this classification exercise.

##      Vehicle ID
unique(bus147$vehicleID)

bus147 <- bus147[,-4]

## c)   As in the previous lab, remove any rows not corresponding to routes 1 or 47.
##      Nothing to remove 

##       Subsetting techniques: newdataset <- originaldataset [originaldataset&column name %in% c("targetrowinfo",),]

###     busnorows <- busdata [busdata$D.routeTag %in% c("1", "47"),]

## 2)nSplit the data into training data and test data. 
## Use 80% of the data for training and 20% of the data for test.

# selects randomly 80% of the data
traindata <- sample(1:nrow(bus147), 0.8*nrow(bus147))
testdata <- setdiff(1:nrow(bus147), traindata)

# normalized the data

normaldata <- function(x) { (x -min(x))/(max(x)-min(x))}

busnormal <- as.data.frame(lapply(bus147[,c(1,2)],normaldata))
summary(busnormal)
## extract training data set
bus_train <- busnormal[traindata,]

## extract testing set
bus_test <- busnormal[-traindata,]

## data set with target

route <- bus147[traindata, c("routeTag")] 
routetest <- bus147[-traindata, c("routeTag")]

## KNN -- Use class package
library(class)
busknn <- knn(bus_train, bus_test, cl = route$routeTag, k=3)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) *100} 
accuracy(cm)

## to create an accuracy function:
######     accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
View (busknn)
#####################################################################

###### consulting with Cal on the error ####
dim(bus_train)
dim(route)

# found the problem
################# route <- bus147[traindata, c("routeTag")] ####


## How is the prediction performance from the k-NN classifier? 
#   Create a confusion matrix and assess accuracy.

cm <- table(busknn, routetest$routeTag)
cm


## Can you identify why some points are mistakenly classified? 
#   Hint: You can visualize the locations of the points 
#   which are misclassified on a scatterplot, 
#   to find out where they are, geographically.
#   Use color to denote the correct classification and shape 
#   to indicate the k-NN prediction.
#   What is special about the points that are misclassified?




index <- routetest$routeTag!= busknn
index
missclasified <- bus_test[index,]

plot(missclasified$lon, missclasified$lat, xlab = "lon", ylab = "lat")


### D. Support Vector Machine 

# Perform the same classification using SVM.
# As before, use 80% of the data to train the SVM classifier 
# and the remaining 20% for testing. 
# Hint: Use the “e1071” package.
# How is the prediction from the SVM classifier? 
# Use the predictions, confusion matrix and scatter plot 
# to understand how well the SVM classifier classifies 
#the test and the train data.


## encoding as factor
getwd()
bus.route.tag <- bus147
bus.route.tag$routeTag <- as.factor(bus.route.tag$routeTag)

## Split data set using caTools
library(caTools)
set.seed(123)

split <- sample.split(bus.route.tag$routeTag, SplitRatio = 0.8)
training_set <- subset(bus.route.tag, split = TRUE)
test_set <- subset(bus.route.tag, split = FALSE)
split


### Scaling the datasets 

training_set[-3] <- scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])

## fitting SVM to training set , package "e1071"
library("e1071")
classifier = svm (formula = routeTag ~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = "linear")
classifier


#########################################

## Predicting the test result

y_predict <- predict(classifier, bus.route.tag = test_set[-3])
y_predict

## Confusion Matrix
svm_cm <- table(Predicted = y_predict, Actual = bus.route.tag$routeTag)
svm_cm
plot(svm_cm, data = bus.route.tag)
1-sum(diag(svm_cm))/sum(svm_cm)


## misclasification rate##
1-sum(diag(cfsnmtx))/sum(cfsnmtx)





##########################################
plot(bus.route.tag)
plot(bus.route.tag$lon, bus.route.tag$lat, col = bus.route.tag$routeTag)
plot(bus.route.tag$lat, bus.route.tag$lon, col = bus.route.tag$routeTag)

library(ggplot2)
qplot(lon, lat, data = bus.route.tag, color = routeTag)
#########################################################
getwd()

library(e1071)
databus <- bus147
databus <- databus[,-4]
colnames(databus)
databus$routeTag <- as.factor(databus$routeTag)
mymodel <- svm(routeTag~., data = databus, 
               kernel = "linear")
summary(mymodel)
plot(mymodel, data = databus)

### confusion matrix and misclassification error##

forecast <- predict(mymodel, databus)
cfsnmtx <- table(Predicted = forecast, Actual = databus$routeTag)
cfsnmtx

## missclasification rate##
1-sum(diag(cfsnmtx))/sum(cfsnmtx)

### The SVM had a 32% of missclassification, If I did things right, 
#   this is reliable.

