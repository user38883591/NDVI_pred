library(randomForest)
library(caTools)

setwd("C:/Users/colloh/Desktop/aa/lesson6/Data")

data <- read.csv("samledata.csv")

head(data)

colnames(data)


colnames(data) <- c("ID", "NDVI", "BLUE", "GREEN", "RED", "NIR", "SWIR1", "SWIR2")

View(data)

trainData <- subset(data, select =-c(ID))

View (trainData)

set.seed(10)

sample = sample.split(trainData, SplitRatio =.70)

View(sample)


#Creating training data
train = subset(trainData, sample==TRUE)
TRUE
test = subset(trainData, sample==FALSE)


#Fiting a random forest model
#Running a Random Forest classification model on the training data
rf.fit <- randomForest(NDVI ~ ., 
                       data=train, 
                       ntree=1000,
                       keep.forest=TRUE,
                       importance=TRUE)

rf.fit

# Plotting variable importance using default plotter
varImpPlot(rf.fit)

#New test data

rf.pred <- predict(rf.fit, newdata=subset(test, select = -NDVI ))

rf.pred
test$NDVI.pred <- rf.pred

View(test)

#plot
plot(NDVI.pred~NDVI, data = test)+abline(lm(NDVI.pred~NDVI, data= test), col= 'red') #