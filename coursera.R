library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(knitr)
set.seed(13433)
trainUrl<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
train<-read.csv(url(trainUrl), na.strings=c("NA","","#DIV/0!"))
test <-read.csv(url(testUrl), na.strings=c("NA","","#DIV/0!"))
missingData = is.na(train)
sensorColumns = grep(pattern = "_belt|_arm|_dumbbell|_forearm", names(train))
length(sensorColumns)
data = train[, c(sensorColumns,160)]
dim(data)
omitColumns = which(colSums(missingData) > 19000)
data = data[, -omitColumns]
dim(data)
intrain=createDataPartition(train$classe,p=0.6,list=FALSE)
trainset<-train[intrain,]
testset<-train[-intrain,]


myDataNZV <- nearZeroVar(trainset, saveMetrics=TRUE)


myNZVvars <- names(trainset) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                      "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                      "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                      "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                      "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                      "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                      "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                      "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                      "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                      "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                      "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                      "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                      "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                      "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                      "stddev_yaw_forearm", "var_yaw_forearm")
trainset <- trainset[!myNZVvars]

trainset <- trainset[c(-1)]
trainingV3 <- trainset #creating another subset to iterate in loop
for(i in 1:length(trainset)) { #for every column in the training dataset
  if( sum( is.na( trainset[, i] ) ) /nrow(trainset) >= .6 ) { #if n?? NAs > 60% of total observations
    for(j in 1:length(trainingV3)) {
      if( length( grep(names(trainset[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
        trainingV3 <- trainingV3[ , -j] #Remove that column
      }   
    } 
  }
}
trainset <- trainingV3
rm(trainingV3)
clean1 <- colnames(trainset)
clean2 <- colnames(trainset[, -58]) #already with classe column removed
testset <- testset[clean1]
test <- test[clean2]

#To check the new N?? of observations
dim(testset)


for (i in 1:length(testset) ) {
  for(j in 1:length(trainset)) {
    if( length( grep(names(trainset[i]), names(testset)[j]) ) ==1)  {
      class(testset[j]) <- class(trainset[i])
    }      
  }      
}

#And to make sure Coertion really worked, simple smart ass technique:
test <- rbind(trainset[2, -58] , test) #note row 2 does not mean anything, this will be removed right.. now:
test <- test[-1,]
modFitA1 <- rpart(classe ~ ., data=trainset, method="class")
p<-predict(modFitA1,newdata=test[1,])

modFitB1 <- randomForest(classe ~. , data=trainset)
predictionsB1 <- predict(modFitB1, testset, type = "class")
confusionMatrix(predictionsB1, testset$classe)