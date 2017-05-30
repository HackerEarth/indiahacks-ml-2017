path <- "/home/manish/Desktop/IH2017/Here/dataset/"
setwd(path)

# Load data and libraries -------------------------------------------------

library(data.table)
library(gbm)

train <- fread("train.csv")
test <- fread("test.csv")
offline <- fread("offline_testcase.csv")


# Encode as integer -------------------------------------------------------

train[,DetectedCamera := as.integer(as.factor(DetectedCamera))-1]
test[,DetectedCamera := as.integer(as.factor(DetectedCamera))-1]

# Rename Target -----------------------------------------------------------

setnames(train, 'SignFacing (Target)', 'Target')

# Model Training ----------------------------------------------------------

library(ranger)
rf.model <- ranger(Target ~ .-Id, data = train, num.trees = 500, mtry = 3,probability = T)
rf.model

pred <- predict(rf.model, test)

sub_RF <- as.data.table(pred$predictions)
sub_RF <- cbind(test$Id, sub_RF)
setnames(sub_RF,"V1","Id")

fwrite(sub_RF,"starterRF.csv") #LB = ~0.99
