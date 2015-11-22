rm(list = ls(all=TRUE))
library(readr)
getwd()


library(h2o)
library(data.table)
library(dplyr)

#Read in the data. Using data.table significantly improves performance in all kind of 
# data munging activities
traindata <- fread("D:/coding/coding_rounds/black_f/train-corrected/train.csv")
testdata <- fread("D:/coding/coding_rounds/black_f/test-corrected/test.csv")

#Initialise H2o server with 3 threads. I have a 4 core processor, so keeping one for other
#activities (like browsing & FB!). Though not necessary for this data set :) 
h2o.server <- h2o.init( nthreads= 3)

## Preprocessing the training data

#Removing all NAs
traindata <- traindata[,lapply(.SD, function(x){ ifelse(is.na(x), 0, x)})]

#Converting all columns to factors
selCols = names(traindata)[3:11]
traindata = traindata[,(selCols) := lapply(.SD, as.factor), .SDcols = selCols]

#Converting to H2o Data frame & splitting
train.hex <- as.h2o(traindata, conn = h2o.server, destination_frame = "train.hex")
custTrain.split = h2o.splitFrame(train.hex, ratios = 0.9) #split frame 75/25

#Training model on the data 
train.gbmF = h2o.gbm( x = 3:11, y = 12, training_frame = custTrain.split[[1]],
                      validation_frame = custTrain.split[[2]], 
                      distribution = "AUTO",
                      ntrees = 100)


## Some pre-processing of test file before predicting Purchases

MySubmission = testdata[, c("User_ID", "Product_ID"), with = FALSE]
testdata = testdata[,c("User_ID", "Product_ID") := NULL, with = FALSE]

#Removing all NAs
testdata <- testdata[,lapply(.SD, function(x){ ifelse(is.na(x), 0, x)})]

#Converting all columns to factors
selCols = names(testdata)
testdata = testdata[,(selCols) := lapply(.SD, as.factor), .SDcols = selCols]

# Converting to H2o.DataFrame
test.hex  = as.h2o(testdata, conn = h2o.server, destination_frame = "test.hex")

#Making the predictions
testPurchase = as.data.frame(h2o.predict(train.gbmF, newdata = test.hex) )

#Final Submission
MySubmission$Purchase = testPurchase$predict
write.csv(MySubmission, "MySubmissionH2o_gbm1_based_splitratio0point9andntree100.csv", row.names = F)

h2o.shutdown()
y


