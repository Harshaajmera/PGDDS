## install.packages("caret")
## install.packages("kernlab")
## install.packages("dplyr")
## install.packages("readr")
## install.packages("ggplot2")
## install.packages("gridExtra")

library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

## Data Cleaning and preparation
## Loading Data

mnist_train <- read.csv('mnist_train.csv', stringsAsFactors = FALSE, header = FALSE)

mnist_test <- read.csv('mnist_test.csv', stringsAsFactors = FALSE, header = FALSE)

## Check the number of dimension

dim(mnist_train) ## 785 dimensions and 60000 rows

dim(mnist_test) ## 785 dimensions and 10000 rows

## Check the column names

colnames(mnist_train)

colnames(mnist_test)

## Change the column names as 

colnames(mnist_train) <- c('Y', paste('pixel_', 1:784, sep = '')) 

colnames(mnist_test) <- c('Y', paste('pixel_', 1:784, sep = '')) 

## Structure of the dataset
str(mnist_train)

str(mnist_test)

## printing first few rows
head(mnist_train)

## Exploring the data
summary(mnist_train)

## Changing label to factor
mnist_train[, 1] <- as.factor(mnist_train[, 1])

mnist_test[, 1] <- as.factor(mnist_test[, 1])

## Let's see if there are any missing values

sapply(mnist_train, function(x) sum(is.na(x))) ## No missing values

sapply(mnist_test, function(x) sum(is.na(x))) ## No missing values

## The data is of huge size, lets take a sample of 1000 rows.

set.seed(1)

train.indices = sample(1:nrow(mnist_train), 0.7*nrow(mnist_train))

train = mnist_train[train.indices,]

Cross_verify = mnist_train[-(train.indices),]

## Let's take 1000 rows for building the model.

train_2000 <- head(train, 2000)
test_2000 <- head(mnist_test, 2000)

## Constructing model

Model_linear <- ksvm(Y~ ., data = train_2000, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test_2000)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test_2000$Y)

## Accuracy = 88.2% which clearly says it is non linear. Let's go with RBF kernel

#Using RBF Kernel
Model_RBF <- ksvm(Y~ ., data = train_2000, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test_2000)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test_2000$Y)

## Accuracy is 90%
## We need to fine tune it with different C ans sigma values.


##   Hyperparameter tuning and Cross Validation 

# We will use the train function from caret package to perform Cross Validation. 


trainControl <- trainControl(method="cv", number=3)


## Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

## Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(1)
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))


## train function takes Target ~ Prediction, Data, Method = Algorithm
## Metric = Type of metric, tuneGrid = Grid of Parameters,
## trcontrol = Our traincontrol method.

fit.svm <- train(Y~., data=train_2000, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

plot(fit.svm)

## The final values used for the model were sigma = 0.05 and C = 1.

## Validating the model results on test data

evaluate_non_linear<- predict(fit.svm, test_1000)

confusionMatrix(evaluate_non_linear, test_1000$Y)















