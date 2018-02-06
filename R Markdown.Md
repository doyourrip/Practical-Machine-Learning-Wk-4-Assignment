title: "Practical Machine Learning - Week 4 Assignment"
author: "C. Staples"
date: "5 February 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har 
(see the section on the Weight Lifting Exercise Dataset).

## Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 

Full source:

Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. “Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human ’13)”. Stuttgart, Germany: ACM SIGCHI, 2013.

Thankyou to the authors for being so generous in allowing their data to be used for this kind of assignment!

“Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).

Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes. Participants were supervised by an experienced weight lifter to make sure the execution complied to the manner they were supposed to simulate. The exercises were performed by six male participants aged between 20-28 years, with little weight lifting experience. We made sure that all participants could easily simulate the mistakes in a safe and controlled manner by using a relatively light dumbbell (1.25kg)."

```{r}

##Library Setup

library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(knitr)

## Data Import

# Download from the URL (Link)
UrlTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
UrlTest  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# Import into R Environment
training <- read.csv(url(UrlTrain))
testing  <- read.csv(url(UrlTest))

# Create partitions (trainign and test) from the dataset 
inTrain  <- createDataPartition(training$classe, p=0.7, list=FALSE)
TrainSet <- training[inTrain, ]
TestSet  <- training[-inTrain, ]

dim(TrainSet)

dim(TestSet)

# Remove any variables with Near Zero Variance
NZV <- nearZeroVar(TrainSet)
TrainSet <- TrainSet[, -NZV]
TestSet  <- TestSet[, -NZV]

dim(TrainSet)

dim(TestSet)

# Remove Uneccesary Variables (where most of the fields are NA) & ID Categories

allNA    <- sapply(TrainSet, function(x) mean(is.na(x))) > 0.95
TrainSet <- TrainSet[, allNA==FALSE]
TestSet  <- TestSet[, allNA==FALSE]

TrainSet <- TrainSet[, -(1:5)]
TestSet  <- TestSet[, -(1:5)]

dim(TrainSet)

dim(TestSet)

```

## The following sections will use three methods for modelling the training data to enable prediction on the test data.  The models will be compared for their accuract and the "best" model will then be used on the final questions.


```{r}

## Model 1 - Decision Tree

set.seed(29)

mod1DT <- rpart(classe ~ ., data = TrainSet, method="class")

## Model 1 - Decision Tree Predictions

prediction <- predict(mod1DT, TestSet, type = "class")

confMatDT <- confusionMatrix(prediction, TestSet$classe)

confMatDT

```
```{r}

## Model 2 - Random Forests

set.seed(29)

controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)

mod2RF <- train(classe ~ ., data=TrainSet, method="rf",trControl=controlRF)

mod2RF$finalModel


## Model 2 - Random Forest Predictions

predictRF <- predict(mod2RF, newdata=TestSet)

confMatRF <- confusionMatrix(predictRF, TestSet$classe)

confMatRF

```

```{r}

## Model 3 - Boost Model

set.seed(29)

controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

mod3GBM  <- train(classe ~ ., data=TrainSet, method = "gbm",trControl = controlGBM, verbose = FALSE)
mod3GBM$finalModel


## Model 3 - Boost Model Predictions

predictGBM <- predict(mod3GBM, newdata=TestSet)

confMatGBM <- confusionMatrix(predictGBM, TestSet$classe)

confMatGBM

```

## The best model was the Random Forest with the highest accuracy (0.9973) vs 0.9802 for Boost Model and 0.7349 for Decision Tree.  Randomf Forest model will be used for the final prediction on the test set.

```{r}

## Predictions

predictTEST <- predict(mod2RF, newdata=testing)
predictTEST

```
