rm(list = ls())

setwd("~/Documents/KaggleAmesHouse/Haozhe")

library(randomForest)
library(xgboost)
library(caret)
library(Boruta)
library(glmnet)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)
train$Train <- 1
train <- train[,c(82,81,1:80)]
test$SalePrice <- NA
test$Train <- 0
test <- test[,c(82,81,1:80)]
featureMat <- rbind(train, test)

### create a new covariate "YearRemodAdd - YearBuilt"
featureMat$YearRemodAdd_diff_YearBuilt <- featureMat$YearRemodAdd - featureMat$YearBuilt

###### standardize all the X's
for(i in 4:ncol(featureMat)){
  if(is.numeric(featureMat[,i]))
    featureMat[,i] <- (featureMat[,i] - mean(featureMat[,i], na.rm = TRUE))/sd(featureMat[,i], na.rm = TRUE)
}

###### Handling missing values
ConvertNA=function(x){
  if(is.factor(x)){
    x=as.character(x)
    x[is.na(x)]="NONE"
    x=as.factor(x)
  }
  return(x)
}

for(i in 1:ncol(featureMat)){
  featureMat[,i] <- ConvertNA(featureMat[,i])
  #if(is.numeric(featureMat[,i])&sum(is.na(featureMat[,i]))>0)
    #print(colnames(featureMat)[i])
}

featureMat$LotFrontage_NA <- as.numeric(is.na(featureMat$LotFrontage))
featureMat$LotFrontage[is.na(featureMat$LotFrontage)] <- 0

featureMat$MasVnrArea_NA <- as.numeric(is.na(featureMat$MasVnrArea))
featureMat$MasVnrArea[is.na(featureMat$MasVnrArea)] <- 0

featureMat$BsmtFinSF1_NA <- as.numeric(is.na(featureMat$BsmtFinSF1))
featureMat$BsmtFinSF1[is.na(featureMat$BsmtFinSF1)] <- 0

featureMat$BsmtFinSF2_NA <- as.numeric(is.na(featureMat$BsmtFinSF2))
featureMat$BsmtFinSF2[is.na(featureMat$BsmtFinSF2)] <- 0

featureMat$BsmtUnfSF_NA <- as.numeric(is.na(featureMat$BsmtUnfSF))
featureMat$BsmtUnfSF[is.na(featureMat$BsmtUnfSF)] <- 0

featureMat$TotalBsmtSF_NA <- as.numeric(is.na(featureMat$TotalBsmtSF))
featureMat$TotalBsmtSF[is.na(featureMat$TotalBsmtSF)] <- 0

featureMat$BsmtFullBath_NA <- as.numeric(is.na(featureMat$BsmtFullBath))
featureMat$BsmtFullBath[is.na(featureMat$BsmtFullBath)] <- 0

featureMat$BsmtHalfBath_NA <- as.numeric(is.na(featureMat$BsmtHalfBath))
featureMat$BsmtHalfBath[is.na(featureMat$BsmtHalfBath)] <- 0

featureMat$GarageArea_NA <- as.numeric(is.na(featureMat$GarageArea))
featureMat$GarageArea[is.na(featureMat$GarageArea)] <- 0

featureMat$GarageYrBlt_NA <- as.numeric(is.na(featureMat$GarageYrBlt))
featureMat$GarageYrBlt[is.na(featureMat$GarageYrBlt)] <- 0

featureMat$GarageCars_NA <- as.numeric(is.na(featureMat$GarageCars))
featureMat$GarageCars[is.na(featureMat$GarageCars)] <- 0

write.csv(featureMat, file="featureMatrix_v1.csv", row.names = FALSE)
