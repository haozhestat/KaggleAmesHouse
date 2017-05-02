rm(list = ls())

setwd("~/Documents/KaggleAmesHouse/xgboost_tune")
library(caret) 
library(glmnet)

df <- read.csv("featureMat_v2.csv", header = TRUE, stringsAsFactors = FALSE)

x_train = df[df$Train == 1, 3:ncol(df)]
x_test = df[df$Train == 0, 3:ncol(df)]
y_train = df$SalePrice[df$Train == 1]

set.seed(602)

PLSTune<-train(y=y_train, 
               x=x_train, 
               method="pls", 
               preProcess = c("center","scale"), 
               tuneGrid=data.frame(.ncomp=seq(1:50)), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=5)) 


y_pred.pls = predict(PLSTune, as.matrix(x_test))
y_train.pls = predict(PLSTune, as.matrix(x_train))
y_pred <- data.frame(Id = 1461:2919 , SalePrice = exp(y_pred.pls))
y_pred_train <- data.frame(Id = setdiff(1:1460, c(524,692,1183,1299)),
                           SalePrice = exp(y_train.pls))
write.csv(y_pred, file="pls_test_fm2.csv", row.names = FALSE)
write.csv(y_pred_train, file="pls_train_fm2.csv", row.names = FALSE)
