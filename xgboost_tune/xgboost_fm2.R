rm(list = ls())

setwd("~/Documents/KaggleAmesHouse/xgboost_tune")
library(caret) 
library(xgboost)

df <- read.csv("featureMat_v2.csv", header = TRUE, stringsAsFactors = FALSE)

x_train = df[df$Train == 1, 3:ncol(df)]
x_test = df[df$Train == 0, 3:ncol(df)]
y_train = df$SalePrice[df$Train == 1]

dtrain = xgb.DMatrix(as.matrix(x_train), label = y_train)
dtest = xgb.DMatrix(as.matrix(x_test))


xgb_params = list(
  booster = 'gbtree',
  #objective = 'reg:linear',
  colsample_bytree=0.6,
  eta=0.005,
  max_depth=8,
  min_child_weight=2,
  #alpha=0.3,
  #lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.5,
  silent=TRUE)

bst = xgb.train(xgb_params,dtrain, nrounds = 10000)

y_pred.xgb = predict(bst, dtest)
y_train.xgb = predict(bst, dtrain)
y_pred <- data.frame(Id = 1461:2919 , SalePrice = exp(y_pred.xgb))
y_pred_train <- data.frame(Id = setdiff(1:1460, c(524,692,1183,1299)),
                           SalePrice = exp(y_train.xgb))
write.csv(y_pred, file="xgboost_test_fm2.csv", row.names = FALSE)
write.csv(y_pred_train, file="xgboost_train_fm2.csv", row.names = FALSE)
