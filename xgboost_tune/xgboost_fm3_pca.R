rm(list = ls())

setwd("~/Documents/KaggleAmesHouse/xgboost_tune")
library(caret) 
library(xgboost)

df <- read.csv("featureMat_v3.csv", header = TRUE, stringsAsFactors = FALSE)
pmatrix <- prcomp(df[,3:ncol(df)], center = TRUE, scale. = TRUE)
df <- cbind(df[,1:2], pmatrix$x[,1:133])

x_train = df[df$Train == 1, 3:ncol(df)]
x_test = df[df$Train == 0, 3:ncol(df)]
y_train = df$SalePrice[df$Train == 1]

dtrain = xgb.DMatrix(as.matrix(x_train), label = y_train)
dtest = xgb.DMatrix(as.matrix(x_test))


xgb_params = list(
  booster = 'gbtree',
  #objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.01,
  max_depth=4,
  min_child_weight=2,
  #alpha=0.3,
  #lambda=0.4,
  gamma=0, # less overfit
  subsample=0.6,
  silent=TRUE)

bst = xgb.train(xgb_params,dtrain, nrounds = 10000)

y_pred.xgb = predict(bst, dtest)
y_train.xgb = predict(bst, dtrain)
y_pred <- data.frame(Id = 1461:2919 , SalePrice = exp(y_pred.xgb))
y_pred_train <- data.frame(Id = 1:1460,
                           SalePrice = exp(y_train.xgb))
write.csv(y_pred, file="xgboost_test_fm3_pca.csv", row.names = FALSE)
write.csv(y_pred_train, file="xgboost_train_fm3_pca.csv", row.names = FALSE)