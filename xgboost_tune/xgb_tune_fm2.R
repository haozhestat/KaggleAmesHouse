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

cv_ctrl = trainControl(method = "repeatedcv", repeats = 2, number = 5, allowParallel=T)

xgb_grid = expand.grid(nrounds = 2000,
                       eta = c(0.01, 0.005, 0.001),
                       max_depth = c(2, 4, 6, 8),
                       colsample_bytree=c(0.8,1),
                       min_child_weight = 2,
                       subsample=c(0.6,0.8),
                       gamma=c(0,0.01))


set.seed(602)
xgb_tune = train(as.matrix(x_train),
                 y_train,
                 method="xgbTree",
                 trControl=cv_ctrl,
                 tuneGrid=xgb_grid,
                 verbose=T,
                 metric="RMSE",
                 preProcess = c("center", "scale"),
                 nthread = 16)
print(xgb_tune)

save.image("xgb_tune_fm2.RData")







