rm(list = ls())

setwd("~/Documents/KaggleAmesHouse/Haozhe")

require(ggplot2) # for data visualization
require(stringr) #extracting string patterns
require(Matrix) # matrix transformations
require(glmnet) # ridge, lasso & elastinet
require(xgboost) # gbm
require(randomForest)
require(Metrics) # rmse
require(dplyr) # load this in last so plyr doens't overlap it
require(caret) # one hot encoding

df <- read.csv("featureMat_v2.csv", header = TRUE, stringsAsFactors = FALSE)

x_train = df[df$Train == 1, 3:ncol(df)]
x_test = df[df$Train == 0, 3:ncol(df)]
y_train = df$SalePrice[df$Train == 1]

cv.ctrl = trainControl(method = "repeatedcv", repeats = 10, number = 10, 
                       allowParallel=T)

# Xgboost
dtrain = xgb.DMatrix(as.matrix(x_train), label = y_train)
dtest = xgb.DMatrix(as.matrix(x_test))

xgb.grid = expand.grid(nrounds = 750,
                       eta = c(0.01,0.005,0.001),
                       max_depth = c(4,6,8),
                       colsample_bytree=c(0,1,10),
                       min_child_weight = 2,
                       subsample=c(0,0.2,0.4,0.6),
                       gamma=0.01)
set.seed(602)
xgb_tune = train(as.matrix(x_train),
       y_train,
       method="xgbTree",
       trControl=cv.ctrl,
       tuneGrid=xgb.grid,
       verbose=T,
       metric="RMSE",
       nthread =3)

xgb_params = list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.005,
  max_depth=4,
  min_child_weight=3,
  alpha=0.3,
  lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.6,
  silent=TRUE)

bst = xgb.train(xgb_params,dtrain, nrounds = 2000)

y_pred.xgb = predict(bst, dtest)
y_pred <- data.frame(Id = 1461:2919 , SalePrice = exp(y_pred.xgb))
#write.csv(y_pred, file="xgboost_pred.csv", row.names = FALSE)


set.seed(602)
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
glmnet_grid <- expand.grid(.alpha=seq(0,1,0.1),.lambda=c(0.01,0.1:0.9,1,5,10))
glmnetFit <- train(as.matrix(x_train), y_train,
                   #data = ameshouse,
                   method = "glmnet",
                   tuneGrid=glmnet_grid,
                   trControl = fitControl,
                   preProc = c("center", "scale"))
y_pred.elastic <- predict(glmnet(as.matrix(x_train), y_train, alpha=0.2,lambda=0.01), as.matrix(x_test))
y_pred <- data.frame(Id = 1461:2919 , SalePrice = exp((y_pred.xgb+y_pred.elastic)/2) - 1)
colnames(y_pred) <- c("Id",'SalePrice')
write.csv(y_pred, file="xgboost_elastic_pred.csv", row.names = FALSE)
