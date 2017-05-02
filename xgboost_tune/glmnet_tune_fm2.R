rm(list = ls())

setwd("~/Documents/KaggleAmesHouse/xgboost_tune")
library(caret) 
library(glmnet)

df <- read.csv("featureMat_v2.csv", header = TRUE, stringsAsFactors = FALSE)

x_train = df[df$Train == 1, 3:ncol(df)]
x_test = df[df$Train == 0, 3:ncol(df)]
y_train = df$SalePrice[df$Train == 1]

set.seed(602)
glmnet_fit <- train(x = as.matrix(x_train),
             y = y_train,
             method = "glmnet",
             standardize.response = TRUE, standardize = TRUE,
             trControl = trainControl(method = 'repeatedcv',
                                      number = 5, repeats = 2, allowParallel = TRUE),
             tuneGrid = expand.grid(.alpha = 0, #seq(0, 1, by = 0.05),
                                    .lambda = seq(0.001, 1, by = 0.001)))
glmnet_fit

glmnet_cv <- cv.glmnet(x = as.matrix(x_train),y = y_train, alpha=0)
lasso_fit <- glmnet(x = as.matrix(x_train),y = y_train, alpha=0, lambda = glmnet_cv$lambda.1se)


y_pred.glmnet = predict(lasso_fit, as.matrix(x_test))
y_train.glmnet = predict(lasso_fit, as.matrix(x_train))
y_pred <- data.frame(Id = 1461:2919 , SalePrice = exp(y_pred.glmnet))
y_pred_train <- data.frame(Id = setdiff(1:1460, c(524,692,1183,1299)),
                           SalePrice = exp(y_train.glmnet))
write.csv(y_pred, file="lasso_test_fm2.csv", row.names = FALSE)
write.csv(y_pred_train, file="lasso_train_fm2.csv", row.names = FALSE)
