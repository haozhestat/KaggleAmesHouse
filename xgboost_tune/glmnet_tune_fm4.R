rm(list = ls())

setwd("~/Documents/KaggleAmesHouse/xgboost_tune")
library(caret) 
library(glmnet)

df <- read.csv("featureMat_v4.csv", header = TRUE, stringsAsFactors = FALSE)
df <- df[,-167]

x_train = df[df$Train == 1, 3:ncol(df)]
x_test = df[df$Train == 0, 3:ncol(df)]
y_train = df$SalePrice[df$Train == 1]

set.seed(602)
glmnet_fit <- train(x = as.matrix(x_train),
                    y = y_train,
                    method = "glmnet",
                    standardize.response = TRUE, standardize = TRUE,
                    trControl = trainControl(method = 'repeatedcv',
                                             number = 5, repeats = 5, allowParallel = TRUE),
                    tuneGrid = expand.grid(.alpha = seq(0, 1, by = 0.05),
                                           .lambda = seq(0.001, 1, by = 0.05)),
                    nthread=16)
print(glmnet_fit)

glmnet_cv <- cv.glmnet(x = as.matrix(x_train),y = y_train, alpha=0.1)
ridge_fit <- glmnet(x = as.matrix(x_train),y = y_train, alpha=0.1, lambda = glmnet_cv$lambda.1se)


y_pred.glmnet = predict(ridge_fit, as.matrix(x_test))
y_train.glmnet = predict(ridge_fit, as.matrix(x_train))
y_pred <- data.frame(Id = 1461:2919 , SalePrice = exp(y_pred.glmnet))
y_pred_train <- data.frame(Id = setdiff(1:1460, c(524,692,1183,1299)),
                           SalePrice = exp(y_train.glmnet))
colnames(y_pred) <- c("Id", "SalePrice")
colnames(y_pred_train) <- c("Id", "SalePrice")
write.csv(y_pred, file="ridge_test_fm4.csv", row.names = FALSE)
write.csv(y_pred_train, file="ridge_train_fm4.csv", row.names = FALSE)