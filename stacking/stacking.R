rm(list = ls())

setwd("~/Documents/KaggleAmesHouse/stacking")

library(glmnet)

pls_train <- read.csv("pls_train_fm4.csv", header = TRUE)
pls_test <- read.csv("pls_test_fm4.csv", header = TRUE)
rf_train <- read.csv("RFTrainPred.csv", header = TRUE)
rf_test <- read.csv("RFTestPred.csv", header = TRUE)
xgb_train <- read.csv("xgboost_train_fm2_newtune.csv", header = TRUE)
xgb_test <- read.csv("xgboost_test_fm2_newtune.csv", header = TRUE)
elastic <- read.csv("el-EMG-pred-CV-ALL.csv", header = TRUE)
elastic_train <- elastic[1:1460,]
elastic_test <- elastic[1461:2919,]
ridge_train <- read.csv("ridge_train_fm4.csv", header=TRUE)
ridge_test <- read.csv("ridge_test_fm4.csv", header=TRUE)

x_train <- data.frame(pls = pls_train$SalePrice[pls_train$Id %in% xgb_train$Id],
                      #rf = rf_train$SalePrice[rf_train$Id %in% xgb_train$Id],
                      xgb = xgb_train$SalePrice,
                      ridge = ridge_train$SalePrice)
x_train <- log(x_train)
x_test <- data.frame(pls = pls_test$SalePrice[pls_test$Id %in% xgb_test$Id],
                     #rf = rf_test$SalePrice[rf_test$Id %in% xgb_test$Id],
                     xgb = xgb_test$SalePrice,
                     ridge = ridge_test$SalePrice)
x_test <- log(x_test)
df <- read.csv("featureMat_v2.csv", header = TRUE, stringsAsFactors = FALSE)
y_train = df$SalePrice[df$Train == 1]
#y_train <- exp(y_train)

glmnet_cv <- cv.glmnet(x=as.matrix(x_train), y=y_train,alpha=0)
lasso_fit <- glmnet(x=as.matrix(x_train), y=y_train, alpha=0, lambda = glmnet_cv$lambda.1se)
lasso_fit$beta
plot(predict(lasso_fit,as.matrix(x_train)), y_train)
abline(a=0,b=1,col="red")

y_pred <- data.frame(Id = 1461:2919, 
                     SalePrice = exp(predict(lasso_fit, as.matrix(x_test))))
colnames(y_pred) <- c("Id", "SalePrice")
write.csv(y_pred, file="stacking_pred_in_featuremat.csv", row.names = FALSE)

mean(abs(predict(lasso_fit, as.matrix(x_train[,c(1,3,4)]))-y_train))

rf_fit <- randomForest(x=as.matrix(x_train[,c(1,3,4)]), y=y_train, nodesize = 3, mtry = 3)
plot(rf_fit$predicted, y_train)
abline(a=0,b=1,col="red")

############ rf tuning

ndsize=c(1,3,5,10)
mtryseq=seq(from=1, to=3, by=1)
RFTune=array(NA, dim=c(length(ndsize),length(mtryseq),5 ))
for (i in 1:length(ndsize)){
  ns=ndsize[i]
  set.seed(602)
  Tune<-train(y=y_train, x=x_train[,c(1,3,4)], method="rf", ntree=500,nodesize=ns, 
              tuneGrid=expand.grid(.mtry=mtryseq),  
              trControl=trainControl(method="repeatedcv",  repeats=2,number=5)) 
  RFTune[i,,]=as.matrix(Tune$results)
  print(i)
}

RFTune[,,2]

########## weight selection
weight_grid <- expand.grid(alpha1=seq(0,1,0.01), alpha2=seq(0,1,0.01))
weight_grid <- weight_grid %>% filter(alpha1+alpha2<=1)
weight_selection <- function(alpha,x_train,y_train){
  mean(abs(y_train -alpha[1]*x_train[,1] - alpha[2]*x_train[,2] -
             (1-alpha[1]-alpha[2])*x_train[,3]))
}
which.min(apply(weight_grid,1,weight_selection,x_train[,c(1,3,4)], y_train))

weight_grid[5151,]


################
plot(lasso_fit)
par(mfrow=c(2,2))
plot(x_train$pls,y_train)
abline(a=0,b=1)
plot(x_train$rf,y_train)
abline(a=0,b=1)
plot(x_train$xgb,y_train)
abline(a=0,b=1)
plot(x_train$elastic,y_train)
abline(a=0,b=1)

set.seed(602)
fit <- train(x = as.matrix(x_train),
             y = y_train,
             method = "glmnet",
             standardize.response = TRUE, standardize = TRUE,
             #preProcess = c("zv", "medianImpute"),
             trControl = trainControl(method = 'repeatedcv',
                                      number = 5, repeats = 10, allowParallel = TRUE),
             tuneGrid = expand.grid(.alpha = seq(0, 1, by = 0.1),
                                    .lambda = seq(0.001, 1, by = 0.05)))
elastic_fit <- glmnet(x=as.matrix(x_train), y=y_train, alpha=0.1, lambda = 0.001)
elastic_fit$beta
plot(predict(elastic_fit,as.matrix(x_train)), y_train)
abline(a=0,b=1,col="red")



