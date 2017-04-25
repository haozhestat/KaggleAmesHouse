rm(list = ls())

setwd("/Users/emilygoren/Documents/School/ISU/Classes/STAT602/Kaggle/honestCV/")

#--------------------------------------------------------------------------------------
# read in data
#--------------------------------------------------------------------------------------

## If you don't read srings as factors, you won't have all the levels in each CV fold.
train <- read.csv("train.csv", stringsAsFactors = TRUE)
train$SalePrice <- log(train$SalePrice) #### TAKE THE LOG
test <- read.csv("test.csv", stringsAsFactors = TRUE)
test$SalePrice <- NA

## Put both datasets together to get all levels of factors needed.
df.combined <- rbind(test, train)
df.combined$MSSubClass <- as.factor(df.combined$MSSubClass)

## Split back into test and training, with Id removed.
d.train <- subset(df.combined, select = -Id, subset = !is.na(SalePrice))
d.test <- subset(df.combined, select = -Id, subset = is.na(SalePrice))

#--------------------------------------------------------------------------------------
# load preprocessing function
# must be named 'preproc' and accept data frame in form of 'd.train' above
# modify this function for different preprocessing
# outputs preprocessed data.frame or xgb.DMatrix that gets fed to fitting function
#--------------------------------------------------------------------------------------
source('preproc.R')
source('preprocXGB.R')

#--------------------------------------------------------------------------------------
# load custom model objects
#--------------------------------------------------------------------------------------
source('glmnet_pp.R')
source('rf_pp.R')
source('pls_pp.R')
source('xgbTree_pp.R')

#--------------------------------------------------------------------------------------
# EG tune Elastic Net (no interactions so it runs faster here)
#--------------------------------------------------------------------------------------

# Fit using pre-processing on each fold.
set.seed(602)
fit <- train(x = subset(d.train, select = -SalePrice),
             y = d.train$SalePrice,
             method = glmnet_pp, ## no longer 'glmnet'
             preProcFun = preproc, ## need this
             na.action = na.pass, ## need this
             standardize = TRUE, standardize.response = TRUE,
             trControl = trainControl(method = 'repeatedcv',
                                      number = 10, repeats = 10),
             tuneGrid = expand.grid(.alpha = seq(0, 1, by = 0.1),
                                    .lambda = seq(0.001, 0.01, by = 0.001)))

# Fit with pre-processing on entire dataset.
set.seed(602)
X <- model.matrix(SalePrice ~ ., data = preproc(d.train))
fit2 <- train(x = X,
             y = d.train$SalePrice,
             method = 'glmnet',
             preProcess = c('center', 'scale'),
             standardize = TRUE, standardize.response = TRUE,
             #preProcFun = preproc, ## need this
             #na.action = na.pass, ## need this
             trControl = trainControl(method = 'repeatedcv',
                                      number = 10, repeats = 10),
             tuneGrid = expand.grid(.alpha = seq(0, 1, by = 0.1),
                                    .lambda = seq(0.001, 0.01, by = 0.001)))

min(fit$results$RMSE)
min(fit2$results$RMSE)


#--------------------------------------------------------------------------------------
# AJ tune random forest & PLS (modified code in Preds_4-22.R)
#--------------------------------------------------------------------------------------

ndsize=c(1,3,5,10)
mtryseq=seq(from=30, to=60, by=10)

# Fit using pre-processing on each fold.
RFTune=array(NA, dim=c(length(ndsize),length(mtryseq),5 ))
for (i in 1:length(ndsize)){
  ns=ndsize[i]
  set.seed(602)
  Tune <- train(y = d.train$SalePrice, 
                x = subset(d.train, select = -SalePrice), 
                method = rf_pp, ## no longer 'rf'
                preProcFun = preproc, ## need this to specify preprocessing function
                na.action = na.pass, ## need this so train() doesn't remove cases with NA      
                ntree=10,
                nodesize=ns, 
                tuneGrid=expand.grid(.mtry=mtryseq),  
                trControl=trainControl(method="repeatedcv",  repeats=10,number=10)) 
  RFTune[i,,]=as.matrix(Tune$results)
  print(i)
}

## Compare to preprocessing entire train dataset then doing CV.
X <- preproc(subset(d.train, select = -SalePrice))
RFTune2=array(NA, dim=c(length(ndsize),length(mtryseq),5 ))
for (i in 1:length(ndsize)){
  ns=ndsize[i]
  set.seed(602)
  Tune <- train(y = d.train$SalePrice, 
                x = X, 
                method = "rf", 
                ntree=10,
                nodesize=ns, 
                tuneGrid=expand.grid(.mtry=mtryseq),  
                trControl=trainControl(method="repeatedcv",  repeats=10,number=10)) 
  RFTune2[i,,]=as.matrix(Tune$results)
  print(i)
}

RFTune[,,2]
RFTune2[,,2]


#############
### PLS
# Fit using pre-processing on each fold.
set.seed(602)
PLSTune<-train(y = d.train$SalePrice, 
               x = subset(d.train, select = -SalePrice),
               method= pls_pp, ## no longer 'pls'
               #preProcess = c("center","scale"), ## would do this before custom preproc
               preProcFun = preproc, ## need this to specify preprocessing function
               na.action = na.pass, ## need this so train() doesn't remove cases with NA   
               tuneGrid=data.frame(.ncomp=seq(1:50)), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=10)) 
# Don't preprocess new data for predictions.
PLSpred <- predict(PLSTune, subset(rbind(d.test, d.train), select = -SalePrice))

## Compare to preprocessing entire train dataset then doing CV.
X <- preproc(subset(d.train, select = -SalePrice))
PLSTune2<-train(y = d.train$SalePrice, 
               x = X,
               method='pls',
               preProcess = c("center","scale"), 
               tuneGrid=data.frame(.ncomp=seq(1:50)), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=10)) 
X.all <- preproc(subset(rbind(d.test, d.train)))
PLSpred2 <- predict(PLSTune2, subset(X.all, select = - SalePrice))

# Compare all fitted values between two CV methods.
plot(PLSpred ~ PLSpred2, pch  = 20)




#--------------------------------------------------------------------------------------
# HZ tune xbgoost (modified code in CV_AmesHouse.R)
#--------------------------------------------------------------------------------------


cv.ctrl = trainControl(method = "repeatedcv", repeats = 10, number = 10, 
                       allowParallel=T)

# Xgboost
#dtrain = xgb.DMatrix(as.matrix(x_train), label = y_train)
#dtest = xgb.DMatrix(as.matrix(x_test))

xgb.grid = expand.grid(nrounds = 750,
                       eta = c(0.01,0.005,0.001),
                       max_depth = c(4,6,8),
                       colsample_bytree=c(0,1,10),
                       min_child_weight = 2,
                       subsample=c(0,0.2,0.4,0.6),
                       gamma=0.01)
set.seed(602)
xgb_tune = train(x = subset(d.train, select = -SalePrice), # A data.frame, will be converted to matrix
                 y = d.train$SalePrice,
                 method= xgbTree_pp, # no longer "xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 preProcFun = preprocXGB, ## need this to specify preprocessing function
                 na.action = na.pass, ## need this so train() doesn't remove cases with NA  
                 verbose=T,
                 metric="RMSE",
                 nthread =3)

# Don't preprocess new data for predictions
xgb_pred <- predict(xgb_tune, subset(rbind(d.test, d.train), select = -SalePrice))