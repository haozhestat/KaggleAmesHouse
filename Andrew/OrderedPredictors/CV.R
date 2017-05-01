setwd("~/Box Sync/Iowa State/2016-17/STAT 602/Kaggle-Ames Housing/KaggleAmesHouse/Andrew/OrderedPredictors")
d <- read.csv("~/Box Sync/Iowa State/2016-17/STAT 602/Kaggle-Ames Housing/KaggleAmesHouse/Andrew/OrderedPredictors/combineddataAJS_4_29.csv", stringsAsFactors = TRUE)

train <-d[1:1460,]
test <-d[1461:2919,]

####################################################################################
#PLS

set.seed(602)
PLSTune<-train(y=train$SalePrice, 
               x=subset(train, select = -SalePrice), 
               method="pls", 
               preProcess = c("center","scale"), 
               tuneGrid=data.frame(.ncomp=seq(1:50)), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=10)) 
PLSTune$results
PLSPred=exp(predict(PLSTune))
PLSPredTest=exp(predict(PLSTune, newdata=test))
write.csv(PLSPred, file="PLSTrainPred.csv")
write.csv(PLSPredTest, file="PLSTestPred.csv")

################################################################################
#Use cross validation to determine optimal number of predictors to consider and mtry
library(randomForest)
npredsvec <- seq(from=50, to=95, by=5) #number of predictor variables to look at
mtryvec <- seq(from=0.1, to=0.9, by=0.1)  #as a proportion of pred. variables in dataset
ndsizevec <- c(1,3,5,10)
nreps <-10
nfolds <-10
RFERRmat=array(NA, dim=c(nreps, nfolds, length(npredsvec), length(mtryvec), length(ndsizevec)))
ntrees=10

set.seed(602)
for (rep in 1:nreps){
foldvec <- sample(rep(1:nfolds, (nrow(train)/nfolds)))
for (fold in 1:nfolds){
train.cv <- train[foldvec!=fold,]
test.cv <-train[foldvec==fold,]
SalePrice.train <- train.cv$SalePrice
SalePrice.test <- test.cv$SalePrice
InitRF <- randomForest(x=subset(train.cv, select=-SalePrice), y=train.cv$SalePrice, ntree=10, importance=TRUE)
Rank=rank(-InitRF$importance[,1])
for (rankind in 1:length(npredsvec)){
  rankcut=npredsvec[rankind]
  train.cv.s <- subset(train.cv, select=-SalePrice)[,Rank<rankcut]
  test.cv.s <-subset(test.cv, select=-SalePrice)[,Rank<rankcut]
  for(mind in 1:length(mtryvec)){
  for(nind in 1:length(ndsizevec)){
    m <- mtryvec[mind]
    n <- ndsizevec[nind]
    RF <- randomForest(x=train.cv.s, y=SalePrice.train, ntree=ntrees,mtry=floor(m*rankcut), nodesize = n)  
    RFPred <- predict(RF, newdata=test.cv.s)
    RFERRmat[rep,fold,rankind,mind,nind] <- sqrt(mean((SalePrice.test-RFPred)^2))
    print(c(rep, fold, rankind, mind, nind))
  }
  }
}
}
}

save(RFERRmat, file="RFERRmatsmall.Rdata")

save(RFERRmat, file="RFERRmat.Rdata")


RFERRmat <- RFERRmat[1,,,,] #dim are #npreds by #mtry by #nodesize

apply(RFERRmat, c(2,3,4), mean)
#average across nodesize
apply(RFERRmat, c(2,3), mean)
#use nodesize=1
apply(RFERRmat[,,,1], c(2,3), mean)
#average across mtry
apply(RFERRmat, c(2,4), mean)
#average across npreds
apply(RFERRmat, c(3,4), mean)

#For when we did more than 1 rep
apply(RFERRmat, c(3,4,5), mean)
#average across nodesize
apply(RFERRmat, c(3,4), mean)
#use a particular nodesize
apply(RFERRmat[,,,,3], c(3,4), mean)
#average across mtry
apply(RFERRmat, c(3,5), mean)
#average across npreds
apply(RFERRmat, c(4,5), mean)

############################################################################################################
#Use nodesize=5, npreds=65, mtry=.5*npreds

library(randomForest)
nreps <-10
nfolds <-10
RFERRmat=array(NA, dim=c(nreps, nfolds))
ntrees=500

set.seed(602)
for (rep in 1:nreps){
  foldvec <- sample(rep(1:nfolds, (nrow(train)/nfolds)))
  for (fold in 1:nfolds){
    train.cv <- train[foldvec!=fold,]
    test.cv <-train[foldvec==fold,]
    SalePrice.train <- train.cv$SalePrice
    SalePrice.test <- test.cv$SalePrice
    InitRF <- randomForest(x=subset(train.cv, select=-SalePrice), y=train.cv$SalePrice, ntree=500, importance=TRUE)
    Rank=rank(-InitRF$importance[,1])
      train.cv.s <- subset(train.cv, select=-SalePrice)[,Rank<65]
      test.cv.s <-subset(test.cv, select=-SalePrice)[,Rank<65]
          RF <- randomForest(x=train.cv.s, y=SalePrice.train, ntree=ntrees,mtry=floor(.5*65), nodesize = 5)  
          RFPred <- predict(RF, newdata=test.cv.s)
          RFERRmat[rep,fold] <- sqrt(mean((SalePrice.test-RFPred)^2))
          print(c(rep, fold))
        }
      }

  #save info corresponding to optimal parameters
save(RFERRmat, file="RFERRCV_fixed_pars.Rdata")

###########################################################################################################
#Make actual predictions for contest
set.seed(602)
InitRF <- randomForest(x=subset(train, select=-SalePrice), y=train$SalePrice, ntree=500, importance=TRUE)
Rank=rank(-InitRF$importance[,1])
SalePrice.train <- train$SalePrice
train.s <- subset(train, select=-SalePrice)[,Rank<65]
test.s <-subset(test, select=-SalePrice)[,Rank<65]
RF <- randomForest(x=train.s, y=SalePrice.train, ntree=500,mtry=floor(.5*65), nodesize = 5)  
RFPred <- exp(predict(RF, newdata=test.s))
RFPredTrain <- exp(predict(RF))

write.csv(RFPred, file="RFTestPred.csv")
write.csv(RFPredTrain, file="RFTrainPred.csv")
