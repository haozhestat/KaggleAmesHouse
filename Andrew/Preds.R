setwd("~/Box Sync/Iowa State/2016-17/STAT 602/Kaggle-Ames Housing/KaggleAmesHouse/Andrew")
data=read.csv("featureMatrix_v1.csv")
data$MSSubClass=as.factor(data$MSSubClass)  #This is int but looks like it should be a factor from description


#Turn cond1 and cond 2 into indicator variables
data$Artery=as.factor(data$Condition1=="Artery" | data$Condition2=="Artery") 
data$Feedr=as.factor(data$Condition1=="Feedr" | data$Condition2=="Feedr") 
data$Norm=as.factor(data$Condition1=="Norm" | data$Condition2=="Norm") 
data$RRNn=as.factor(data$Condition1=="RRNn" | data$Condition2=="RRNn") 
data$RRAn=as.factor(data$Condition1=="RRAn" | data$Condition2=="RRAn") 
data$PosN=as.factor(data$Condition1=="PosN" | data$Condition2=="PosN") 
data$PosA=as.factor(data$Condition1=="PosA" | data$Condition2=="PosA") 
data$RRNe=as.factor(data$Condition1=="RRNe" | data$Condition2=="RRNe") 
data$RRAe=as.factor(data$Condition1=="RRAe" | data$Condition2=="RRAe") 

#Split into test/training data and take out id col
train=data[data$Train==1, -c(1,3)]
test=data[data$Train==0, -c(1,3)]


set.seed(04132017)
ndsize=seq(from=5, to=30, by=5)
mtry=seq(from=10, to=50, by=10)
nfolds=10
CFTuneH=array(NA, dim=c(nfolds, length(ndsize),length(mtry)))
RFTuneH=array(NA, dim=c(nfolds, length(ndsize),length(mtry)))
folds=ceiling(sample(1:nrow(train),replace=F )/146)
for (fold in 1:nfolds){
  Tr=train[folds!=fold, ]
  Te=train[folds==fold, ]
for (i in 1:length(ndsize)){
  for (j in 1:length(mtry)){
  CF=cforest(SalePrice~., data=Tr, controls=cforest_unbiased(ntree=100, minsplit=ndsize[i], mtry=mtry[j]))
  CFPred=predict(CF, newdata=Te)
  CFTuneH[fold, i,j]=sqrt(mean((CFPred-Te$SalePrice)^2))
  RF=randomForest(SalePrice~., data=Tr, ntree=100, nodesize=ndsize[i], mtry=mtry[j])
  RFPred=predict(RF, newdata=Te)
  RFTuneH[fold, i,j]=sqrt(mean((RFPred-Te$SalePrice)^2))
  }
}
  print(fold)
}

apply(CFTuneH, c(2,3), mean)
#CFbest is mtry=30, nodesize=10
#RMSE: 30489.85

apply(RFTuneH, c(2,3), mean)
#RFbest is mtry=30, nodesize=5
#RMSE:27928.89

rowMeans(apply(CFTuneH, c(2,3), mean))
rowMeans(apply(RFTuneH, c(2,3), mean))

save(CFTuneH, RFTuneH, file="RFCFTuning.Rdata")

set.seed(04132017)
CF=cforest(SalePrice~., data=train, controls=cforest_unbiased(ntree=500, minsplit=10, mtry=30))
CFPred=predict(CF, newdata=test)
RF=randomForest(SalePrice~., data=train, ntree=500, nodesize=5, mtry=30)
RFPred=predict(RF, newdata=test)

qplot(RFPred, CFPred)
cor(RFPred, CFPred)

library(caret)
library(rpart)
library(randomForest)
library(xgboost)
library(pls)
library(party)

set.seed(04132017)
PCRTune<-train(y=train[,1], 
               x=train[,-c(1)], 
               method="pcr", 
               preProcess = c("center","scale"), 
               tuneGrid=data.frame(.ncomp=25:75), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=10)) 
PCRTune$results
PCRpredH=predict(PCRTune)
#RMSE: 34286.54

PLSTune<-train(y=train[,1], 
               x=train[,-c(1)], 
               method="pls", 
               preProcess = c("center","scale"), 
               tuneGrid=data.frame(.ncomp=seq(1:50)), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=10)) 
PLSTune$results
#ncomp=17, RMSE=32350
PLSpredH=predict(PLSTune)
PLSPred=predict(PLSTune, newdata=test)

summary(CFPred)
summary(RFPred)
summary(PLSPred)

RF_PLS=(RFPred+PLSPred)/2
RF_CF_PLS=(RFPred+CFPred+PLSPred)/3

write.csv(RF_PLS, file="RF_PLS.csv")
write.csv(RF_CF_PLS, file="RF_CF_PLS.csv")










library(xgboost)

xgbTuneH <- train( x=train[,-c(1)],
  y= train[,1],
  trControl = trainControl(method="repeatedcv",repeats=1,number=10), 
 # tuneGrid = xgb_grid_1,
  method="xgbTree"
)

xgbTuneH<-train(y=train[,1], 
                x=train[,-c(1)], 
                #preProcess = c("center","scale"), 
                method="xgbTree", 
                # tuneGrid=data.frame(expand.grid(eta=c(0.2,0.3,0.4),maxdepth=1:3 ,nrounds=c(10,50,100,150))), 
                trControl=trainControl(method="repeatedcv", 
                                       repeats=1,number=10)) 
xgbTuneH
xgbpredH=predict(xgbTuneH)
































write.csv(PLSTestPred, file="PLS_ajs.csv")
write.csv(PCRTestPred, file="PCR_ajs.csv")
write.csv(TreeTestPred, file="Tree_ajs.csv")
write.csv(RFTestPred, file="RF_ajs.csv")



pairs(~PLSTestPred+PCRTestPred+TreeTestPred+RFTestPred )
df=data.frame(PLSTestPred,PCRTestPred,TreeTestPred,RFTestPred)
cor(df)

AvgPred=(PLSTestPred+PCRTestPred+TreeTestPred+RFTestPred)/4
write.csv(AvgPred, file="FirstPredictions.csv")

#April 6 Progress
#Replaced missing values with "NONE" category for factor variables or 0 for numerical ones.
#Used random forest variable importance to identify variables to include in models
#Used caret to find optimal parameter settings for PLS, PCR, tree and RF and used these techniques to make predictions on new data.
#Tried these techniques using all variables and only those with importance >0 using RF. Did better using only important ones.
#Averaged predictions from 4 techniques