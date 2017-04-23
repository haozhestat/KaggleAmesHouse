#New feature matrix-version 3
setwd("~/Box Sync/Iowa State/2016-17/STAT 602/Kaggle-Ames Housing/KaggleAmesHouse/Andrew")
data=read.csv("featureMat_v3.csv")
data$MSSubClass=as.factor(data$MSSubClass)  #This is int but looks like it should be a factor from description

train=data[data$Train==1, -c(1)]
test=data[data$Train!=1, -c(1)]

#use random forest to pick out important variables
#set.seed(04132017)
#rf=randomForest(y=train[,1], x=train[,c(-1)], ntree=1000, importance=T)
#include=rownames(rf$importance)[rf$importance[,1]>0]
#data1=cbind(data[,2], data[,names(data)%in%include])
#names(data1)[1]="SalePrice"
#Split into test/training data and take out id col
#train=data1[data$Train==1, ]
#test=data1[data$Train==0, ]



ndsize=c(1,3,5,10)
mtryseq=seq(from=30, to=60, by=10)
RFTune=array(NA, dim=c(length(ndsize),length(mtryseq),5 ))
for (i in 1:length(ndsize)){
ns=ndsize[i]
set.seed(602)
Tune<-train(y=train[,1], x=train[,-c(1)], method="rf", ntree=10,nodesize=ns, tuneGrid=expand.grid(.mtry=mtryseq),  trControl=trainControl(method="repeatedcv",  repeats=10,number=10)) 
RFTune[i,,]=as.matrix(Tune$results)
print(i)
}

RFTune[,,2]

set.seed(602)
RF=randomForest(data=train, SalePrice~., mtry=50, nodesize=3)
LogPred=predict(RF, newdata=test)
Pred=exp(LogPred)
write.csv(Pred, file="RFnew_4_23.csv")

#############
#PLS with new feature matrix
set.seed(602)
PLSTune<-train(y=train[,1], 
               x=train[,-c(1)], 
               method="pls", 
               preProcess = c("center","scale"), 
               tuneGrid=data.frame(.ncomp=seq(1:50)), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=10)) 
PLSTune$results
PLSpredH=predict(PLSTune)
PLSPred=exp(predict(PLSTune, newdata=test))
write.csv(PLSPred, file="PLS_new_4_23.csv")



#####################################################################################################
#Old feature matrix
setwd("~/Box Sync/Iowa State/2016-17/STAT 602/Kaggle-Ames Housing/KaggleAmesHouse/Andrew")
data=read.csv("featureMatrix_v1.csv")
data$MSSubClass=as.factor(data$MSSubClass)  #This is int but looks like it should be a factor from description
data$SalePrice=log(data$SalePrice)

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

train=data[data$Train==1, -c(1,3)]
test=data[data$Train!=1, -c(1,3)]


ndsize=c(1,3,5,10)
mtryseq=seq(from=30, to=60, by=10)
RFTune=array(NA, dim=c(length(ndsize),length(mtryseq),5 ))
for (i in 1:length(ndsize)){
  ns=ndsize[i]
  set.seed(602)
  Tune<-train(y=train[,1], x=train[,-c(1)], method="rf", ntree=10,nodesize=ns, tuneGrid=expand.grid(.mtry=mtryseq),  trControl=trainControl(method="repeatedcv",  repeats=10,number=10)) 
  RFTune[i,,]=as.matrix(Tune$results)
  print(i)
}

RFTune[,,2]

set.seed(602)
RF=randomForest(data=train, SalePrice~., mtry=50, nodesize=5)
LogPred=predict(RF, newdata=test)
Pred=exp(LogPred)
write.csv(Pred, file="RFold_4_23.csv")

#################################################################################
#PLS with old feature matrix

set.seed(602)
PLSTune<-train(y=train[,1], 
               x=train[,-c(1)], 
               method="pls", 
               preProcess = c("center","scale"), 
               tuneGrid=data.frame(.ncomp=seq(1:50)), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=10)) 
PLSTune$results
PLSpredH=predict(PLSTune)
PLSPred=exp(predict(PLSTune, newdata=test))
write.csv(PLSPred, file="PLS_old_4_23.csv")

