train=read.csv("train.csv")
test=read.csv("test.csv")
test$SalePrice=NA
Data=rbind(train, test)
dim(Data)

ConvertNA=function(x){
if(is.factor(x)){
x=as.character(x)
x[is.na(x)]="NONE"
x=as.factor(x)}
if(is.integer(x)){
    x[is.na(x)]=0} 
return(x)
}

for(i in 1:81){
Data[,i]=ConvertNA(Data[,i])
}
str(Data)
Data$MSSubClass=as.factor(Data$MSSubClass)  #This is int but looks like it should be a factor from description

Data$Artery=as.factor(Data$Condition1=="Artery" | Data$Condition2=="Artery") 
Data$Feedr=as.factor(Data$Condition1=="Feedr" | Data$Condition2=="Feedr") 
Data$Norm=as.factor(Data$Condition1=="Norm" | Data$Condition2=="Norm") 
Data$RRNn=as.factor(Data$Condition1=="RRNn" | Data$Condition2=="RRNn") 
Data$RRAn=as.factor(Data$Condition1=="RRAn" | Data$Condition2=="RRAn") 
Data$PosN=as.factor(Data$Condition1=="PosN" | Data$Condition2=="PosN") 
Data$PosA=as.factor(Data$Condition1=="PosA" | Data$Condition2=="PosA") 
Data$RRNe=as.factor(Data$Condition1=="RRNe" | Data$Condition2=="RRNe") 
Data$RRAe=as.factor(Data$Condition1=="RRAe" | Data$Condition2=="RRAe") 

train=Data[1:1460,]
test=Data[1461:2919,]
test$SalePrice=NA

#random forest
set.seed(04062017)
rf=randomForest(y=train$SalePrice, x=train[,c(2:80,82:90)], ntree=500, importance=T)
rfpred=predict(rf,OOB=T)
qplot(rfpred, train$SalePrice)
rf$importance
include=rownames(rf$importance)[rf$importance[,1]>0]

train1=train[,names(train)%in%include]
train1$SalePrice=train$SalePrice

library(caret)
library(rpart)
library(randomForest)
library(xgboost)
library(pls)


PCRTune<-train(y=train1[,71], 
               x=train1[,-c(71)], 
               method="pcr", 
               preProcess = c("center","scale"), 
               tuneGrid=data.frame(.ncomp=seq(1:50)), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=10)) 
PCRTune$results
PCRpredH=predict(PCRTune)

PLSTune<-train(y=train1[,71], 
               x=train1[,-c(71)], 
               method="pls", 
               preProcess = c("center","scale"), 
               tuneGrid=data.frame(.ncomp=seq(1:50)), 
               trControl=trainControl(method="repeatedcv", repeats=10,number=10)) 
PLSTune$results
PLSpredH=predict(PLSTune)

TreeTuneH<-train(y=train1[,71], 
                 x=train1[,-c(71)], 
                 method="rpart", 
                 tuneGrid=data.frame(.cp=seq(from=0, to=0.1, by=0.01)), 
                 trControl=trainControl(method="repeatedcv", 
                                        repeats=10,number=10)) 
TreeTuneH$results
TreepredH=predict(TreeTuneH)

ndsize=c(1,3,5,10,15,20,25)
mtry=seq(from=3, to=24, by=3)
RFTuneH=array(NA, dim=c(length(ndsize),length(mtry),5 ))
for (i in 1:length(ndsize)){
  Tune<-train(y=train1[,71], x=train1[,-c(71)], method="rf",  tuneGrid=expand.grid(.mtry=mtry),  trControl=trainControl(method="repeatedcv",  repeats=1,number=10),nodesize=3, ntree=10) 
  RFTuneH[i,,]=as.matrix(Tune$results)
  print(i)
}

#Show RMSE for nodesize by mtry combo
RFTuneH[,,2]
RF=randomForest(y=train1[,71], x=train1[,-c(71)], mtry=21, nodesize=5)
RFpredH=predict(RF)

pairs(~RFpredH+PLSpredH+PCRpredH+TreepredH )

#Predict test cases
test1=test[,names(test)%in%names(train1)]

PLSTestPred=predict(PLSTune, newdata = test1)
PCRTestPred=predict(PCRTune, newdata = test1)
TreeTestPred=predict(TreeTuneH, newdata = test1)
test1$SalePrice=0
RFTestPred=predict(RF, newdata = test1)

PLSTestPred[PLSTestPred<0]=min(PLSTestPred[PLSTestPred>0])
PCRTestPred[PCRTestPred<0]=min(PCRTestPred[PCRTestPred>0])

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