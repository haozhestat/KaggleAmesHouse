
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

write.csv(data, "featureMatrix_v2.csv", row.names=F)
