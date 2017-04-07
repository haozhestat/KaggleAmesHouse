train=read.csv("train.csv")
dim(train)

str(train)
train$MSSubClass=as.factor(train$MSSubClass)  #This is int but looks like it should be a factor from description

#Some quick observations about missing or strange observations
summary(train)
#LotFrontage 259 NA's
#Alley-1369 NA's means no alley
#Utilities-only 1 NoSeWa, rest have AllPub
#Neighborhood 707 other
#Condition1: 15 other
#Condition2: 2 others  #Might want to set up indicator variables for each condition
#Housestyle 19 other
#RoofMatl 2 other
#Exterior1st and Exterior 2nd have 128 and 136 others (not sure if we should combine as indicators-is 1st more prevelant?)
#MasVnrType 8 NA's   #There is a different category for none
#MasVnrArea 8 NA's   #There is a different category for none
#Bsmt Variables NA means no basement. There are 37 of these.
sum(train$BsmtFinSF1==0) #467 0's which is much more than 37.
#BsmtFinType2 38 NA's seems strange is there only 1 house with only one type?
#Electrical 1 NA
#X2ndFlrSF     LowQualFinSF  are heavily skewed right
#NA FireplaceQu indicates no fireplace 690 of these
#Garage vars have 81 NA's for no garage
#PoolQC  Only 7 with pool
#Fence NA's means no fence 1179 of these
#MiscFeature means something else not in dataset 1406 of these are NA's. 49 sheds
#MiscVal #Heavily skewed right-1more than 1,000 and it's 15500.
#SaleType 9 other


