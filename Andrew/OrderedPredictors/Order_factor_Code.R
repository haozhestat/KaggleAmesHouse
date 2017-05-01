## If you don't read srings as factors, you won't have all the levels in each CV fold.
train <- read.csv("~/Box Sync/Iowa State/2016-17/STAT 602/Kaggle-Ames Housing/KaggleAmesHouse/Andrew/honestCV/train.csv", stringsAsFactors = TRUE)
train$SalePrice <- log(train$SalePrice) #### TAKE THE LOG
test <- read.csv("~/Box Sync/Iowa State/2016-17/STAT 602/Kaggle-Ames Housing/KaggleAmesHouse/Andrew/honestCV/test.csv", stringsAsFactors = TRUE)
test$SalePrice <- NA

## Put both datasets together to get all levels of factors needed.
df.combined <- rbind(test, train)
df.combined$MSSubClass <- as.factor(df.combined$MSSubClass)

## Split back into test and training, with Id removed.
d.train <- subset(df.combined, select = -Id, subset = !is.na(SalePrice))
d.test <- subset(df.combined, select = -Id, subset = is.na(SalePrice))

d <- rbind(d.train, d.test)

#Fix 2 NA's in utilities
levels(d$Utilities) <- c(levels(d$Utilities), 'Unknown')
d$Utilities[is.na(d$Utilities)] <- "Unknown"

#Set 3 pools with unreported quantity to "Good" and sort as ordered numeric variable.
levels(d$PoolQC) <- c(levels(d$PoolQC), 'None')
d$PoolQC[is.na(d$PoolQC)] <- 'None'
d$PoolQC[d$PoolArea>0 & d$PoolQC=="None"] <-'Gd'  #Set pools with missing quality to "Good" This is an arbitrary choice, not based on data.
d$PoolQC = factor(d$PoolQC,levels(d$PoolQC)[c(4,3,2,1)])
d$PoolQC <- as.numeric(d$PoolQC)

# If there a garage and year built is missing, set to house year built.
idx <- which(is.na(d$GarageYrBlt) & (d$GarageArea > 0 | !is.na(d$GarageType)))
d[idx, 'GarageYrBlt'] <- d[idx, 'YearBuilt']
d$GarageYrBlt[d$GarageYrBlt==2207] <- 2007  #Fix a typo
# If there is NO GARAGE, say it was built when the house was built.
idx <- which(is.na(d$GarageYrBlt))
d[idx, 'GarageYrBlt'] <- d[idx, 'YearBuilt']
#setting missing garage quality to typical/average (only one such garage)
idx <- which(d$GarageArea > 0 & is.na(d$GarageQual))
d$GarageQual[idx] <- "TA"
idx <- which(d$GarageArea > 0 & is.na(d$GarageFinish))
d$GarageFinish[idx] <- "RFn"
idx <- which(d$GarageArea > 0 & is.na(d$GarageCond))
d$GarageCond[idx] <- "TA"
# Some more imputations for garage. 
# If numeric and missing, replace with 0. If categorical, replace with 'None'.
garage.cols <- c('GarageArea', 'GarageCars', 'GarageQual', 
                 'GarageFinish', 'GarageCond', 'GarageType')
for (col in garage.cols){
  if (is.numeric(d[,col]) == TRUE){
    d[,col][is.na(d[,col])] <- 0
  }
  else {
    levels(d[,col]) <- c(levels(d[,col]), 'None')
    d[,col][is.na(d[,col])] <- 'None'
  }
}
#Convert ordered factor variables to numeric
d$GarageQual = factor(d$GarageQual,levels(d$GarageQual)[c(6,3,1,4,2,5)])
d$GarageQual <- as.numeric(d$GarageQual)
d$GarageCond = factor(d$GarageCond,levels(d$GarageCond)[c(6,4,2,5,3,1)])
d$GarageCond <- as.numeric(d$GarageCond)
d$GarageFinish = factor(d$GarageFinish,levels(d$GarageFinish)[c(4,3,2,1)])
d$GarageFinish <- as.numeric(d$GarageFinish)
d$NoGarage <-as.numeric(d$GarageArea==0)

# Impute missing kitchen quality with most common level. 
idx <- which(is.na(d$KitchenQual))
d$KitchenQual[idx] <-"TA"
d$KitchenQual = factor(d$KitchenQual,levels(d$KitchenQual)[c(2,4,3,1)])
d$KitchenQual <- as.numeric(d$KitchenQual)

# Impute missing electrical with most common level.
idx <- which(is.na(d$Electrical))
d$Electrical[idx] <- "FuseF"
d$Electrical = factor(d$Electrical,levels(d$Electrical)[c(5,3,2,1,4)])
d$Electrical <- as.numeric(d$Electrical)

# If there is a basement, set exposure to average
idx <- which(is.na(d$BsmtExposure) & d$TotalBsmtSF > 0)
d$BsmtExposure[idx] <- "Av"
# Some more imputations for basement. 
# If numeric and missing, replace with 0. If categorical, replace with 'None'.
bsmt.cols <- names(d)[sapply(names(d), function(x) str_detect(x, 'Bsmt'))]
for (col in bsmt.cols){
  if (is.numeric(d[,col]) == TRUE){
    d[,col][is.na(d[,col])] <- 0
  }
  else{
    levels(d[,col]) <- c(levels(d[,col]), 'None')
    d[,col][is.na(d[,col])] <- 'None'
  }
}
d$NoBsmt <- as.numeric(d$BsmtQual=="None")
d$BsmtQual<- factor(d$BsmtQual,levels(d$BsmtQual)[c(5,2,4,3,1)])
d$BsmtQual <- as.numeric(d$BsmtQual)
d$BsmtCond <- factor(d$BsmtCond,levels(d$BsmtCond)[c(5,3,1,4,2)])
d$BsmtCond <- as.numeric(d$BsmtCond)
d$BsmtExposure <- factor(d$BsmtExposure,levels(d$BsmtExposure)[c(5,4,3,1,2)])
d$BsmtExposure <- as.numeric(d$BsmtExposure)
d$BsmtFinType1 <- factor(d$BsmtFinType1,levels(d$BsmtFinType1)[c(7,6,4,5,2,1,3)])
d$BsmtFinType1 <- as.numeric(d$BsmtFinType1)
d$BsmtFinType2 <- factor(d$BsmtFinType2,levels(d$BsmtFinType2)[c(7,6,4,5,2,1,3)])
d$BsmtFinType2 <- as.numeric(d$BsmtFinType2)

# If missing exterior variable, say it is other.
levels(d$Exterior1st) <- c(levels(d$Exterior1st), 'Other')
levels(d$Exterior2nd) <- c(levels(d$Exterior2nd), 'Other')
d$Exterior1st[is.na(d$Exterior1st)] <- 'Other'
d$Exterior2nd[is.na(d$Exterior2nd)] <- 'Other'

# If missing sale type, set to "Other".
idx <- which(is.na(d$SaleType))
d$SaleType[idx] <- "Oth"

# If missing function, use Typ.
idx <- which(is.na(d$Functional))
d$Functional[idx] <- "Typ"
d$Functional = factor(d$Functional,levels(d$Functional)[c(7,1,2,3,4,5,6)])
d$Functional <- as.numeric(d$Functional)

# Impute MSZoning by most frequent level for that MSSubClass.
idx <- which(is.na(d$MSZoning))
levels(d$MSZoning) <- c(levels(d$MSZoning), 'Missing')
d$MSZoning[idx] <- "Missing"

# If house has masonry but the type is missing, set to none since no obvious choice. There's only one case so really not much we can do.
idx <- which(is.na(d$MasVnrType) & d$MasVnrArea > 0)
d$MasVnrType[idx] <- "None"

# If there is no masonry, put area = 0 and type = none.
d$MasVnrType[is.na(d$MasVnrType)] <- 'None'
d$MasVnrArea[is.na(d$MasVnrArea)] <- 0

# If lot frontage is missing set =0, then setup anoth variable as an indicator of missing.
d$LotFrontage[is.na(d$LotFrontage)] <- 0
d$LotFrontageMissing <- as.numeric(d$LotFrontage==0)

# If fence, alley, fireplace quality, or misc feature is missing, set to None.
levels(d$Fence) <- c(levels(d$Fence), 'None')
d$Fence[is.na(d$Fence)] <- 'None'
levels(d$MiscFeature) <- c(levels(d$MiscFeature), 'None')
d$MiscFeature[is.na(d$MiscFeature)] <- 'None'
levels(d$FireplaceQu) <- c(levels(d$FireplaceQu), 'None')
d$FireplaceQu[is.na(d$FireplaceQu)] <- 'None'
levels(d$Alley) <- c(levels(d$Alley), 'None')
d$Alley[is.na(d$Alley)] <- 'None'

# Combine condition categories into single indicators/dummy vars.
cond1 <- data.frame(model.matrix(~ Condition1 + 0, data = d))
names(cond1) <- sub(".*1", "", names(cond1))
cond2 <- data.frame(model.matrix(~ Condition2 + 0, data = d))
names(cond2) <- sub(".*2", "", names(cond2))
idx <- names(cond1) %in% names(cond2)
cond <- cond1
cond[, idx] <- cond1[, idx] + cond2
cond <- ifelse(cond == 0, 0, 1)
cond <- as.data.frame(t(apply(cond, 1, as.factor)))
d <- subset(d, select = -c(Condition1, Condition2))
d <- cbind(d, cond)

# Create some new variables.
d$SecondFlr <- as.factor(as.numeric((d$X2ndFlrSF > 0))) # Does it have a 2nd floor?
d$Remodeled <- as.factor(as.numeric((d$YearBuilt != d$YearRemodAdd))) # Was it remodeled?
d$BrandNew <- as.factor(as.numeric((d$YearBuilt == d$YrSold))) # Is the house brand new?
d$HighSeason <- as.factor(as.numeric((d$MoSold %in% c(5,6,7)))) # Was the house sold in summer?
d$BedSize <- ifelse(d$BedroomAbvGr == 0, 0, (d$X1stFlrSF + d$X2ndFlrSF) / d$BedroomAbvGr) # Attempt at average non-basement bedroom size.
d$RoomSize <- (d$X1stFlrSF + d$X2ndFlrSF) /d$TotRmsAbvGrd # Average non-basement room size.
levels(d$SecondFlr) <- levels(d$Remodeled) <- levels(d$BrandNew) <- levels(d$HighSeason) <- c('0', '1')

#Order more factors
d$ExterQual = factor(d$ExterQual,levels(d$ExterQual)[c(2,4,3,1)])
d$ExterQual <- as.numeric(d$ExterQual)
d$ExterCond = factor(d$ExterCond,levels(d$ExterCond)[c(4,2,5,3,1)])
d$ExterCond <- as.numeric(d$ExterCond)
d$HeatingQC = factor(d$HeatingQC,levels(d$HeatingQC)[c(4,2,5,3,1)])
d$HeatingQC <- as.numeric(d$HeatingQC)
d$FireplaceQu = factor(d$FireplaceQu,levels(d$FireplaceQu)[c(6,4,2,5,3,1)])
d$FireplaceQu <- as.numeric(d$FireplaceQu)
d$Fence = factor(d$Fence,levels(d$Fence)[c(6,4,2,5,3,1)])
d$Fence <- as.numeric(d$Fence)

setwd("~/Box Sync/Iowa State/2016-17/STAT 602/Kaggle-Ames Housing/KaggleAmesHouse/Andrew/OrderedPredictors")
write.csv(d, file="combineddataAJS_4_29.csv", row.names = F)
