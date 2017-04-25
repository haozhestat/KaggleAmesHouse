# A function for preprocessing
# Inputs: d, a dataframe (with variable names!)
# Outputs: a data matrix
# If you want to do feature selection, add at the end and return 'out' with your feature subset

preprocXGB <- function(d) {
  require(caret)
  require(stringr) #extracting string patterns
  
  # If there is a pool and quality is missing, 
  # then impute by nearest neighbor according to pool area.
  if ( sum(d$PoolArea > 0 & is.na(d$PoolQC)) > 0) {
    kPoolQC <- unique(d$PoolQC[d$PoolArea > 0])
    kPoolQC <- length(kPoolQC[!is.na(kPoolQC)]) ## Get true k.
    pool.train <- d$PoolArea[(d$PoolArea > 0) & !is.na(d$PoolQC)]
    pool.train.cl <- d$PoolQC[(d$PoolArea > 0) & !is.na(d$PoolQC)]
    pool.test <- d$PoolArea[(d$PoolArea > 0) & is.na(d$PoolQC)] 
    require(class)
    pool.impute <- knn(train = data.frame(pool.train), 
                       test = data.frame(pool.test), 
                       cl = pool.train.cl, k = kPoolQC)
    d$PoolQC[(d$PoolArea > 0) & is.na(d$PoolQC)] <- pool.impute
  }
  # If there is NO pool, set quality to none.
  levels(d$PoolQC) <- c(levels(d$PoolQC), 'None')
  d$PoolQC[is.na(d$PoolQC)] <- 'None'
  
  # If there a garage and year built is missing, set to house year built.
  idx <- which(is.na(d$GarageYrBlt) & (d$GarageArea > 0 | !is.na(d$GarageType)))
  d[idx, 'GarageYrBlt'] <- d[idx, 'YearBuilt']
  
  # If there is NO GARAGE, say it was built when the house was built.
  idx <- which(is.na(d$GarageYrBlt))
  d[idx, 'GarageYrBlt'] <- d[idx, 'YearBuilt']
  
  # If there is a garage and other garage info is missing, impute by most common level.
  idx <- which(d$GarageArea > 0 & is.na(d$GarageQual))
  d$GarageQual[idx] <- names(sort(-table(d$GarageQual)))[1]
  idx <- which(d$GarageArea > 0 & is.na(d$GarageFinish))
  d$GarageFinish[idx] <- names(sort(-table(d$GarageFinish)))[1]
  idx <- which(d$GarageArea > 0 & is.na(d$GarageCond))
  d$GarageCond[idx] <- names(sort(-table(d$GarageCond)))[1]
  
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
  
  # Impute missing kitchen quality with most common level. 
  idx <- which(is.na(d$KitchenQual))
  d$KitchenQual[idx] <- names(sort(-table(d$KitchenQual)))[1]
  
  # Impute missing electrical with most common level.
  idx <- which(is.na(d$Electrical))
  d$Electrical[idx] <- names(sort(-table(d$Electrical)))[1]
  
  # If there is a basement, impute basement exposure with most common level.
  idx <- which(is.na(d$BsmtExposure) & d$TotalBsmtSF > 0)
  d$BsmtExposure[idx] <- names(sort(-table(d$BsmtExposure)))[1]
  
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
  
  # If missing exterior variable, say it is other.
  levels(d$Exterior1st) <- c(levels(d$Exterior1st), 'Other')
  levels(d$Exterior2nd) <- c(levels(d$Exterior2nd), 'Other')
  d$Exterior1st[is.na(d$Exterior1st)] <- 'Other'
  d$Exterior2nd[is.na(d$Exterior2nd)] <- 'Other'
  
  # If missing sale type, impute by most common level.
  idx <- which(is.na(d$SaleType))
  d$SaleType[idx] <- names(sort(-table(d$SaleType)))[1]
  
  # If missing function, impute by most common level.
  idx <- which(is.na(d$Functional))
  d$Functional[idx] <- names(sort(-table(d$Functional)))[1]
  
  # Impute MSZoning by most frequent level for that MSSubClass.
  freqs <- table(d$MSZoning, d$MSSubClass)
  idx <- which(is.na(d$MSZoning))
  for (i in idx) {
    impt <- names(which.max(freqs[,colnames(freqs) == d$MSSubClass[i]]))
    d$MSZoning[i] <- impt
  }
  
  # If house has masonry but the type is missing, impute with most common type.
  idx <- which(is.na(d$MasVnrType) & d$MasVnrArea > 0)
  d$MasVnrType[idx] <- names(sort(-table(d$MasVnrType[d$MasVnrArea > 0])))[1]
  
  # If there is no masonry, put area = 0 and type = none.
  d$MasVnrType[is.na(d$MasVnrType)] <- 'None'
  d$MasVnrArea[is.na(d$MasVnrArea)] <- 0
  
  # Impute missing lot frontage by neighborhood median.
  tmp <- aggregate(LotFrontage ~ Neighborhood, data = d, FUN = median)
  idx <- (1:nrow(d))[is.na(d$LotFrontage)]
  for (i in idx) {
    impute.val <- tmp$LotFrontage[tmp$Neighborhood == d$Neighborhood[i]]
    if (length(impute.val) == 0) 
      impute.val <- median(d$LotFrontage, na.rm = TRUE) #if failed, use overall median
    d$LotFrontage[i] <- impute.val
  }
  # Use overall median if no other obs from that house's neighborhood.
  d$LotFrontage[is.na(d$LotFrontage)] <- median(d$LotFrontage, na.rm = TRUE)
  
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
  
  # Create some new variables
  d$SecondFlr <- as.factor(as.numeric((d$X2ndFlrSF > 0))) # Does it have a 2nd floor?
  d$Remodeled <- as.factor(as.numeric((d$YearBuilt != d$YearRemodAdd))) # Was it remodeled?
  d$BrandNew <- as.factor(as.numeric((d$YearBuilt == d$YrSold))) # Is the house brand new?
  d$HighSeason <- as.factor(as.numeric((d$MoSold %in% c(5,6,7)))) # Was the house sold in summer?
  d$RoomSize <- (d$X1stFlrSF + d$X2ndFlrSF) /d$TotRmsAbvGrd # Average non-basement room size.
  # make sure all factor levels are in the factor.
  levels(d$SecondFlr) <- levels(d$Remodeled) <- levels(d$BrandNew) <- levels(d$HighSeason) <- c('0', '1')

  # Center and scale numeric variables using caret's functions.
  # To remove zero var predictors, add 'zv' to method.
  # To remove near zero var predictors, add 'nzv' to method.
  #caret.preprocer <- preProcess(d, method = c("center", "scale"))
  #caret.preprocer <- preProcess(d, method = c("center", "scale", "zv"))
  #caret.preprocer <- preProcess(d, method = c("center", "scale", "zv", "nzv"))
  
  # This applies caret's preprocessing stored in the 'caret.preprocer' object.
  #d <- predict(caret.preprocer, newdata = d)
  
  out <- apply(data.matrix(d), 2, as.numeric)

  return(out)
}

