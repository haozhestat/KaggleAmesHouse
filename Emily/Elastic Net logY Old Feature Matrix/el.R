require(glmnet)
require(caret)

### Put directory of file location here
dir <- '/home/egoren/'

## ----data----------------------------------------------------------------
train <- read.csv(paste0(dir, "train.csv"))
test <- read.csv(paste0(dir, "test.csv"))
testID <- test$Id
test$SalePrice <- 0
df.combined <- rbind(train, test)


## ----data2---------------------------------------------------------------
df.combined$MSSubClass <- as.factor(as.numeric(df.combined$MSSubClass))

# Add indicator for missing garage year built
df.combined$GarageMiss <- as.factor(is.na(df.combined$GarageYrBlt))

# Replace NA's.
missing <- apply(df.combined, 2, function(x) sum(is.na(x)))
missing[missing > 0]
missvars <- names(missing[missing > 0])
str(subset(df.combined, select = missvars))
# Add NA as a factor level for categorical variables
for (i in 1:length(missvars)) {
    thisvar <- df.combined[ , missvars[i]]
    if (is.factor(thisvar))
        thisvar <- addNA(thisvar)
    df.combined[ , missvars[i]] <- thisvar
}
# Make indicators for conditions.
options(na.action = 'na.pass')
cond1 <- data.frame(model.matrix(~ Condition1 + 0, data = df.combined))
names(cond1) <- sub(".*1", "", names(cond1))
cond2 <- data.frame(model.matrix(~ Condition2 + 0, data = df.combined))
names(cond2) <- sub(".*2", "", names(cond2))
idx <- names(cond1) %in% names(cond2)
cond <- cond1
cond[, idx] <- cond1[, idx] + cond2
cond <- ifelse(cond == 0, 0, 1)
cond <- as.data.frame(t(apply(cond, 1, as.factor)))
df.combined <- subset(df.combined, select = -c(Condition1, Condition2))
df.combined <- cbind(df.combined, cond)
df.proc <- preProcess(df.combined, method = 'medianImpute')
df.proc <- predict(df.proc, df.combined)
df.proc$SalePrice[df.proc$SalePrice == 0] <- NA
df.proc$SalePrice <- log(df.proc$SalePrice)

## ----mod------------------------------------------------------------
d <- df.proc
X <- model.matrix(SalePrice ~ (.)^2 + 0, data = d)
train.idx <- !is.na(d$SalePrice)
set.seed(602)
fit <- train(x = X[train.idx,],
             y = d$SalePrice[train.idx],
             method = "glmnet",
             standardize.response = TRUE, standardize = TRUE,
             #preProcess = c("zv"),
             trControl = trainControl(method = 'repeatedcv',
                                      number = 10, repeats = 10),
             tuneGrid = expand.grid(.alpha = seq(0, 1, by = 0.1),
                                    .lambda = seq(0.001, 1, by = 0.05)))
fit$bestTune
fit$results
hat.train <- predict(fit$finalModel, X[train.idx,])
hat.test <- predict(fit$finalModel, X[!train.idx,])

fit2 <- glmnet(x = X[train.idx,],
               y = d$SalePrice[train.idx],
               standardize.response = TRUE, standardize = TRUE,
               lambda = fit$bestTune$lambda,
               alpha = fit$bestTune$alpha)

save.image('/home/egoren/el.Rdat')


####
localdir <- '/Users/emilygoren/Documents/School/ISU/Classes/STAT602/Kaggle/glmnet/'
load(paste0(localdir, 'el.Rdat'))

hat <- predict(fit$finalModel, X, exact = TRUE, s = fit$bestTune$lambda)
out <- data.frame(Id = (1:nrow(d))[!train.idx],
                  SalePrice = exp(hat[!train.idx]))
write.csv(out, row.names = FALSE, file = paste0(localdir, 'el-EMG-pred.csv'))
