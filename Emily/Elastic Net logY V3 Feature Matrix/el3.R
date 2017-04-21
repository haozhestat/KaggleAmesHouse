require(glmnet)
require(caret)

### Put directory of file location here
dir <- '/home/egoren/'

## ----data----------------------------------------------------------------
d <- read.csv(paste0(dir, "X3.csv"))
options(na.action = 'na.pass')

## ----mod------------------------------------------------------------
X <- model.matrix(SalePrice ~ (.)^2 + 0, data = d)
train.idx <- !is.na(d$SalePrice)
set.seed(602)
fit <- train(x = X[train.idx,],
             y = d$SalePrice[train.idx],
             method = "glmnet",
             standardize.response = TRUE, standardize = TRUE,
             #preProcess = c("zv", "medianImpute"),
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

save.image('/home/egoren/el3.Rdat')

####
localdir <- '/Users/emilygoren/Documents/School/ISU/Classes/STAT602/Kaggle/glmnet/'
load(paste0(localdir, 'el3.Rdat'))

hat <- predict(fit$finalModel, X, exact = TRUE, s = fit$bestTune$lambda)
out <- data.frame(Id = (1:nrow(d))[!train.idx],
                  SalePrice = exp(hat[!train.idx]))
write.csv(out, row.names = FALSE, file = paste0(localdir, 'el-EMG-pred-v3.csv'))
