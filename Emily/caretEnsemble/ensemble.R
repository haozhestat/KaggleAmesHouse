require(glmnet)
require(xgboost)
require(caretEnsemble)
require(caret)

### Put directory of file location here
d <- read.csv('/Users/emilygoren/Documents/School/ISU/Classes/STAT602/Kaggle/featureMat_v3.csv')

# Is the case in the training set?

d.train <- subset(d, select = -Train, subset = Train == 1)
d.test <- subset(d, select = -Train, subset = Train != 1)

# Tune control.
my_tc <- trainControl(method = "repeatedcv", savePredictions = "final", 
                      number = 10, repeats = 10)

# Method specific options.
xgb.grid = expand.grid(nrounds = 750,
                       eta = 0.005,
                       max_depth = 4,
                       colsample_bytree = 1,
                       min_child_weight = 2,
                       subsample = 0.6,
                       gamma = 0.01)

my_tl <- list(
  gl = caretModelSpec(method = "glmnet", preProcess = c("center","scale"), 
                      tuneGrid = expand.grid(.alpha = seq(0, 0.3, by = 0.05),
                                             .lambda = seq(0.001, 0.1, by = 0.01))),
  pls = caretModelSpec(method = "pls", preProcess = c("center","scale"), 
                      tuneGrid = data.frame(.ncomp=seq(10:10))),
  xg = caretModelSpec(method = "xgbTree", preProcess = c("center","scale"), tuneGrid = xgb.grid)
)


model_list <- caretList(y = d.train$SalePrice,
                        x = subset(d.train, select = -SalePrice),
                        trControl = my_tc,
                        tuneList = my_tl)
set.seed(602)
fit <- caretEnsemble(model_list, trControl = my_tc)
summary(fit)
fit$error

X <- subset(rbind(d.train, d.test), select = -SalePrice)
newd <- data.frame(gl = predict(fit$models$gl, X),
                   pls = predict(fit$models$pls, X),
                   xg = predict(fit$models$xg, newdata = as.matrix(X)))
cor(newd)

beta <- coef(fit$ens_model$finalModel)
beta
hat <- as.matrix(cbind(1, newd)) %*% beta

plot((hat) ~ (rbind(d.train, d.test)$SalePrice), pch = 20)
abline(a = 0, b = 1, col = "red", lwd = 2)

out <- data.frame(Id = 1:length(hat), SalePrice = exp(hat))
write.csv(out[1461:2919, ], 'caretEnsemble.csv', row.names = FALSE)

