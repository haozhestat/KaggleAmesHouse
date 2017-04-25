require(caret)

## Get info from xgboost to modify.
xgbTree_pp <- getModelInfo('xgbTree')$xgbTree

## Rename model that is specified in model statement in train().
xgbTree_pp$label <- 'xgbTree_pp'

## Change fit function to include preprocessing.
xgbTree_pp$fit <- function(x, y, wts, param, lev, last, classProbs, preProcFun = preprocXGB, ...) {
  x <- preProcFun(x) ### added this line
  if (!inherits(x, "xgb.DMatrix")) 
    x <- as.matrix(x)        
  if (is.factor(y)) {
    if (length(lev) == 2) {
      y <- ifelse(y == lev[1], 1, 0)
      if (!inherits(x, "xgb.DMatrix")) 
        x <- xgb.DMatrix(x, label = y, missing = NA)
      else setinfo(x, "label", y)
      if (!is.null(wts)) 
        setinfo(x, "weight", wts)
      out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
                            gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
                            min_child_weight = param$min_child_weight, subsample = param$subsample), 
                       data = x, nrounds = param$nrounds, objective = "binary:logistic", 
                       ...)
    }
    else {
      y <- as.numeric(y) - 1
      if (!inherits(x, "xgb.DMatrix")) 
        x <- xgb.DMatrix(x, label = y, missing = NA)
      else setinfo(x, "label", y)
      if (!is.null(wts)) 
        setinfo(x, "weight", wts)
      out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
                            gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
                            min_child_weight = param$min_child_weight, subsample = param$subsample), 
                       data = x, num_class = length(lev), nrounds = param$nrounds, 
                       objective = "multi:softprob", ...)
    }
  }
  else {
    if (!inherits(x, "xgb.DMatrix")) 
      x <- xgb.DMatrix(x, label = y, missing = NA)
    else setinfo(x, "label", y)
    if (!is.null(wts)) 
      setinfo(x, "weight", wts)
    out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
                          gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
                          min_child_weight = param$min_child_weight, subsample = param$subsample), 
                     data = x, nrounds = param$nrounds, objective = "reg:linear", 
                     verbose = T, nthread = 3)
  }
  out
}


## Change fit function to include preprocessing.
xgbTree_pp$predict <- function(modelFit, newdata, submodels = NULL, preProcFun = preprocXGB){
  newdata <- preProcFun(newdata) ### Added this line.
  if (!inherits(newdata, "xgb.DMatrix")) {
    newdata <- as.matrix(newdata)
    newdata <- xgb.DMatrix(data = newdata, missing = NA)
  }
  out <- predict(modelFit, newdata)
  if (modelFit$problemType == "Classification") {
    if (length(modelFit$obsLevels) == 2) {
      out <- ifelse(out >= 0.5, modelFit$obsLevels[1], 
                    modelFit$obsLevels[2])
    }
    else {
      out <- matrix(out, ncol = length(modelFit$obsLevels), 
                    byrow = TRUE)
      out <- modelFit$obsLevels[apply(out, 1, which.max)]
    }
  }
  if (!is.null(submodels)) {
    tmp <- vector(mode = "list", length = nrow(submodels) + 
                    1)
    tmp[[1]] <- out
    for (j in seq(along = submodels$nrounds)) {
      tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
      if (modelFit$problemType == "Classification") {
        if (length(modelFit$obsLevels) == 2) {
          tmp_pred <- ifelse(tmp_pred >= 0.5, modelFit$obsLevels[1], 
                             modelFit$obsLevels[2])
        }
        else {
          tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
                             byrow = TRUE)
          tmp_pred <- modelFit$obsLevels[apply(tmp_pred, 
                                               1, which.max)]
        }
      }
      tmp[[j + 1]] <- tmp_pred
    }
    out <- tmp
  }
  out
}