require(caret)

## Get info from glmnet to modify.
glmnet_pp <- getModelInfo('glmnet')$glmnet 

## Rename model that is specified in model statement in train().
glmnet_pp$label <- 'glmnet_pp'

## Change fit function to include preprocessing.
glmnet_pp$fit <- function (x, y, wts, param, lev, last, classProbs, preProcFun = preproc, ...) # added fun option
{
  ###################### start added code
  x <- as.data.frame(x)
  x <- preProcFun(x) 
  x$tmpY <- 42
  tmpD <- x
  x <- model.matrix(tmpY ~ . + 0, data = tmpD)
  ###################### end added code
  
  numLev <- if (is.character(y) | is.factor(y)) 
    length(levels(y))
  else NA
  theDots <- list(...)
  if (all(names(theDots) != "family")) {
    if (!is.na(numLev)) {
      fam <- ifelse(numLev > 2, "multinomial", "binomial")
    }
    else fam <- "gaussian"
    theDots$family <- fam
  }
  if (!is.null(wts)) 
    theDots$weights <- wts
  if (!(class(x)[1] %in% c("matrix", "sparseMatrix"))) 
    x <- as.matrix(x)
  modelArgs <- c(list(x = x, y = y, alpha = param$alpha), theDots)
  #out <- do.call("glmnet", modelArgs)
  out <- glmnet(x = x, y = y, alpha = param$alpha)
  if (!is.na(param$lambda[1])) 
    out$lambdaOpt <- param$lambda[1]
  out
}

## Change predict function to include preprocessing.
glmnet_pp$predict <- function (modelFit, newdata, submodels = NULL, preProcFun = preproc) #added preproc 
{
  ###################### start added code
  newdata <- as.data.frame(newdata)
  newdata <- preProcFun(newdata)
  newdata$tmpY <- 42
  tmpD <- newdata
  newdata <- model.matrix(tmpY ~ . + 0, data = tmpD)
  ###################### end added code
  
  if (!is.matrix(newdata)) 
    newdata <- as.matrix(newdata)
  if (length(modelFit$obsLevels) < 2) {
    out <- predict(modelFit, newdata, s = modelFit$lambdaOpt)
  }
  else {
    out <- predict(modelFit, newdata, s = modelFit$lambdaOpt, 
                   type = "class")
  }
  if (is.matrix(out)) 
    out <- out[, 1]
  if (!is.null(submodels)) {
    if (length(modelFit$obsLevels) < 2) {
      tmp <- as.list(as.data.frame(predict(modelFit, newdata, 
                                           s = submodels$lambda)))
    }
    else {
      tmp <- predict(modelFit, newdata, s = submodels$lambda, 
                     type = "class")
      tmp <- if (is.matrix(tmp)) 
        as.data.frame(tmp, stringsAsFactors = FALSE)
      else as.character(tmp)
      tmp <- as.list(tmp)
    }
    out <- c(list(out), tmp)
  }
  out
}
