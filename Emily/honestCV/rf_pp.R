require(caret)

## Get info from rf to modify.
rf_pp <- getModelInfo('rf')$rf

## Rename model that is specified in model statement in train().
rf_pp$label <- 'rf_pp'

## Change fit function to include preprocessing.
rf_pp$fit <- function (x, y, wts, param, lev, last, classProbs, preProcFun = preproc, ...) # added fun option
{
  x <- preProcFun(x) #### added this line
  fit <- randomForest(x, y, mtry = param$mtry, ...)
  return(fit)
}

## Change fit function to include preprocessing.
rf_pp$predict <- function (modelFit, newdata, submodels = NULL, preProcFun = preproc) # added fun option
{
  if (is.null(newdata)) {
    out <- predict(modelFit)
  } else {
    newdata <- preProcFun(newdata)
    out <- predict(modelFit, newdata)
  }
  return(out)
}  