require(caret)

## Get info from pls to modify.
pls_pp <- getModelInfo('pls')$pls

## Rename model that is specified in model statement in train().
pls_pp$label <- 'pls_pp'

## Change fit function to include preprocessing.
pls_pp$fit <- function (x, y, wts, param, lev, last, classProbs, preProcFun = preproc, ...) # added fun option
{
  x <- preProcFun(x) ### added this line
  out <- if (is.factor(y)) {
    plsda(x, y, method = "oscorespls", ncomp = param$ncomp, 
          ...)
  }
  else {
    dat <- if (is.data.frame(x)) 
      x
    else as.data.frame(x)
    dat$.outcome <- y
    plsr(.outcome ~ ., data = dat, method = "oscorespls", 
         ncomp = param$ncomp, ...)
  }
  out
}


## Change fit function to include preprocessing.
pls_pp$predict <- function (modelFit, newdata, submodels = NULL, preProcFun = preproc) # added fun option
{
  newdata <- preProcFun(newdata) ### Added this line.
  out <- if (modelFit$problemType == "Classification") {
    if (!is.matrix(newdata)) 
      newdata <- as.matrix(newdata)
    out <- predict(modelFit, newdata, type = "class")
  }
  else as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))
  if (!is.null(submodels)) {
    tmp <- vector(mode = "list", length = nrow(submodels))
    if (modelFit$problemType == "Classification") {
      if (length(submodels$ncomp) > 1) {
        tmp <- as.list(predict(modelFit, newdata, ncomp = submodels$ncomp))
      }
      else tmp <- list(predict(modelFit, newdata, ncomp = submodels$ncomp))
    }
    else {
      tmp <- as.list(as.data.frame(apply(predict(modelFit, 
                                                 newdata, ncomp = submodels$ncomp), 3, function(x) list(x))))
    }
    out <- c(list(out), tmp)
  }
  out
}