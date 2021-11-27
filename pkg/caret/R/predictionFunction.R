#' @rdname caret-internal
#' @importFrom stats predict
#' @export
predictionFunction <- function(method, modelFit, newdata, preProc = NULL, param = NULL)
{
  if(!is.null(newdata) && !is.null(preProc)) newdata <- predict(preProc, newdata)

  # Be aware of potential feature reduction
  if (isS4(modelFit)) {
    features <- attr(modelFit, "xNamesEffective")
  } else {
    features <- modelfit[["xNamesEffective"]]
  }

  if (!is.null(features)) {
    newdata <- newdata[, features, drop = FALSE]
  }

  out <- method$predict(modelFit = modelFit,
                        newdata = newdata,
                        submodels = param)
  ## TODO convert to character with classification
  out
}


