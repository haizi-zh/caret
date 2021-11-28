#' Internal Functions
#' @name caret-internal
#' @aliases createModel resampleWrapper sortImp caretTheme progress hasTerms predictionFunction probFunction expandParameters flatTable MeanSD sbfIter gamFormula bagEarthStats cforestStats ipredStats rfStats well_numbered
#'
#' @description Internal functions
#'
#'
#' @author Max Kuhn, but \code{caretTheme} uses an expanded grid of the "Blues" palette designed by Cynthia Brewer and Mark Harrower
#'
#' @importFrom stats predict
#' @export
#' @keywords internal
"createModel" <-function(x, y, wts, method, tuneValue, obsLevels, pp = NULL, last = FALSE, sampling = NULL, classProbs, ...) {

  ## To get of warnings "some row.names duplicated: " when resampling with replacement
  if(is.data.frame(x) | is.matrix(x)) 
    rownames(x) <- make.names(rownames(x), unique = TRUE)

  if(!is.null(sampling) && sampling$first) {
    tmp <- sampling$func(x, y)
    x <- tmp$x
    y <- tmp$y
    rm(tmp)
  }

  if(!is.null(pp$options)) {
    pp$method <- pp$options
    pp$options <- NULL
    if("ica" %in% pp$method) pp$n.comp <- pp$ICAcomp
    pp$ICAcomp <- NULL
    pp$x <- x
    pp$outcome <- y
    ppObj <- do.call("preProcess", pp)
    ppObj$call <- "scrubed"
    x <- predict(ppObj, x)
    rm(pp)
  } else ppObj <- NULL

  if(!is.null(sampling) && !sampling$first) {
    tmp <- sampling$func(x, y)
    x <- tmp$x
    y <- tmp$y
    rm(tmp)
  }

  # With CV SBF enabled, it's possible the "effective" features make up a subset
  # of the original features
  ellipsis_args <- list(...)
  cvsbf <- ellipsis_args$cvsbf
  if (length(cvsbf) != 0) {
    retained <- sbfRetained(cvsbf, x, y)
    ## deal with zero length results
    x2 <- subset_x(x = x, ind = which(retained), by_column = TRUE)
    xNamesEffective <- colnames(x2)
  } else {
    x2 <- x
    xNamesEffective <- NULL
  }

  ellipsis_args$cvsbf <- NULL
  modelFit <- do.call(what = method$fit, args = c(
    list(
      x = x2,
      y = y,
      wts = wts,
      param = tuneValue,
      lev = obsLevels,
      last = last,
      classProbs = classProbs
    ),
    ellipsis_args
  ))

  ## for models using S4 classes, you can't easily append data, so
  ## exclude these and we'll use other methods to get this information
  if(is.null(method$label)) method$label <- ""
  if(!isS4(modelFit) &
       !(method$label %in% c("Ensemble Partial Least Squares Regression",
                             "Ensemble Partial Least Squares Regression with Feature Selection"))) {
    modelFit$xNames <- colnames(x)
    if (length(xNamesEffective) != 0)
      modelFit$xNamesEffective <- xNamesEffective
    modelFit$problemType <- if(is.factor(y)) "Classification" else "Regression"
    modelFit$tuneValue <- tuneValue
    modelFit$obsLevels <- obsLevels
    modelFit$param <- list(...)
  } else {
    attr(modelFit, "xNames") <- colnames(x)
    if (length(xNamesEffective) != 0)
      attr(modelFit, "xNamesEffective") <- xNamesEffective
  }

  list(fit = modelFit, preProc = ppObj)
}
