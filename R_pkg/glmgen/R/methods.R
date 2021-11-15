#' Get predictions from a trendfilter object
#'
#' @param obj
#'   Object of class `trendfilter`.
#' @param type
#'   Scale of the predictions.
#' @param lambdas
#'   (Optional) Vector of lambda values to calculate coefficients
#'   at. If missing, will use break points in the fit.
#' @param x.new
#'   Vector of new `x` points. Set to `NULL` (the default) to use the
#'   original locations.
#' @param zero_tol
#'   Numerical tolerance parameter, for determining whether a
#    coefficient should be rounded to zero.
#'
#' @export
predict.trendfilter <- function(obj, lambdas = NULL, x.new = NULL, zero_tol = 1e-6) {
  
  if (is.null(x.new)) x.new <- obj$x
  if (is.null(lambdas)) lambdas <- obj$lambdas
  
  co <- coef(object, lambdas)

  z <- .Call("tf_predict_R",
    sX = as.double(obj$x),
    sBeta = as.double(co),
    sN = length(obj$y),
    sK = as.integer(obj$k),
    sX0 = as.double(x.new),
    sN0 = length(x.new),
    sNLambda = length(lambdas),
    sFamily = 0L,
    sZeroTol = as.double(zero_tol),
    PACKAGE = "glmgen"
  )

  matrix(z, ncol = ncol(co), dimnames = list(NULL, colnames(co)))
}


#' Get coefficients from a trendfilter object
#'
#' @param obj
#'   Object of class `trendfilter`
#' @param lambdas
#'   (Optional) Vector of lambda values to calculate coefficients
#'   at. If missing, will use break points in the fit.
#'
#' @export
coef.trendfilter <- function(obj, lambdas = NULL) {
  # If no lambdas given, just return beta
  if (is.null(lambdas)) {
    return(obj$beta)
  }

  # If all lambdas are equal to some computed lambda, just
  # return propely transformed version of `obj$beta`
  if (all(!is.na(index <- match(lambdas, obj$lambdas)))) {
    return(obj$beta[, index, drop = FALSE])
  }

  if (min(lambdas) < 0) stop("All specified lambda values must be nonnegative.")
  if (min(lambdas) < min(obj$lambdas) | max(lambdas) > max(obj$lambdas)) {
    stop("Cannot predict lambda outside the range used when fitting.")
  }

  # If here, need to interpolate `lambdas`
  o <- order(lambdas, decreasing = TRUE)
  o2 <- order(obj$lambdas, decreasing = TRUE)
  lambdas <- lambdas[o]
  knots <- obj$lambdas[o2]
  k <- length(lambdas)
  mat <- matrix(rep(knots, each = k), nrow = k)
  b <- lambdas >= mat
  blo <- max.col(b, ties.method = "first")
  bhi <- pmax(blo - 1, 1)
  i <- bhi == blo
  p <- numeric(k)
  p[i] <- 0
  p[!i] <- ((lambdas - knots[blo]) / (knots[bhi] - knots[blo]))[!i]

  betas <- obj$beta[, o2, drop = FALSE]
  beta <- t((1 - p) * t(betas[, blo, drop = FALSE]) +
    p * t(betas[, bhi, drop = FALSE]))
  colnames(beta) <- as.character(round(lambdas, 3))

  beta[, order(o), drop = FALSE]
}


#' Print the output of a trendfilter object
#'
#' @param obj
#'   Object of class `trendfilter`
#'
#' @export
print.trendfilter <- function(obj) {
  cat("\nCall:\n")
  dput(obj$call)
  cat("\nOutput:\n")
  cat(paste0(
    class(obj), " model with ", length(obj$lambdas),
    " values of lambda.", "\n\n"
  ))
}


#' Summarize a trendfilter object
#'
#' @param obj
#'   Object of class `trendfilter`
#'
#' @export
summary.trendfilter <- function(obj) {
  df <- apply(obj$beta != 0, 2, sum)
  rss <- colSums((obj$y - predict(obj, type = "response"))^2)
  mat <- cbind(df, obj$lambdas, rss)
  rownames(mat) <- rep("", nrow(mat))
  colnames(mat) <- c("df", "lambdas", "rss")
  class(mat) <- "summary.trendfilter"
  mat
}


#' Print the output of a trendfilter summary object
#'
#' @param obj
#'   Object of class `trendfilter`
#'
#' @export
print.summary.trendfilter <- function(obj) {
  class(obj) <- "matrix"
  print(obj, digits = 4, print.gap = 3)
}
