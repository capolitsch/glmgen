#' @useDynLib glmgen tf_R
#' @noRd
#' @export
tf_fit <- function(x, y, weights, lambdas, admm_params, k = 2L) {
  invisible(
    .Call("tf_R",
      sX = x,
      sY = y,
      sW = weights,
      sN = length(y),
      sK = k,
      sFamily = 0L,
      sMethod = 0L,
      sBeta0 = NULL,
      sLamFlag = 1L,
      sLambda = lambdas,
      sNlambda = length(lambdas),
      sLambdaMinRatio = 0.5 * max(lambdas) / min(lambdas),
      sVerbose = 0L,
      sControl = admm_params,
      PACKAGE = "glmgen"
    )
  )
}


#' @useDynLib glmgen thin_R
#' @noRd
#' @export
tf_thin <- function(x, y, weights, admm_params, k = 2L) {
  invisible(
    .Call("thin_R",
      sX = x,
      sY = y,
      sW = weights,
      sN = length(y),
      sK = k,
      sControl = admm_params,
      PACKAGE = "glmgen"
    )
  )
}


#' @useDynLib glmgen matMultiply_R
#' @noRd
#' @export
tf_multiply <- function(x, y, k = 2L) {
  z <- .Call("matMultiply_R",
    x = x,
    sB = y,
    sK = k,
    sMatrixCode = 0L,
    PACKAGE = "glmgen"
  )

  z[1:(length(z) - k)]
}


#' @useDynLib glmgen tf_predict_R
#' @noRd
#' @export
tf_predict <- function(obj, lambdas, x_eval, coefs, zero_tol = 1e-6) {
  invisible(
    .Call("tf_predict_R",
      sX = obj$x,
      sBeta = coefs,
      sN = length(obj$y),
      sK = obj$k,
      sX0 = x_eval,
      sN0 = length(x_eval),
      sNLambda = length(lambdas),
      sFamily = 0L,
      sZeroTol = zero_tol,
      PACKAGE = "glmgen"
    )
  )
}
