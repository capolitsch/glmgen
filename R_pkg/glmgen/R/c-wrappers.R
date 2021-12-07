#' Minimalist wrappers of some C functions from the `glmgen` package
#'
#' These wrappers are located within a fork of the `glmgen` package and are for
#' internal use only in the `trendfiltering` package.

#' @useDynLib glmgen tf_R
#' @noRd
.tf_fit <- function(x, y, weights, k = 2L, lambda, admm_params, ...) {
  invisible(
    .Call("tf_R",
      sX = as.double(x),
      sY = as.double(y),
      sW = as.double(weights),
      sN = length(y),
      sK = as.integer(k),
      sFamily = 0L,
      sMethod = 0L,
      sBeta0 = NULL,
      sLamFlag = 1L,
      sLambda = as.double(lambda),
      sNlambda = length(lambda),
      sLambdaMinRatio = as.double(0.5 * max(lambda) / min(lambda)),
      sVerbose = 0L,
      sControl = admm_params,
      PACKAGE = "glmgen"
    )
  )
}


#' @useDynLib glmgen thin_R
#' @noRd
.tf_thin <- function(x, y, weights, k = 2L, admm_params, ...) {
  invisible(
    .Call("thin_R",
          sX = as.double(x),
          sY = as.double(y),
          sW = as.double(weights),
          sN = length(y),
          sK = as.integer(k),
          sControl = admm_params,
          PACKAGE = "glmgen"
    )
  )
}


#' @useDynLib glmgen matMultiply_R
#' @noRd
.tf_multiply <- function(x, y, k = 2L, ...) {
  z <- .Call("matMultiply_R",
    x = as.double(x),
    sB = as.double(y),
    sK = as.integer(k),
    sMatrixCode = 0L,
    PACKAGE = "glmgen"
  )

  z[1:(length(z) - k)]
}


#' @useDynLib glmgen tf_predict_R
#' @noRd
.tf_predict <- function(obj, lambda, x_eval, coefs, zero_tol = 1e-6, ...) {
  invisible(
    .Call("tf_predict_R",
      sX = as.double(obj$x),
      sBeta = coefs,
      sN = length(obj$y),
      sK = as.integer(obj$k),
      sX0 = as.double(x_eval),
      sN0 = length(x_eval),
      sNLambda = length(lambda),
      sFamily = 0L,
      sZeroTol = as.double(zero_tol),
      PACKAGE = "glmgen"
    )
  )
}
