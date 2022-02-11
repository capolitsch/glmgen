#' Minimalist wrappers of some C functions from the `glmgen` package
#'
#' These wrappers are located within a fork of the `glmgen` package and are for
#' internal use only in the `trendfiltering` package.

#' @useDynLib glmgen tf_R
#' @noRd
#' @importFrom rlang %||%
#' @export
.tf_fit <- function(x,
                    y,
                    weights,
                    k = 2L,
                    admm_params,
                    lambda = NULL,
                    nlambda = NULL,
                    lambda_min_ratio = 1e-16,
                    ...) {

  if (is.null(lambda)) {
    nlambda <- nlambda %||% 250L
    lambda <- rep(1L, nlambda)
    slambda_flag <- FALSE
  } else {
    slambda_flag <- TRUE
  }

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
      sLamFlag = as.integer(slambda_flag),
      sLambda = as.double(lambda),
      sNlambda = length(lambda),
      sLambdaMinRatio = as.double(lambda_min_ratio),
      sVerbose = 0L,
      sControl = admm_params,
      PACKAGE = "glmgen"
    )
  )
}


#' @useDynLib glmgen thin_R
#' @noRd
#' @export
.tf_thin <- function(x, y, weights, k, admm_params, ...) {
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
#' @export
.tf_multiply <- function(x, y, k = 2L, ...) {
  mult_out <- .Call("matMultiply_R",
    x = as.double(x),
    sB = as.double(y),
    sK = as.integer(k),
    sMatrixCode = 0L,
    PACKAGE = "glmgen"
  )

  mult_out[1:(length(mult_out) - k)]
}


#' @useDynLib glmgen tf_predict_R
#' @noRd
#' @export
.tf_predict <- function(obj,
                        lambda,
                        x_eval,
                        fitted_values,
                        zero_tol,
                        ...) {
  p <- .Call("tf_predict_R",
    sX = as.double(obj$x / obj$scale_xy["x"]),
    sBeta = as.double(fitted_values / obj$scale_xy["y"]),
    sN = length(obj$y),
    sK = as.integer(obj$k),
    sX0 = as.double(x_eval / obj$scale_xy["x"]),
    sN0 = length(x_eval),
    sNLambda = length(lambda),
    sFamily = 0L,
    sZeroTol = as.double(zero_tol),
    PACKAGE = "glmgen"
  )
  p * as.numeric(obj$scale_xy["y"])
}


#' @useDynLib glmgen tf_predict_R
#' @noRd
#' @export
.tf_boot <- function(x,
                     x_eval,
                     fitted_values,
                     k,
                     zero_tol,
                     ...) {
  .Call("tf_predict_R",
        sX = as.double(x),
        sBeta = as.double(fitted_values),
        sN = length(x),
        sK = as.integer(k),
        sX0 = as.double(x_eval),
        sN0 = length(x_eval),
        sNLambda = 1L,
        sFamily = 0L,
        sZeroTol = as.double(zero_tol),
        PACKAGE = "glmgen"
  )
}
