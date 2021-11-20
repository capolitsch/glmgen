#' @useDynLib glmgen tf_R
#' @noRd
#' @export
tf_R_wrapper <- function(x,
                         y,
                         weights,
                         lambdas,
                         admm_params,
                         k = 2L) {
  lambda_min_ratio <- 0.5 * max(lambdas) / min(lambdas)

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
      sLambdaMinRatio = lambda_min_ratio,
      sVerbose = 0L,
      sControl = admm_params,
      PACKAGE = "glmgen"
    )
  )
}


#' @useDynLib glmgen thin_R
#' @importFrom tidyr tibble
#' @noRd
#' @export
thin_R_wrapper <- function(x,
                           y,
                           weights,
                           admm_params,
                           k = 2L) {
  mindx <- min(diff(x))

  if (mindx <= admm_params$x_tol) {
    c_thin <- .Call("thin_R",
      sX = x,
      sY = y,
      sW = weights,
      sN = length(y),
      sK = k,
      sControl = admm_params,
      PACKAGE = "glmgen"
    )

    return(tibble(x = c_thin$x, y = c_thin$y, weights = c_thin$w))
  } else {
    return(tibble(x = x, y = y, weights = weights))
  }
}


#' @useDynLib glmgen matMultiply_R
#' @noRd
#' @export
tfMultiply <- function(x, y, k = 2L) {
  z <- .Call("matMultiply_R",
    x = as.numeric(x),
    sB = as.numeric(y),
    sK = as.integer(k),
    sMatrixCode = 0L,
    PACKAGE = "glmgen"
  )

  z[1:(length(z) - k)]
}
