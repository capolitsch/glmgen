#' Fit a trend filtering model
#'
#' @param x
#'   vector of observed data locations, or when \code{y} is NULL, vector of
#'   observed responses.
#' @param y
#'   vector of observed reponses. If missing or `NULL`, the responses are assumed
#'   to be given through `x`, and the locations are assumed to be 1 through
#'   the length of `x`.
#' @param weights
#'   optional vector of sample weights. If missing, the weights will be assumed
#'   to be constant (unity) across all samples.
#' @param k
#'   the polynomial order of the trendfilter fit; a nonnegative integer (orders
#'   larger than 3 are not recommended). For instance, constant trend filtering
#'   (i.e., the fused lasso) uses `k` equal to 0, linear trend filtering uses
#'   `k` equal to 1, quadratic trend filtering uses `k` equal to 2, etc.
#' @param lambdas
#'   a sequence of lambda values at which to produce a fit. Can be left blank
#'   (highly recommended for general use), at which point the algorithm will
#'   determine appropriate lambda values.
#' @param nlambdas
#'   If `lambdas` is missing, this determines the number of lambda values
#'   dynamically constructed by the algorithm.
#' @param lambda.min.ratio
#'   if `lambdas` is missing, this determines the ratio between the largest
#'   and smallest `lambda` values. The values are evenly spaced on a log scale,
#'   so this ratio should typically be set fairly small.
#' @param thinning
#'   logical. If true, then the data are preprocessed so that a smaller, better
#'   conditioned data set is used for fitting. When set to `NULL`, the
#'   default, function will auto detect whether thinning should be applied
#'   (i.e., cases in which the numerical fitting algorithm will struggle to converge).
#' @param verbose
#'   logical. Should the function print out intermediate results as it is running.
#' @param control
#'   An optional named list of control parameters to pass to the underlying algorithm;
#'   see Details for more information. Names not matching any valid parameters
#'   will be silently ignored.
#'
#' @return An object of class `'trendfilter'`.
#' @author Taylor Arnold, Aaditya Ramdas, Veeranjaneyulu Sadhanala, Ryan Tibshirani
#'
#' @useDynLib glmgen thin_R tf_R
#'
#' @export
trendfilter <- function(x,
                        y,
                        weights,
                        k = 2L,
                        lambdas,
                        nlambdas = 50L,
                        lambda.min.ratio = 1e-5,
                        thinning = NULL,
                        verbose = FALSE,
                        control = trendfilter.control.list(x_tol = 1e-6 * max(IQR(x), diff(range(x)) / 2))) {
  cl <- match.call()
  n <- length(y)
  ord <- order(x)
  y <- y[ord]
  x <- x[ord]

  if (missing(weights)) weights <- rep(1L, length(y))
  weights <- weights[ord]

  mindx <- min(diff(x))
  if (!is.null(thinning) && !thinning && mindx == 0) {
    stop("Cannot pass duplicate x values; use observation weights, or use `thinning = TRUE`.")
  }

  # If the minimum difference between x points is < 1e-6 times the interquartile
  # range, then apply thinning, unless they explicitly tell us not to
  if (mindx <= control$x_tol) {
    if (!is.null(thinning) && !thinning) {
      warning(
        paste(
          "The x values are ill-conditioned. Consider thinning. \nSee",
          "?trendfilter for more info."
        )
      )
    } else {
      z <- .Call("thin_R",
        sX = as.double(x),
        sY = as.double(y),
        sW = as.double(weights),
        sN = length(y),
        sK = as.integer(k),
        sControl = control,
        PACKAGE = "glmgen"
      )
      x <- z$x
      y <- z$y
      weights <- z$w
      n <- z$n
    }
  }

  if (missing(lambdas)) {
    lambdas <- rep(0, nlambdas)
    lambda_flag <- FALSE
  } else {
    nlambdas <- length(lambdas)
    lambda_flag <- TRUE
  }

  if (!is.list(control) || (is.null(names(control)) && length(control) != 0L)) {
    stop("control must be a named list.")
  }

  control <- lapply(control, function(v) {
    ifelse(is.numeric(v),
      as.double(v[[1]]), stop("Elements of control must be numeric.")
    )
  })

  c_out <- .Call("tf_R",
    sX = as.double(x),
    sY = as.double(y),
    sW = as.double(weights),
    sN = length(y),
    sK = as.integer(k),
    sFamily = 0L,
    sMethod = 0L,
    sBeta0 = NULL,
    sLamFlag = as.integer(lambda_flag),
    sLambda = as.double(lambdas),
    sNlambda = as.integer(nlambdas),
    sLambdaMinRatio = as.double(lambda.min.ratio),
    sVerbose = as.integer(verbose),
    sControl = control,
    PACKAGE = "glmgen"
  )

  colnames(c_out$beta) <- as.character(round(c_out$lambda, 3))

  structure(
    list(
      x = x,
      y = y,
      weights = weights,
      k = as.integer(k),
      lambdas = c_out$lambda,
      edfs = c_out$df,
      beta = c_out$beta,
      p = length(y),
      m = length(y) - as.integer(k) - 1L,
      obj = c_out$obj,
      status = c_out$status,
      iter = c_out$iter,
      call = cl
    ),
    class = "trendfilter"
  )
}

#' Control list for tuning trend filtering algorithm
#'
#' @param rho
#'  this is a scaling factor for the augmented Lagrangian parameter in the ADMM
#'  algorithm. To solve a given trend filtering problem with locations `x`
#'  at a tuning parameter value `lambdas`, the augmented Lagrangian parameter
#'  is set to be `rho * lambda * ((max(x)-min(x))/n)^k`.
#' @param obj_tol
#'  the tolerance used in the stopping criterion; when the relative change in
#'  objective values is less than this value, the algorithm terminates.
#' @param max_iter
#'  number of ADMM iterations used; ignored for `k = 0`.
#' @param x_tol
#'  defines uniqueness or sameness of x's. If we make bins of size `x_tol` and
#'  find at least two x's which fall into the same bin, then we thin the data.
#'
#' @return a list of parameters.
#' @export
trendfilter.control.list <- function(rho = 1,
                                     obj_tol = 1e-5,
                                     obj_tol_newton = obj_tol,
                                     max_iter = 200L,
                                     max_iter_newton = 50L,
                                     x_tol = 1e-6,
                                     alpha_ls = 0.5,
                                     gamma_ls = 0.8,
                                     max_iter_ls = 30L,
                                     tridiag = 0) {
  list(
    rho = rho,
    obj_tol = obj_tol,
    obj_tol_newton = obj_tol_newton,
    max_iter = max_iter,
    max_iter_newton = max_iter_newton,
    x_tol = x_tol,
    alpha_ls = alpha_ls,
    gamma_ls = gamma_ls,
    max_iter_ls = max_iter_ls,
    tridiag = tridiag
  )
}

#' Multiply a vector by trendfilter matricies
#'
#' @param x
#'   Vector of observed inputs
#' @param y
#'   Numeric vector to which the multiplication should be supplied.
#' @param k
#'   Order of the trendfiltering matrix.
#'
#' @return A numeric vector with the result of the multiplication.
#' @seealso [trendfilter()]
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
