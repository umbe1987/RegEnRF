new_RegEnRF <- function(x, ..., class = character()) {
  structure(x, class = "RegEnRF")
}

validate_RegEnRF <- function(x) {
  if (!all(c("lasso", "rf") %in% names(x))) {
    stop(
      "Not a valid `RegEnRF` object"
    )
  }

  if (!inherits(x$lasso, "elnet")) {
    stop(
      "`lasso` must be of class `elnet`",
      call. = FALSE
    )
  }
  if (!inherits(x$rf, "randomForest")) {
    stop(
      "`rf` must be of class `randomForest`",
      call. = FALSE
    )
  }

  x
}

#' Regression-Enhanced Random Forests
#'
#' `RegEnRF()` implements Regression-Enhanced Random Forests algorithm (based on
#' Zhang et al., 2019 paper) for regression.
#'
#' @param x A numeric matrix of predictors. Requirement: nvars >1;
#'   in other words, x should have 2 or more columns. This is a constraint
#'   of [glmnet::glmnet()].
#' @param y A numeric response vector.
#' @param lambda See 'lambda' argument in [glmnet::glmnet()].
#' @param ... other arguments passed to [glmnet::glmnet()] and
#'   [randomForest::randomForest()].
#' @details This function is based on the packages `randomForest::randomForest`
#'   and `glmnet::glmnet`.
#' @author Umberto Minora \email{umbertofilippo@@tiscali.it}, based on the paper
#'   by Zhang et al. (2019).
#' @references Zhang, H., Nettleton, D., & Zhu, Z. (2019). Regression-enhanced
#'   random forests. arXiv preprint
#'   \url{https://doi.org/10.48550/arXiv.1904.10416}.
#' @returns An object with S3 class "RegEnRF"
#' @examples
#' set.seed(111)
#' data(co2)
#' x <- matrix(c(time(co2), cycle(co2)), ncol = 2)
#' y <- as.numeric(co2)
#' mod <- RegEnRF(x, y, lambda = 0.1)
#' freq <- frequency(co2)
#' startt <- tsp(co2)[2] + 1 / freq
#' xnew.t <- seq(startt, by = 1 / freq, length.out = freq * 3)
#' xnew <- matrix(c(xnew.t, cycle(tail(co2, freq * 3))), ncol = 2)
#' pred <- predict(mod, xnew)
#' pred.ts <- ts(pred, start = startt, frequency = freq)
#' plot(ts.union(co2, pred.ts), plot.type = "single", col = c("black", "red"))
#' @export
RegEnRF <- function(x, y, lambda, ...) {
  fit.lasso <- glmnet::glmnet(x = x, y = y, lambda = lambda, ...)
  pred.lasso <- glmnet::predict.glmnet(fit.lasso, newx = x)
  res <- y - pred.lasso
  fit.rf <- randomForest::randomForest(x = x, y = res, ...)
  fit <- list(lasso = fit.lasso, rf = fit.rf)
  validate_RegEnRF(new_RegEnRF(fit))
}

#' Prediction of test data using Regression-Enhanced Random Forests.
#'
#' @param object an object of class "RegEnRF", as that created by the function RegEnRF
#' @param newx matrix of new values for x at which predictions are to be made
#'   function will abort.
#' @param ... other arguments passed to [glmnet::predict.glmnet] and
#'   randomForest:::predict.randomForest.
#' @return A vector of predicted values.
#' @examples
#' set.seed(111)
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' mod <- RegEnRF(x, y, lambda = 0.1)
#' predict(mod, newx = x)
#' @import randomForest
#' @importFrom stats predict
#' @exportS3Method stats::predict
predict.RegEnRF <- function(object, newx, ...) {
  if (missing(newx)) stop("You need to supply a value for 'newx'")
  stopifnot(inherits(object, "RegEnRF"))

  pred.lasso <- glmnet::predict.glmnet(object$lasso, newx = newx, ...)
  pred.rf <- predict(object$rf, newdata = newx, ...)
  pred.RegEnRF <- pred.lasso + pred.rf

  return(pred.RegEnRF)
}
