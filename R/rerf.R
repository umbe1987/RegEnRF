new_rerf <- function(x, ..., class = character()) {
  structure(x, class = "rerf")
}

validate_rerf <- function(x) {
  if (!all(c("lasso", "rf") %in% names(x))) {
    stop(
      "Not a valid `rerf` object"
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
#' `rerf()` implements Regression-Enhanced Random Forests algorithm (based on
#' Zhang et al., 2019 paper) for regression.
#'
#' @param x A numeric matrix of predictors. Requirement: nvars >1;
#'   in other words, x should have 2 or more columns. This is a constraint
#'   of [glmnet::glmnet()].
#' @param y A numeric response vector.
#' @param lambda See 'lambda' argument in [glmnet::glmnet()].
#' @param ntree See 'ntree' argument in [randomForest::randomForest()].
#' @param nodesize See 'nodesize' argument in [randomForest::randomForest()].
#' @param weights See 'weights' argument in [glmnet::glmnet()].
#' @param ... other arguments
#' @details This function is based on the packages `randomForest::randomForest`
#'   and `glmnet::glmnet`.
#' @author Umberto Minora \email{umbertofilippo@@tiscali.it}, based on the paper
#'   by Zhang et al. (2019).
#' @references Zhang, H., Nettleton, D., & Zhu, Z. (2019). Regression-enhanced
#'   random forests. arXiv preprint
#'   \url{https://doi.org/10.48550/arXiv.1904.10416}.
#' @returns An object with S3 class "rerf"
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' mod <- rerf(x, y, 0.1)
#' @export
rerf <- function(x, y, lambda, ntree = 500, nodesize = 5, weights = NULL, ...) {
  fit.lasso <- glmnet::glmnet(x = x, y = y, lambda = lambda, weights = weights)
  pred.lasso <- glmnet::predict.glmnet(fit.lasso, newx = x)
  res <- y - pred.lasso
  fit.rf <- randomForest::randomForest(x = x, y = res, ntree = ntree, nodesize = nodesize)
  fit <- list(lasso = fit.lasso, rf = fit.rf)
  validate_rerf(new_rerf(fit))
}

#' Prediction of test data using Regression-Enhanced Random Forests.
#'
#' @param object an object of class "rerf", as that created by the function rerf
#' @param newx matrix of new values for x at which predictions are to be made
#'   function will abort.
#' @return A vector of predicted values.
#' @examples
#' x <- matrix(rnorm(100 * 20), 100, 20)
#' y <- rnorm(100)
#' mod <- rerf(x, y, 0.1)
#' predict(mod, newx = x)
#' @exportS3Method stats::predict
predict.rerf <- function(object, newx) {
  if (missing(newx)) stop("You need to supply a value for 'newx'")
  stopifnot(inherits(object, "rerf"))

  pred.lasso <- glmnet::predict.glmnet(object$lasso, newx = newx)
  pred.rf <- randomForest:::predict.randomForest(object$rf, newdata = newx)
  pred.rerf <- pred.lasso + pred.rf

  return(pred.rerf)
}
