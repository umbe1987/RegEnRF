#' Regression-Enhanced Random Forests
#'
#' `rerf()` implements Regression-Enhanced Random Forests algorithm (based on
#' Zhang et al., 2019 paper) for regression.
#'
#' @param x A numeric matrix of predictors.
#' @param y A numeric response vector.
#' @param lambda See 'lambda' argument in [glmnet::glmnet()].
#' @param ntree See 'ntree' argument in [randomForest::randomForest()].
#' @param nodesize See 'nodesize' argument in [randomForest::randomForest()].
#' @param weights See 'weights' argument in [glmnet::glmnet()].
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
rerf <- function(x, y, lambda, ntree = 500, nodesize = 5, weights = NULL) {
  fit.lasso <- glmnet::glmnet(x = x, y = y, lambda = lambda, weights = weights)
  pred.lasso <- glmnet::predict.glmnet(fit.lasso, newx = x)
  res <- y - pred.lasso
  fit.rf <- randomForest::randomForest(x = x, y = res, ntree = ntree, nodesize = nodesize)
  fit <- list(fit.lasso = fit.lasso, fit.rf = fit.rf)
  class(fit) <- c(class(fit), "rerf")

  return(fit)
}
